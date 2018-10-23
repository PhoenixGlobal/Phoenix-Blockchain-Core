package com.apex.core

import java.nio.file.Path

import com.apex.common.ApexLogging
import com.apex.consensus.ProducerUtil
import com.apex.settings.{ChainSettings, ConsensusSettings}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey, PublicKeyHash}
import com.apex.crypto.{BinaryData, Crypto, Fixed8, MerkleTree, UInt160, UInt256}
import com.apex.storage.{Batch, LevelDbStorage}
import org.iq80.leveldb.WriteBatch
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map, Set}
import scala.collection.immutable

trait Blockchain extends Iterable[Block] with ApexLogging {
  def getLatestHeader: BlockHeader

  def getHeight(): Int

  def getHeadTime(): Long

  def headTimeSinceGenesis(): Long

  def getHeader(id: UInt256): Option[BlockHeader]

  def getHeader(index: Int): Option[BlockHeader]

  def getNextBlockId(id: UInt256): Option[UInt256]

  def getBlock(height: Int): Option[Block]

  def getBlock(id: UInt256): Option[Block]

  //def getBlockInForkBase(id: UInt256): Option[Block]

  def containBlock(id: UInt256): Boolean

  def getPendingTransaction(txid: UInt256): Option[Transaction]

  //  def produceBlock(producer: PublicKey, privateKey: PrivateKey, timeStamp: Long,
  //                   transactions: Seq[Transaction]): Option[Block]

  def startProduceBlock(producer: PublicKey)

  def addTransaction(tx: Transaction): Boolean

  def produceBlockFinalize(producer: PublicKey, privateKey: PrivateKey, timeStamp: Long): Option[Block]

  def isProducingBlock(): Boolean

  def tryInsertBlock(block: Block, doApply: Boolean): Boolean

  //
  //  def getTransaction(id: UInt256): Option[Transaction]
  //
  //  def containsTransaction(id: UInt256): Boolean

  //def verifyBlock(block: Block): Boolean

  //def verifyTransaction(tx: Transaction): Boolean

  def getBalance(address: UInt160): Option[collection.immutable.Map[UInt256, Long]]

  def getAccount(address: UInt160): Option[Account]

  def getGenesisBlockChainId: String

  def close()
}

object Blockchain {
  private var chain: LevelDBBlockchain = null

  //  final val Current: Blockchain = new LevelDBBlockchain()
  def populate(chainSettings: ChainSettings, consensusSettings: ConsensusSettings): LevelDBBlockchain = {
    val populateChain = new LevelDBBlockchain(chainSettings, consensusSettings)
    chain = populateChain
    populateChain
  }

  def getLevelDBBlockchain: LevelDBBlockchain = chain
}

class LevelDBBlockchain(chainSettings: ChainSettings, consensusSettings: ConsensusSettings) extends Blockchain {
  private val genesisProducer = PublicKey(BinaryData(chainSettings.genesis.publicKey)) // TODO: read from settings
  private val genesisProducerPrivKey = new PrivateKey(BinaryData(chainSettings.genesis.privateKey))

  private val blockBase = new BlockBase(chainSettings.blockBase)

  private val dataBase = new DataBase(chainSettings.dataBase)

  private val forkBase = new ForkBase(
    chainSettings.forkBase,
    consensusSettings.initialWitness,
    onConfirmed,
    onSwitch)

  // TODO: zero is not a valid pub key, need to work out other method
  private val minerCoinFrom = PublicKey(BinaryData(chainSettings.minerCoinFrom)) // 33 bytes pub key
  private val minerAward = Fixed8.fromDecimal(chainSettings.minerAward)

  private val genesisCoinToAddress = PublicKeyHash.fromAddress(chainSettings.genesis.coinToAddr).get
  private val genesisTx = new Transaction(TransactionType.Miner, minerCoinFrom,
    genesisCoinToAddress, "", minerAward, UInt256.Zero, 0, BinaryData.empty, BinaryData.empty)

  private val genesisBlockHeader: BlockHeader = BlockHeader.build(0,
    chainSettings.genesis.timeStamp, UInt256.Zero, UInt256.Zero,
    genesisProducer, genesisProducerPrivKey)
  private val genesisBlock: Block = Block.build(genesisBlockHeader, Seq(genesisTx))

  private var latestHeader: BlockHeader = genesisBlockHeader

  //  private var latestProdState: ProducerStatus = null

  private val pendingTxs = ArrayBuffer.empty[Transaction] //TODO: change to seq map // TODO: save to DB?
  private val unapplyTxs = mutable.Map.empty[UInt256, Transaction] // TODO: save to DB?

  populate()

  override def getGenesisBlockChainId: String = genesisBlockHeader.id.toString

  override def iterator: Iterator[Block] = new BlockchainIterator(this)

  override def close() = {
    log.info("blockchain closing")
    blockBase.close()
    dataBase.close()
    forkBase.close()
    log.info("blockchain closed")
  }

  override def getHeight(): Int = {
    forkBase.head.map(_.block.height).getOrElse(genesisBlockHeader.index)
  }

  override def getHeadTime(): Long = {
    //    latestHeader.timeStamp
    forkBase.head.map(_.block.timeStamp).getOrElse(0)
  }

  override def getLatestHeader(): BlockHeader = {
    forkBase.head.map(_.block.header).getOrElse(genesisBlockHeader)
  }

  override def headTimeSinceGenesis(): Long = {
    getLatestHeader.timeStamp - genesisBlockHeader.timeStamp
  }

  override def getHeader(id: UInt256): Option[BlockHeader] = {
    forkBase.get(id).map(_.block.header).orElse(blockBase.getBlock(id).map(_.header))
  }

  override def getHeader(height: Int): Option[BlockHeader] = {
    forkBase.get(height).map(_.block.header).orElse(blockBase.getBlock(height).map(_.header))
  }

  override def getNextBlockId(id: UInt256): Option[UInt256] = {
    var target: Option[UInt256] = None
    val block = getBlock(id)
    if (block.isDefined) {
      val nextBlock = getBlock(block.get.height() + 1)
      if (nextBlock.isDefined)
        target = Some(nextBlock.get.id())
    }
    if (target == None) {
      target = forkBase.getNext(id)
    }
    target
  }

  override def getBlock(id: UInt256): Option[Block] = {
    forkBase.get(id).map(_.block).orElse(blockBase.getBlock(id))
  }

  override def getBlock(height: Int): Option[Block] = {
    forkBase.get(height).map(_.block).orElse(blockBase.getBlock(height))
  }

  //  override def getBlockInForkBase(id: UInt256): Option[Block] = {
  //    forkBase.get(id).map(_.block.id).flatMap(getBlock)
  //  }

  override def containBlock(id: UInt256): Boolean = {
    forkBase.contains(id) || blockBase.containBlock(id)
  }

  def getPendingTransaction(txid: UInt256): Option[Transaction] = {
    if (pendingTxs.map(_.id()).contains(txid)) {
      pendingTxs.find(tx => tx.id().equals(txid))
    }
    else {
      unapplyTxs.get(txid)
    }
  }

  override def startProduceBlock(producer: PublicKey) = {
    require(pendingTxs.isEmpty)

    val forkHead = forkBase.head.get
    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
      producer.pubKeyHash, "", minerAward, UInt256.Zero,
      forkHead.block.height + 1,
      BinaryData(Crypto.randomBytes(8)), // add random bytes to distinct different blocks with same block index during debug in some cases
      BinaryData.empty
    )
    //isPendingBlock = true
    dataBase.startSession()

    val applied = applyTransaction(minerTx)
    require(applied)
    pendingTxs.append(minerTx)

    unapplyTxs.foreach(p => addTransaction(p._2))

    pendingTxs.foreach(tx => unapplyTxs.remove(tx.id))
  }

  override def isProducingBlock(): Boolean = {
    !pendingTxs.isEmpty
  }

  override def addTransaction(tx: Transaction): Boolean = {
    if (isProducingBlock()) {
      if (applyTransaction(tx)) {
        pendingTxs.append(tx)
        true
      }
      else
        false
    }
    else {
      if (!unapplyTxs.contains(tx.id)) {
        unapplyTxs += (tx.id -> tx)
      }
      true
    }
  }

  override def produceBlockFinalize(producer: PublicKey, privateKey: PrivateKey,
                                    timeStamp: Long): Option[Block] = {
    require(!pendingTxs.isEmpty)

    val forkHead = forkBase.head.get
    val merkleRoot = MerkleTree.root(pendingTxs.map(_.id))

    val header = BlockHeader.build(
      forkHead.block.height + 1, timeStamp, merkleRoot,
      forkHead.block.id, producer, privateKey)
    val block = Block.build(header, pendingTxs.clone)
    pendingTxs.clear()
    if (tryInsertBlock(block, false)) {
      Some(block)
    } else {
      None
    }
  }

  override def tryInsertBlock(block: Block, doApply: Boolean): Boolean = {
    var inserted = false

    if (!pendingTxs.isEmpty) {
      pendingTxs.foreach(tx => {
        if (tx.txType != TransactionType.Miner)
          unapplyTxs += (tx.id -> tx)
      })
      pendingTxs.clear()

      dataBase.rollBack()
    }

    if (forkBase.head.get.block.id.equals(block.prev())) {
      if (doApply == false) { // check first !
        if (forkBase.add(block)) {
          inserted = true
          latestHeader = block.header
        }
        else
          log.error(s"Error during forkBase add block ${block.height()}  ${block.id()}")
      }
      else if (applyBlock(block)) {
        if (forkBase.add(block)) {
          inserted = true
          latestHeader = block.header
        }
        else
          log.error(s"Error during forkBase add block ${block.height()}  ${block.id()}")
      }
      else {
        log.info(s"block ${block.height} ${block.id} apply error")
        //forkBase.removeFork(block.id)
      }
    }
    else {
      log.info(s"received block try add to minor fork chain. block ${block.height} ${block.id}")
      if (forkBase.add(block))
        inserted = true
      else
        log.info("add fail")
    }
    if (inserted) {
      block.transactions.foreach(tx => unapplyTxs.remove(tx.id))
    }
    inserted
  }

  private def applyBlock(block: Block): Boolean = {
    var applied = true
    //    if (isPendingBlock) {
    //      rollBack()
    //    }
    if (verifyBlock(block)) {
      dataBase.startSession()
      block.transactions.foreach(tx => {
        if (!applyTransaction(tx))
          applied = false
      })
    }
    else
      applied = false
    if (!applied) {
      log.info(s"Block apply fail #${block.height()} ${block.id()}")
      //TODO: dataBase.rollBack() ?
    }
    applied
  }

  private def applyTransaction(tx: Transaction): Boolean = {
    var txValid = true

    val fromAccount = dataBase.getAccount(tx.fromPubKeyHash()).getOrElse(new Account(true, "", immutable.Map.empty[UInt256, Fixed8], 0))
    val toAccount = dataBase.getAccount(tx.toPubKeyHash).getOrElse(new Account(true, "", immutable.Map.empty[UInt256, Fixed8], 0))

    if (tx.txType == TransactionType.Miner) {
      // TODO
    }
    else {
      if (!fromAccount.balances.contains(tx.assetId))
        txValid = false
      else if (tx.amount > fromAccount.balances(tx.assetId))
        txValid = false
      else if (tx.nonce != fromAccount.nextNonce)
        txValid = false
    }

    if (txValid) {
      val fromBalance = (fromAccount.balances.toSeq ++ Seq((tx.assetId, -tx.amount))).groupBy(_._1)
        .map(p => (p._1, Fixed8.sum(p._2.map(_._2).sum)))
        .filter(_._2.value > 0)
      val toBalance = (toAccount.balances.toSeq ++ Seq((tx.assetId, tx.amount))).groupBy(_._1)
        .map(p => (p._1, Fixed8.sum(p._2.map(_._2).sum)))
        .filter(_._2.value > 0)

      dataBase.setAccount((tx.fromPubKeyHash(), new Account(true, fromAccount.name, fromBalance, fromAccount.nextNonce + 1)),
        (tx.toPubKeyHash, new Account(true, toAccount.name, toBalance, toAccount.nextNonce)))
    }
    txValid
  }

  private def verifyBlock(block: Block): Boolean = {
    if (!verifyHeader(block.header))
      false
    else if (block.transactions.size == 0) {
      log.info("verifyBlock error: block.transactions.size == 0")
      false
    }
    else if (!block.merkleRoot().equals(block.header.merkleRoot))
      false
    else if (!block.transactions.forall(verifyTransaction))
      false
    else if (!verifyRegisterNames(block.transactions))
      false
    else
      true
  }

  // TODO: merge with applyTransaction()
  private def verifyTransaction(tx: Transaction): Boolean = {
    def checkAmount(): Boolean = {
      // TODO
      true
    }

    if (tx.txType == TransactionType.Miner) {
      // TODO check miner and only one miner tx
      true
    }
    else {
      var isValid = tx.verifySignature()
      // More TODO
      isValid && checkAmount()
    }
  }

  private def verifyHeader(header: BlockHeader): Boolean = {
    val prevBlock = forkBase.get(header.prevBlock)
    if (prevBlock.isEmpty) {
      log.info("verifyHeader error: prevBlock not found")
      false
    }
    else if (header.index != prevBlock.get.block.height() + 1) {
      log.info(s"verifyHeader error: index error ${header.index} ${prevBlock.get.block.height()}")
      false
    }
    else if (!ProducerUtil.isProducerValid(header.timeStamp, header.producer, consensusSettings)) {
      log.info("verifyHeader error: producer not valid")
      false
    }
    else if (!header.verifySig()) {
      log.info("verifyHeader error: verifySig fail")
      false
    }
    else {
      // verify merkleRoot in verifyBlock()
      true
    }
  }

  private def verifyRegisterNames(transactions: Seq[Transaction]): Boolean = {
    var isValid = true
    val newNames = Set.empty[String]
    val registers = Set.empty[UInt160]
    transactions.foreach(tx => {
      if (tx.txType == TransactionType.RegisterName) {
        val name = new String(tx.data, "UTF-8")
        if (name.length != 10) // TODO: read "10" from config file
          isValid = false
        if (newNames.contains(name))
          isValid = false
        if (registers.contains(tx.fromPubKeyHash()))
          isValid = false
        newNames.add(name)
        registers.add(tx.fromPubKeyHash())
      }
    })

    isValid = !newNames.exists(dataBase.nameExists)
    isValid = !registers.exists(dataBase.registerExists)
    isValid
  }

  override def getBalance(address: UInt160): Option[collection.immutable.Map[UInt256, Long]] = {
    dataBase.getBalance(address).map(_.mapValues(_.value))
  }

  override def getAccount(address: UInt160): Option[Account] = {
    dataBase.getAccount(address)
  }

  private def populate(): Unit = {
    if (blockBase.head.isEmpty) {
      blockBase.add(genesisBlock)
    }

    if (forkBase.head.isEmpty) {
      //tryInsertBlock(genesisBlock, true)
      forkBase.add(genesisBlock)
    }

    require(forkBase.head.isDefined)

    forkBase.switchState.foreach(resolveSwitchFailure)

    forkBase.head.foreach(resolveDbUnConsistent)

    require(forkBase.head.map(_.block.height).get >= blockBase.head.map(_.index).get)

    latestHeader = forkBase.head.get.block.header

    log.info(s"populate() latest block ${latestHeader.index} ${latestHeader.id}")
  }

  private def resolveSwitchFailure(state: SwitchState): Unit = {
    val oldBranch = forkBase.getBranch(state.oldHead, state.forkPoint)
    val newBranch = forkBase.getBranch(state.newHead, state.forkPoint)
    val result = onSwitch(oldBranch, newBranch, state)
    forkBase.endSwitch(oldBranch, newBranch, result)
  }

  private def resolveDbUnConsistent(head: ForkItem): Unit = {
    while (dataBase.revision > head.height + 1) {
      dataBase.rollBack()
    }
  }

  private def onConfirmed(block: Block): Unit = {
    if (block.height > 0) {
      log.info(s"confirm block ${block.height} (${block.id})")
      dataBase.commit(block.height)
      blockBase.add(block)
    }
  }

  private def onSwitch(from: Seq[ForkItem], to: Seq[ForkItem], switchState: SwitchState): SwitchResult = {
    def printChain(title: String, fork: Seq[ForkItem]): Unit = {
      log.info(s"$title: ${fork.map(_.block.id.toString.substring(0, 6)).mkString(" <- ")}")
    }

    printChain("old chain", from)
    printChain("new chain", to)

    require(dataBase.revision == from.last.height + 1)
    while (dataBase.revision > switchState.height  + 1) {
      dataBase.rollBack()
    }

    var appliedCount = 0
    for (item <- to if applyBlock(item.block)) {
      appliedCount += 1
    }

    if (appliedCount < to.size) {
      while (dataBase.revision > switchState.height + 1) {
        dataBase.rollBack()
      }
      from.foreach(item => applyBlock(item.block))
      SwitchResult(false, to(appliedCount))
    } else {
      SwitchResult(true)
    }
  }
}

