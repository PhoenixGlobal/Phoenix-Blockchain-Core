package com.apex.core

import java.time.Instant

import akka.actor.ActorRef
import com.apex.common.ApexLogging
import com.apex.consensus.ProducerUtil
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey, PublicKeyHash}
import com.apex.crypto.{BinaryData, Crypto, Fixed8, MerkleTree, UInt160, UInt256}
import com.apex.settings.{ChainSettings, ConsensusSettings, Witness}

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, Set}

case class ChainInfo(id: String)

trait Blockchain extends Iterable[Block] with ApexLogging {
  def getChainInfo(): ChainInfo

  def getLatestHeader(): BlockHeader

  def getHeight(): Int

  def getHeadTime(): Long

  def headTimeSinceGenesis(): Long

  def getHeader(id: UInt256): Option[BlockHeader]

  def getHeader(index: Int): Option[BlockHeader]

  def getNextBlockId(id: UInt256): Option[UInt256]

  def getBlock(height: Int): Option[Block]

  def getBlock(id: UInt256): Option[Block]

  //def getBlockInForkBase(id: UInt256): Option[Block]

  def containsBlock(id: UInt256): Boolean

  def getPendingTransaction(txid: UInt256): Option[Transaction]

  //  def produceBlock(producer: PublicKey, privateKey: PrivateKey, timeStamp: Long,
  //                   transactions: Seq[Transaction]): Option[Block]

  def startProduceBlock(producer: Witness, blockTime: Long): Unit

  def addTransaction(tx: Transaction): Boolean

  def produceBlockFinalize(): Option[Block]

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

  def Id: String

  def close()
}

object Blockchain {
  //private var chain: LevelDBBlockchain = null

  //  final val Current: Blockchain = new LevelDBBlockchain()
  def populate(chainSettings: ChainSettings, consensusSettings: ConsensusSettings, notification: Notification): LevelDBBlockchain = {
    new LevelDBBlockchain(chainSettings, consensusSettings, notification)
  }

  //def getLevelDBBlockchain: LevelDBBlockchain = chain
}

class PendingState {
  var producer: Witness = _
  var blockTime: Long = _
  var startTime: Long = _

  val txs = ArrayBuffer.empty[Transaction]

  def set(producer: Witness, blockTime: Long) = {
    this.producer = producer
    this.blockTime = blockTime
    this.startTime = Instant.now.toEpochMilli
  }
}

class LevelDBBlockchain(chainSettings: ChainSettings, consensusSettings: ConsensusSettings, notification: Notification) extends Blockchain {
  private val genesisProducer = PublicKey(BinaryData(chainSettings.genesis.publicKey)) // TODO: read from settings
  private val genesisProducerPrivKey = new PrivateKey(BinaryData(chainSettings.genesis.privateKey))

  private val blockBase = new BlockBase(chainSettings.blockBase)

  private val dataBase = new DataBase(chainSettings.dataBase)

  private val forkBase = new ForkBase(
    chainSettings.forkBase,
    consensusSettings.initialWitness,
    onConfirmed,
    onSwitch)

  // the zero is not valid pub key, and the NULL is special to handle,
  // so just set minerCoinFrom to any valid compressed pub key, it will not be seen by user
  private val minerCoinFrom = PublicKey(BinaryData("02866facba8742cd702b302021a9588e78b3cd96599a3b1c85688d6dc0a72585e6")) // 33 bytes pub key

  private val minerAward = Fixed8.fromDecimal(chainSettings.minerAward)

  private val genesisBlock: Block = buildGenesisBlock() //Block.build(genesisBlockHeader, genesisTxs)

  private var latestHeader: BlockHeader = genesisBlock.header

  //  private var latestProdState: ProducerStatus = null

  //  private val pendingTxs = ArrayBuffer.empty[Transaction] //TODO: change to seq map // TODO: save to DB?
  private val unapplyTxs = mutable.Map.empty[UInt256, Transaction] // TODO: save to DB?

  private val pendingState = new PendingState

  populate()

  private def buildGenesisBlock(): Block = {
    val genesisTxs = ArrayBuffer.empty[Transaction]

    chainSettings.genesis.genesisCoinAirdrop.foreach(airdrop => {
      genesisTxs.append(new Transaction(TransactionType.Miner, minerCoinFrom,
        PublicKeyHash.fromAddress(airdrop.addr).get, "", Fixed8.fromDecimal(airdrop.coins),
        UInt256.Zero, 0, consensusSettings.fingerprint(), BinaryData.empty))
    })

    val genesisBlockHeader: BlockHeader = BlockHeader.build(0,
      chainSettings.genesis.timeStamp.toEpochMilli, MerkleTree.root(genesisTxs.map(_.id)),
      UInt256.Zero, genesisProducer, genesisProducerPrivKey)

    Block.build(genesisBlockHeader, genesisTxs)
  }

  override def Id: String = genesisBlock.id.toString

  override def iterator: Iterator[Block] = new BlockchainIterator(this)

  override def close() = {
    log.info("blockchain closing")
    blockBase.close()
    dataBase.close()
    forkBase.close()
    log.info("blockchain closed")
  }

  override def getChainInfo(): ChainInfo = {
    ChainInfo(genesisBlock.id.toString)
  }

  override def getHeight(): Int = {
    forkBase.head.map(_.block.height).getOrElse(genesisBlock.height)
  }

  override def getHeadTime(): Long = {
    //    latestHeader.timeStamp
    forkBase.head.map(_.block.timeStamp).getOrElse(0)
  }

  override def getLatestHeader(): BlockHeader = {
    forkBase.head.map(_.block.header).getOrElse(genesisBlock.header)
  }

  override def headTimeSinceGenesis(): Long = {
    getLatestHeader.timeStamp - genesisBlock.header.timeStamp
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

  override def containsBlock(id: UInt256): Boolean = {
    forkBase.contains(id) || blockBase.containBlock(id)
  }

  def getPendingTransaction(txid: UInt256): Option[Transaction] = {
    if (pendingState.txs.map(_.id()).contains(txid)) {
      pendingState.txs.find(tx => tx.id().equals(txid))
    }
    else {
      unapplyTxs.get(txid)
    }
  }

  override def startProduceBlock(producer: Witness, blockTime: Long): Unit = {
    require(!isProducingBlock())
    pendingState.set(producer, blockTime)
    log.debug(s"start block at: ${pendingState.startTime}")

    val forkHead = forkBase.head.get
    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
      producer.pubkey.pubKeyHash, "", minerAward, UInt256.Zero,
      forkHead.block.height + 1,
      BinaryData(Crypto.randomBytes(8)), // add random bytes to distinct different blocks with same block index during debug in some cases
      BinaryData.empty
    )
    //isPendingBlock = true
    dataBase.startSession()

    val applied = applyTransaction(minerTx)
    require(applied)
    pendingState.txs.append(minerTx)

    unapplyTxs.foreach(p => addTransaction(p._2)) // addTransaction() may fail

    unapplyTxs.clear() // pendingState.txs.foreach(tx => unapplyTxs.remove(tx.id))

  }

  override def isProducingBlock(): Boolean = {
    !pendingState.txs.isEmpty
  }

  override def addTransaction(tx: Transaction): Boolean = {
    var added = false
    if (isProducingBlock()) {
      if (applyTransaction(tx)) {
        pendingState.txs.append(tx)
        added = true
      }
    }
    else {
      if (!unapplyTxs.contains(tx.id)) {
        unapplyTxs += (tx.id -> tx)
      }
      added = true
    }
    if (added)
      notification.send(AddTransactionNotify(tx))
    added
  }

  override def produceBlockFinalize(): Option[Block] = {
    val endTime = Instant.now.toEpochMilli
    if (!isProducingBlock()) {
      log.info("block canceled")
      None
    } else {
      log.debug(s"block time: ${pendingState.blockTime}, end time: $endTime, produce time: ${endTime - pendingState.startTime}")
      val forkHead = forkBase.head.get
      val merkleRoot = MerkleTree.root(pendingState.txs.map(_.id))
      val privateKey = pendingState.producer.privkey.get
      val publicKey = pendingState.producer.pubkey
      val timeStamp = pendingState.blockTime
      val header = BlockHeader.build(
        forkHead.block.height + 1, timeStamp, merkleRoot,
        forkHead.block.id, publicKey, privateKey)
      val block = Block.build(header, pendingState.txs.clone)
      pendingState.txs.clear()
      if (tryInsertBlock(block, false)) {
        log.info(s"block #${block.height} ${block.shortId} produced by ${block.header.producer.address.substring(0, 7)} ${block.header.timeString()}")
        notification.send(NewBlockProducedNotify(block))
        Some(block)
      } else {
        None
      }
    }
  }

  override def tryInsertBlock(block: Block, doApply: Boolean): Boolean = {
    var inserted = false
    if (isProducingBlock()) {
      pendingState.txs.foreach(tx => {
        if (tx.txType != TransactionType.Miner)
          unapplyTxs += (tx.id -> tx)
      })
      pendingState.txs.clear()
      dataBase.rollBack()
    }

    if (forkBase.head.get.block.id.equals(block.prev())) {
      if (doApply == false) { // check first !
        if (forkBase.add(block)) {
          inserted = true
          latestHeader = block.header
        }
        else
          log.error(s"Error during forkBase add block ${block.height()}  ${block.shortId}")
      }
      else if (applyBlock(block)) {
        if (forkBase.add(block)) {
          inserted = true
          latestHeader = block.header
        }
        else
          log.error(s"Error during forkBase add block ${block.height()}  ${block.shortId}")
      }
      else
        log.info(s"block ${block.height} ${block.shortId} apply error")
      if (inserted)
        notification.send(BlockAddedToHeadNotify(block))
    }
    else {
      log.debug(s"received block try add to minor fork chain. block ${block.height} ${block.shortId}")
      if (forkBase.add(block))
        inserted = true
      else
        log.debug("add fail")
    }
    if (inserted) {
      block.transactions.foreach(tx => unapplyTxs.remove(tx.id))
    }
    inserted
  }

  private def applyBlock(block: Block, verify: Boolean = true, enableSession: Boolean = true): Boolean = {
    var applied = true
    //    if (isPendingBlock) {
    //      rollBack()
    //    }
    if (!verify || verifyBlock(block)) {
      if (enableSession)
        dataBase.startSession()
      block.transactions.foreach(tx => {
        if (applied && !applyTransaction(tx))
          applied = false
      })
      if (enableSession && !applied)
        dataBase.rollBack()
    }
    else
      applied = false
    if (!applied) {
      log.error(s"Block apply fail #${block.height()} ${block.shortId}")
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
        .map(p => (p._1, p._2.map(_._2).sum))
        .filter(_._2.value > 0)
      val toBalance = (toAccount.balances.toSeq ++ Seq((tx.assetId, tx.amount))).groupBy(_._1)
        .map(p => (p._1, p._2.map(_._2).sum))
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
    else if (!verifyTxTypeAndSignature(block.transactions))
      false
    else if (!verifyRegisterNames(block.transactions))
      false
    else
      true
  }

  private def verifyTxTypeAndSignature(txs: Seq[Transaction]): Boolean = {
    var isValid = true
    var minerTxNum = 0
    txs.foreach(tx => {
      if (tx.txType == TransactionType.Miner)
        minerTxNum += 1
      else if (!tx.verifySignature())
        isValid = false
    })
    if (minerTxNum > 1)
      isValid = false
    isValid
  }

  private def verifyHeader(header: BlockHeader): Boolean = {
    val prevBlock = forkBase.get(header.prevBlock)
    if (prevBlock.isEmpty) {
      log.info("verifyHeader error: prevBlock not found")
      false
    }
    else if (header.timeStamp <= prevBlock.get.block.header.timeStamp) {
      log.info(s"verifyHeader error: timeStamp not valid  ${header.timeStamp}  ${prevBlock.get.block.header.timeStamp}")
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
    if (forkBase.head.isEmpty) {
      applyBlock(genesisBlock, false, false)
      blockBase.add(genesisBlock)
      forkBase.add(genesisBlock)
      notification.send(BlockAddedToHeadNotify(genesisBlock))
    }

    require(forkBase.head.isDefined)

    forkBase.switchState.foreach(resolveSwitchFailure)

    forkBase.head.foreach(resolveDbUnConsistent)

    require(forkBase.head.map(_.block.height).get >= blockBase.head.map(_.index).get)

    latestHeader = forkBase.head.get.block.header

    log.info(s"populate() latest block ${latestHeader.index} ${latestHeader.shortId()}")
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
      log.info(s"confirm block ${block.height} (${block.shortId})")
      dataBase.commit(block.height)
      blockBase.add(block)
    }
    notification.send(BlockConfirmedNotify(block))
  }

  private def onSwitch(from: Seq[ForkItem], to: Seq[ForkItem], switchState: SwitchState): SwitchResult = {
    def printChain(title: String, fork: Seq[ForkItem]): Unit = {
      log.info(s"$title: ${fork.map(_.block.shortId).mkString(" <- ")}")
    }

    printChain("old chain", from)
    printChain("new chain", to)

    require(dataBase.revision == from.last.height + 1)
    while (dataBase.revision > switchState.height + 1) {
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
      notification.send(ForkSwitchNotify(from, to))
      SwitchResult(true)
    }
  }
}

