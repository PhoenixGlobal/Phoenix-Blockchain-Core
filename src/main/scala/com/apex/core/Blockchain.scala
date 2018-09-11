package com.apex.core

import java.nio.file.Path

import com.apex.common.ApexLogging
import com.apex.settings.{ChainSettings, ConsensusSettings}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.crypto.{BinaryData, Crypto, Fixed8, MerkleTree, UInt160, UInt256}
import com.apex.storage.LevelDbStorage
import org.iq80.leveldb.WriteBatch

import scala.collection.mutable.{Map, Set}

trait Blockchain extends Iterable[Block] with ApexLogging {
  def getLatestHeader: BlockHeader

  def getHeight(): Int

  def getHeadTime(): Long

  def getForkHeadBlock(): Block

  def headTimeSinceGenesis(): Long

  def getDistance(): Long

  def getHeader(id: UInt256): Option[BlockHeader]

  def getHeader(index: Int): Option[BlockHeader]

  def getBlock(height: Int): Option[Block]

  def getBlock(id: UInt256): Option[Block]

  def getBlockInForkBase(id: UInt256): Option[Block]

  def containsBlock(id: UInt256): Boolean

  def produceBlock(producer: PublicKey, privateKey: PrivateKey, timeStamp: Long,
                   transactions: Seq[Transaction]): Option[Block]

  def tryInsertBlock(block: Block): Boolean

  def getTransaction(id: UInt256): Option[Transaction]

  def containsTransaction(id: UInt256): Boolean

  def verifyBlock(block: Block): Boolean

  def verifyTransaction(tx: Transaction): Boolean

  def getBalance(address: UInt160): Option[collection.immutable.Map[UInt256, Long]]

  def getAccount(address: UInt160): Option[Account]

  def getGenesisBlockChainId: String
}

object Blockchain {
  var chain: LevelDBBlockchain = null

  //  final val Current: Blockchain = new LevelDBBlockchain()
  def populate(chainSettings: ChainSettings, consensusSettings: ConsensusSettings): LevelDBBlockchain = {
    val populateChain = new LevelDBBlockchain(chainSettings, consensusSettings)
    chain = populateChain
    populateChain
  }

  def getLevelDBBlockchain: LevelDBBlockchain = chain
}

class LevelDBBlockchain(chainSettings: ChainSettings, consensusSettings: ConsensusSettings) extends Blockchain {
  private val db: LevelDbStorage = LevelDbStorage.open(chainSettings.dbDir)

  private val genesisProducer = PublicKey(BinaryData(chainSettings.genesis.publicKey)) // TODO: read from settings
  private val genesisProducerPrivKey = new PrivateKey(BinaryData(chainSettings.genesis.privateKey))

  private val headerStore = new HeaderStore(db, 10)
  private val heightStore = new HeightStore(db, 10)
  private val txStore = new TransactionStore(db, 10)
  private val accountStore = new AccountStore(db, 10)
  //  private val addressStore = new AddressStore(db)
  private val blkTxMappingStore = new BlkTxMappingStore(db, 10)
  private val headBlkStore = new HeadBlockStore(db)
  //private val utxoStore = new UTXOStore(db, 10)
  private val nameToAccountStore = new NameToAccountStore(db, 10)
  // TODO:  pubkeyNonceStore
  private val prodStateStore = new ProducerStateStore(db)

  private val forkBase = new ForkBase(
    chainSettings.forkDir,
    consensusSettings.initialWitness,
    onConfirmed,
    onSwitch)

  // TODO: zero is not a valid pub key, need to work out other method
  private val minerCoinFrom = PublicKey(BinaryData(chainSettings.miner)) // 33 bytes pub key
  private val minerAward = Fixed8.Ten

  private val genesisMinerAddress = UInt160.parse("f54a5851e9372b87810a8e60cdd2e7cfd80b6e31").get
  private val genesisTx = new Transaction(TransactionType.Miner, minerCoinFrom,
    genesisMinerAddress, "", minerAward, UInt256.Zero, 0, BinaryData.empty, BinaryData.empty)

  private val genesisBlockHeader: BlockHeader = BlockHeader.build(0,
    chainSettings.genesis.timeStamp, UInt256.Zero, UInt256.Zero,
    genesisProducer, genesisProducerPrivKey)
  private val genesisBlock: Block = Block.build(genesisBlockHeader, Seq(genesisTx))

  private var latestHeader: BlockHeader = genesisBlockHeader

  private var latestProdState: ProducerStatus = null

  populate()

  override def getGenesisBlockChainId: String = genesisBlockHeader.id.toString

  override def iterator: Iterator[Block] = new BlockchainIterator(this)

  override def getLatestHeader: BlockHeader = latestHeader

  override def getHeight(): Int = {
    latestHeader.index
  }

  override def getHeadTime(): Long = {
//    latestHeader.timeStamp
    forkBase.head.map(_.block.timeStamp).getOrElse(0)
  }

  override def getForkHeadBlock(): Block = {
    forkBase.head().get.block
  }

  override def headTimeSinceGenesis(): Long = {
    latestHeader.timeStamp - genesisBlockHeader.timeStamp
  }

  override def getDistance(): Long = {
    val state = prodStateStore.get
    assert(!state.isEmpty)
    state.get.distance
  }

  override def getHeader(id: UInt256): Option[BlockHeader] = {
    headerStore.get(id)
  }

  override def getHeader(index: Int): Option[BlockHeader] = {
    heightStore.get(index) match {
      case Some(id) => getHeader(id)
      case None => None
    }
  }

  override def getBlock(id: UInt256): Option[Block] = {
    def getTxs(blkTx: BlkTxMapping): Seq[Transaction] = {
      blkTx.txIds.map(txStore.get).filterNot(_.isEmpty).map(_.get)
    }

    def getBlk(header: BlockHeader): Block = {
      val txs = blkTxMappingStore.get(header.id).map(getTxs).getOrElse(Seq.empty)
      Block.build(header, txs)
    }

    headerStore.get(id).map(getBlk)
  }

  override def getBlock(index: Int): Option[Block] = {
    heightStore.get(index) match {
      case Some(id) => getBlock(id)
      case None => None
    }
  }

  override def getBlockInForkBase(id: UInt256): Option[Block] = {
    val forkItem = forkBase.get(id)
    if (forkItem.isEmpty)
      None
    else
      Some(forkItem.get.block)
  }

  override def containsBlock(id: UInt256): Boolean = {
    headerStore.contains(id)
  }

  override def produceBlock(producer: PublicKey, privateKey: PrivateKey,
                            timeStamp: Long, transactions: Seq[Transaction]): Option[Block] = {
    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
      producer.pubKeyHash, "", minerAward, UInt256.Zero, latestHeader.index + 1, BinaryData.empty, BinaryData.empty)
    val txs = Seq(minerTx) ++ transactions.filter(verifyTransaction)
    val merkleRoot = MerkleTree.root(txs.map(_.id))
    val forkHead = forkBase.head.get
    val header = BlockHeader.build(
      forkHead.block.height + 1, timeStamp, merkleRoot,
      forkHead.block.id, producer, privateKey)
    val block = Block.build(header, txs)
    if (forkBase.add(block)) {
      Some(block)
    } else {
      None
    }
  }

  override def tryInsertBlock(block: Block): Boolean = {
    forkBase.add(block)
//    log.info(s"received block ${block.height}")
//    if (verifyBlock(block)) {
//      forkBase.add(block)
//    } else {
//      false
//    }
  }

  override def getTransaction(id: UInt256): Option[Transaction] = {
    txStore.get(id)
  }

  override def containsTransaction(id: UInt256): Boolean = {
    txStore.contains(id)
  }

  private def saveBlockToStores(block: Block): Boolean = {
    def calcBalancesInBlock(balances: Map[UInt160, Map[UInt256, Fixed8]], spent: Boolean,
                            address: UInt160, amounts: Fixed8, assetId: UInt256) = {
      val amount = if (spent) -amounts else amounts
      balances.get(address) match {
        case Some(balance) => {
          balance(assetId) += amount
        }
        case None => balances.put(address, Map((assetId, amount)))
      }
    }

    def updateAccout(accounts: Map[UInt160, Account], tx: Transaction) = {
      // TODO
    }

    //temp check
    require(latestHeader.id.equals(block.prev))
    require(block.header.index == latestHeader.index + 1)

    try {
      db.batchWrite(batch => {
        headerStore.set(block.header.id, block.header, batch)
        heightStore.set(block.header.index, block.header.id, batch)
        headBlkStore.set(HeadBlock.fromHeader(block.header), batch)
        //prodStateStore.set(latestProdState, batch)
        val blkTxMapping = BlkTxMapping(block.id, block.transactions.map(_.id))
        blkTxMappingStore.set(block.id, blkTxMapping, batch)
        val accounts = Map.empty[UInt160, Account]
        val balances = Map.empty[UInt160, Map[UInt256, Fixed8]]
        block.transactions.foreach(tx => {
          txStore.set(tx.id, tx, batch)
          calcBalancesInBlock(balances, true, tx.fromPubKeyHash, tx.amount, tx.assetId)
          calcBalancesInBlock(balances, false, tx.toPubKeyHash, tx.amount, tx.assetId)
          updateAccout(accounts, tx)
        })
        balances.foreach(p => {
          val account = accountStore.get(p._1).map(a => {
            val merged = a.balances.toSeq ++ p._2.toSeq
            val balances = merged.groupBy(_._1)
              .map(p => (p._1, Fixed8.sum(p._2.map(_._2).sum)))
              .filter(_._2.value > 0)
            new Account(a.active, a.name, balances, a.nextNonce, a.version)
          }).getOrElse(new Account(true, "", p._2.filter(_._2.value > 0).toMap, 0))
          accountStore.set(p._1, account, batch)
        })
        // TODO accounts.foreach()
      })
      latestHeader = block.header
      true
    } catch {
      case e: Throwable => {
        log.error("produce block failed", e)
        false
      }
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
    // make sure name is not used
    newNames.foreach(name => {
      if (nameToAccountStore.get(name) != None)
        isValid = false
    })
    // make sure register never registed before
    registers.foreach(register => {
      val account = accountStore.get(register)
      if (account != None && account.get.name != "") {
        isValid = false
      }
    })
    isValid
  }

  override def verifyBlock(block: Block): Boolean = {
    if (!verifyHeader(block.header))
      false
    else if (!block.transactions.forall(verifyTransaction))
      false
    else if (!verifyRegisterNames(block.transactions))
      false
    else
      true
  }

  override def verifyTransaction(tx: Transaction): Boolean = {
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

  override def getBalance(address: UInt160): Option[collection.immutable.Map[UInt256, Long]] = {
    accountStore.get(address).map(account =>
      if (account.active) {
        Some(account.balances.map(b => b._1 -> b._2.value))
      } else {
        None
      }).getOrElse(None)
  }

  override def getAccount(address: UInt160): Option[Account] = {
    accountStore.get(address)
  }

  private def populate(): Unit = {
    def initDB(batch: WriteBatch): BlockHeader = {
      val blkTxMapping = BlkTxMapping(genesisBlock.id, genesisBlock.transactions.map(_.id))
      headerStore.set(genesisBlock.id, genesisBlockHeader, batch)
      heightStore.set(genesisBlock.height, genesisBlock.id, batch)
      blkTxMappingStore.set(genesisBlock.id, blkTxMapping, batch)
      headBlkStore.set(HeadBlock.fromHeader(genesisBlockHeader), batch)
      prodStateStore.set(ProducerStatus(1), batch)
      genesisBlockHeader
    }

    def reInitDB(batch: WriteBatch): BlockHeader = {
      headerStore.foreach((k, _) => headerStore.delete(k, batch))
      heightStore.foreach((k, _) => heightStore.delete(k, batch))
      blkTxMappingStore.foreach((k, _) => blkTxMappingStore.delete(k, batch))
      accountStore.foreach((k, _) => accountStore.delete(k, batch))
      //utxoStore.foreach((k, _) => utxoStore.delete(k, batch))
      nameToAccountStore.foreach((k, _) => nameToAccountStore.delete(k, batch))
      prodStateStore.delete(batch)
      headBlkStore.delete(batch)
      initDB(batch)
    }

    def reInit() = {
      db.batchWrite(reInitDB)
    }

    def init(headBlock: HeadBlock) = {
      headerStore.get(headBlock.id).getOrElse(reInit)
    }

    latestHeader = headBlkStore.get.map(init).getOrElse(reInit)
    if (forkBase.head().isEmpty) {
      forkBase.add(genesisBlock)
    }
    //latestProdState = prodStateStore.get.get
  }

  private def verifyHeader(header: BlockHeader): Boolean = {
    if (header.index != latestHeader.index + 1)
      false
    else if (header.timeStamp < latestHeader.timeStamp)
      false
    // TODO: verify rule of timeStamp and producer
    else if (header.id.equals(latestHeader.id))
      false
    else if (!header.prevBlock.equals(latestHeader.id))
      false
    else if (!header.verifySig())
      false
    else
      true
  }

  private def onConfirmed(block: Block): Unit = {
    log.info(s"confirm block ${block.height} (${block.id})")
    if (block.height != 0) {
      saveBlockToStores(block)
    }
  }

  private def onSwitch(from: Seq[ForkItem], to: Seq[ForkItem]): Unit = {
    log.info(s"old chain: ${from.map(_.block.id.toString.substring(0, 6)).mkString(" -> ")}")
    log.info(s"new chain: ${to.map(_.block.id.toString.substring(0, 6)).mkString(" -> ")}")
    //TODO: db undo
  }
}

