package com.apex.core

import java.nio.file.Path

import com.apex.common.ApexLogging
import com.apex.settings.{ChainSettings, ConsensusSettings}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.crypto.{BinaryData, Crypto, Fixed8, MerkleTree, UInt160, UInt256}
import com.apex.storage.{Batch, LevelDbStorage}
import org.iq80.leveldb.WriteBatch

import scala.collection.mutable.{Map, Set}

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

  def getBlockInForkBase(id: UInt256): Option[Block]

  def produceBlock(producer: PublicKey, privateKey: PrivateKey, timeStamp: Long,
                   transactions: Seq[Transaction]): Option[Block]

  def tryInsertBlock(block: Block): Boolean

  //
  //  def getTransaction(id: UInt256): Option[Transaction]
  //
  //  def containsTransaction(id: UInt256): Boolean

  //def verifyBlock(block: Block): Boolean

  //def verifyTransaction(tx: Transaction): Boolean

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
  private val genesisProducer = PublicKey(BinaryData(chainSettings.genesis.publicKey)) // TODO: read from settings
  private val genesisProducerPrivKey = new PrivateKey(BinaryData(chainSettings.genesis.privateKey))

  //  private val headerStore = new HeaderStore(db, 10)
  //  private val heightStore = new HeightStore(db, 10)
  //  private val txStore = new TransactionStore(db, 10)
  //  private val accountStore = new AccountStore(db, 10)
  //  //  private val addressStore = new AddressStore(db)
  //  private val blkTxMappingStore = new BlkTxMappingStore(db, 10)
  //  private val headBlkStore = new HeadBlockStore(db)
  //  //private val utxoStore = new UTXOStore(db, 10)
  //  private val nameToAccountStore = new NameToAccountStore(db, 10)
  //  // TODO:  pubkeyNonceStore
  //  private val prodStateStore = new ProducerStateStore(db)
  private val blockBase = new BlockBase(chainSettings.blockBase)

  private val dataBase = new DataBase(chainSettings.dataBase)

  private val forkBase = new ForkBase(
    chainSettings.forkBase,
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

  //  private var latestProdState: ProducerStatus = null

  populate()

  override def getGenesisBlockChainId: String = genesisBlockHeader.id.toString

  override def iterator: Iterator[Block] = new BlockchainIterator(this)

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

  override def getBlockInForkBase(id: UInt256): Option[Block] = {
    forkBase.get(id).flatMap(item => getBlock(item.block.id))
  }

  override def produceBlock(producer: PublicKey, privateKey: PrivateKey,
                            timeStamp: Long, transactions: Seq[Transaction]): Option[Block] = {
    val forkHead = forkBase.head.get
    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
      producer.pubKeyHash, "", minerAward, UInt256.Zero,
      forkHead.block.height + 1, BinaryData.empty, BinaryData.empty
    )
    //val txs = Seq(minerTx) ++ transactions.filter(verifyTransaction)
    val txs = Seq(minerTx) ++ transactions
    val merkleRoot = MerkleTree.root(txs.map(_.id))
    val header = BlockHeader.build(
      forkHead.block.height + 1, timeStamp, merkleRoot,
      forkHead.block.id, producer, privateKey)
    val block = Block.build(header, txs)
    if (tryInsertBlock(block)) {
      Some(block)
    } else {
      None
    }
  }

  override def tryInsertBlock(block: Block): Boolean = {
    var inserted = false
    forkBase.add(block)
    if (forkBase.head.get.block.prev().equals(latestHeader.id)) {
      if (dataBase.applyBlock(block)) {
        inserted = true
        latestHeader = block.header
      }
      else {
        log.info(s"block ${block.height} ${block.id} apply error")
        forkBase.removeFork(block.id)
      }
    }
    else {
      log.info(s"received block added to minor fork chain. block ${block.height} ${block.id}")
      inserted = true
    }
    inserted
  }

  //  override def getTransaction(id: UInt256): Option[Transaction] = {
  //    txStore.get(id)
  //  }
  //
  //  override def containsTransaction(id: UInt256): Boolean = {
  //    txStore.contains(id)
  //  }

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
    //    require(latestHeader.id.equals(block.prev))
    //    require(block.header.index == latestHeader.index + 1)
    require(block.header.verifySig())

    try {
//      db.newSession()
//      db.batchWrite(batch => {
//
//        // TODO accounts.foreach()
//      })
      //      latestHeader = block.header
      true
    } catch {
      case e: Throwable => {
        log.error("produce block failed", e)
        false
      }
    }
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
      tryInsertBlock(genesisBlock)
    }

    require(
      forkBase.head.map(_.block.height).get >=
      blockBase.head.map(_.index).get
    )

    if (forkBase.head.isDefined)
      latestHeader = forkBase.head().get.block.header
    else
      latestHeader = blockBase.head().get
  }

  private def onConfirmed(block: Block): Unit = {
    log.debug(s"confirm block ${block.height} (${block.id})")
    if (block.height > 0) {
      dataBase.commit(block.height)
      blockBase.add(block)
    }
  }

  private def onSwitch(from: Seq[ForkItem], to: Seq[ForkItem]): Unit = {
    def printChain(title: String, fork: Seq[ForkItem]): Unit = {
      log.debug(s"$title: ${fork.map(_.block.id.toString.substring(0, 6)).mkString(" -> ")}")
    }

    printChain("old chain", from)
    printChain("new chain", to)

    from.foreach(_ => dataBase.rollBack())
    //TODO apply all blocks switched to
  }
}

