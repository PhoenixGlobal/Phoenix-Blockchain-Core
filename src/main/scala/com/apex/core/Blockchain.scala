package com.apex.core

import com.apex.common.ApexLogging
import com.apex.core.script.Script
import com.apex.settings.ChainSettings
import com.apex.crypto.Ecdsa.PrivateKey
import com.apex.crypto.{BinaryData, Crypto, Fixed8, MerkleTree, UInt160, UInt256}
import com.apex.storage.LevelDbStorage
import org.iq80.leveldb.WriteBatch

import scala.collection.mutable.{Map, Set}

trait Blockchain extends Iterable[Block] with ApexLogging {
  def getLatestHeader: BlockHeader

  def getHeight(): Int

  def getHeadTime(): Long

  def headTimeSinceGenesis(): Long

  def getDistance(): Long

  def getHeader(id: UInt256): Option[BlockHeader]

  def getHeader(index: Int): Option[BlockHeader]

  def getBlock(height: Int): Option[Block]

  def getBlock(id: UInt256): Option[Block]

  def containsBlock(id: UInt256): Boolean

  def produceBlock(producer: BinaryData, privateKey: PrivateKey, timeStamp: Long,
                   transactions: Seq[Transaction]): Option[Block]

  def tryInsertBlock(block: Block): Boolean

  def getTransaction(id: UInt256): Option[Transaction]

  def containsTransaction(id: UInt256): Boolean

  def verifyBlock(block: Block): Boolean

  def verifyTransaction(tx: Transaction): Boolean

  def getBalance(address: UInt160): Option[collection.immutable.Map[UInt256, Long]]
}

object Blockchain {
  //  final val Current: Blockchain = new LevelDBBlockchain()
  def populate(settings: ChainSettings) = new LevelDBBlockchain(settings)
}

class LevelDBBlockchain(val settings: ChainSettings) extends Blockchain {
  private val db: LevelDbStorage = LevelDbStorage.open(settings.dbDir)

  private val genesisProducer = BinaryData("03b4534b44d1da47e4b4a504a210401a583f860468dec766f507251a057594e682") // TODO: read from settings
  private val genesisProducerPrivKey = new PrivateKey(BinaryData("7a93d447bffe6d89e690f529a3a0bdff8ff6169172458e04849ef1d4eafd7f86"))

  private val headerStore = new HeaderStore(db, 10)
  private val heightStore = new HeightStore(db, 10)
  private val txStore = new TransactionStore(db, 10)
  private val accountStore = new AccountStore(db, 10)
  //  private val addressStore = new AddressStore(db)
  private val blkTxMappingStore = new BlkTxMappingStore(db, 10)
  private val headBlkStore = new HeadBlockStore(db)
  //private val utxoStore = new UTXOStore(db, 10)
  // TODO:  pubkeyNameMappingStore
  // TODO:  pubkeyNonceStore
  private val prodStateStore = new ProducerStateStore(db)

  private val minerCoinFrom = BinaryData("000000000000000000000000000000000000000000000000000000000000000000")   // 33 bytes pub key
  private val minerAddress = UInt160.parse("f54a5851e9372b87810a8e60cdd2e7cfd80b6e31").get
  private val minerAward = Fixed8.Ten

  private val genesisTx = new Transaction(TransactionType.Miner, minerCoinFrom,
    minerAddress, "", minerAward, UInt256.Zero, 0, BinaryData.empty, BinaryData.empty)

  private val genesisBlockHeader: BlockHeader = BlockHeader.build(0, 1537790400000L,
    UInt256.Zero, UInt256.Zero, genesisProducer, genesisProducerPrivKey)
  private val genesisBlock: Block = Block.build(genesisBlockHeader, Seq(genesisTx))

  private var latestHeader: BlockHeader = genesisBlockHeader

  private var latestProdState: ProducerStatus = null

  populate()

  override def iterator: Iterator[Block] = new BlockchainIterator(this)

  override def getLatestHeader: BlockHeader = latestHeader

  override def getHeight(): Int = {
    latestHeader.index
  }

  override def getHeadTime(): Long = {
    latestHeader.timeStamp
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

  override def containsBlock(id: UInt256): Boolean = {
    headerStore.contains(id)
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

    try {
      db.batchWrite(batch => {
        headerStore.set(block.header.id, block.header, batch)
        heightStore.set(block.header.index, block.header.id, batch)
        headBlkStore.set(HeadBlock.fromHeader(block.header), batch)
        //prodStateStore.set(latestProdState, batch)
        val blkTxMapping = BlkTxMapping(block.id, block.transactions.map(_.id))
        blkTxMappingStore.set(block.id, blkTxMapping, batch)
        val balances = Map.empty[UInt160, Map[UInt256, Fixed8]]
        block.transactions.foreach(tx => {
          txStore.set(tx.id, tx, batch)
          calcBalancesInBlock(balances, true, tx.fromAddress, tx.amount, tx.assetId)
          calcBalancesInBlock(balances, false, tx.toAddress, tx.amount, tx.assetId)
        })
        balances.foreach(p => {
          val account = accountStore.get(p._1).map(a => {
            val merged = a.balances.toSeq ++ p._2.toSeq
            val balances = merged.groupBy(_._1)
              .map(p => (p._1, Fixed8.sum(p._2.map(_._2).sum)))
              .filter(_._2.value > 0)
            new Account(a.active, balances, a.version)
          }).getOrElse(new Account(true, p._2.filter(_._2.value > 0).toMap))
          accountStore.set(p._1, account, batch)
        })
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

  override def produceBlock(producer: BinaryData, privateKey: PrivateKey,
                            timeStamp: Long, transactions: Seq[Transaction]): Option[Block] = {
    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
      minerAddress, "", minerAward, UInt256.Zero, latestHeader.index + 1, BinaryData.empty, BinaryData.empty)
    val txs = Seq(minerTx) ++ transactions.filter(verifyTransaction)
    val merkleRoot = MerkleTree.root(txs.map(_.id))
    val header = BlockHeader.build(
      latestHeader.index + 1, timeStamp, merkleRoot,
      latestHeader.id, producer, privateKey)
    val block = Block.build(header, txs)

    //latestProdState = latestProdState plusDistance distance

    if (tryInsertBlock(block))
      Some(block)
    else
      None
  }

  override def tryInsertBlock(block: Block): Boolean = {
    if (verifyBlock(block))
      if (saveBlockToStores(block))
        return true

    return false
  }

  override def getTransaction(id: UInt256): Option[Transaction] = {
    txStore.get(id)
  }

  override def containsTransaction(id: UInt256): Boolean = {
    txStore.contains(id)
  }

  override def verifyBlock(block: Block): Boolean = {
    if (verifyHeader(block.header) &&
      block.transactions.forall(verifyTransaction)) {
      true
    } else {
      false
    }
  }

  override def verifyTransaction(tx: Transaction): Boolean = {
    def checkAmount(): Boolean = {
      // TODO
      true
    }

    if (tx.txType == TransactionType.Miner) {
      // TODO check miner and only one miner tx
      return true
    }

    var isValid = tx.verifySignature()

    // More TODO

    isValid && checkAmount()
  }

  override def getBalance(address: UInt160): Option[collection.immutable.Map[UInt256, Long]] = {
    accountStore.get(address).map(account =>
      if (account.active) {
        Some(account.balances.map(b => b._1 -> b._2.value))
      } else {
        None
      }).getOrElse(None)
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
    //latestProdState = prodStateStore.get.get
  }

  private def verifyHeader(header: BlockHeader): Boolean = {
    if (header.index != latestHeader.index + 1)
      return false
    if (header.timeStamp < latestHeader.timeStamp)
      return false
    // TODO: verify rule of timeStamp and producer
    if (header.id.equals(latestHeader.id))
      return false
    if (!header.prevBlock.equals(latestHeader.id))
      return false
    if (header.producer.length != 33)
      return false
    if (!header.verifySig())
      return false

    true
  }
}


