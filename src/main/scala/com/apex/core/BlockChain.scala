package com.apex.core

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import akka.http.scaladsl.model.DateTime
import com.apex.common.ApexLogging
import com.apex.common.Serializable
import com.apex.core.TransactionOutput.deserialize
import com.apex.crypto.{MerkleTree, UInt256}
import com.apex.storage.{BlockChainStorage, LevelDbStorage, Storage}
import org.iq80.leveldb.WriteBatch

import scala.collection.mutable.Map
import scala.collection.mutable.Set

trait Blockchain extends Iterable[Block] with ApexLogging {
  def getLatestHeader: BlockHeader

  def getHeader(id: UInt256): Option[BlockHeader]

  def getHeader(index: Int): Option[BlockHeader]

  def getBlock(height: Int): Option[Block]

  def getBlock(id: UInt256): Option[Block]

  def containsBlock(id: UInt256): Boolean

  def produceBlock(transactions: Seq[Transaction]): Option[Block]

  def getTransaction(id: UInt256): Option[Transaction]

  def containsTransaction(id: UInt256): Boolean

  def verifyBlock(block: Block): Boolean

  def verifyTransaction(tx: Transaction): Boolean
}

object Blockchain {
  final val Current: Blockchain = new LevelDBBlockchain()

}

class LevelDBBlockchain extends Blockchain {
  private val db: LevelDbStorage = LevelDbStorage.open("test_db")
  private val blkTxMappingStore = new BlkTxMappingStore(db)
  private val txStore = new TransactionStore(db)
  private val headerStore = new BlockStore(db)

  private val genesisBlockHeader: BlockHeader = BlockHeader.build(0, 0, UInt256.Zero, UInt256.Zero)
  private val genesisBlock: Block = Block.build(genesisBlockHeader, Seq.empty)

  private val txBlockMap: Map[UInt256, UInt256] = Map.empty
  private val blockIndexMap: Map[Int, UInt256] = Map.empty
  private val utxoSet: Set[(UInt256, Int)] = Set.empty
  private val blkSet: Set[UInt256] = Set.empty

  private var latestHeader: BlockHeader = genesisBlockHeader

  populate()

  override def iterator: Iterator[Block] = new BlockchainIterator(this)

  override def getLatestHeader: BlockHeader = latestHeader

  override def getHeader(id: UInt256): Option[BlockHeader] = {
    headerStore.get(id) match {
      case Some(blk) => Some(blk)
      case None => None
    }
  }

  override def getHeader(index: Int): Option[BlockHeader] = {
    blockIndexMap.get(index) match {
      case Some(id) => getHeader(id)
      case _ => None
    }
  }

  override def getBlock(id: UInt256): Option[Block] = {
    headerStore.get(id) match {
      case Some(header) => {
        var txs = Seq.empty[Transaction]
        val mapping = blkTxMappingStore.get(header.id)
        if (!mapping.isEmpty) {
          txs = mapping.get.txIds
            .map(txStore.get(_))
            .filterNot(_.isEmpty)
            .map(_.get)
        }
        Some(Block.build(header, txs))
      }
      case None => None
    }
  }

  override def getBlock(index: Int): Option[Block] = {
    blockIndexMap.get(index) match {
      case Some(id) => getBlock(id)
      case _ => None
    }
  }

  override def containsBlock(id: UInt256): Boolean = {
    headerStore.contains(id)
  }

  override def produceBlock(transactions: Seq[Transaction]): Option[Block] = {
    val merkleRoot = MerkleTree.root(transactions.map(_.id))
    val header = BlockHeader.build(
      latestHeader.index + 1, DateTime.now.clicks,
      merkleRoot, latestHeader.id)
    val block = Block.build(header, transactions)
    val ret = db.batchWrite(batch => {
      headerStore.set(block.id, header, batch)
      transactions.foreach(tx => txStore.set(tx.id, tx, batch))
      updateUTXOSet(transactions)
    })
    if (ret) {
      latestHeader = header
      Some(block)
    } else {
      None
    }
  }

  override def getTransaction(id: UInt256): Option[Transaction] = {
    txStore.get(id)
  }

  override def containsTransaction(id: UInt256): Boolean = {
    txStore.contains(id)
  }

  override def verifyBlock(block: Block): Boolean = {
    if (verfyHeader(block.header)
      && verifyTxs(block.transactions)) {
      true
    } else {
      false
    }
  }

  override def verifyTransaction(tx: Transaction): Boolean = {
    if (tx.inputs.groupBy(i => (i.txId, i.index)).exists(_._2.size > 0)) {
      false
    } else {
      true
    }
  }

  private def populate() = {
    val it = iterator
    while (it.hasNext) {
      val blk = it.next()
      blkSet.add(blk.id)
      blockIndexMap.put(blk.header.index, blk.id)
      updateUTXOSet(blk.transactions)
      for (tx <- blk.transactions) {
        txBlockMap.put(tx.id, blk.id)
      }
    }
  }

  private def verfyHeader(header: BlockHeader): Boolean = {
    if (header.index != latestHeader.index + 1
      || header.timeStamp < latestHeader.timeStamp
      || !header.id.equals(latestHeader.id)) {
      false
    } else {
      true
    }
  }

  private def verifyTxs(txs: Seq[Transaction]): Boolean = {
    txs.isEmpty || txs(0).txType == TransactionType.Miner
  }

  private def updateUTXOSet(transactions: Seq[Transaction]) = {
    for (tx <- transactions) {
      for (i <- tx.inputs) {
        utxoSet.remove(i.txId, i.index)

      }
      for (i <- 0 to tx.outputs.length - 1) {
        utxoSet.add(tx.id, i)
      }
    }
  }
}

object StoreType extends Enumeration {
  val Block = Value(0x00)
  val Transaction = Value(0x01)
  val UTXO = Value(0x02)
  val BlkTxMapping = Value(0x03)
}

trait Cache[K, V] {
  def contains(key: K): Boolean

  def get(key: K): Option[V]

  def set(key: K, value: V): Boolean
}

class LRUCache[K, V](val size: Int) extends Cache[K, V] {
  override def contains(key: K): Boolean = ???

  override def get(key: K): Option[V] = ???

  override def set(key: K, value: V): Boolean = ???
}

abstract class DbStore[K <: Serializable, V <: Serializable](val db: LevelDbStorage, val storeType: StoreType.Value) {
  protected val cache: Cache[K, V] = new LRUCache(100) //TODO: read config file

  def contains(key: K): Boolean = {
    cache.contains(key) match {
      case true => true
      case false => getFromBackStore(key) match {
        case Some(_) => true
        case None => false
      }
    }
  }

  def get(key: K): Option[V] = {
    var item = cache.get(key)
    if (item.isEmpty) {
      item = getFromBackStore(key)
      if (item.isEmpty) {
        None
      } else {
        cache.set(key, item.get)
        item
      }
    } else {
      item
    }
  }

  def set(key: K, value: V, batch: WriteBatch = null): Boolean = {
    if (setBackStore(key, value, batch)) {
      cache.set(key, value)
      true
    } else {
      false
    }
  }

  protected def genKey(key: K): Array[Byte] = {
    val bos = new ByteArrayOutputStream(keySize + 1)
    val os = new DataOutputStream(bos)
    os.writeByte(storeType.id.toByte)
    key.serialize(os)
    bos.toByteArray
  }

  protected def getFromBackStore(key: K): Option[V] = {
    db.get(genKey(key)) match {
      case Some(value) => Some(fromBytes(value))
      case None => None
    }
  }

  protected def setBackStore(key: K, value: V, batch: WriteBatch): Boolean = {
    if (batch == null) {
      db.set(genKey(key), value.toBytes)
    } else {
      batch.put(genKey(key), value.toBytes)
      true
    }
  }

  protected def keySize: Int

  protected def fromBytes(bytes: Array[Byte]): V
}

//class BlockStore(db: LevelDbStorage) extends DbStore[UInt256, Block](db, StoreType.Block) {
//  override protected def keySize = UInt256.Size
//
//  override protected def fromBytes(bytes: Array[Byte]): Block = {
//    Block.fromBytes(bytes)
//  }
//}

class BlockStore(db: LevelDbStorage) extends DbStore[UInt256, BlockHeader](db, StoreType.Block) {
  override protected def keySize = UInt256.Size

  override protected def fromBytes(bytes: Array[Byte]): BlockHeader = {
    BlockHeader.fromBytes(bytes)
  }
}

class TransactionStore(db: LevelDbStorage) extends DbStore[UInt256, Transaction](db, StoreType.Transaction) {
  override protected def keySize: Int = UInt256.Size

  override protected def fromBytes(bytes: Array[Byte]): Transaction = {
    Transaction.fromBytes(bytes)
  }
}

case class BlkTxMapping(blkId: UInt256, txIds: Seq[UInt256]) extends Serializable {
  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.write(blkId)
    os.writeSeq(txIds)
  }
}

object BlkTxMapping {
  def deserialize(is: DataInputStream): BlkTxMapping = {
    import com.apex.common.Serializable._
    BlkTxMapping(
      is.readObj(UInt256.deserialize),
      is.readSeq(UInt256.deserialize)
    )
  }

  def fromBytes(bytes: Array[Byte]): BlkTxMapping = {
    val bs = new ByteArrayInputStream(bytes)
    val is = new DataInputStream(bs)
    deserialize(is)
  }
}

class BlkTxMappingStore(db: LevelDbStorage) extends DbStore[UInt256, BlkTxMapping](db, StoreType.BlkTxMapping) {
  override protected def keySize: Int = UInt256.Size

  override protected def fromBytes(bytes: Array[Byte]): BlkTxMapping = {
    BlkTxMapping.fromBytes(bytes)
  }
}

case class UTXOKey(val txId: UInt256, val index: Int) extends Serializable {
  override def serialize(os: DataOutputStream): Unit = {
    os.write(txId)
    os.writeInt(index)
  }
}

object UTXOKey {
  val Size: Int = UInt256.Size + 4

  def deserialize(is: DataInputStream): UTXOKey = {
    import com.apex.common.Serializable._
    UTXOKey(
      is.readObj(UInt256.deserialize),
      is.readInt()
    )
  }
}

class UTXOStore(db: LevelDbStorage) extends DbStore[UTXOKey, TransactionOutput](db, StoreType.UTXO) {
  override protected def keySize: Int = UTXOKey.Size

  override protected def fromBytes(bytes: Array[Byte]): TransactionOutput = {
    TransactionOutput.fromBytes(bytes)
  }
}