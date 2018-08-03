package com.apex.core

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import akka.http.scaladsl.model.DateTime
import com.apex.common.{ApexLogging, Serializable}
import com.apex.crypto.{Fixed8, MerkleTree, UInt160, UInt256}
import com.apex.storage.LevelDbStorage
import org.iq80.leveldb.{ReadOptions, WriteBatch}

import scala.collection.mutable
import scala.collection.mutable.{Map, Set}

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

  def getBalance(address: UInt160): Option[collection.immutable.Map[UInt256, Long]]

  def getUTXOSet: UTXOSet
  
  def getUTXObyAddress(address: UInt160): Option[Set[(UInt256, Int)]]
}

object Blockchain {
  final val Current: Blockchain = new LevelDBBlockchain()

}

class LevelDBBlockchain extends Blockchain {
  private val db: LevelDbStorage = LevelDbStorage.open("test_db")

  private val headerStore = new HeaderStore(db)
  private val heightStore = new HeightStore(db)
  private val txStore = new TransactionStore(db)
  private val accountStore = new AccountStore(db)
  private val blkTxMappingStore = new BlkTxMappingStore(db)
  private val headBlkStore = new HeadBlockStore(db)
  private val utxoStore = new UTXOStore(db)

  private val genesisBlockHeader: BlockHeader = BlockHeader.build(0, 0, UInt256.Zero, UInt256.Zero)
  private val genesisBlock: Block = Block.build(genesisBlockHeader, Seq.empty)

  private val utxoSet: Set[(UInt256, Int)] = Set.empty

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
    heightStore.get(index) match {
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
    heightStore.get(index) match {
      case Some(id) => getBlock(id)
      case _ => None
    }
  }

  override def containsBlock(id: UInt256): Boolean = {
    headerStore.contains(id)
  }

  override def produceBlock(transactions: Seq[Transaction]): Option[Block] = {
    def calcBalancesInBlock(balances: mutable.Map[UInt160, mutable.Map[UInt256, Fixed8]], output: TransactionOutput, spent: Boolean) = {
      val amount = if (spent) -output.amount else output.amount
      balances.get(output.address) match {
        case Some(balance) => {
          balance(output.assetId) += amount
        }
        case None => balances.put(output.address,
          Map((output.assetId, amount)))
      }
    }

    val txs = transactions.filter(verifyTransaction(_))
    val merkleRoot = MerkleTree.root(transactions.map(_.id))
    val header = BlockHeader.build(
      latestHeader.index + 1, DateTime.now.clicks,
      merkleRoot, latestHeader.id)
    val block = Block.build(header, txs)
    val ret = db.batchWrite(batch => {
      headBlkStore.set(header, batch)
      headerStore.set(header.id, header, batch)
      heightStore.set(header.index, header.id, batch)
      val blkTxMapping = BlkTxMapping(block.id, txs.map(_.id))
      blkTxMappingStore.set(block.id, blkTxMapping, batch)
      val balances = Map.empty[UInt160, Map[UInt256, Fixed8]]
      txs.foreach(tx => {
        txStore.set(tx.id, tx, batch)
        for (index <- 0 to tx.outputs.length - 1) {
          val key = UTXOKey(tx.id, index)
          val output = tx.outputs(index)
          utxoStore.set(key, output, batch)
          calcBalancesInBlock(balances, output, false)
        }
        for (i <- tx.inputs) {
          val key = UTXOKey(i.txId, i.index)
          utxoStore.delete(key, batch)
          val output = getTransaction(i.txId).get.outputs(i.index)
          calcBalancesInBlock(balances, output, true)
        }
      })
      balances.foreach(p => {
        accountStore.get(p._1) match {
          case Some(account) => {
            val merged = account.balances.toSeq ++ p._2.toSeq
            val balances = merged
              .groupBy(_._1)
              .map(p => (p._1, Fixed8.sum(p._2.map(_._2).sum)))
              .filter(_._2.value > 0)
            val a = new Account(account.active, balances, account.version)
            accountStore.set(p._1, a, batch)
          }
          case None => {
            val a = new Account(true, p._2.filter(_._2.value > 0).toMap)
            accountStore.set(p._1, a, batch)
          }
        }
      })
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

  override def getBalance(address: UInt160): Option[collection.immutable.Map[UInt256, Long]] = {
    accountStore.get(address) match {
      case Some(account) => {
        if (account.active) {
          Some(account.balances.map(b => b._1 -> b._2.value))
        } else {
          None
        }
      }
      case None => None
    }
  }

  override def getUTXOSet: UTXOSet = {
    new UTXOSet(utxoStore)
  }
  
  override def getUTXObyAddress(address: UInt160): Option[Set[(UInt256, Int)]] = {
    
    // return all the (TxID, outputIndex) in UTXO of the specified address
    // maybe also search the mempool for 0-confirmation 
    //
    // todo: need replaced by WalletIndexer for full transaction history of some specified address, 
    //       so need to search all the blocks, not only just the UTXO
    
    None
  }

  private def populate(): Unit = {
    headBlkStore.get match {
      case Some(headBlock) =>
        headerStore.get(headBlock.id) match {
          case Some(header) => latestHeader = header
          case None => db.batchWrite(batch => {
            headerStore.set(genesisBlockHeader.id, genesisBlockHeader, batch)
            headBlkStore.set(genesisBlockHeader, batch)
          })
        }
      case None => {
        headBlkStore.set(genesisBlockHeader)
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
}

object StoreType extends Enumeration {
  val Block = Value(0x00)
  val Transaction = Value(0x01)
  val Height = Value(0x02)
  val HeadBlock = Value(0x03)
  val UTXO = Value(0x04)
  val BlkTxMapping = Value(0x05)
  val Account = Value(0x06)
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

class UTXOSet(val utxoStore: UTXOStore) {
  def get(key: UTXOKey): Option[TransactionOutput] = {
    utxoStore.get(key)
  }

  def foreach(func: (UTXOKey, TransactionOutput) => Unit): Unit = {
    utxoStore.foreach(func)
  }
}

trait Cache[K, V] {
  def contains(key: K): Boolean

  def get(key: K): Option[V]

  def set(key: K, value: V): Boolean

  def delete(key: K): Unit
}

class LRUCache[K, V](val size: Int) extends Cache[K, V] {
  override def contains(key: K): Boolean = ???

  override def get(key: K): Option[V] = ???

  override def set(key: K, value: V): Boolean = ???

  override def delete(key: K): Unit = ???
}

abstract class DbStore[K <: Serializable, V <: Serializable](val db: LevelDbStorage, val storeType: StoreType.Value) {
  protected val cache: Cache[K, V] = new LRUCache(100) //TODO: read config file

  def foreach(func: (K, V) => Unit): Unit = {
    db.find(Array(storeType.id.toByte), (k, v) => {
      func(toKey(k), toValue(v))
    })
  }

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

  def delete(key: K, batch: WriteBatch = null): Unit = {
    deleteBackStore(key, batch)
    cache.delete(key)
  }

  protected def genKey(key: K): Array[Byte] = {
    val bos = new ByteArrayOutputStream(keySize + 1)
    val os = new DataOutputStream(bos)
    os.writeByte(storeType.id.toByte)
    key.serialize(os)
    bos.toByteArray
  }

  protected def getFromBackStore(key: K): Option[V] = {
    val opt = new ReadOptions().fillCache(false)
    db.get(genKey(key), opt) match {
      case Some(value) => Some(toValue(value))
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

  protected def deleteBackStore(key: K, batch: WriteBatch): Unit = {
    if (batch == null) {
      db.delete(genKey(key))
    } else {
      batch.delete(genKey(key))
    }
  }

  protected def keySize: Int

  protected def toKey(bytes: Array[Byte]): K

  protected def toValue(bytes: Array[Byte]): V
}

class HeaderStore(db: LevelDbStorage) extends DbStore[UInt256, BlockHeader](db, StoreType.Block) {
  override protected def keySize = UInt256.Size

  override protected def toKey(bytes: Array[Byte]): UInt256 = {
    UInt256.fromBytes(bytes)
  }

  override protected def toValue(bytes: Array[Byte]): BlockHeader = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(BlockHeader.deserialize)
  }
}

case class IntKey(value: Int) extends Serializable {
  override def serialize(os: DataOutputStream): Unit = {
    os.writeInt(value)
  }
}

class IntKeyStore[V <: Serializable](db: LevelDbStorage, storeType: StoreType.Value, toV: Array[Byte] => V) extends DbStore[IntKey, V](db, storeType) {
  override protected def keySize: Int = 4

  override protected def toKey(bytes: Array[Byte]): IntKey = {
    val ba = new ByteArrayInputStream(bytes)
    val is = new DataInputStream(ba)
    IntKey(is.readInt)
  }

  override protected def toValue(bytes: Array[Byte]): V = {
    toV(bytes)
  }
}

class HeightStore(db: LevelDbStorage) {
  val innerStore = new IntKeyStore[UInt256](db, StoreType.Height, UInt256.fromBytes)

  def contains(key: Int): Boolean = {
    innerStore.contains(IntKey(key))
  }

  def get(key: Int): Option[UInt256] = {
    innerStore.get(IntKey(key))
  }

  def set(key: Int, value: UInt256, batch: WriteBatch = null): Boolean = {
    innerStore.set(IntKey(key), value, batch)
  }

  def foreach(func: (Int, UInt256) => Unit): Unit = {
    innerStore.foreach((key, value) => {
      func(key.value, value)
    })
  }
}

class TransactionStore(db: LevelDbStorage) extends DbStore[UInt256, Transaction](db, StoreType.Transaction) {
  override protected def keySize: Int = UInt256.Size

  override protected def toKey(bytes: Array[Byte]): UInt256 = {
    UInt256.fromBytes(bytes)
  }

  override protected def toValue(bytes: Array[Byte]): Transaction = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(Transaction.deserialize)
  }
}

class BlkTxMappingStore(db: LevelDbStorage) extends DbStore[UInt256, BlkTxMapping](db, StoreType.BlkTxMapping) {
  override protected def keySize: Int = UInt256.Size

  override protected def toKey(bytes: Array[Byte]): UInt256 = {
    UInt256.fromBytes(bytes)
  }

  override protected def toValue(bytes: Array[Byte]): BlkTxMapping = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(BlkTxMapping.deserialize)
  }
}

class UTXOStore(db: LevelDbStorage) extends DbStore[UTXOKey, TransactionOutput](db, StoreType.UTXO) {
  override protected def keySize: Int = UTXOKey.Size

  override protected def toKey(bytes: Array[Byte]): UTXOKey = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(UTXOKey.deserialize)
  }

  override protected def toValue(bytes: Array[Byte]): TransactionOutput = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(TransactionOutput.deserialize)
  }
}

class AccountStore(db: LevelDbStorage) extends DbStore[UInt160, Account](db, StoreType.Account) {
  override protected def keySize: Int = UInt160.Size

  override protected def toKey(bytes: Array[Byte]): UInt160 = {
    UInt160.fromBytes(bytes)
  }

  override protected def toValue(bytes: Array[Byte]): Account = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(Account.deserialize)
  }
}

case class HeadBlock(height: Int, id: UInt256) extends Serializable {
  override def serialize(os: DataOutputStream): Unit = {
    os.writeInt(height)
    os.write(id)
  }
}

object HeadBlock {
  def deserialize(is: DataInputStream): HeadBlock = {
    import com.apex.common.Serializable._
    HeadBlock(
      is.readInt(),
      is.readObj(UInt256.deserialize)
    )
  }
}

class HeadBlockStore(db: LevelDbStorage) {
  private val key = Array(StoreType.HeadBlock.id.toByte) ++ StoreType.HeadBlock.toString.getBytes

  def get(): Option[HeadBlock] = {
    db.get(key) match {
      case Some(bytes) => Some(fromBytes(bytes))
      case None => None
    }
  }

  def set(blockHeader: BlockHeader, writeBatch: WriteBatch = null): Boolean = {
    val value = HeadBlock(blockHeader.index, blockHeader.id).toBytes
    if (writeBatch == null) {
      db.set(key, value)
    } else {
      writeBatch.put(key, value)
      true
    }
  }

  private def fromBytes(bytes: Array[Byte]): HeadBlock = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(HeadBlock.deserialize)
  }
}