package com.apex.core

import java.io.{DataInputStream, DataOutputStream}

import com.apex.common.{Cache, LRUCache, Serializable}
import com.apex.crypto.{UInt160, UInt256}
import com.apex.storage.LevelDbStorage
import org.iq80.leveldb.{ReadOptions, WriteBatch}

class HeaderStore(db: LevelDbStorage, capacity: Int)
  extends StoreBase[UInt256, BlockHeader](db, capacity)
    with HeaderPrefix 
    with UInt256Key 
    with BlockHeaderValue

class TransactionStore(db: LevelDbStorage, capacity: Int)
  extends StoreBase[UInt256, Transaction](db, capacity)
    with TxPrefix 
    with UInt256Key 
    with TransactionValue

class AccountStore(db: LevelDbStorage, capacity: Int)
  extends StoreBase[UInt160, Account](db, capacity)
    with AccountPrefix 
    with UInt160Key 
    with AccountValue

class HeightStore(db: LevelDbStorage, capacity: Int)
  extends StoreBase[Int, UInt256](db, capacity)
    with HeightToIdIndexPrefix 
    with IntKey 
    with UInt256Value

class BlkTxMappingStore(db: LevelDbStorage, capacity: Int)
  extends StoreBase[UInt256, BlkTxMapping](db, capacity)
    with BlockIdToTxIdIndexPrefix
    with UInt256Key
    with BlkTxMappingValue

//class UTXOStore(db: LevelDbStorage, capacity: Int)
//  extends StoreBase[UTXOKey, TransactionOutput](db, capacity)
//    with UTXOIndexPrefix
//    with UTXOKeyKey
//    with TxOutputValue

class HeadBlockStore(db: LevelDbStorage)
  extends StateStore[HeadBlock](db)
    with HeadBlockStatePrefix
    with HeadBlockValue

class ProducerStateStore(db: LevelDbStorage)
  extends StateStore[ProducerStatus](db)
    with ProducerStatePrefix
    with ProducerStatusValue

object StoreType extends Enumeration {
  val Data = Value(0x00)
  val Index = Value(0x01)
  val State = Value(0x02)
}

object DataType extends Enumeration {
  val BlockHeader = Value(0x00)
  val Transaction = Value(0x01)
  val Account = Value(0x02)
}

object IndexType extends Enumeration {
  val BlockHeightToId = Value(0x00)
  val BlockIdToTxId = Value(0x01)
  val UTXO = Value(0x02)
}

object StateType extends Enumeration {
  val HeadBlock = Value(0x00)
  val Producer = Value(0x01)
}

trait Prefix {
  val prefixBytes: Array[Byte]
}

trait DataPrefix extends Prefix {
  val storeType = StoreType.Data
  val dataType: DataType.Value
  override lazy val prefixBytes: Array[Byte] = Array(storeType.id.toByte, dataType.id.toByte)
}

trait IndexPrefix extends Prefix {
  val storeType = StoreType.Index
  val indexType: IndexType.Value
  override lazy val prefixBytes: Array[Byte] = Array(storeType.id.toByte, indexType.id.toByte)
}

trait StatePrefix extends Prefix {
  val storeType = StoreType.Index
  val stateType: StateType.Value
  override lazy val prefixBytes: Array[Byte] = Array(storeType.id.toByte, stateType.id.toByte)
}

trait HeaderPrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.BlockHeader
}

trait TxPrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.Transaction
}

trait AccountPrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.Account
}

trait HeightToIdIndexPrefix extends IndexPrefix {
  override val indexType: IndexType.Value = IndexType.BlockHeightToId
}

trait BlockIdToTxIdIndexPrefix extends IndexPrefix {
  override val indexType: IndexType.Value = IndexType.BlockIdToTxId
}

trait UTXOIndexPrefix extends IndexPrefix {
  override val indexType: IndexType.Value = IndexType.UTXO
}

trait HeadBlockStatePrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.HeadBlock
}

trait ProducerStatePrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.Producer
}

trait Converter[A] {
  def toBytes(key: A): Array[Byte]

  def fromBytes(bytes: Array[Byte]): A
}

trait IntKey extends KeyConverterProvider[Int] {
  override val keyConverter: Converter[Int] = new IntConverter
}

trait UInt160Key extends KeyConverterProvider[UInt160] {
  override val keyConverter: Converter[UInt160] = new SerializableConverter(UInt160.deserialize)
}

trait UInt256Key extends KeyConverterProvider[UInt256] {
  override val keyConverter: Converter[UInt256] = new SerializableConverter(UInt256.deserialize)
}

//trait UTXOKeyKey extends KeyConverterProvider[UTXOKey] {
//  override val keyConverter: Converter[UTXOKey] = new SerializableConverter(UTXOKey.deserialize)
//}

trait UInt256Value extends ValueConverterProvider[UInt256] {
  override val valConverter: Converter[UInt256] = new SerializableConverter(UInt256.deserialize)
}

trait BlockHeaderValue extends ValueConverterProvider[BlockHeader] {
  override val valConverter: Converter[BlockHeader] = new SerializableConverter(BlockHeader.deserialize)
}

trait TransactionValue extends ValueConverterProvider[Transaction] {
  override val valConverter: Converter[Transaction] = new SerializableConverter(Transaction.deserialize)
}

trait AccountValue extends ValueConverterProvider[Account] {
  override val valConverter: Converter[Account] = new SerializableConverter(Account.deserialize)
}

trait BlkTxMappingValue extends ValueConverterProvider[BlkTxMapping] {
  override val valConverter: Converter[BlkTxMapping] = new SerializableConverter(BlkTxMapping.deserialize)
}

//trait TxOutputValue extends ValueConverterProvider[TransactionOutput] {
//  override val valConverter: Converter[TransactionOutput] = new SerializableConverter(TransactionOutput.deserialize)
//}

trait ProducerStatusValue extends ValueConverterProvider[ProducerStatus] {
  override val valConverter: Converter[ProducerStatus] = new SerializableConverter(ProducerStatus.deserialize)
}

trait HeadBlockValue extends ValueConverterProvider[HeadBlock] {
  override val valConverter: Converter[HeadBlock] = new SerializableConverter(HeadBlock.deserialize)
}

class IntConverter extends Converter[Int] {
  override def fromBytes(bytes: Array[Byte]): Int = {
    BigInt(bytes).toInt
  }

  override def toBytes(key: Int): Array[Byte] = {
    BigInt(key).toByteArray
  }
}

class SerializableConverter[A <: Serializable](f: DataInputStream => A) extends Converter[A] {
  override def fromBytes(bytes: Array[Byte]): A = {
    import com.apex.common.Serializable._
    bytes.toInstance(f)
  }

  override def toBytes(key: A): Array[Byte] = {
    key.toBytes
  }
}

trait KeyConverterProvider[A] {
  val keyConverter: Converter[A]
}

trait ValueConverterProvider[A] {
  val valConverter: Converter[A]
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

case class HeadBlock(height: Int, id: UInt256) extends Serializable {
  override def serialize(os: DataOutputStream): Unit = {
    os.writeInt(height)
    os.write(id)
  }
}

object HeadBlock {
  final val Size = 4 + UInt256.Size

  def deserialize(is: DataInputStream): HeadBlock = {
    import com.apex.common.Serializable._
    HeadBlock(
      is.readInt(),
      is.readObj(UInt256.deserialize)
    )
  }

  def fromHeader(header: BlockHeader) = {
    HeadBlock(header.index, header.id)
  }
}

case class ProducerStatus(val distance: Long) extends Serializable {
  def plusDistance(n: Long) = this.copy(distance = this.distance + n)

  override def serialize(os: DataOutputStream): Unit = {
    os.writeLong(distance)
  }
}

object ProducerStatus {
  def deserialize(is: DataInputStream): ProducerStatus = {
    ProducerStatus(is.readLong)
  }
}

abstract class StateStore[V <: Serializable](db: LevelDbStorage) {
  protected var cached: Option[V] = None

  def prefixBytes(): Array[Byte]

  val valConverter: Converter[V]

  def get(): Option[V] = {
    if (cached.isEmpty) {
      val bytes = db.get(prefixBytes)
      if (!bytes.isEmpty) {
        cached = Some(valConverter.fromBytes(bytes.get))
      }
    }
    cached
  }

  def set(value: V, batch: WriteBatch = null): Boolean = {
    if (value == null) {
      false
    } else {
      if (batch != null) {
        batch.put(prefixBytes, valConverter.toBytes(value))
        cached = Some(value)
        true
      } else {
        if (db.set(prefixBytes, valConverter.toBytes(value))) {
          cached = Some(value)
          true
        } else {
          false
        }
      }
    }
  }

  def delete(batch: WriteBatch = null): Unit = {
    if (batch != null) {
      batch.delete(prefixBytes)
    } else {
      db.delete(prefixBytes)
    }
    cached = None
  }
}

abstract class StoreBase[K, V](val db: LevelDbStorage, cacheCapacity: Int) {
  protected val cache: Cache[K, V] = new LRUCache(cacheCapacity)

  val prefixBytes: Array[Byte]

  val keyConverter: Converter[K]

  val valConverter: Converter[V]

  def foreach(func: (K, V) => Unit): Unit = {
    db.find(prefixBytes, (k, v) => {
      val kData = k.drop(prefixBytes.length)
      func(keyConverter.fromBytes(kData),
        valConverter.fromBytes(v))
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


  def set(key: K, value: V, writeBatch: WriteBatch = null): Boolean = {
    if (setBackStore(key, value, writeBatch)) {
      cache.set(key, value)
      true
    } else {
      false
    }
  }

  def delete(key: K, writeBatch: WriteBatch = null): Unit = {
    deleteBackStore(key, writeBatch)
    cache.delete(key)
  }

  protected def genKey(key: K): Array[Byte] = {
    prefixBytes ++ keyConverter.toBytes(key)
  }

  protected def getFromBackStore(key: K): Option[V] = {
    val opt = new ReadOptions().fillCache(false)
    db.get(genKey(key), opt) match {
      case Some(value) => Some(valConverter.fromBytes(value))
      case None => None
    }
  }

  protected def setBackStore(key: K, value: V, batch: WriteBatch): Boolean = {
    if (batch != null) {
      batch.put(genKey(key), valConverter.toBytes(value))
      true
    } else {
      db.set(genKey(key), valConverter.toBytes(value))
    }
  }

  protected def deleteBackStore(key: K, batch: WriteBatch): Unit = {
    if (batch != null) {
      batch.delete(genKey(key))
    } else {
      db.delete(genKey(key))
    }
  }
}