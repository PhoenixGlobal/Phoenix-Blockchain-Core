package com.apex.core

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.common.{Cache, LRUCache, Serializable}
import com.apex.crypto.{UInt160, UInt256}
import com.apex.storage.LevelDbStorage
import org.iq80.leveldb.{ReadOptions, WriteBatch}

object StoreType extends Enumeration {
  val Block = Value(0x00)
  val Transaction = Value(0x01)
  val Height = Value(0x02)
  val HeadBlock = Value(0x03)
  val UTXO = Value(0x04)
  val BlkTxMapping = Value(0x05)
  val Account = Value(0x06)
  val Address = Value(0x07)
  val Producer = Value(0x08)
}

abstract class DataStore[K <: Serializable, V <: Serializable](val db: LevelDbStorage,
                                                               val storeType: StoreType.Value,
                                                               val cacheCapacity: Int = 100 /*TODO: read config file*/) {
  protected val cache: Cache[K, V] = new LRUCache(cacheCapacity)

  def foreach(func: (K, V) => Unit): Unit = {
    db.find(Array(storeType.id.toByte), (k, v) => {
      func(toKey(k.drop(1)), toValue(v))
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

class HeaderStore(db: LevelDbStorage) extends DataStore[UInt256, BlockHeader](db, StoreType.Block) {
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

class IntKeyStore[V <: Serializable](db: LevelDbStorage, storeType: StoreType.Value, toV: Array[Byte] => V) extends DataStore[IntKey, V](db, storeType) {
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

  def delete(key: Int, batch: WriteBatch = null): Unit = {
    innerStore.delete(IntKey(key), batch)
  }

  def foreach(func: (Int, UInt256) => Unit): Unit = {
    innerStore.foreach((key, value) => {
      func(key.value, value)
    })
  }
}

class TransactionStore(db: LevelDbStorage) extends DataStore[UInt256, Transaction](db, StoreType.Transaction) {
  override protected def keySize: Int = UInt256.Size

  override protected def toKey(bytes: Array[Byte]): UInt256 = {
    UInt256.fromBytes(bytes)
  }

  override protected def toValue(bytes: Array[Byte]): Transaction = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(Transaction.deserialize)
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
}

class BlkTxMappingStore(db: LevelDbStorage) extends DataStore[UInt256, BlkTxMapping](db, StoreType.BlkTxMapping) {
  override protected def keySize: Int = UInt256.Size

  override protected def toKey(bytes: Array[Byte]): UInt256 = {
    UInt256.fromBytes(bytes)
  }

  override protected def toValue(bytes: Array[Byte]): BlkTxMapping = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(BlkTxMapping.deserialize)
  }
}

class UTXOStore(db: LevelDbStorage) extends DataStore[UTXOKey, TransactionOutput](db, StoreType.UTXO) {
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

class AccountStore(db: LevelDbStorage) extends DataStore[UInt160, Account](db, StoreType.Account) {
  override protected def keySize: Int = UInt160.Size

  override protected def toKey(bytes: Array[Byte]): UInt160 = {
    UInt160.fromBytes(bytes)
  }

  override protected def toValue(bytes: Array[Byte]): Account = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(Account.deserialize)
  }
}

//class AddressStore(db: LevelDbStorage) extends DataStore[UInt160, Seq[UTXOKey]](db, StoreType.Address) {
//  override protected def keySize: Int = UInt160.Size
//
//  override protected def toKey(bytes: Array[Byte]): UInt160 = UInt160.fromBytes(bytes)
//
//  override protected def toValue(bytes: Array[Byte]): Seq[UTXOKey] = {
//    import com.apex.common.Serializable.Reader
//    bytes.toInstances(UTXOKey.deserialize)
//  }
//}

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

case class StoreTypeKey(val storeType: StoreType.Value) extends Serializable {
  val bytes = Array(storeType.id.toByte) ++ storeType.toString.getBytes
  val Size = bytes.length

  override def serialize(os: DataOutputStream): Unit = {
    os.write(bytes)
  }
}

object StoreTypeKey {
  def deserialize(is: DataInputStream): StoreTypeKey = {
    StoreTypeKey(StoreType(is.readByte))
  }
}

abstract class StateStore[V <: Serializable](db: LevelDbStorage, storeType: StoreType.Value) extends DataStore[StoreTypeKey, V](db, storeType) {
  val key = StoreTypeKey(storeType)

  def get(): Option[V] = {
    get(key)
  }

  def set(value: V, writeBatch: WriteBatch): Boolean = {
    set(key, value, writeBatch)
  }

  def delete(writeBatch: WriteBatch): Unit = {
    delete(key, writeBatch)
  }
}

class HeadBlockStore(db: LevelDbStorage) extends StateStore[HeadBlock](db, StoreType.HeadBlock) {
  override protected def keySize: Int = key.bytes.length

  override protected def toKey(bytes: Array[Byte]): StoreTypeKey = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(StoreTypeKey.deserialize)
  }

  override protected def toValue(bytes: Array[Byte]): HeadBlock = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(HeadBlock.deserialize)
  }
}

case class ProducerState(val distance: Long) extends Serializable {
  def plusDistance(n: Long) = this.copy(distance = this.distance + n)

  override def serialize(os: DataOutputStream): Unit = {
    os.writeLong(distance)
  }
}

object ProducerState {
  def deserialize(is: DataInputStream): ProducerState = {
    ProducerState(is.readLong)
  }
}

class ProducerStateStore(db: LevelDbStorage) extends StateStore[ProducerState](db, StoreType.Producer) {
  override protected def keySize: Int = key.bytes.length

  override protected def toKey(bytes: Array[Byte]): StoreTypeKey = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(StoreTypeKey.deserialize)
  }

  override protected def toValue(bytes: Array[Byte]): ProducerState = {
    import com.apex.common.Serializable.Reader
    bytes.toInstance(ProducerState.deserialize)
  }
}
