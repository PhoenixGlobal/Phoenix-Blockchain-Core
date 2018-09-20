package com.apex.core

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.common.{Cache, LRUCache, Serializable}
import com.apex.crypto.{UInt160, UInt256}
import com.apex.storage.{LevelDbStorage, PersistentStack}
import org.iq80.leveldb.{ReadOptions, WriteBatch}

import scala.collection.mutable.ListBuffer

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

class NameToAccountStore(db: LevelDbStorage, capacity: Int)
  extends StoreBase[String, UInt160](db, capacity)
    with NameToAccountIndexPrefix
    with StringKey
    with UInt160Value

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
  val Session = Value(0x03)
}

object IndexType extends Enumeration {
  val BlockHeightToId = Value(0x00)
  val BlockIdToTxId = Value(0x01)
  val UTXO = Value(0x02)
  val NameToAccount = Value(0x03)
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

trait NameToAccountIndexPrefix extends IndexPrefix {
  override val indexType: IndexType.Value = IndexType.NameToAccount
}

trait HeadBlockStatePrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.HeadBlock
}

trait ProducerStatePrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.Producer
}

trait Converter[A] {
  def toBytes(key: A): Array[Byte] = {
    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    serializer(key, os)
    bs.toByteArray
  }

  def fromBytes(bytes: Array[Byte]): A = {
    val bs = new ByteArrayInputStream(bytes)
    val is = new DataInputStream(bs)
    deserializer(is)
  }

  def deserializer(is: DataInputStream): A

  def serializer(key: A, os: DataOutputStream): Unit
}

trait IntKey extends KeyConverterProvider[Int] {
  override val keyConverter: Converter[Int] = new IntConverter
}

trait StringKey extends KeyConverterProvider[String] {
  override val keyConverter: Converter[String] = new StringConverter
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

trait UInt160Value extends ValueConverterProvider[UInt160] {
  override val valConverter: Converter[UInt160] = new SerializableConverter(UInt160.deserialize)
}

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
  override def deserializer(is: DataInputStream): Int = {
    import com.apex.common.Serializable._
    is.readVarInt
  }

  override def serializer(key: Int, os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeVarInt(key)
  }
}

class StringConverter extends Converter[String] {
  override def deserializer(is: DataInputStream): String = {
    import com.apex.common.Serializable._
    is.readString
  }

  override def serializer(key: String, os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeString(key)
  }
}

class SerializableConverter[A <: Serializable](f: DataInputStream => A) extends Converter[A] {
  override def deserializer(is: DataInputStream): A = {
    f(is)
  }

  override def serializer(key: A, os: DataOutputStream): Unit = {
    key.serialize(os)
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

//  private var session: Option[Session] = None
  private val sessionMgr = new SessionManager(this)

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
    cache.contains(key) || backContains(key)
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
    val batch = sessionMgr.beginSet(key, value, writeBatch)
    if (setBackStore(key, value, batch)) {
      cache.set(key, value)
      true
    } else {
      false
    }
  }

  def delete(key: K, writeBatch: WriteBatch = null): Unit = {
    val batch = sessionMgr.beginDelete(key, writeBatch)
    deleteBackStore(key, batch)
    cache.delete(key)
  }

  def beginTransaction(): Unit = {
    sessionMgr.newSession()
  }

  def commit(): Unit = {
    sessionMgr.commit()
  }

  def rollBack(): Unit = {
    sessionMgr.rollBack()
  }

  protected def genKey(key: K): Array[Byte] = {
    prefixBytes ++ keyConverter.toBytes(key)
  }

  protected def backContains(key: K): Boolean = {
    db.containsKey(genKey(key))
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

  import collection.mutable.Map

  class SessionItem(val insert: Map[K, V] = Map.empty[K, V],
                    val update: Map[K, V] = Map.empty[K, V],
                    val delete: Map[K, V] = Map.empty[K, V])
    extends Serializable {
    override def serialize(os: DataOutputStream): Unit = {
      import com.apex.common.Serializable._
      os.writeMap(insert.toMap)(keyConverter.serializer, valConverter.serializer)
      os.writeMap(update.toMap)(keyConverter.serializer, valConverter.serializer)
      os.writeMap(delete.toMap)(keyConverter.serializer, valConverter.serializer)
    }

    def fill(bytes: Array[Byte]): Unit = {
      import com.apex.common.Serializable._
      val bs = new ByteArrayInputStream(bytes)
      val is = new DataInputStream(bs)
      is.readMap(keyConverter.deserializer, valConverter.deserializer).foreach(fillInsert)
      is.readMap(keyConverter.deserializer, valConverter.deserializer).foreach(fillUpdate)
      is.readMap(keyConverter.deserializer, valConverter.deserializer).foreach(fillDelete)
    }

    def clear(): Unit = {
      insert.clear()
      update.clear()
      delete.clear()
    }

    private def fillInsert(kv: (K, V)): Unit = {
      insert.put(kv._1, kv._2)
    }

    private def fillUpdate(kv: (K, V)): Unit = {
      update.put(kv._1, kv._2)
    }

    private def fillDelete(kv: (K, V)): Unit = {
      delete.put(kv._1, kv._2)
    }
  }

  class Session(store: StoreBase[K, V], val level: Int) {

    private val sessionId = Array(StoreType.Data.id.toByte, DataType.Session.id.toByte) ++ store.prefixBytes

    private val item = new SessionItem

    init()

    def onSet(k: K, v: V, batch: WriteBatch): WriteBatch = {
      var modified = true
      if (item.insert.contains(k) || item.update.contains(k)) {
        modified = false
      } else if (item.delete.contains(k)) {
        item.update.put(k, item.delete(k))
        item.delete.remove(k)
      } else {
        store.get(k) match {
          case Some(old) => {
            item.update.put(k, old)
          }
          case None => {
            item.insert.put(k, v)
          }
        }
      }
      if (modified) {
        persist(batch)
      } else {
        batch
      }
    }

    def onDelete(k: K, batch: WriteBatch): WriteBatch = {
      var modified = false
      if (item.insert.contains(k)) {
        item.insert.remove(k)
        modified = true
      } else if (item.update.contains(k)) {
        item.delete.put(k, item.update(k))
        item.update.remove(k)
        modified = true
      } else if (!item.delete.contains(k)) {
        val old = store.get(k)
        if (old.isDefined) {
          item.delete.put(k, old.get)
          modified = true
        }
      }
      if (modified) {
        persist(batch)
      } else {
        batch
      }
    }

    def close(): Unit = {
      store.db.delete(sessionId)
      item.clear
    }

    def rollBack(): Unit = {
      store.db.batchWrite(batch => {
        item.insert.foreach(p => store.delete(p._1, batch))
        item.update.foreach(p => store.set(p._1, p._2, batch))
        item.delete.foreach(p => store.set(p._1, p._2, batch))
        close()
      })
    }

    private def persist(batch: WriteBatch): WriteBatch = {
      if (batch != null) {
        batch.put(sessionId, item.toBytes)
      } else {
        val batch = store.db.beginBatch
        batch.put(sessionId, item.toBytes)
        batch
      }
    }

    private def init(): Unit = {
      val value = store.db.get(sessionId)
      if (value.isDefined) {
        item.fill(value.get)
      }
    }
  }

  class SessionManager(store: StoreBase[K, V]) {
    private val sessions = ListBuffer.empty[Session]
    private var level = 1

    def beginSet(key: K, value: V, batch: WriteBatch): WriteBatch = {
      sessions.lastOption.map(_.onSet(key, value, batch)).orNull
    }

    def beginDelete(key: K, batch: WriteBatch): WriteBatch = {
      sessions.lastOption.map(_.onDelete(key, batch)).orNull
    }

    def commit(): Unit = {
      sessions.lastOption.foreach(s => {
        s.close()
        sessions.dropRight(1)
      })
    }

    def commit(level: Int): Unit = {
      sessions.takeWhile(_.level <= level).foreach(s => {
          s.close()
          sessions -= s
        })
    }

    def rollBack(): Unit = {
      sessions.lastOption.foreach(s => {
        s.rollBack()
        sessions.dropRight(1)
        level -= 1
      })
    }

    def newSession(): Unit = {
      sessions.append(new Session(store, level))
      level += 1
    }
  }
}
