package com.apex.core

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.util.Map

import com.apex.common.{Cache, LRUCache, Serializable}
import com.apex.consensus._
import com.apex.crypto.Ecdsa.PublicKey
import com.apex.crypto.{FixedNumber, UInt160, UInt256}
import com.apex.proposal.{Proposal, ProposalVoteList}
import com.apex.settings.DataBaseSettings
import com.apex.storage.{Batch, ByteArray, LevelDbStorage, Storage}

import scala.collection.mutable.ArrayBuffer

class BlockStore(db: Storage.raw, capacity: Int)
  extends StoreBase[UInt256, Block](db, capacity)
    with BlockPrefix
    with UInt256Key
    with BlockValue

class BlockCacheStore(db: Storage.raw, capacity: Int)
  extends StoreBase[Long, Block](db, capacity)
    with BlockCachePrefix
    with LongKey
    with BlockValue

class HeaderStore(db: Storage.raw, capacity: Int)
  extends StoreBase[UInt256, BlockHeader](db, capacity)
    with HeaderPrefix
    with UInt256Key
    with BlockHeaderValue

class ScheduleTxStore(db: Storage.raw, capacity: Int)
  extends StoreBase[UInt256, Transaction](db, capacity)
    with ScheduleTxPrefix
    with UInt256Key
    with TransactionValue

class AccountStore(db: Storage.raw, capacity: Int)
  extends StoreBase[UInt160, Account](db, capacity)
    with AccountPrefix
    with UInt160Key
    with AccountValue

class WitnessVoteStore(db: Storage.raw, capacity: Int)
  extends StoreBase[UInt160, WitnessVote](db, capacity)
    with WitnessVotePrefix
    with UInt160Key
    with WitnessVoteValue

class ContractStore(db: Storage.raw, capacity: Int)
  extends StoreBase[UInt160, Contract](db, capacity)
    with ContractPrefix
    with UInt160Key
    with ContractValue

class ContractStateStore(db: Storage.raw, capacity: Int)
  extends StoreBase[Array[Byte], Array[Byte]](db, capacity)
    with ContractStatePrefix
    with ByteArrayKey
    with ByteArrayValue

class ReceiptStore(db: Storage.raw, capacity:Int)
  extends StoreBase[UInt256, TransactionReceipt](db, capacity)
    with ReceiptPrefix
    with UInt256Key
    with ReceiptValue

class HeightStore(db: Storage.raw, capacity: Int)
  extends StoreBase[Long, UInt256](db, capacity)
    with HeightToIdIndexPrefix
    with LongKey
    with UInt256Value

class BlockIdStore(db: Storage.raw, capacity: Int)
  extends StoreBase[UInt256, Long](db, capacity)
    with IdToHeightIndexPrefix
    with UInt256Key
    with LongValue

class ForkItemStore(db: Storage.raw, capacity: Int)
  extends StoreBase[UInt256, ForkItem](db, capacity)
    with ForkItemPrefix
    with UInt256Key
    with ForkItemValue

class HeadBlockStore(db: Storage.raw)
  extends StateStore[BlockHeader](db)
    with HeadBlockStatePrefix
    with BlockHeaderValue

class ProducerStateStore(db: Storage.raw)
  extends StateStore[ProducerStatus](db)
    with ProducerStatePrefix
    with ProducerStatusValue

class LatestConfirmedBlockStore(db: Storage.raw)
  extends StateStore[BlockHeader](db)
    with LatestConfirmedStatePrefix
    with BlockHeaderValue

class SwitchStateStore(db: Storage.raw)
  extends StateStore[SwitchState](db)
    with SwitchStatePrefix
    with SwitchStateValue

class PreviousWitnessStore(db: Storage.raw)
  extends StateStore[WitnessList](db)
    with PreviousWitnessStatePrefix
    with WitnessListValue

class CurrentWitnessStore(db: Storage.raw)
  extends StateStore[WitnessList](db)
    with CurrentWitnessStatePrefix
    with WitnessListValue

class PendingWitnessStore(db: Storage.raw)
  extends StateStore[WitnessList](db)
    with PendingWitnessStatePrefix
    with WitnessListValue

class ProposalVoteListStore(db: Storage.raw)
  extends StateStore[ProposalVoteList](db)
    with ProposalVoteListPrefix
    with ProposalVoteListValue

class WitnessBlockCountLastWeekStore(db: Storage.raw)
  extends StateStore[WitnessBlockCount](db)
    with BlockCountLastWeekPrefix
    with WitnessBlockCountValue

class WitnessBlockCountThisWeekStore(db: Storage.raw)
  extends StateStore[WitnessBlockCount](db)
    with BlockCountThisWeekPrefix
    with WitnessBlockCountValue

class MinerAwardStore(db: Storage.raw)
  extends StateStore[FixedNumber](db)
    with MinerAwardPrefix
    with FixedNumberValue

//class WitnessInfoStore(db: Storage.raw)
//  extends StateStore[WitnessMap](db)
//    with AllWitnessMapStatePrefix
//    with WitnessMapValue

class WitnessInfoStore(db: Storage.raw, capacity: Int)
  extends StoreBase[UInt160, WitnessInfo](db, capacity)
    with WitnessInfoPrefix
    with UInt160Key
    with WitnessInfoValue

class ProposalStore(db: Storage.raw, capacity: Int)
  extends StoreBase[UInt256, Proposal](db, capacity)
    with ProposalPrefix
    with UInt256Key
    with ProposalValue

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
  val Block = Value(0x04)
  val ForkItem = Value(0x05)
  val Contract = Value(0x06)
  val ContractState = Value(0x07)
  val Receipt = Value(0x08)
//  val Peer = Value(0x09)
  val WitnessVotes = Value(0x0a)
  val scheduleTransaction = Value(0x0b)
  val WitnessInfo = Value(0x0c)
  val Proposal = Value(0x0d)
  val BlockCache = Value(0x0e)
}

object IndexType extends Enumeration {
  val BlockHeightToId = Value(0x00)
  val BlockIdToHeight = Value(0x01)
  //val UTXO = Value(0x02)
  //val NameToAccount = Value(0x03)
}

object StateType extends Enumeration {
  val HeadBlock = Value(0x00)
  val Producer = Value(0x01)
  val LatestConfirmed = Value(0x02)
  val SwitchState = Value(0x03)
  val PreviousWitnessState = Value(0x04)
  val CurrentWitnessState = Value(0x05)
  val PendingWitnessState = Value(0x06)
  val AllWitnessMapState = Value(0x07)
  val ProposalVoteList = Value(0x08)
  val BlockCountLastWeek = Value(0x09)
  val BlockCountThisWeek = Value(0x0a)
  val MinerAward = Value(0x0b)
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

trait BlockPrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.Block
}

trait BlockCachePrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.BlockCache
}

trait HeaderPrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.BlockHeader
}

trait ScheduleTxPrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.scheduleTransaction
}

trait AccountPrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.Account
}

trait WitnessVotePrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.WitnessVotes
}

trait ContractPrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.Contract
}

trait ContractStatePrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.ContractState
}

trait ReceiptPrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.Receipt
}

trait ForkItemPrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.ForkItem
}

trait WitnessInfoPrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.WitnessInfo
}

trait ProposalPrefix extends DataPrefix {
  override val dataType: DataType.Value = DataType.Proposal
}

trait HeightToIdIndexPrefix extends IndexPrefix {
  override val indexType: IndexType.Value = IndexType.BlockHeightToId
}

trait IdToHeightIndexPrefix extends IndexPrefix {
  override val indexType: IndexType.Value = IndexType.BlockIdToHeight
}

trait HeadBlockStatePrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.HeadBlock
}

trait ProducerStatePrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.Producer
}

trait LatestConfirmedStatePrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.LatestConfirmed
}

trait SwitchStatePrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.SwitchState
}

trait PreviousWitnessStatePrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.PreviousWitnessState
}

trait CurrentWitnessStatePrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.CurrentWitnessState
}

trait PendingWitnessStatePrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.PendingWitnessState
}

trait ProposalVoteListPrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.ProposalVoteList
}

trait BlockCountLastWeekPrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.BlockCountLastWeek
}

trait BlockCountThisWeekPrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.BlockCountThisWeek
}

trait MinerAwardPrefix extends StatePrefix {
  override val stateType: StateType.Value = StateType.MinerAward
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

trait LongKey extends KeyConverterProvider[Long] {
  override val keyConverter: Converter[Long] = new LongConverter
}

trait StringKey extends KeyConverterProvider[String] {
  override val keyConverter: Converter[String] = new StringConverter
}

trait ByteArrayKey extends KeyConverterProvider[Array[Byte]] {
  override val keyConverter: Converter[Array[Byte]] = new ByteArrayConverter
}

trait UInt160Key extends KeyConverterProvider[UInt160] {
  override val keyConverter: Converter[UInt160] = new SerializableConverter(UInt160.deserialize)
}

trait UInt256Key extends KeyConverterProvider[UInt256] {
  override val keyConverter: Converter[UInt256] = new SerializableConverter(UInt256.deserialize)
}

trait PublicKeyKey extends KeyConverterProvider[PublicKey] {
  override val keyConverter: Converter[PublicKey] = new SerializableConverter(PublicKey.deserialize)
}

trait ContractStateKey extends KeyConverterProvider[Array[Byte]] {
  override val keyConverter: Converter[Array[Byte]] = new ByteArrayConverter
}

//trait UTXOKeyKey extends KeyConverterProvider[UTXOKey] {
//  override val keyConverter: Converter[UTXOKey] = new SerializableConverter(UTXOKey.deserialize)
//}

trait IntValue extends ValueConverterProvider[Int] {
  override val valConverter: Converter[Int] = new IntConverter
}

trait LongValue extends ValueConverterProvider[Long] {
  override val valConverter: Converter[Long] = new LongConverter
}

trait StringValue extends ValueConverterProvider[String] {
  override val valConverter: Converter[String] = new StringConverter
}

trait ByteArrayValue extends ValueConverterProvider[Array[Byte]] {
  override val valConverter: Converter[Array[Byte]] = new ByteArrayConverter
}

trait UInt160Value extends ValueConverterProvider[UInt160] {
  override val valConverter: Converter[UInt160] = new SerializableConverter(UInt160.deserialize)
}

trait UInt256Value extends ValueConverterProvider[UInt256] {
  override val valConverter: Converter[UInt256] = new SerializableConverter(UInt256.deserialize)
}

trait BlockValue extends ValueConverterProvider[Block] {
  override val valConverter: Converter[Block] = new SerializableConverter(Block.deserialize)
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

trait WitnessVoteValue extends ValueConverterProvider[WitnessVote] {
  override val valConverter: Converter[WitnessVote] = new SerializableConverter(WitnessVote.deserialize)
}

trait ContractValue extends ValueConverterProvider[Contract]{
  override val valConverter: Converter[Contract] = new SerializableConverter(Contract.deserialize)
}

trait ContractStateValue extends ValueConverterProvider[ContractState]{
  override val valConverter: Converter[ContractState] = new SerializableConverter(ContractState.deserialize)
}

trait ReceiptValue extends ValueConverterProvider[TransactionReceipt]{
  override val valConverter: Converter[TransactionReceipt] = new SerializableConverter(TransactionReceipt.deserialize)
}

trait ForkItemValue extends ValueConverterProvider[ForkItem] {
  override val valConverter: Converter[ForkItem] = new SerializableConverter(ForkItem.deserialize)
}

trait BlkTxMappingValue extends ValueConverterProvider[BlkTxMapping] {
  override val valConverter: Converter[BlkTxMapping] = new SerializableConverter(BlkTxMapping.deserialize)
}

trait ProducerStatusValue extends ValueConverterProvider[ProducerStatus] {
  override val valConverter: Converter[ProducerStatus] = new SerializableConverter(ProducerStatus.deserialize)
}

trait HeadBlockValue extends ValueConverterProvider[HeadBlock] {
  override val valConverter: Converter[HeadBlock] = new SerializableConverter(HeadBlock.deserialize)
}

trait SwitchStateValue extends ValueConverterProvider[SwitchState] {
  override val valConverter: Converter[SwitchState] = new SerializableConverter(SwitchState.deserialize)
}

trait WitnessListValue extends ValueConverterProvider[WitnessList] {
  override val valConverter: Converter[WitnessList] = new SerializableConverter(WitnessList.deserialize)
}

trait ProposalVoteListValue extends ValueConverterProvider[ProposalVoteList] {
  override val valConverter: Converter[ProposalVoteList] = new SerializableConverter(ProposalVoteList.deserialize)
}

trait WitnessBlockCountValue extends ValueConverterProvider[WitnessBlockCount] {
  override val valConverter: Converter[WitnessBlockCount] = new SerializableConverter(WitnessBlockCount.deserialize)
}

trait FixedNumberValue extends ValueConverterProvider[FixedNumber] {
  override val valConverter: Converter[FixedNumber] = new SerializableConverter(FixedNumber.deserialize)
}

trait WitnessMapValue extends ValueConverterProvider[WitnessMap] {
  override val valConverter: Converter[WitnessMap] = new SerializableConverter(WitnessMap.deserialize)
}

trait WitnessInfoValue extends ValueConverterProvider[WitnessInfo] {
  override val valConverter: Converter[WitnessInfo] = new SerializableConverter(WitnessInfo.deserialize)
}

trait ProposalValue extends ValueConverterProvider[Proposal] {
  override val valConverter: Converter[Proposal] = new SerializableConverter(Proposal.deserialize)
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

class LongConverter extends Converter[Long] {
  override def deserializer(is: DataInputStream): Long = {
    import com.apex.common.Serializable._
    is.readLong
  }

  override def serializer(key: Long, os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeLong(key)
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

class ByteArrayConverter extends Converter[Array[Byte]] {
  override def deserializer(is: DataInputStream): Array[Byte] = {
    import com.apex.common.Serializable._
    is.readByteArray
  }

  override def serializer(key: Array[Byte], os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeByteArray(key)
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

case class HeadBlock(height: Long, id: UInt256) extends Serializable {
  override def serialize(os: DataOutputStream): Unit = {
    os.writeLong(height)
    os.write(id)
  }
}

object HeadBlock {
  //final val Size = 8 + UInt256.Size

  def deserialize(is: DataInputStream): HeadBlock = {
    import com.apex.common.Serializable._
    HeadBlock(
      is.readLong(),
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



//
//class TrackKey[K](val key: K) {
//  override def equals(obj: scala.Any): Boolean = {
//    obj match {
//      case that: TrackKey[K] => key.equals(that.key)
//      case _ => false
//    }
//  }
//
//  override def hashCode(): Int = {
//    key.hashCode()
//  }
//}

class TrackValue(var value: Array[Byte], var deleted: Boolean) {
  def this(v: Array[Byte]) = this(v, false)

  def this() = this(null, true)

  def set(v: Array[Byte]): Unit = {
    deleted = false
    value = v
  }

  def delete(): Unit = {
    deleted = true
    value = null
  }
}

class Tracking(backend: Storage.raw) extends Storage.raw {
  protected val buffer = collection.mutable.Map.empty[ByteArray, TrackValue]

  def newTracking() = {
    new Tracking(this)
  }

  override def get(key: Array[Byte]): Option[Array[Byte]] = {
    buffer.get(ByteArray(key)) match {
      case Some(trackValue) if trackValue.deleted =>
        None
      case Some(trackValue) =>
        Some(trackValue.value)
      case None =>
        backend.get(key)
    }
  }

  override def set(k: Array[Byte], value: Array[Byte], batch: Batch = null): Boolean = {
    val key = ByteArray(k)
    buffer.get(key) match {
      case Some(trackValue) =>
        trackValue.set(value)
      case None =>
        buffer.put(key, new TrackValue(value))
    }
    true
  }

  override def scan(prefix: Array[Byte]): ArrayBuffer[Array[Byte]] = {
    val lists = ArrayBuffer.empty[ Array[Byte]]
    buffer.foreach(element => {
      if(element._1.bytes.slice(0,2).sameElements(prefix)) lists.append(element._2.value)
    })
    lists.appendAll(backend.scan(prefix))
    lists
  }

  override def delete(k: Array[Byte], batch: Batch = null): Boolean = {
    val key = ByteArray(k)
    buffer.get(key) match {
      case Some(trackValue) =>
        trackValue.delete()
      case None =>
        buffer.put(key, new TrackValue())
    }
    true
  }

  override def commit(): Unit = {
    for ((key, trackValue) <- buffer) {
      if (trackValue.deleted) {
        backend.delete(key.bytes, null)
      } else {
        backend.set(key.bytes, trackValue.value, null)
      }
    }
    buffer.clear()
  }

  override def rollBack(): Unit = {
    buffer.clear()
  }

  override def commit(revision: Long): Unit = ???

  override def newSession(): Unit = ???

  override def revision(): Long = ???

}

class TrackingRoot(db: Storage.lowLevelRaw) extends Tracking(db) {
  override def newSession(): Unit = {
    db.newSession()
  }


  override def revision(): Long = {
    db.revision()
  }

  override def commit(): Unit = {
    db.batchWrite(batch => {
      for ((key, trackValue) <- buffer) {
        if (trackValue.deleted) {
          db.delete(key.bytes, batch)
        } else {
          db.set(key.bytes, trackValue.value, batch)
        }
      }
    })
    buffer.clear()
  }

  override def commit(revision: Long): Unit = {
    commit()
    db.commit(revision)
  }

  override def rollBack(): Unit = {
    buffer.clear()
    db.rollBack()
  }
}

object Tracking {
  def root(db: Storage.lowLevelRaw) = {
    new TrackingRoot(db)
  }
}

abstract class StateStore[V ](db: Storage.raw) {
  protected var cached: Option[V] = None

  def prefixBytes(): Array[Byte]

  val valConverter: Converter[V]

  def get(): Option[V] = {
    db.get(prefixBytes).map(valConverter.fromBytes)
  }

  def getLists(prefix: Array[Byte]) = {
    //val lowLevelDB = db.asInstanceOf[Storage.raw]
    //lowLevelDB.scan(prefix).map(records => valConverter.fromBytes(records.getValue))
    getListFromBackStore(prefix)
  }

  protected def getListFromBackStore(prefix: Array[Byte]): ArrayBuffer[V] = {
    db.scan(prefix).map(records => valConverter.fromBytes(records))
  }

  def set(value: V, batch: Batch = null): Boolean = {
    db.set(prefixBytes, valConverter.toBytes(value), batch)
  }

  def delete(batch: Batch = null): Unit = {
    db.delete(prefixBytes, batch)
  }
}

abstract class StoreBase[K, V](val db: Storage.raw, cacheCapacity: Int) {
  val prefixBytes: Array[Byte]

  val keyConverter: Converter[K]

  val valConverter: Converter[V]

  def contains(key: K) = {
    backContains(key)
  }

  def get(key: K) = {
    getFromBackStore(key)
  }

  def getLists(prefix: Array[Byte]) = {
    //val lowLevelDB = db.asInstanceOf[Storage.raw]
    //lowLevelDB.scan(prefix).map(records => valConverter.fromBytes(records.getValue))
    getListFromBackStore(prefix)
  }

  def set(key: K, value: V, batch: Batch = null) = {
    setBackStore(key, value, batch)
  }

  def delete(key: K, batch: Batch = null) = {
    deleteBackStore(key, batch)
  }

  def foreach(action: (K, V) => Unit) = {
    val lowLevelDB = db.asInstanceOf[Storage.lowLevelRaw]
    lowLevelDB.find(prefixBytes, (k, v) => {
      val key = keyConverter.fromBytes(k.drop(prefixBytes.length))
      val value = valConverter.fromBytes(v)
      action(key, value)
    })
  }

  protected def genKey(key: K): Array[Byte] = {
    prefixBytes ++ keyConverter.toBytes(key)
  }

  protected def backContains(key: K): Boolean = {
    db.contains(genKey(key))
  }

  protected def getFromBackStore(key: K): Option[V] = {
    db.get(genKey(key)).map(valConverter.fromBytes)
  }

  protected def getListFromBackStore(prefix: Array[Byte]): ArrayBuffer[V] = {
    db.scan(prefix).map(records => valConverter.fromBytes(records))
  }

  protected def setBackStore(key: K, value: V, batch: Batch): Boolean = {
    db.set(genKey(key), valConverter.toBytes(value), batch)
  }

  protected def deleteBackStore(key: K, batch: Batch): Boolean = {
    db.delete(genKey(key), batch)
  }
}