package com.apex.storage

import java.io.DataOutputStream
import java.util
import java.util.Map.Entry

import com.apex.settings.DBType

import scala.collection.mutable.ListBuffer

object Storage {
  type raw = Storage[Array[Byte], Array[Byte]]

  type lowLevelRaw = LowLevelStorage[Array[Byte], Array[Byte]]

  def open(dbType: DBType.Value, path: String): lowLevelRaw = {
    dbType match {
      case DBType.LevelDB => LevelDbStorage.open(path)
      case _ => throw new NotImplementedError
    }
  }
}

// base trait for KV store
trait Storage[Key, Value] {
  // whether key exists
  def contains(key: Key): Boolean = get(key).isDefined

  // get value the key associated with
  def get(key: Key): Option[Value]

  // add a new key/value pair if key not exist or overwrite value the key associated with
  def set(key: Key, value: Value, batch: Batch): Boolean

  // delete the key and associated value
  def delete(key: Key, batch: Batch): Boolean

  // start a new session
  def newSession(): Unit

  // commit all operations in sessions whose revision is equal to or larger than the specified revision
  def commit(revision: Long): Unit

  // commit all operations in the latest session
  def commit(): Unit

  // undo all operations in the latest session
  def rollBack(): Unit

  // return latest revision
  def revision(): Long
}

trait LowLevelStorage[Key, Value] extends Storage[Key, Value] {
  //
  def batchWrite(action: Batch => Unit): Boolean

  // return last element
  def last(): Option[Entry[Array[Byte], Array[Byte]]]

  // apply func to all key/value pairs
  def scan(func: (Key, Value) => Unit): Unit

  // apply func to all key/value pairs which key is start with prefix
  def find(prefix: Array[Byte], func: (Key, Value) => Unit): Unit

  // close this KV Store
  def close(): Unit

  // return all uncommitted session revisions
  def uncommitted(): Seq[Long]

  def onRollback(action: () => Unit): Unit
}

// low level db iterator adapter
trait LowLevelDBIterator {
  def seek(prefix: Array[Byte]): Unit

  def next(): (Array[Byte], Array[Byte])

  def hasNext(): Boolean
}

// low level db batch adapter
trait LowLevelWriteBatch {
  def set(key: Array[Byte], value: Array[Byte]): Unit

  def delete(key: Array[Byte]): Unit

  def close(): Unit
}

// low level db adapter
trait LowLevelDB {
  def get(key: Array[Byte]): Array[Byte]

  def set(key: Array[Byte], value: Array[Byte]): Unit

  def delete(key: Array[Byte]): Unit

  def iterator(): LowLevelDBIterator

  def batchWrite(action: LowLevelWriteBatch => Unit)
}

trait BatchItem

case class DeleteOperationItem(key: Array[Byte]) extends BatchItem

case class PutOperationItem(key: Array[Byte], value: Array[Byte]) extends BatchItem

class Batch(val ops: ListBuffer[BatchItem] = ListBuffer.empty[BatchItem]) {
  def put(key: Array[Byte], value: Array[Byte]): Batch = {
    ops.append(PutOperationItem(key, value))
    this
  }

  def delete(key: Array[Byte]): Batch = {
    ops.append(DeleteOperationItem(key))
    this
  }
}

// wrapper class for Array[Byte], can be used as Map key
case class ByteArray(bytes: Array[Byte]) extends com.apex.common.Serializable {
  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: ByteArray => bytes sameElements that.bytes
      case _ => false
    }
  }

  override def hashCode(): Int = {
    util.Arrays.hashCode(bytes)
  }

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeByteArray(bytes)
  }
}