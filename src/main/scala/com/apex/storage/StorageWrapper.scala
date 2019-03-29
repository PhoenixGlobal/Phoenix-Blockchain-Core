package com.apex.storage

import java.util.Map.Entry

import com.apex.common.ApexLogging
import com.apex.settings.DBType
import com.apex.storage.Storage.lowLevelRaw

import scala.collection.mutable.ArrayBuffer

class StorageWrapper (private val storage: lowLevelRaw) extends LowLevelStorage[Array[Byte], Array[Byte]] with ApexLogging{

  // register callback function executed on rollback
  def onRollback(action: () => Unit): Unit = {
    storage.onRollback(action)
  }

  // get value the key associated with
  override def get(key: Array[Byte]): Option[Array[Byte]] = {
    storage.get(key)
  }

  // add a new key/value pair if key not exist or overwrite value the key associated with
  override def set(key: Array[Byte], value: Array[Byte], batch: Batch = null): Boolean = {
    storage.set(key, value, batch)
  }

  // delete the key and associated value
  override def delete(key: Array[Byte], batch: Batch = null): Boolean = {
    storage.delete(key, batch)
  }

  override def batchWrite(action: Batch => Unit): Boolean = {
    storage.batchWrite(action)
  }

  // return last element
  override def last(): Option[Entry[Array[Byte], Array[Byte]]] = {
    storage.last()
  }

  // apply func to all key/value pairs
  override def scan(func: (Array[Byte], Array[Byte]) => Unit): Unit = {
    storage.scan(func)
  }

  def scan(prefix: Array[Byte]): ArrayBuffer[Array[Byte]] = {
    storage.scan(prefix)
  }

  // apply func to all key/value pairs which key is start with prefix
  override def find(prefix: Array[Byte], func: (Array[Byte], Array[Byte]) => Unit): Unit = {
    storage.find(prefix, func)
  }



  // start a new session
  override def newSession(): Unit = {
    storage.newSession()
  }

  // commit all operations in sessions whose revision is equal to or larger than the specified revision
  override def commit(revision: Long): Unit = {
    storage.commit(revision)
  }

  // commit all operations in the latest session
  override def commit(): Unit = {
    storage.commit()
  }

  // undo all operations in the latest session
  override def rollBack(): Unit = {
    storage.rollBack()
  }

  // close this KV Store
  override def close(): Unit = {
    storage.close()
  }

  // return latest revision
  override def revision(): Long = {
    storage.revision()
  }

  // return all uncommitted session revisions
  override def uncommitted(): Seq[Long] = {
    storage.uncommitted()
  }
}

object StorageWrapper {
  def open(dbType: DBType.Value, path: String): lowLevelRaw = {
    dbType match {
      case DBType.LevelDB => new StorageWrapper(LevelDbStorage.open(path))
      case DBType.RocksDB => new StorageWrapper(RocksDBStorage.open(path))
      case _ => throw new NotImplementedError
    }
  }
}