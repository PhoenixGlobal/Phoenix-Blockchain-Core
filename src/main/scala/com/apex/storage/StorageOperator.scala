package com.apex.storage

import com.apex.core.{DataType, StoreType}
import org.iq80.leveldb.{DB, DBIterator, WriteBatch}

import scala.collection.mutable.ListBuffer

class StorageOperator(val db: LowLevelDB)/* extends LowLevelStorage[Array[Byte], Array[Byte]] */{

  private lazy val sessionMgr = new SessionManagerTemp(db)

  private val actions = ListBuffer.empty[() => Unit]

  def get(key: Array[Byte]): Unit ={
    db.get(key)
  }

  def set(key: Array[Byte], value: Array[Byte]): Unit ={
    db.set(key, value)
  }

  def delete(key: Array[Byte]): Unit = {
    db.delete(key)
  }

  def iterator(): LowLevelDBIterator = {
    db.iterator()
  }

  def batchWrite(action: LowLevelWriteBatch => Unit): Unit ={
    db.batchWrite(action)
  }

}

class SessionManagerTemp(db: LowLevelDB) {
  private val prefix = Array(StoreType.Data.id.toByte, DataType.Session.id.toByte)

  private val sessions = ListBuffer.empty[RollbackSessionTemp]

  private val defaultSession = new Session

  private var _revision: Long = 1

  init()

  def revision(): Long = _revision

  def revisions(): Seq[Long] = sessions.map(_.revision)

  def beginSet(key: Array[Byte], value: Array[Byte], batch: Batch): Batch = {
    sessions.lastOption.getOrElse(defaultSession).onSet(key, value, batch)
  }

  def beginDelete(key: Array[Byte], batch: Batch): Batch = {
    sessions.lastOption.getOrElse(defaultSession).onDelete(key, batch)
  }

  // commit all operations in sessions whose revision is equal to or larger than the specified revision
  def commit(revision: Long): Unit = {
    val toCommit = sessions.takeWhile(_.revision <= revision)
    sessions.remove(0, toCommit.length)
    toCommit.foreach(_.close)
  }

  // commit all operations in the latest session
  def commit(): Unit = {
    sessions.headOption.foreach(s => {
      sessions.remove(0)
      s.close()
    })
  }

  // undo all operations in the latest session
  def rollBack(): Unit = {
    sessions.lastOption.foreach(s => {
      sessions.remove(sessions.length - 1)
      _revision -= 1
      s.rollBack()
    })
  }

  // start a new session
  def newSession(): Session = {
    val session = new RollbackSessionTemp(db, prefix, _revision)
    session.init(batch => batch.put(prefix, BigInt(_revision + 1).toByteArray))
    sessions.append(session)
    _revision += 1
    session
  }

  // load all sessions from low level db
  private def init(): Unit = {
    def reloadRevision(iterator: LowLevelDBIterator) = {
      if (iterator.hasNext) {
        val kv = iterator.peekNext().get
        if (kv.getKey.startsWith(prefix)) {
          require(kv.getKey.sameElements(prefix))
          _revision = BigInt(kv.getValue).toLong
          iterator.next
          true
        } else {
          false
        }
      } else {
        false
      }
    }

    // load all sessions
    def reloadSessions(iterator: LowLevelDBIterator) = {

      var eof = false
      val temp = ListBuffer.empty[RollbackSessionTemp]
      while (!eof && iterator.hasNext) {
        val kv = iterator.peekNext().get
        val key = kv.getKey
        if (key.startsWith(prefix)) {
          val value = kv.getValue
          val revision = BigInt(key.drop(prefix.length)).toLong
          val session = new RollbackSessionTemp(db, prefix, revision)
          session.init(value)
          temp.append(session)
          iterator.next
        } else {
          eof = true
        }
      }

      temp
    }

    val iterator = db.iterator

    try {
      iterator.seek(prefix)

      if (reloadRevision(iterator)) {
        val temp = reloadSessions(iterator)
        sessions.appendAll(temp.sortBy(_.revision))
      }
    } finally {
      iterator.close()
    }

    require(sessions.lastOption.forall(_.revision == _revision - 1))
  }
}

// session with capable of undoing all operations happened in this session
class RollbackSessionTemp(db: LowLevelDB, val prefix: Array[Byte], val revision: Long) extends Session {
  private val sessionId = prefix ++ BigInt(revision).toByteArray

  private val item = new SessionItem

  private var closed = false

  // load session data
  def init(data: Array[Byte]): Unit = {
    require(!closed)

    item.fill(data)
  }

  def init(action: LowLevelWriteBatch => Unit): Unit = {
    require(!closed)

    val batch = db.createWriteBatch()
    try {
      batch.put(sessionId, item.toBytes)
      action(batch)
      db.write(batch)
    } finally {
      batch.close()
    }
  }

  // close this session
  def close(): Unit = {
    require(!closed)

    db.delete(sessionId)
    closed = true
  }

  // undo all operations in this session
  def rollBack(): Unit = {
    require(!closed)

    val batch = db.createWriteBatch()
    try {
      item.insert.foreach(p => batch.delete(p._1.bytes))
      item.update.foreach(p => batch.put(p._1.bytes, p._2))
      item.delete.foreach(p => batch.put(p._1.bytes, p._2))
      batch.put(prefix, BigInt(revision).toByteArray)
      batch.delete(sessionId)
      db.write(batch)
    } finally {
      batch.close()
    }
  }

  override def onSet(key: Array[Byte], v: Array[Byte], batch: Batch): Batch = {
    require(!closed)

    val newBatch = originOrNew(batch)
    newBatch.put(key, v)

    var modified = true
    val k = ByteArray(key)
    if (item.insert.contains(k) || item.update.contains(k)) {
      modified = false
    } else if (item.delete.contains(k)) {
      item.update.put(k, item.delete(k))
      item.delete.remove(k)
    } else {
      val old = db.get(key)
      if (old != null) {
        item.update.put(k, old)
      } else {
        item.insert.put(k, v)
      }
    }

    if (modified) {
      newBatch.put(sessionId, item.toBytes)
    }
    newBatch
  }

  override def onDelete(key: Array[Byte], batch: Batch): Batch = {
    require(!closed)

    val newBatch = originOrNew(batch)
    newBatch.delete(key)

    var modified = false
    val k = ByteArray(key)
    if (item.insert.contains(k)) {
      item.insert.remove(k)
      modified = true
    } else if (item.update.contains(k)) {
      item.delete.put(k, item.update(k))
      item.update.remove(k)
      modified = true
    } else if (!item.delete.contains(k)) {
      val old = db.get(key)
      if (old != null) {
        item.delete.put(k, old)
        modified = true
      }
    }

    if (modified) {
      newBatch.put(sessionId, item.toBytes)
    }
    newBatch
  }
}
