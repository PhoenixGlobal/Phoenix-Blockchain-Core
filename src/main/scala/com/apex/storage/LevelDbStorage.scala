package com.apex.storage

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream, File}
import java.util
import java.util.Map.Entry

import com.apex.common.{ApexLogging, Serializable}
import com.apex.core.{DataType, StoreType}
import org.fusesource.leveldbjni.JniDBFactory._
import org.iq80.leveldb._

import scala.collection.mutable.{ListBuffer, Map}
import scala.util.Try

// KV Store implementation, use LevelDB as low level store
class LevelDbStorage(private val db: DB) extends LowLevelStorage[Array[Byte], Array[Byte]] with ApexLogging {
  private lazy val sessionMgr = new SessionManager(db)

  private val actions = ListBuffer.empty[() => Unit]

  // register callback function executed on rollback
  def onRollback(action: () => Unit): Unit = {
    actions.append(action)
  }

  // get value the key associated with
  override def get(key: Array[Byte]): Option[Array[Byte]] = {
    val opt = new ReadOptions().fillCache(true)
    val value = db.get(key, opt)
    if (value == null) {
      None
    } else {
      Some(value)
    }
  }

  // add a new key/value pair if key not exist or overwrite value the key associated with
  override def set(key: Array[Byte], value: Array[Byte], batch: Batch = null): Boolean = {
    val newBatch = sessionMgr.beginSet(key, value, batch)
    if (newBatch != batch) {
      applyBatch(newBatch)
    } else {
      true
    }
  }

  // delete the key and associated value
  override def delete(key: Array[Byte], batch: Batch = null): Boolean = {
    val newBatch = sessionMgr.beginDelete(key, batch)
    if (newBatch != batch) {
      applyBatch(newBatch)
    } else {
      true
    }
  }

  override def batchWrite(action: Batch => Unit): Boolean = {
    val batch = new Batch
    action(batch)
    applyBatch(batch)
  }

  // return last element
  override def last(): Option[Entry[Array[Byte], Array[Byte]]] = {
    val it = db.iterator()
    try {
      it.seekToLast()
      if (it.hasNext) {
        Some(it.peekNext())
      } else {
        None
      }
    } finally {
      it.close()
    }
  }

  // apply func to all key/value pairs
  override def scan(func: (Array[Byte], Array[Byte]) => Unit): Unit = {
    seekThenApply(
      it => it.seekToFirst(),
      entry => {
        func(entry.getKey, entry.getValue)
        true
      })
  }

  // apply func to all key/value pairs which key is start with prefix
  override def find(prefix: Array[Byte], func: (Array[Byte], Array[Byte]) => Unit): Unit = {
    seekThenApply(
      it => it.seek(prefix),
      entry => {
        if (entry.getKey.length < prefix.length
          || !prefix.sameElements(entry.getKey.take(prefix.length))) {
          false
        } else {
          func(entry.getKey, entry.getValue)
          true
        }
      })
  }

  // start a new session
  override def newSession(): Unit = {
    sessionMgr.newSession()
  }

  // commit all operations in sessions whose revision is equal to or larger than the specified revision
  override def commit(revision: Long): Unit = {
    sessionMgr.commit(revision)
  }

  // commit all operations in the latest session
  override def commit(): Unit = {
    sessionMgr.commit()
  }

  // undo all operations in the latest session
  override def rollBack(): Unit = {
    actions.foreach(action => Try(action()))
    sessionMgr.rollBack()
  }

  // close this KV Store
  override def close(): Unit = {
    db.close()
  }

  // return latest revision
  override def revision(): Long = {
    sessionMgr.revision()
  }

  // return all uncommitted session revisions
  override def uncommitted(): Seq[Long] = {
    sessionMgr.revisions()
  }

  private def seekThenApply(seekAction: DBIterator => Unit, func: Entry[Array[Byte], Array[Byte]] => Boolean): Unit = {
    val iterator = db.iterator
    try {
      seekAction(iterator)
      while (iterator.hasNext && func(iterator.peekNext())) {
        iterator.next
      }
    } catch {
      case e: Throwable => log.error("seek", e)
    } finally {
      iterator.close()
    }
  }

  private def applyBatch(batch: Batch): Boolean = {
    val update = db.createWriteBatch()
    try {
      batch.ops.foreach(_ match {
        case PutOperationItem(k, v) => update.put(k, v)
        case DeleteOperationItem(k) => update.delete(k)
      })
      db.write(update)
      true
    } catch {
      case e: Throwable => {
        log.error("apply batch failed", e)
        false
      }
    } finally {
      update.close()
    }
  }

}

// adapter class for low level db batch
class LevelDBWriteBatch(val batch: WriteBatch) extends LowLevelWriteBatch {
  override def set(key: Array[Byte], value: Array[Byte]): Unit = {
    batch.put(key, value)
  }

  override def delete(key: Array[Byte]): Unit = {
    batch.delete(key)
  }

  override def close(): Unit = {
    batch.close()
  }
}

// LevelDBStorage iterator
class LevelDBIterator(it: DBIterator) extends LowLevelDBIterator {

  // move to the position so that next element's key is equal to or larger than the prefix
  override def seek(prefix: Array[Byte]): Unit = {
    it.seek(prefix)
  }

  // return next element
  override def next(): (Array[Byte], Array[Byte]) = {
    val entry = it.next()
    (entry.getKey, entry.getValue)
  }

  // whether has next element
  override def hasNext(): Boolean = {
    it.hasNext
  }
}

// adapter class for low level db
class LevelDB(db: DB) extends LowLevelDB {
  override def get(key: Array[Byte]): Array[Byte] = {
    db.get(key)
  }

  override def set(key: Array[Byte], value: Array[Byte]): Unit = {
    db.put(key, value)
  }

  override def delete(key: Array[Byte]): Unit = {
    db.delete(key)
  }

  override def iterator(): LowLevelDBIterator = {
    new LevelDBIterator(db.iterator())
  }

  override def batchWrite(action: LowLevelWriteBatch => Unit): Unit = {
    val update = new LevelDBWriteBatch(db.createWriteBatch())
    try {
      action(update)
      db.write(update.batch)
    } finally {
      update.close()
    }
  }
}

class SessionItem(val insert: Map[ByteArray, Array[Byte]] = Map.empty[ByteArray, Array[Byte]],
                  val update: Map[ByteArray, Array[Byte]] = Map.empty[ByteArray, Array[Byte]],
                  val delete: Map[ByteArray, Array[Byte]] = Map.empty[ByteArray, Array[Byte]])
  extends Serializable {
  override def serialize(os: DataOutputStream): Unit = {
    writeBytes(os, insert)
    writeBytes(os, update)
    writeBytes(os, delete)
  }

  def fill(bytes: Array[Byte]): Unit = {
    import com.apex.common.Serializable._
    val bs = new ByteArrayInputStream(bytes)
    val is = new DataInputStream(bs)
    is.readMap.foreach(fillInsert)
    is.readMap.foreach(fillUpdate)
    is.readMap.foreach(fillDelete)
  }

  def clear(): Unit = {
    insert.clear()
    update.clear()
    delete.clear()
  }

  private def fillInsert(kv: (Array[Byte], Array[Byte])): Unit = {
    insert.put(ByteArray(kv._1), kv._2)
  }

  private def fillUpdate(kv: (Array[Byte], Array[Byte])): Unit = {
    update.put(ByteArray(kv._1), kv._2)
  }

  private def fillDelete(kv: (Array[Byte], Array[Byte])): Unit = {
    delete.put(ByteArray(kv._1), kv._2)
  }

  private def writeBytes(os: DataOutputStream, dict: Map[ByteArray, Array[Byte]]): Unit = {
    import com.apex.common.Serializable._
    os.writeVarInt(dict.size)
    for (elem <- dict) {
      os.writeByteArray(elem._1.bytes)
      os.writeByteArray(elem._2)
    }
  }
}

// base KV Store session
class Session {
  def onSet(key: Array[Byte], value: Array[Byte], batch: Batch): Batch = {
    val newBatch = originOrNew(batch)
    newBatch.put(key, value)
    newBatch
  }

  def onDelete(key: Array[Byte], batch: Batch): Batch = {
    val newBatch = originOrNew(batch)
    newBatch.delete(key)
    newBatch
  }

  protected def originOrNew(batch: Batch): Batch = {
    if (batch == null) new Batch else batch
  }
}

// session with capable of undoing all operations happened in this session
class RollbackSession(db: DB, val prefix: Array[Byte], val revision: Long) extends Session {
  private val sessionId = prefix ++ BigInt(revision).toByteArray

  private val item = new SessionItem

  private var closed = false

  // load session data
  def init(data: Array[Byte]): Unit = {
    require(!closed)

    item.fill(data)
  }

  def init(action: WriteBatch => Unit): Unit = {
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

class SessionManager(db: DB) {
  private val prefix = Array(StoreType.Data.id.toByte, DataType.Session.id.toByte)

  private val sessions = ListBuffer.empty[RollbackSession]

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
    val session = new RollbackSession(db, prefix, _revision)
    session.init(batch => batch.put(prefix, BigInt(_revision + 1).toByteArray))
    sessions.append(session)
    _revision += 1
    session
  }

  // load all sessions from low level db
  private def init(): Unit = {
    def reloadRevision(iterator: DBIterator) = {
      if (iterator.hasNext) {
        val kv = iterator.peekNext()
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
    def reloadSessions(iterator: DBIterator) = {

      var eof = false
      val temp = ListBuffer.empty[RollbackSession]
      while (!eof && iterator.hasNext) {
        val kv = iterator.peekNext()
        val key = kv.getKey
        if (key.startsWith(prefix)) {
          val value = kv.getValue
          val revision = BigInt(key.drop(prefix.length)).toLong
          val session = new RollbackSession(db, prefix, revision)
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

object LevelDbStorage {
  def open(path: String, createIfMissing: Boolean = true): LevelDbStorage = {
    val options = new Options
    options.createIfMissing(createIfMissing)
    val db = factory.open(new File(path), options)
    new LevelDbStorage(db)
  }
}