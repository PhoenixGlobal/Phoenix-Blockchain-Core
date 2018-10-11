package com.apex.storage

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream, File}
import java.util
import java.util.Map.Entry

import com.apex.common.{ApexLogging, Serializable}
import com.apex.core.{DataType, StoreType}
import org.fusesource.leveldbjni.JniDBFactory._
import org.iq80.leveldb._

import scala.collection.mutable.{ListBuffer, Map}

class LevelDbStorage(private val db: DB) extends Storage[Array[Byte], Array[Byte]] with ApexLogging {
  private lazy val sessionMgr = new SessionManager(db)

  override def get(key: Array[Byte]): Option[Array[Byte]] = {
    val opt = new ReadOptions().fillCache(true)
    val value = db.get(key, opt)
    if (value == null) {
      None
    } else {
      Some(value)
    }
  }

  override def set(key: Array[Byte], value: Array[Byte], batch: Batch = null): Boolean = {
    val newBatch = sessionMgr.beginSet(key, value, batch)
    if (newBatch != batch) {
      applyBatch(newBatch)
    } else {
      true
    }
  }

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

  override def scan(func: (Array[Byte], Array[Byte]) => Unit): Unit = {
    seekThenApply(
      it => it.seekToFirst(),
      entry => {
        func(entry.getKey, entry.getValue)
        true
      })
  }

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

  override def newSession(): Unit = {
    sessionMgr.newSession()
  }

  override def commit(revision: Int): Unit = {
    sessionMgr.commit(revision)
  }

  override def commit(): Unit = {
    sessionMgr.commit()
  }

  override def rollBack(): Unit = {
    sessionMgr.rollBack()
  }

  override def close(): Unit = {
    db.close()
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

  override def revision(): Int = {
    sessionMgr.revision()
  }

  override def uncommitted(): Seq[Int] = {
    sessionMgr.revisions()
  }
}

case class ByteArrayKey(bytes: Array[Byte]) extends com.apex.common.Serializable {
  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: ByteArrayKey => bytes sameElements that.bytes
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

class SessionItem(val insert: Map[ByteArrayKey, Array[Byte]] = Map.empty[ByteArrayKey, Array[Byte]],
                  val update: Map[ByteArrayKey, Array[Byte]] = Map.empty[ByteArrayKey, Array[Byte]],
                  val delete: Map[ByteArrayKey, Array[Byte]] = Map.empty[ByteArrayKey, Array[Byte]])
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
    insert.put(ByteArrayKey(kv._1), kv._2)
  }

  private def fillUpdate(kv: (Array[Byte], Array[Byte])): Unit = {
    update.put(ByteArrayKey(kv._1), kv._2)
  }

  private def fillDelete(kv: (Array[Byte], Array[Byte])): Unit = {
    delete.put(ByteArrayKey(kv._1), kv._2)
  }

  private def writeBytes(os: DataOutputStream, dict: Map[ByteArrayKey, Array[Byte]]): Unit = {
    import com.apex.common.Serializable._
    os.writeVarInt(dict.size)
    for (elem <- dict) {
      os.writeByteArray(elem._1.bytes)
      os.writeByteArray(elem._2)
    }
  }
}

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

class RollbackSession(db: DB, val prefix: Array[Byte], val revision: Int) extends Session {
  private val sessionId = prefix ++ BigInt(revision).toByteArray

  private val item = new SessionItem

  private var closed = false

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

  def close(): Unit = {
    require(!closed)

    db.delete(sessionId)
    closed = true
  }

  def rollBack(onRollBack: WriteBatch => Unit): Unit = {
    require(!closed)

    val batch = db.createWriteBatch()
    try {
      item.insert.foreach(p => batch.delete(p._1.bytes))
      item.update.foreach(p => batch.put(p._1.bytes, p._2))
      item.delete.foreach(p => batch.put(p._1.bytes, p._2))
      batch.delete(sessionId)
      onRollBack(batch)
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
    val k = ByteArrayKey(key)
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
    val k = ByteArrayKey(key)
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

  private var _revision = 1

  init()

  def revision(): Int = _revision

  def revisions(): Seq[Int] = sessions.map(_.revision)

  def beginSet(key: Array[Byte], value: Array[Byte], batch: Batch): Batch = {
    sessions.lastOption.getOrElse(defaultSession).onSet(key, value, batch)
  }

  def beginDelete(key: Array[Byte], batch: Batch): Batch = {
    sessions.lastOption.getOrElse(defaultSession).onDelete(key, batch)
  }

  def commit(revision: Int): Unit = {
    val toCommit = sessions.takeWhile(_.revision <= revision)
    sessions.remove(0, toCommit.length)
    toCommit.foreach(_.close)
  }

  def commit(): Unit = {
    sessions.headOption.foreach(s => {
      sessions.remove(0)
      s.close()
    })
  }

  def rollBack(): Unit = {
    sessions.lastOption.foreach(s => {
      s.rollBack(batch => batch.put(prefix, BigInt(_revision - 1).toByteArray))
      sessions.remove(sessions.length - 1)
      _revision -= 1
    })
  }

  def newSession(): Session = {
    val session = new RollbackSession(db, prefix, _revision)
    session.init(batch => batch.put(prefix, BigInt(_revision + 1).toByteArray))
    sessions.append(session)
    _revision += 1
    session
  }

  private def init(): Unit = {
    def reloadRevision(iterator: DBIterator) = {
      if (iterator.hasNext) {
        val kv = iterator.peekNext()
        if (kv.getKey.startsWith(prefix)) {
          require(kv.getKey.sameElements(prefix))
          _revision = BigInt(kv.getValue).toInt
          iterator.next
          true
        } else {
          false
        }
      } else {
        false
      }
    }

    def reloadSessions(iterator: DBIterator) = {

      var eof = false
      val temp = ListBuffer.empty[RollbackSession]
      while (!eof && iterator.hasNext) {
        val kv = iterator.peekNext()
        val key = kv.getKey
        if (key.startsWith(prefix)) {
          val value = kv.getValue
          val revision = BigInt(key.drop(prefix.length)).toInt
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