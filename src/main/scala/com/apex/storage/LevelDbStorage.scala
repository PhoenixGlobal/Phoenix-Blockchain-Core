package com.apex.storage

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream, File}
import java.nio.ByteBuffer
import java.util
import java.util.Map.Entry

import com.apex.common.{ApexLogging, Serializable}
import com.apex.core.{DataType, StoreType}
import com.apex.exceptions.InvalidOperationException
import org.fusesource.leveldbjni.JniDBFactory._
import org.iq80.leveldb._

import scala.collection.mutable.{ListBuffer, Map}

class LevelDbStorage(private val db: DB) extends Storage[Array[Byte], Array[Byte]] with ApexLogging {
  private lazy val sessionMgr = new SessionManager(db)

  override def get(key: Array[Byte]): Option[Array[Byte]] = {
    val opt = new ReadOptions().fillCache(false)
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
    }
    true
  }

  override def delete(key: Array[Byte], batch: Batch = null): Unit = {
    val newBatch = sessionMgr.beginDelete(key, batch)
    if (newBatch != batch) {
      applyBatch(newBatch)
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

  override def commit(revision: Int = -1): Unit = {
    sessionMgr.commit(revision)
  }

  override def rollBack(): Unit = {
    sessionMgr.rollBack()
  }

  override def close(): Unit = {
    db.close()
  }

  def batchWrite(action: Batch => Unit): Boolean = {
    val batch = new Batch
    action(batch)
    applyBatch(batch)
  }

  def last(): Option[Entry[Array[Byte], Array[Byte]]] = {
    val it = db.iterator()
    it.seekToLast()
    if (it.hasNext) {
      Some(it.peekNext())
    } else {
      None
    }
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
    }catch {
      case e: Throwable => {
        log.error("apply batch failed", e)
        false
      }
    } finally {
      update.close()
    }
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

class Session(db: DB, val prefix: Array[Byte], val revision: Int) {

  private val sessionId = prefix ++ BigInt(revision).toByteArray

  private val item = new SessionItem

  private var closed = false

  init()

  def onSet(key: Array[Byte], v: Array[Byte], batch: Batch): Batch = {
    if (closed) throw new InvalidOperationException("closed session")

    val newBatch = if (batch == null) new Batch else batch
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
      batch.put(sessionId, item.toBytes)
    }
    newBatch
  }

  def onDelete(key: Array[Byte], batch: Batch): Batch = {
    if (closed) throw new InvalidOperationException("closed session")

    val newBatch = if (batch == null) new Batch else batch
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
      batch.put(sessionId, item.toBytes)
    }
    newBatch
  }

  def close(): Unit = {
    db.delete(sessionId)
    closed = true
  }

  def rollBack(onRollBack: WriteBatch => Unit): Unit = {
    val batch = db.createWriteBatch()
    try {
      item.insert.foreach(p => batch.delete(p._1.bytes))
      item.update.foreach(p => batch.put(p._1.bytes, p._2))
      item.delete.foreach(p => batch.put(p._1.bytes, p._2))
      batch.delete(sessionId)
      onRollBack(batch)
    } finally {
      batch.close()
    }
  }

  private def init(): Unit = {
    val session = db.get(sessionId)
    if (session != null) {
      db.put(sessionId, item.toBytes)
    } else {
      item.fill(session)
    }
  }
}

class SessionManager(db: DB) {
  private val prefix = Array(StoreType.Data.id.toByte, DataType.Session.id.toByte)

  private val sessions = ListBuffer.empty[Session]

  private var _revision = 1

  init()

  def revision(): Int = _revision

  def revisions(): Seq[Int] = sessions.map(_.revision)

  def beginSet(key: Array[Byte], value: Array[Byte], batch: Batch = null): Batch = {
    sessions.lastOption.map(_.onSet(key, value, batch)).orNull
  }

  def beginDelete(key: Array[Byte], batch: Batch = null): Batch = {
    sessions.lastOption.map(_.onDelete(key, batch)).orNull
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
    db.put(prefix, BigInt(_revision + 1).toByteArray)
    val session = new Session(db, prefix, _revision)
    sessions.append(session)
    _revision += 1
    session
  }

  private def init(): Unit = {
    val iterator = db.iterator
    try {
      iterator.seek(prefix)
      while (iterator.hasNext) {
        val kv = iterator.peekNext()
        val k = kv.getKey
        val v = kv.getValue
        if (k.length > prefix.length) {
          val level = BigInt(k.drop(prefix.length)).toInt
          val session = new Session(db, prefix, level)
          sessions.append(session)
        } else {
          _revision = BigInt(v).toInt
        }
        iterator.next
      }
    } finally {
      iterator.close()
    }

    require(sessions.lastOption.forall(_.revision < _revision))
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