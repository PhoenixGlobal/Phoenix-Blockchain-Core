/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: RocksDBStorage.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-10-16 下午5:41@version: 1.0
 *
 */

package com.apex.storage

import java.util
import java.util.Map
import java.util.Map.Entry

import cats.instances.byte
import com.apex.common.ApexLogging
import com.apex.core.{DataType, StoreType}
import org.rocksdb._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Try

class RocksDBStorage(val db: RocksDB) extends LowLevelStorage[Array[Byte], Array[Byte]] with ApexLogging {

  private lazy val sessionMgr = new RocksDBSessionManagerWrapper(db)

  private val actions = ListBuffer.empty[() => Unit]

  override def get(key: Array[Byte]): Option[Array[Byte]] = {
    val opt = new ReadOptions().setFillCache(true)
    val value = db.get(opt, key)
    if (value == null) {
      None
    } else {
      Some(value)
    }
  }

  override def set(key: Array[Byte], value: Array[Byte], batch: Batch = null): Boolean = {
    if(batch == null) {
      db.put(key, value)
      true
    }
    else applyBatch(batch)
  }

  private def applyBatch(batch: Batch): Boolean = {
    val writeOpt = new WriteOptions()
    val update = new WriteBatch
    try {
      batch.ops.foreach(_ match {
        case PutOperationItem(k, v) => update.put(k, v)
        case DeleteOperationItem(k) => update.remove(k)
      })
      db.write(writeOpt, update)
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

  override def delete(key: Array[Byte], batch: Batch = null): Boolean = {
    val writeOpt = new WriteOptions()
    val writeBatch =  new WriteBatch()
    if(batch == null){
      writeBatch.remove(key)
      db.write(writeOpt, writeBatch)
      true
    }
    else {
      try{
        batch.ops.foreach(op => if(op.isInstanceOf[DeleteOperationItem]){
          writeBatch.remove(op.asInstanceOf[DeleteOperationItem].key)
        })
        db.write(writeOpt, writeBatch)
        true
      }
      catch {
        case e:Throwable => log.error("apply batch failed", e)
          false
      }
      finally {
        writeBatch.close()
      }
    }

  }

  override def batchWrite(action: Batch => Unit): Boolean = {
    val batch = new Batch
    action(batch)
    applyBatch(batch)
  }

  override def last(): Option[Map.Entry[Array[Byte], Array[Byte]]] = {
    val m = new util.HashMap[Array[Byte], Array[Byte]]()
    val iterator = db.newIterator()
    iterator.seekToLast()

    if(iterator.isValid){
      m.put(iterator.key(), iterator.value())
      val r: util.Iterator[Map.Entry[Array[Byte], Array[Byte]]] = m.entrySet().iterator()
      if(r.hasNext){
        val entry: Map.Entry[Array[Byte], Array[Byte]] = r.next()
        Some(entry)
      }
      else None
    }
    else None
  }

  override def scan(func: (Array[Byte], Array[Byte]) => Unit): Unit = {
    seekThenApply(
      it => it.seekToFirst(),
      entry => {
        func(entry.getKey, entry.getValue)
        true
      })
  }

  private def seekThenApply(seekAction: RocksIterator  => Unit, func: Entry[Array[Byte], Array[Byte]] => Boolean): Unit = {
    val iterator = db.newIterator()
    try {
      seekAction(iterator)
      while (iterator.isValid && func(peekNext(iterator).get)) {
        iterator.next()
      }
    } catch {
      case e: Throwable => log.error("seek", e)
    } finally {
      iterator.close()
    }
  }

  private def peekNext(iterator: RocksIterator): Option[Map.Entry[Array[Byte], Array[Byte]]] = {
    val m = new util.HashMap[Array[Byte], Array[Byte]]()
    if(iterator.isValid){
      m.put(iterator.key(), iterator.value())
      val r: util.Iterator[Map.Entry[Array[Byte], Array[Byte]]] = m.entrySet().iterator()
      if(r.hasNext){
        val entry: Map.Entry[Array[Byte], Array[Byte]] = r.next()
        Some(entry)
      }
      else None
    }
    else None
  }

  override def scan(prefix: Array[Byte]):ArrayBuffer[Array[Byte]] = {
    val records = ArrayBuffer.empty[Array[Byte]]
    val iterator = db.newIterator()
    iterator.seek(prefix)
    while (iterator.isValid) {
      val entry = peekNext(iterator)
      if (entry.isDefined && entry.get.getKey.length >= prefix.length
        && prefix.sameElements(entry.get.getKey.take(prefix.length))){
        records.append(entry.get.getValue)
      }
      iterator.next()
    }
    records
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

  override def commit(revision: Long): Unit = {
    sessionMgr.commit(revision)
  }

  override def commit(): Unit = {
    sessionMgr.commit()
  }

  override def rollBack(): Unit = {
    actions.foreach(action => Try(action()))
    sessionMgr.rollBack()
  }

  override def close(): Unit = {
    db.close()
  }

  override def revision(): Long = {
    sessionMgr.revision()
  }

  override def uncommitted(): Seq[Long] = {
    sessionMgr.revisions()
  }

  override def onRollback(action: () => Unit): Unit = {
    actions.append(action)
  }
}

class RocksDBWriteBatch(val batch: WriteBatch) extends LowLevelWriteBatch {
  override def set(key: Array[Byte], value: Array[Byte]): Unit = {
    batch.put(key, value)
  }

  override def delete(key: Array[Byte]): Unit = {
    batch.remove(key)
  }

  override def close(): Unit = {
    batch.close()
  }
}

class RocksDBIterator(it: RocksIterator) extends LowLevelDBIterator {
  override def seek(prefix: Array[Byte]): Unit = {
    it.seek(prefix)
  }

  override def next(): (Array[Byte], Array[Byte]) = {
    val cur = (it.key(), it.value())
    it.next()
    cur
  }

  override def hasNext(): Boolean = {
    it.isValid
  }
}

class RocksDatabase(db: RocksDB) extends LowLevelDB {
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
    new RocksDBIterator(db.newIterator())
  }

  override def batchWrite(action: LowLevelWriteBatch => Unit): Unit = {
    val update = new RocksDBWriteBatch(new WriteBatch)
    try {
      action(update)
      db.write(new WriteOptions(), update.batch)
    } finally {
      update.close()
    }
  }
}

// session with capable of undoing all operations happened in this session
class RocksDBRollbackSession(db: RocksDB, val prefix: Array[Byte], val revision: Long) extends Session {
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

    val batch = new WriteBatch()
    try {
      batch.put(sessionId, item.toBytes)
      action(batch)
      db.write(new WriteOptions(), batch)
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

    val batch = new WriteBatch()
    try {
      item.insert.foreach(p => batch.remove(p._1.bytes))
      item.update.foreach(p => batch.put(p._1.bytes, p._2))
      item.delete.foreach(p => batch.put(p._1.bytes, p._2))
      batch.put(prefix, BigInt(revision).toByteArray)
      batch.remove(sessionId)
      db.write(new WriteOptions(), batch)
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

class RocksDBSessionManagerWrapper(db: RocksDB) {
  private val prefix = Array(StoreType.Data.id.toByte, DataType.Session.id.toByte)

  private val sessions = ListBuffer.empty[RocksDBRollbackSession]

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
    val session = new RocksDBRollbackSession(db, prefix, _revision)
    session.init(batch => batch.put(prefix, BigInt(_revision + 1).toByteArray))
    sessions.append(session)
    _revision += 1
    session
  }

  // load all sessions from low level db
  private def init(): Unit = {
    def reloadRevision(iterator: RocksIterator) = {
      if (iterator.isValid) {
        val kv = peekNext(iterator)
        if (kv.get.getKey.startsWith(prefix)) {
          require(kv.get.getKey.sameElements(prefix))
          _revision = BigInt(kv.get.getValue).toLong
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
    def reloadSessions(iterator: RocksIterator) = {

      var eof = false
      val temp = ListBuffer.empty[RocksDBRollbackSession]
      while (!eof && iterator.isValid) {
        val kv = peekNext(iterator)
        val key = kv.get.getKey
        if (key.startsWith(prefix)) {
          val value = kv.get.getValue
          val revision = BigInt(key.drop(prefix.length)).toLong
          val session = new RocksDBRollbackSession(db, prefix, revision)
          session.init(value)
          temp.append(session)
          iterator.next
        } else {
          eof = true
        }
      }

      temp
    }

    val iterator = db.newIterator()

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

  private def peekNext(iterator: RocksIterator): Option[Map.Entry[Array[Byte], Array[Byte]]] = {
    val m = new util.HashMap[Array[Byte], Array[Byte]]()
    if(iterator.isValid){
      m.put(iterator.key(), iterator.value())
      val r: util.Iterator[Map.Entry[Array[Byte], Array[Byte]]] = m.entrySet().iterator()
      if(r.hasNext){
        val entry: Map.Entry[Array[Byte], Array[Byte]] = r.next()
        Some(entry)
      }
      else None
    }
    else None
  }
}

object RocksDBStorage {
  // a static method that loads the RocksDB C++ library.
  RocksDB.loadLibrary()

  def open(path: String, createIfMissing: Boolean = true): RocksDBStorage = {
    val options = new Options
    options.setCreateIfMissing(createIfMissing)
    val db = RocksDB.open(options, path)
    new RocksDBStorage(db)
  }
}
