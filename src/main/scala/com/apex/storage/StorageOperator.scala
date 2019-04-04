package com.apex.storage

import java.io.File
import java.nio.file.{Files, Paths}
import java.util.Map.Entry

import com.apex.common.ApexLogging
import com.apex.core.{DataType, StoreType}
import com.apex.settings.DBType
import org.fusesource.leveldbjni.JniDBFactory.factory
import org.iq80.leveldb.Options
import org.rocksdb.{Options, RocksDB}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Try

class StorageOperator(val db: LowLevelDB) extends LowLevelStorage[Array[Byte], Array[Byte]] with ApexLogging{

  private lazy val sessionMgr = new SessionManagerTemp(db)

  private val actions = ListBuffer.empty[() => Unit]

  // register callback function executed on rollback
  def onRollback(action: () => Unit): Unit = {
    actions.append(action)
  }

  // get value the key associated with
  override def get(key: Array[Byte]): Option[Array[Byte]] = {
    val value = db.get(key)
    Option(value)
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

  override def last(): Option[Entry[Array[Byte], Array[Byte]]] = {
    val it = db.iterator()
    try {
      it.seekToLast()
      if (it.hasNext()) {
        it.peekNext()
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

  private def seekThenApply(seekAction: LowLevelDBIterator => Unit, func: Entry[Array[Byte], Array[Byte]] => Boolean): Unit = {
    val iterator = db.iterator
    try {
      seekAction(iterator)
      while (iterator.hasNext && func(iterator.peekNext().get)) {
        iterator.next
      }
    } catch {
      case e: Throwable => log.error("seek", e)
    } finally {
      iterator.close()
    }
  }

  def scan(prefix: Array[Byte]): ArrayBuffer[Array[Byte]] = {
    val records = ArrayBuffer.empty[Array[Byte]]
    val iterator = db.iterator
    iterator.seek(prefix)
    while (iterator.hasNext) {
      val entry = iterator.peekNext().get
      if (entry.getKey.length >= prefix.length
        && prefix.sameElements(entry.getKey.take(prefix.length))){
        records.append(entry.getValue)
      }
      iterator.next
    }
    records
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

//  def batchWrite(action: LowLevelWriteBatch => Unit): Unit ={
//    db.batchWrite(action)
//  }

  private def applyBatch(batch: Batch): Boolean = {
    val update = db.createWriteBatch()
    try {
      batch.ops.foreach(_ match {
        case PutOperationItem(k, v) => update.set(k, v)
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

//object StorageOperator {
//  type raw = Storage[Array[Byte], Array[Byte]]
//
//  type lowLevelRaw = LowLevelStorageTemp[Array[Byte], Array[Byte]]
//
//  def open(dbType: DBType.Value, path: String): LowLevelDB = {
//    dbType match {
//      case DBType.LevelDB => openLevelDb(path)
//      case DBType.RocksDB => openRocksDb(path)
//      case _ => throw new NotImplementedError
//    }
//  }
//
//  def openLevelDb(path: String, createIfMissing: Boolean = true): LowLevelDB = {
//    import org.iq80.leveldb.Options
//      val options = new Options
//      options.createIfMissing(createIfMissing)
//      val db = factory.open(new File(path), options)
//      new LevelDB(db)
//  }
//
//  RocksDB.loadLibrary()
//
//  def openRocksDb(path: String, createIfMissing: Boolean = true): LowLevelDB = {
//    import org.rocksdb.Options
//    val options = new Options
//    options.setCreateIfMissing(createIfMissing)
//    val db = RocksDB.open(options, path)
//    new RocksDatabase(db)
//  }
//}

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
    session.init(batch => batch.set(prefix, BigInt(_revision + 1).toByteArray))
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
      batch.set(sessionId, item.toBytes)
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
      item.update.foreach(p => batch.set(p._1.bytes, p._2))
      item.delete.foreach(p => batch.set(p._1.bytes, p._2))
      batch.set(prefix, BigInt(revision).toByteArray)
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

object StorageOperator{
    def open(dbType: DBType.Value, path: String): StorageOperator = {
      dbType match {
        case DBType.LevelDB => openLevelDB(path)
        case DBType.RocksDB => openRocksDB(path)
        case _ => throw new NotImplementedError
      }
    }

  def openRocksDB(path: String, createIfMissing: Boolean = true): StorageOperator = {
    import org.rocksdb.Options
    val options = new Options
    options.setCreateIfMissing(createIfMissing)
    val dir = Paths.get(path)
    if(dir.getNameCount > 1){
      println(dir.getParent.toString)
      if (!Files.isSymbolicLink(dir.getParent)) Files.createDirectories(dir.getParent)
    }
    val db = RocksDB.open(options, path)
    new StorageOperator(new RocksDatabase(db))
  }

  def openLevelDB(path: String, createIfMissing: Boolean = true): StorageOperator = {
    import org.iq80.leveldb.Options
    val options = new Options
    options.createIfMissing(createIfMissing)
    val db = factory.open(new File(path), options)
    new StorageOperator(new LevelDB(db))
  }
}
