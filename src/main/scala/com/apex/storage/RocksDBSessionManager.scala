//package com.apex.storage
//
//import java.util
//import java.util.Map
//
//import com.apex.core.{DataType, StoreType}
//import org.rocksdb.{RocksDB, RocksIterator}
//
//import scala.collection.mutable.ListBuffer
//
//class RocksDBSessionManager(db: RocksDB) extends SessionManagerWrapper{
//  init()
//
//  override private val sessions = ListBuffer.empty[RocksDBRollbackSession]
//
//  // start a new session
//  def newSession(): Session = {
//    val session = new RocksDBRollbackSession(db, prefix, _revision)
//    session.init(batch => batch.put(prefix, BigInt(_revision + 1).toByteArray))
//    sessions.append(session)
//    _revision += 1
//    session
//  }
//
//  // load all sessions from low level db
//  private def init(): Unit = {
//    def reloadRevision(iterator: RocksIterator) = {
//      if (iterator.isValid) {
//        val kv = peekNext(iterator)
//        if (kv.get.getKey.startsWith(prefix)) {
//          require(kv.get.getKey.sameElements(prefix))
//          _revision = BigInt(kv.get.getValue).toLong
//          iterator.next
//          true
//        } else {
//          false
//        }
//      } else {
//        false
//      }
//    }
//
//    // load all sessions
//    def reloadSessions(iterator: RocksIterator) = {
//
//      var eof = false
//      val temp = ListBuffer.empty[RocksDBRollbackSession]
//      while (!eof && iterator.isValid) {
//        val kv = peekNext(iterator)
//        val key = kv.get.getKey
//        if (key.startsWith(prefix)) {
//          val value = kv.get.getValue
//          val revision = BigInt(key.drop(prefix.length)).toLong
//          val session = new RocksDBRollbackSession(db, prefix, revision)
//          session.init(value)
//          temp.append(session)
//          iterator.next
//        } else {
//          eof = true
//        }
//      }
//
//      temp
//    }
//
//    val iterator = db.newIterator()
//
//    try {
//      iterator.seek(prefix)
//
//      if (reloadRevision(iterator)) {
//        val temp = reloadSessions(iterator)
//        sessions.appendAll(temp.sortBy(_.revision))
//      }
//    } finally {
//      iterator.close()
//    }
//
//    require(sessions.lastOption.forall(_.revision == _revision - 1))
//  }
//
//  private def peekNext(iterator: RocksIterator): Option[Map.Entry[Array[Byte], Array[Byte]]] = {
//    val m = new util.HashMap[Array[Byte], Array[Byte]]()
//    if(iterator.isValid){
//      m.put(iterator.key(), iterator.value())
//      val r: util.Iterator[Map.Entry[Array[Byte], Array[Byte]]] = m.entrySet().iterator()
//      if(r.hasNext){
//        val entry: Map.Entry[Array[Byte], Array[Byte]] = r.next()
//        Some(entry)
//      }
//      else None
//    }
//    else None
//  }
//}
