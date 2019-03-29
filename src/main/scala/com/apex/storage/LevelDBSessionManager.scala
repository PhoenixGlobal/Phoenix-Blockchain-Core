//package com.apex.storage
//
//import com.apex.core.{DataType, StoreType}
//import org.iq80.leveldb.{DB, DBIterator}
//
//import scala.collection.mutable.ListBuffer
//
//class LevelDBSessionManager(db: DB)  extends SessionManagerWrapper {
//
//  init()
//
//  private val sessions = ListBuffer.empty[LevelDBRollbackSession]
//
//  private val prefix = Array(StoreType.Data.id.toByte, DataType.Session.id.toByte)
//
//
//  // start a new session
//  def newSession(): Session = {
//    val session = new LevelDBRollbackSession(db, prefix, _revision)
//    session.init(batch => batch.put(prefix, BigInt(_revision + 1).toByteArray))
//    sessions.append(session)
//    _revision += 1
//    session
//  }
//
//  // load all sessions from low level db
//  private def init(): Unit = {
//    def reloadRevision(iterator: DBIterator) = {
//      if (iterator.hasNext) {
//        val kv = iterator.peekNext()
//        if (kv.getKey.startsWith(prefix)) {
//          require(kv.getKey.sameElements(prefix))
//          _revision = BigInt(kv.getValue).toLong
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
//    def reloadSessions(iterator: DBIterator) = {
//
//      var eof = false
//      val temp = ListBuffer.empty[LevelDBRollbackSession]
//      while (!eof && iterator.hasNext) {
//        val kv = iterator.peekNext()
//        val key = kv.getKey
//        if (key.startsWith(prefix)) {
//          val value = kv.getValue
//          val revision = BigInt(key.drop(prefix.length)).toLong
//          val session = new LevelDBRollbackSession(db, prefix, revision)
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
//    val iterator = db.iterator
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
//}
