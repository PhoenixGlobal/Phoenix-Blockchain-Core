package com.apex.storage

import java.io.File
import java.util.Map.Entry

import com.apex.common.ApexLogging
import org.fusesource.leveldbjni.JniDBFactory._
import org.iq80.leveldb._

class LevelDbStorage(private val db: DB) extends Storage[Array[Byte], Array[Byte]] with ApexLogging {
  override def set(key: Array[Byte], value: Array[Byte]): Boolean = {
    try {
      db.put(key, value)
      true
    } catch {
      case e: Exception => {
        log.error("db set failed", e)
        false
      }
    }
  }

  override def get(key: Array[Byte]): Option[Array[Byte]] = {
    try {
      val value = db.get(key)
      if (value == null) None else Some(value)
    } catch {
      case e: Exception => {
        log.error("db get failed", e)
        None
      }
    }
  }

  def get(key: Array[Byte], opt: ReadOptions): Option[Array[Byte]] = {
    try {
      val value = db.get(key, opt)
      if (value == null) None else Some(value)
    } catch {
      case e: Exception => {
        log.error("db get failed", e)
        None
      }
    }
  }

  override def delete(key: Array[Byte]): Unit = {
    db.delete(key)
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

  override def commit(): Unit = {
  }

  override def close(): Unit = {
    db.close()
  }

  def batchWrite(action: WriteBatch => Unit): Boolean = {
    val batch = db.createWriteBatch()
    try {
      action(batch)
      db.write(batch)
      true
    } catch {
      case e: Throwable =>
        log.error("batchWrite failed", e)
        false
    } finally {
      batch.close()
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
}

object LevelDbStorage {
  def open(path: String, createIfMissing: Boolean = true): LevelDbStorage = {
    val options = new Options
    options.createIfMissing(createIfMissing)
    val db = factory.open(new File(path), options)
    return new LevelDbStorage(db)
  }
}