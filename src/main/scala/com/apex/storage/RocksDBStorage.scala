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

import java.util.Map

import com.apex.common.ApexLogging
import org.rocksdb._

class RocksDBStorage(db: RocksDB) extends Storage[Array[Byte], Array[Byte]] with ApexLogging {
  override def get(key: Array[Byte]): Option[Array[Byte]] = {
    val opt = new ReadOptions().setFillCache(true)
    val value = db.get(opt, key)
    if (value == null) {
      None
    } else {
      Some(value)
    }
  }

  override def set(key: Array[Byte], value: Array[Byte], batch: Batch): Boolean = ???

  override def delete(key: Array[Byte], batch: Batch): Boolean = ???

  override def batchWrite(action: Batch => Unit): Boolean = ???

  override def last(): Option[Map.Entry[Array[Byte], Array[Byte]]] = ???

  override def scan(func: (Array[Byte], Array[Byte]) => Unit): Unit = ???

  override def find(prefix: Array[Byte], func: (Array[Byte], Array[Byte]) => Unit): Unit = ???

  override def newSession(): Unit = ???

  override def commit(revision: Long): Unit = ???

  override def commit(): Unit = ???

  override def rollBack(): Unit = ???

  override def close(): Unit = ???

  override def revision(): Long = ???

  override def uncommitted(): Seq[Long] = ???

  override def onRollback(action: () => Unit): Unit = ???
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

object RocksDBStorage {
  // a static method that loads the RocksDB C++ library.
  RocksDB.loadLibrary();

  def open(path: String, createIfMissing: Boolean = true): RocksDBStorage = {
    val options = new Options
    options.setCreateIfMissing(createIfMissing)
    val db = RocksDB.open(options, path)
    new RocksDBStorage(db)
  }
}
