/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: LevelDBStorageTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-25 下午2:09@version: 1.0
 */

package com.apex.test

import com.apex.core.HeaderStore
import com.apex.storage.LevelDbStorage
import org.junit.{AfterClass, Test}

import scala.collection.mutable.ListBuffer
import scala.reflect.io.Directory

@Test
class LevelDBStorageTest {
  @Test
  def testSet() = {
    val storage = LevelDBStorageTest.open("test_db")
    assert(true == storage.set("testSet".getBytes, "testSetValue".getBytes))
    storage.close()
  }

  @Test
  def testGet() = {
    val key = "testGet".getBytes
    val valueString = "testGetValue"
    val storage = LevelDBStorageTest.open("test_db")
    assert(true == storage.set(key, valueString.getBytes))
    val value = storage.get(key)
    assert(value.isDefined)
    assert(new String(value.get).equals(valueString))
    storage.close()
  }

  @Test
  def testUpdate() = {
    val key = "testUpdate".getBytes
    val valueString = "testUpdateValue"
    val newValueString = "testUpdateValueNew"
    val storage = LevelDBStorageTest.open("test_db")
    assert(true == storage.set(key, valueString.getBytes))
    val value = storage.get(key)
    assert(value.isDefined)
    assert(new String(value.get).equals(valueString))
    assert(true == storage.set(key, newValueString.getBytes))
    val newValue = storage.get(key)
    assert(newValue.isDefined)
    assert(new String(newValue.get).equals(newValueString))
    storage.close()
  }

  @Test
  def testGetKeyNotExists() = {
    val storage = LevelDBStorageTest.open("test_db")
    val value = storage.get("testNotExistKey".getBytes)
    assert(value.isEmpty)
    storage.close()
  }

  @Test
  def testDelete() = {
    val key = "testDelete".getBytes
    val value = "testDeleteValue".getBytes
    val storage = LevelDBStorageTest.open("test_db")
    assert(true == storage.set(key, value))
    assert(storage.get(key).isDefined)
    storage.delete(key)
    assert(storage.get(key).isEmpty)
    storage.close()
  }

  @Test
  def testScan() = {
    val storage = LevelDBStorageTest.open("scan_test_db")
    var pairs = collection.mutable.Seq.empty[(String, String)]
    for (i <- 1 to 10) {
      val key = s"key$i"
      val value = s"value$i"
      pairs = pairs :+ (key, value)
      if (storage.get(key.getBytes).isEmpty) {
        assert(storage.set(key.getBytes, value.getBytes))
      }
    }
    var i = 0
    pairs = pairs.sortBy(_._1)
    storage.scan((k, v) => {
      assert((new String(k), new String(v)).equals(pairs(i)))
      i += 1
    })
    assert(i == 10)
    storage.close()
  }

  @Test
  def testFind() = {
    val storage = LevelDBStorageTest.open("find_test_db")
    val seqArr = Array(
      collection.mutable.Seq.empty[(String, String)],
      collection.mutable.Seq.empty[(String, String)]
    )
    val prefixes = Seq("key_a_", "key_b_")
    for (i <- 1 to 10) {
      val key = s"${prefixes(i % 2)}$i"
      val keyBytes = key.getBytes
      val value = s"value$i"
      seqArr(i % 2) = seqArr(i % 2) :+ (key, value)
      if (storage.get(keyBytes).isEmpty) {
        assert(storage.set(keyBytes, value.getBytes))
      }
    }

    for (j <- 0 to 1) {
      var i = 0
      val seq = seqArr(j).sortBy(_._1)
      storage.find(prefixes(j).getBytes, (k, v) => {
        assert((new String(k), new String(v)).equals(seq(i)))
        i += 1
      })
      assert(i == 5)
    }
    storage.close()
  }

  @Test
  def testLastEmpty(): Unit = {
    val storage = LevelDBStorageTest.open("last_test_empty_db")
    assert(storage.last().isEmpty)
  }

  @Test
  def testLast(): Unit = {
    val storage = LevelDBStorageTest.open("last_test_db")
    for (i <- 0 to 10) {
      storage.set(BigInt(i).toByteArray, s"test$i".getBytes)
    }

    val last = storage.last.get
    assert(BigInt(last.getKey).toInt == 10)
    assert(new String(last.getValue).equals("test10"))
    storage.close()
  }

  @Test
  def testSession(): Unit = {

    def assertUncommittedSessions(levels: Seq[Int], min: Int, max: Int) = {
      assert(levels.length == max - min + 1)
      var start = min
      for (elem <- levels) {
        assert(elem == start)
        start += 1
      }
      assert(start == max + 1)
    }

    {
      val db = LevelDBStorageTest.open("test_session")
      try {
        db.newSession()
        assert(db.revision() == 2)
        assertUncommittedSessions(db.uncommitted(), 1, 1)
      } finally {
        db.close()
      }
    }
    {
      val db = LevelDBStorageTest.open("test_session")
      try {
        db.newSession()
        assert(db.revision() == 3)
        assertUncommittedSessions(db.uncommitted(), 1, 2)
      } finally {
        db.close()
      }
    }
    {
      val db = LevelDBStorageTest.open("test_session")
      try {
        assert(db.revision() == 3)
        db.rollBack()
        assert(db.revision() == 2)
        assertUncommittedSessions(db.uncommitted(), 1, 1)
      } finally {
        db.close()
      }
    }
    {
      val db = LevelDBStorageTest.open("test_session")
      try {
        assert(db.revision() == 2)
        db.commit()
        assert(db.uncommitted().isEmpty)
      } finally {
        db.close()
      }
    }
    {
      val db = LevelDBStorageTest.open("test_session")
      try {
        assert(db.revision() == 2)
        assert(db.uncommitted().isEmpty)
        db.newSession()
        db.newSession()
        db.newSession()
        db.newSession()
        db.newSession()
        db.newSession()
        assert(db.revision() == 8)
      } finally {
        db.close()
      }
    }
    {
      val db = LevelDBStorageTest.open("test_session")
      try {
        assert(db.revision() == 8)
        assertUncommittedSessions(db.uncommitted(), 2, 7)
        db.commit(5)
        assertUncommittedSessions(db.uncommitted(), 6, 7)
      } finally {
        db.close()
      }
    }
    {
      val db = LevelDBStorageTest.open("test_session")
      try {
        assert(db.revision() == 8)
        assertUncommittedSessions(db.uncommitted(), 6, 7)
        db.rollBack()
        db.rollBack()
        assert(db.uncommitted().isEmpty)
        assert(db.revision() == 6)
      } finally {
        db.close()
      }
    }
    {
      val db = LevelDBStorageTest.open("test_session")
      try {
        assert(db.uncommitted().isEmpty)
        assert(db.revision() == 6)
        println(s"final revision ${db.revision()}")
      } finally {
        db.close()
      }
    }
  }
}

object LevelDBStorageTest {
  private final val dirs = ListBuffer.empty[String]
  private final val dbs = ListBuffer.empty[LevelDbStorage]

  def open(dir: String): LevelDbStorage = {
    val db = LevelDbStorage.open(dir)
    if (!dirs.contains(dir)) {
      dirs.append(dir)
    }
    dbs.append(db)
    db
  }

  def closeDB(db: LevelDbStorage): Unit = {
    db.close()
    dbs -= db
  }

  @AfterClass
  def cleanUp: Unit = {
    dbs.foreach(_.close())
    dirs.foreach(deleteDir)
  }

  private def deleteDir(dir: String): Unit = {
    try {
      Directory(dir).deleteRecursively()
    } catch {
      case e: Throwable => println(e.getMessage)
    }
  }
}
