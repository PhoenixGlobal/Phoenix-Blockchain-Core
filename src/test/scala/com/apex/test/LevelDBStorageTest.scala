/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: LevelDBStorageTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-25 下午2:09@version: 1.0
 */

package com.apex.test

import com.apex.storage.LevelDbStorage
import org.junit.Test

@Test
class LevelDBStorageTest {
  @Test
  def testSet() = {
    val storage = LevelDbStorage.open("test_db")
    assert(true == storage.set("testSet".getBytes, "testSetValue".getBytes))
    storage.close()
  }

  @Test
  def testGet() = {
    val key = "testGet".getBytes
    val valueString = "testGetValue"
    val storage = LevelDbStorage.open("test_db")
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
    val storage = LevelDbStorage.open("test_db")
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
    val storage = LevelDbStorage.open("test_db")
    val value = storage.get("testNotExistKey".getBytes)
    assert(value.isEmpty)
    storage.close()
  }

  @Test
  def testDelete() = {
    val key = "testDelete".getBytes
    val value = "testDeleteValue".getBytes
    val storage = LevelDbStorage.open("test_db")
    assert(true == storage.set(key, value))
    assert(storage.get(key).isDefined)
    storage.delete(key)
    assert(storage.get(key).isEmpty)
    storage.close()
  }

  @Test
  def testScan() = {
    val storage = LevelDbStorage.open("scan_test_db")
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
    val storage = LevelDbStorage.open("find_test_db")
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
}
