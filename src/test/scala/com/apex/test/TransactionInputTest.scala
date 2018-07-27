/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: TransactionInputTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午4:56@version: 1.0
 */

package com.apex.test

import com.apex.core.TransactionInput
import org.junit.Test
import play.api.libs.json.Json

@Test
class TransactionInputTest {
  @Test
  def testEqual = {
    val a = TransactionInput(SerializerTest.testHash256("test"), 0)
    val b = TransactionInput(SerializerTest.testHash256("test"), 0)
    val c = TransactionInput(SerializerTest.testHash256("test"), 1)
    val d = TransactionInput(SerializerTest.testHash256("Test"), 0)
    assert(a.equals(a))
    assert(a.equals(b))
    assert(!a.equals(c))
    assert(!a.equals(d))
    assert(!a.equals(null))
    val e = TransactionInput(SerializerTest.testHash256("中文"), 0)
    val f = TransactionInput(SerializerTest.testHash256("中文"), 0)
    assert(e.equals(f))
  }

  @Test
  def testHashCode = {
    val a = TransactionInput(SerializerTest.testHash256("test"), 0)
    val b = TransactionInput(SerializerTest.testHash256("test"), 0)
    assert(a.hashCode() == b.hashCode())
  }

  @Test
  def testSerialize = {
    val o = new SerializerTest(TransactionInput.deserialize)
    val a = TransactionInput(SerializerTest.testHash256(), Int.MaxValue)
    o.test(a)
    val b = TransactionInput(SerializerTest.testHash256(), Int.MinValue)
    o.test(b)
    val c = TransactionInput(SerializerTest.testHash256("中文"), 0)
    o.test(c)
  }

  @Test
  def testToJSON = {
    val a = TransactionInput(SerializerTest.testHash256(), Int.MaxValue)
    assert(Json.toJson(a).toString.equals(s"""{"blockId":"${a.blockId}","index":${a.index}}"""))
  }
}
