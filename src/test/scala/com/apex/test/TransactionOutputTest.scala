/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: TransactionOutputTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午6:11@version: 1.0
 */

package com.apex.test

import com.apex.core.TransactionOutput
import com.apex.crypto.Ecdsa.PublicKey
import com.apex.crypto.Fixed8
import org.junit.Test
import play.api.libs.json.Json

@Test
class TransactionOutputTest {
  @Test
  def testEqual = {
    val a = TransactionOutput(SerializerTest.testHash160(), SerializerTest.testHash256(), Fixed8.One, "1234")
    val b = TransactionOutput(SerializerTest.testHash160(), SerializerTest.testHash256(), Fixed8.One, "1234")
    val c = TransactionOutput(SerializerTest.testHash160(), SerializerTest.testHash256(), Fixed8.Zero, "1234")
    val d = TransactionOutput(SerializerTest.testHash160("Test"), SerializerTest.testHash256(), Fixed8.One, "1234")
    val e = TransactionOutput(SerializerTest.testHash160(), SerializerTest.testHash256("Test"), Fixed8.One, "1234")
    assert(a.equals(a))
    assert(a.equals(b))
    assert(!a.equals(c))
    assert(!a.equals(d))
    assert(!a.equals(e))
    assert(!a.equals(null))
  }

  @Test
  def testSerialize = {
    val o = new SerializerTest(TransactionOutput.deserialize)
    val a = TransactionOutput(SerializerTest.testHash160(), SerializerTest.testHash256(), Fixed8.One, "1234")
    o.test(a)
    val b = TransactionOutput(SerializerTest.testHash160("中文"), SerializerTest.testHash256(), Fixed8.One, "1234")
    o.test(b)
    val c = TransactionOutput(SerializerTest.testHash160(), SerializerTest.testHash256("中文"), Fixed8.One, "1234")
    o.test(c)
  }

  @Test
  def testToJSON = {
    val a = TransactionOutput(SerializerTest.testHash160(), SerializerTest.testHash256(), Fixed8.One, "1234")
    assert(Json.toJson(a).toString.equals(s"""{"address":"${PublicKey.toAddress(a.address.data)}","assetId":"${a.assetId}","amount":"${a.amount.toString}","pubKeyScript":"${a.pubKeyScript.toString}","version":${a.version}}"""))
  }
}
