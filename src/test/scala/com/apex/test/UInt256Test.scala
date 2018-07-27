/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: UInt256Test.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午1:37@version: 1.0
 */

package com.apex.test

import com.apex.crypto.UInt256
import org.junit.Test

import scala.util.Random

@Test
class UInt256Test {
  @Test(expected = classOf[IllegalArgumentException])
  def testCtorNull: Unit = {
    new UInt256(null)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testCtorWrongSize1: Unit = {
    new UInt256(Array.fill(31)(0))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testCtorWrongSize2: Unit = {
    new UInt256(Array.fill(33)(0))
  }

  @Test def testEquals = {
    assert(!UInt256.Zero.equals(null))
    assert(UInt256.Zero.equals(UInt256.Zero))
    val a = SerializerTest.testHash256()
    val b = SerializerTest.testHash256()
    val c = SerializerTest.testHash160()
    val d = SerializerTest.testHash256("Test")
    assert(a.equals(a))
    assert(a.equals(b))
    assert(!a.equals(c))
    assert(!a.equals(d))
    assert(!a.equals(null))
  }

  @Test
  def testCompare = {
    assert(UInt256.Zero.compare(UInt256.Zero) == 0)
    val a = SerializerTest.testHash256()
    val b = SerializerTest.testHash256()
    val c = SerializerTest.testHash256("Test")
    assert(a.compare(b) == 0)
    assert(a.compare(c) < 0)
    assert(c.compare(a) > 0)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testCompareNull: Unit = {
    UInt256.Zero.compare(null)
  }

  @Test
  def testSerialize = {
    val o = new SerializerTest(UInt256.deserialize)
    o.test(SerializerTest.testHash256())
    o.test(UInt256.Zero)
  }
}
