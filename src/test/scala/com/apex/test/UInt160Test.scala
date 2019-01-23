/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: UInt160Test.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午2:46@version: 1.0
 */

package com.apex.test

import com.apex.crypto.UInt160
import org.junit.Test

import scala.util.Random

class UInt160Test {
  @Test(expected = classOf[IllegalArgumentException])
  def testCtorNull: Unit = {
    new UInt160(null)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testCtorWrongSize1: Unit = {
    new UInt160(Array.fill(15)(0))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testCtorWrongSize2: Unit = {
    new UInt160(Array.fill(17)(0))
  }

  @Test def testEquals = {
    assert(!UInt160.Zero.equals(null))
    assert(UInt160.Zero.equals(UInt160.Zero))
    val a = SerializerHelper.testHash160()
    val b = SerializerHelper.testHash160()
    val c = SerializerHelper.testHash256()
    val d = SerializerHelper.testHash160("Test")
    assert(a.equals(a))
    assert(a.equals(b))
    assert(!a.equals(c))
    assert(!a.equals(d))
    assert(!a.equals(null))
  }

  @Test
  def testCompare = {
    assert(UInt160.Zero.compare(UInt160.Zero) == 0)
    val a = SerializerHelper.testHash160()
    val b = SerializerHelper.testHash160()
    val c = SerializerHelper.testHash160("Test")
    assert(a.compare(b) == 0)
    assert(a.compare(c) < 0)
    assert(c.compare(a) > 0)
  }

  @Test
  def testCompare2 = {
    val a = UInt160.parse("1212121212121212121212121212121212121211").get
    val b = UInt160.parse("1212121212121212121212121212121212121212").get
    val c = UInt160.parse("1212121212121212121212121212121212121213").get
    val d = UInt160.parse("1212121212121212121212121212121212121211").get

    assert(a == d)
    assert(a != b)
    assert(a != c)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testCompareNull: Unit = {
    UInt160.Zero.compare(null)
  }

  @Test
  def testSerialize = {
    val o = new SerializerHelper(UInt160.deserialize)
    o.test(SerializerHelper.testHash160())
    o.test(UInt160.Zero)
  }
}
