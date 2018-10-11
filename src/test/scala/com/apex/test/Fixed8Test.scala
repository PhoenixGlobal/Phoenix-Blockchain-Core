package com.apex.test

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.crypto.Fixed8
import org.junit.{Assert, Test}

@Test
class Fixed8Test extends Assert {

  @Test
  def testToString = {
    assert(new Fixed8(1).toString == "0.00000001")
    assert(new Fixed8(10).toString == "0.00000010")
    assert(new Fixed8(111).toString == "0.00000111")
    assert(new Fixed8(100000000).toString == "1.00000000")
    assert(new Fixed8(100000001).toString == "1.00000001")
    assert(new Fixed8(100000101).toString == "1.00000101")
    assert(new Fixed8(100000100).toString == "1.00000100")
    assert(new Fixed8(1200000100).toString == "12.00000100")
    assert(new Fixed8(12300000100L).toString == "123.00000100")
    assert(new Fixed8(1234567800001100L).toString == "12345678.00001100")
    assert(new Fixed8(1234567891200001100L).toString == "12345678912.00001100")
  }

  @Test
  def testOperatorMinus = {
    assert((-Fixed8.MaxValue).value == -Long.MaxValue)
    assert((-Fixed8.MinValue).value == -Long.MinValue)
    assert((-Fixed8.One).value == -(Fixed8.One.value))
    assert((-Fixed8.Zero).value == 0)
    assert((-new Fixed8(1)).value == -1)
  }

  @Test
  def testOperatorEquals = {
    assert(Fixed8.MaxValue == Fixed8.MaxValue)
    assert(Fixed8.MinValue == Fixed8.MinValue)
    assert(Fixed8.One == Fixed8.One)
    assert(Fixed8.Zero == Fixed8.Zero)
    assert(!(Fixed8.MaxValue == Fixed8.MinValue))
    assert(!(Fixed8.MinValue == Fixed8.One))
    assert(!(Fixed8.One == Fixed8.Zero))
    assert(new Fixed8(0) == new Fixed8(0))
    assert(!(new Fixed8(0) == new Fixed8(1)))
  }

  @Test
  def testEquals = {
    assert(Fixed8.MaxValue.equals(Fixed8.MaxValue))
    assert(Fixed8.MinValue.equals(Fixed8.MinValue))
    assert(Fixed8.One.equals(Fixed8.One))
    assert(Fixed8.Zero.equals(Fixed8.Zero))
    assert(!Fixed8.MaxValue.equals(Fixed8.MinValue))
    assert(!Fixed8.MinValue.equals(Fixed8.One))
    assert(!Fixed8.One.equals(Fixed8.Zero))
    assert(new Fixed8(2018).equals(new Fixed8(2018)))
    assert(!new Fixed8(2018).equals(new Fixed8(2017)))
    assert(!new Fixed8(2018).equals(null))
  }

  @Test
  def testSum = {
    val arr1 = Array(new Fixed8(1), new Fixed8(2), new Fixed8(3))
    assert(arr1.sum == new Fixed8(1 + 2 + 3))

    val arr2 = Array(new Fixed8(-1), new Fixed8(1))
    assert(arr2.sum == Fixed8.Zero)
  }

  @Test
  def testMax = {
    val arr = Array(new Fixed8(-1), Fixed8.Zero, new Fixed8(1))
    assert(arr.max == arr(2))
  }

  @Test
  def testOrdering = {
    val arr = Array(new Fixed8(1), Fixed8.Zero, new Fixed8(-1))
    val sorted = arr.sorted
    assert(sorted(0) == arr(2))
    assert(sorted(1) == arr(1))
    assert(sorted(2) == arr(0))
  }

  @Test
  def testSerialize = {
    val o = new SerializerHelper(Fixed8.deserialize)
    o.test(Fixed8.Zero)
    o.test(Fixed8.One)
    o.test(Fixed8.MaxValue)
    o.test(Fixed8.MinValue)
    o.test(new Fixed8(2018))
  }

}
