package com.apex.test

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.crypto.FixedNumber
import org.junit.{Assert, Test}

@Test
class FixedNumberTest extends Assert {

  @Test
  def testToString = {
    assert(new FixedNumber(0).toString   == "0")
    assert(new FixedNumber(1).toString   == "0.000000000000000001")
    assert(new FixedNumber(10).toString  == "0.00000000000000001")
    assert(new FixedNumber(111).toString == "0.000000000000000111")
    assert(new FixedNumber(1000000000000000000L).toString == "1")
    assert(new FixedNumber(1000000000000000001L).toString == "1.000000000000000001")
    assert(new FixedNumber(1000000000000000101L).toString == "1.000000000000000101")
    assert(new FixedNumber(1000000000000000100L).toString == "1.0000000000000001")
    assert(new FixedNumber(BigInt("12000000000000000100")).toString == "12.0000000000000001")
    assert(new FixedNumber(BigInt("123000000000000000100")).toString == "123.0000000000000001")
    assert(new FixedNumber(BigInt("12345678000000000000001100")).toString == "12345678.0000000000000011")
    assert(new FixedNumber(BigInt("12345678912000000000000001100")).toString == "12345678912.0000000000000011")
  }

  @Test
  def testOperatorMinus = {
    //assert((-FixedNumber.MaxValue).value == -Long.MaxValue)
    assert((-FixedNumber.MinValue).value == -1)
    assert((-FixedNumber.One).value == -(FixedNumber.One.value))
    assert((-FixedNumber.Zero).value == 0)
    assert((-new FixedNumber(1)).value == -1)
  }

  @Test
  def testOperatorEquals = {
    //assert(FixedNumber.MaxValue == FixedNumber.MaxValue)
    assert(FixedNumber.MinValue == FixedNumber.MinValue)
    assert(FixedNumber.One == FixedNumber.One)
    assert(FixedNumber.Zero == FixedNumber.Zero)
    //assert(!(FixedNumber.MaxValue == FixedNumber.MinValue))
    assert(!(FixedNumber.MinValue == FixedNumber.One))
    assert(!(FixedNumber.One == FixedNumber.Zero))
    assert(new FixedNumber(0) == new FixedNumber(0))
    assert(!(new FixedNumber(0) == new FixedNumber(1)))
  }

  @Test
  def testEquals = {
    //assert(FixedNumber.MaxValue.equals(FixedNumber.MaxValue))
    assert(FixedNumber.MinValue.equals(FixedNumber.MinValue))
    assert(FixedNumber.One.equals(FixedNumber.One))
    assert(FixedNumber.Zero.equals(FixedNumber.Zero))
    //assert(!FixedNumber.MaxValue.equals(FixedNumber.MinValue))
    assert(!FixedNumber.MinValue.equals(FixedNumber.One))
    assert(!FixedNumber.One.equals(FixedNumber.Zero))
    assert(new FixedNumber(2018).equals(new FixedNumber(2018)))
    assert(!new FixedNumber(2018).equals(new FixedNumber(2017)))
    assert(!new FixedNumber(2018).equals(null))
  }

  @Test
  def testSum = {
    val arr1 = Array(new FixedNumber(1), new FixedNumber(2), new FixedNumber(3))
    assert(arr1.sum == new FixedNumber(1 + 2 + 3))

    val arr2 = Array(new FixedNumber(-1), new FixedNumber(1))
    assert(arr2.sum == FixedNumber.Zero)
  }

  @Test
  def testMax = {
    val arr = Array(new FixedNumber(-1), FixedNumber.Zero, new FixedNumber(1))
    assert(arr.max == arr(2))
  }

  @Test
  def testOrdering = {
    val arr = Array(new FixedNumber(1), FixedNumber.Zero, new FixedNumber(-1))
    val sorted = arr.sorted
    assert(sorted(0) == arr(2))
    assert(sorted(1) == arr(1))
    assert(sorted(2) == arr(0))
  }

  @Test
  def testSerialize = {
    val o = new SerializerHelper(FixedNumber.deserialize)
    o.test(FixedNumber.Zero)
    o.test(FixedNumber.One)
    //o.test(FixedNumber.MaxValue)
    o.test(FixedNumber.MinValue)
    o.test(new FixedNumber(2018))
  }

}
