package com.apex.test

import com.apex.crypto.BinaryData
import com.apex.vm.DataWord
import org.junit.Test

@Test
class DataWordTest {

  @Test
  def testSignExtend1 = {
    val x = DataWord.of(BinaryData("f2"))
    val k: Byte = 0
    val expected = BinaryData("fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff2")

    val signExtend = x.signExtend(k)
    assert(expected sameElements signExtend.data)
  }

  @Test
  def testSignExtend2 = {
    val x = DataWord.of(BinaryData("f2"))
    val k: Byte = 1
    val expected = BinaryData("00000000000000000000000000000000000000000000000000000000000000f2")

    val signExtend = x.signExtend(k)
    assert(expected sameElements signExtend.data)
  }

  @Test
  def testSignExtend3 = {
    val x = DataWord.of(BinaryData("0f00ab"))
    val k: Byte = 1
    val expected = BinaryData("00000000000000000000000000000000000000000000000000000000000000ab")

    val signExtend = x.signExtend(k)
    assert(expected sameElements signExtend.data)
  }

  @Test
  def testSignExtend4 = {
    val x = DataWord.of(BinaryData("ffff"))
    val k: Byte = 1
    val expected = BinaryData("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")

    val signExtend = x.signExtend(k)
    assert(expected sameElements signExtend.data)
  }

  @Test
  def testSignExtend5 = {
    val x = DataWord.of(BinaryData("ffffffff"))
    val k: Byte = 3
    val expected = BinaryData("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")

    val signExtend = x.signExtend(k)
    assert(expected sameElements signExtend.data)
  }

  @Test
  def testSignExtend6 = {
    val x = DataWord.of(BinaryData("ab02345678"))
    val k: Byte = 3
    val expected = BinaryData("0000000000000000000000000000000000000000000000000000000002345678")

    val signExtend = x.signExtend(k)
    assert(expected sameElements signExtend.data)
  }

  @Test
  def testSignExtend7 = {
    val x = DataWord.of(BinaryData("ab82345678"))
    val k: Byte = 3
    val expected = BinaryData("ffffffffffffffffffffffffffffffffffffffffffffffffffffffff82345678")

    val signExtend = x.signExtend(k)
    assert(expected sameElements signExtend.data)
  }

  @Test
  def testSignExtend8 = {
    val x = DataWord.of(BinaryData("ff34567882345678823456788234567882345678823456788234567882345678"))
    val k: Byte = 30
    val expected = BinaryData("0034567882345678823456788234567882345678823456788234567882345678")

    val signExtend = x.signExtend(k)
    assert(expected sameElements signExtend.data)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testSignExtend9 = {
    val x = DataWord.ZERO
    val k: Byte = -1

    val signExtend = x.signExtend(k)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testSignExtend10 = {
    val x = DataWord.ZERO
    val k: Byte = 32

    val signExtend = x.signExtend(k)
  }

}
