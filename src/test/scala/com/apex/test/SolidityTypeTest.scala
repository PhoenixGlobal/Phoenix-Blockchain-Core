package com.apex.test

import org.junit.Assert.assertEquals
import java.math.BigInteger
import org.junit.Test
import org.spongycastle.util.encoders.Hex
import com.apex.solidity.SolidityType

class SolidityTypeTest {
  @Test
  def ensureUnsignedInteger_isDecodedWithCorrectSignum(): Unit = {
    val bigNumberByteArray = Array(-13, -75, 19, 86, -119, 67, 112, -4, 118, -86, 98, -46, 103, -42, -126, 63, -60, -15, -87, 57, 43, 11, -17, -52, 0, 3, -65, 14, -67, -40, 65, 119).map(f => f.toByte)
    val testObject = new SolidityType.UnsignedIntType("uint256")
    val decode: AnyRef = testObject.decode(bigNumberByteArray)
    assert(decode.isInstanceOf[BigInteger])
    val actualBigInteger = decode.asInstanceOf[BigInteger]
    val expectedBigInteger = new BigInteger(Hex.toHexString(bigNumberByteArray), 16)
    assertEquals(expectedBigInteger, actualBigInteger)
  }

  @Test
  def ensureSignedInteger_isDecoded(): Unit = {
    val bigNumberByteArray = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 127, -1, -1, -1, -1, -1, -1, -1).map(_.toByte)
    val testObject = new SolidityType.IntType("int256")
    val decode = testObject.decode(bigNumberByteArray)
    assert(decode.isInstanceOf[BigInteger])
    val actualBigInteger = decode.asInstanceOf[BigInteger]
    val expectedBigInteger = new BigInteger(bigNumberByteArray)
    assertEquals(expectedBigInteger, actualBigInteger)
  }
}
