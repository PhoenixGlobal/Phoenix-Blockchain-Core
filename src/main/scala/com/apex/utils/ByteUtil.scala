package com.apex.utils

import java.math.BigInteger
import java.util
import java.util.Arrays

import org.spongycastle.util.encoders.Hex

object ByteUtil {

  def toHexString(data: Array[Byte]): String = {
    if (data == null)
      ""
    else
      Hex.toHexString(data)
  }

  def bigIntegerToBytes(value: BigInteger): Array[Byte] = {
    if (value == null) return null
    var data = value.toByteArray
    if (data.length != 1 && data(0) == 0) {
      val tmp = new Array[Byte](data.length - 1)
      System.arraycopy(data, 1, tmp, 0, tmp.length)
      data = tmp
    }
    data
  }

  def bigIntegerToBytes(b: BigInteger, numBytes: Int): Array[Byte] = {
    if (b == null) return null
    val bytes = new Array[Byte](numBytes)
    val biBytes = b.toByteArray
    val start = if (biBytes.length == numBytes + 1) 1
    else 0
    val length = Math.min(biBytes.length, numBytes)
    System.arraycopy(biBytes, start, bytes, numBytes - length, length)
    bytes
  }

  def bigIntegerToBytesSigned(b: BigInteger, numBytes: Int): Array[Byte] = {
    if (b == null) return null
    //val bytes: Array[Byte] = new Array[Byte](numBytes)
    val bytes = Array.fill[Byte](numBytes)(if (b.signum < 0) 0xFF.toByte else 0x00)
    val biBytes: Array[Byte] = b.toByteArray
    val start: Int = if (biBytes.length == numBytes + 1) 1
    else 0
    val length: Int = Math.min(biBytes.length, numBytes)
    System.arraycopy(biBytes, start, bytes, numBytes - length, length)
    bytes
  }

  def bytesToBigInteger(bb: Array[Byte]): BigInteger = {
    if (bb == null || bb.length == 0)
      BigInteger.ZERO
    else
      new BigInteger(1, bb)
  }

  def merge(arrays: Array[Array[Byte]]): Array[Byte] = {
    var count = 0
    for (array <- arrays) {
      count += array.length
    }
    // Create new array and copy all array contents
    val mergedArray = new Array[Byte](count)
    var start = 0
    for (array <- arrays) {
      System.arraycopy(array, 0, mergedArray, start, array.length)
      start += array.length
    }
    mergedArray
  }

}
