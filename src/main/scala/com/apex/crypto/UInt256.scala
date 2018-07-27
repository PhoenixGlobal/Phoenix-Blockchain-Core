/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: UInt256.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.crypto

import java.io.DataInputStream
import java.math.BigInteger

class UInt256(data: Array[Byte]) extends UIntBase(UInt256.Size, data) with Ordered[UInt256] {
  override def compare(that: UInt256): Int = UIntBase.compare(this, that)
}

object UInt256 {
  final val Size: Int = 32
  final val Zero: UInt256 = UInt256.fromBytes(Array.fill(Size)(0.toByte))

  def fromBytes(bytes: Array[Byte]): UInt256 = new UInt256(bytes)

  def tryParse(str: String): Option[UInt256] = {
    if (str == null || str.isEmpty) {
      None
    } else {
      val s = if (str.startsWith("0x")) str.substring(2) else str
      if (s.length != 64) {
        None;
      } else {
        val data = new BigInteger(s, 16).toByteArray.reverse
        Some(UInt256.fromBytes(data))
      }
    }
  }

  def deserialize(is: DataInputStream): UInt256 = {
    val data = Array.fill(UInt256.Size)(0.toByte)
    is.read(data, 0, UInt256.Size)
    UInt256.fromBytes(data)
  }
}
