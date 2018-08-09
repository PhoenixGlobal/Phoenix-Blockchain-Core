/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: UInt160.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.crypto

import java.io.DataInputStream
import com.apex.wallets.Wallet
import org.bouncycastle.util.encoders.Hex

class UInt160(data: Array[Byte]) extends UIntBase(UInt160.Size, data) with Ordered[UInt160] {
  override def compare(that: UInt160): Int = UIntBase.compare(this, that)

  def toAddressString(): String = Wallet.toAddress(data)
}

object UInt160 {
  final val Size: Int = 20
  final val Zero: UInt160 = UInt160.fromBytes(Array.fill(Size)(0.toByte))

  def fromBytes(bytes: Array[Byte]): UInt160 = {
    require(bytes.length == 20)
    new UInt160(bytes)
  }

  def tryParse(str: String): Option[UInt160] = {
    if (str == null || str.isEmpty) {
      None
    } else {
      val s = if (str.startsWith("0x")) str.substring(2) else str
      if (s.length != 40) {
        None;
      } else {
        Some(UInt160.fromBytes(Hex.decode(s)))
      }
    }
  }

  def deserialize(is: DataInputStream): UInt160 = {
    val data = Array.fill(UInt160.Size)(0.toByte)
    is.read(data, 0, UInt160.Size)
    UInt160.fromBytes(data)
  }
}