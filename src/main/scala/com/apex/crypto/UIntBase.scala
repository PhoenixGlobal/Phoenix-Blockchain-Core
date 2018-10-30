/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: UIntBase.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.crypto

import java.io.{DataInputStream, DataOutputStream}
import java.nio.ByteBuffer
import com.apex.common.Serializable
import org.bouncycastle.util.encoders.Hex

abstract class UIntBase(val size: Int, val data: Array[Byte]) extends Serializable {
  if (data == null || data.length != size) {
    throw new IllegalArgumentException("data")
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: UIntBase => data sameElements that.data
      case _ => false
    }
  }

  override def hashCode(): Int = ByteBuffer.wrap(data).getInt

  override def toString: String = Hex.toHexString(data)

  def shortString(): String = {
    toString.substring(0, 7)
  }

  override def serialize(os: DataOutputStream): Unit = os.write(data)
}

object UIntBase {
  def compare[T <: UIntBase](x: T, y: T): Int = {
    if (x == null || y == null) throw new IllegalArgumentException
    var r = 0
    if (x != y) {
      for (i <- 0 to x.size - 1 if r == 0) {
        if (x.data(i) < y.data(i)) {
          r = -1
        } else if (x.data(i) > y.data(i)) {
          r = 1
        }
      }
    }
    r
  }
}