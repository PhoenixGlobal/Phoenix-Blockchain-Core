/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Fixed8.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.crypto

import java.io.{DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.exceptions.OverflowException

class Fixed8(val value: Long = 0) extends Serializable {
  def ceiling: Fixed8 = {
    val remainder = value % Fixed8.One.value
    if (remainder == 0) return this
    if (remainder > 0) {
      new Fixed8(value - remainder + Fixed8.One.value)
    } else {
      new Fixed8(value - remainder)
    }
  }

  def equals(that: Fixed8): Boolean = this == that

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: Fixed8 => equals(that)
      case _ => false
    }
  }

  override def serialize(os: DataOutputStream): Unit = {
    os.writeLong(value)
  }

  def unary_-(): Fixed8 = new Fixed8(-value)

  def +(that: Fixed8): Fixed8 = new Fixed8(value + that.value)

  def -(that: Fixed8): Fixed8 = new Fixed8(value - that.value)

  def *(that: Fixed8): Fixed8 = new Fixed8(value * that.value)

  def >(that: Fixed8): Boolean = value > that.value

  def ==(that: Fixed8): Boolean = {
    that match {
      case null => false
      case _ => value == that.value
    }
  }

}

object Fixed8 {
  final val MaxValue: Fixed8 = new Fixed8(Long.MaxValue)
  final val MinValue: Fixed8 = new Fixed8(Long.MinValue)
  final val One: Fixed8 = new Fixed8(100000000)
  final val Zero: Fixed8 = new Fixed8(0)

  def sum(args: Fixed8*): Fixed8 = args.sum

  def max(args: Fixed8*): Fixed8 = args.max

  def min(args: Fixed8*): Fixed8 = args.min

  def deserialize(is: DataInputStream): Fixed8 = new Fixed8(is.readLong)

  def fromDecimal(d: BigDecimal): Fixed8 = {
    val value = d * One.value
    if (value < Long.MinValue || value > Long.MaxValue) throw new OverflowException
    return new Fixed8(value.toLong)
  }

  implicit object Fixed8Numeric extends Numeric[Fixed8] {
    override def plus(x: Fixed8, y: Fixed8): Fixed8 = x + y

    override def minus(x: Fixed8, y: Fixed8): Fixed8 = x - y

    override def times(x: Fixed8, y: Fixed8): Fixed8 = x * y

    override def negate(x: Fixed8): Fixed8 = -x

    override def fromInt(x: Int): Fixed8 = new Fixed8(x)

    override def toInt(x: Fixed8): Int = x.value.toInt

    override def toLong(x: Fixed8): Long = x.value

    override def toFloat(x: Fixed8): Float = x.value.toFloat

    override def toDouble(x: Fixed8): Double = x.value.toDouble

    override def compare(x: Fixed8, y: Fixed8): Int = {
      if (x == null || y == null) throw new IllegalArgumentException
      x.value.compareTo(y.value)
    }
  }
}

