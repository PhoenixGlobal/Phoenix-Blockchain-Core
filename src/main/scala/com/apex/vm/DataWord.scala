/*
 * Copyright (c) [2016] [ <ether.camp> ]
 * This file is part of the ethereumJ library.
 *
 * The ethereumJ library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The ethereumJ library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with the ethereumJ library. If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: DataWord.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-27 下午7:53@version: 1.0
 *
 */

package com.apex.vm

import java.io.ByteArrayOutputStream
import java.util
import java.util.Arrays

import org.bouncycastle.util.encoders.DecoderException
import org.spongycastle.util.encoders.{Encoder, Hex, HexEncoder}

class DataWord(val data: Array[Byte]) {

  import DataWord._

  def value = {
    BigInt(1, data)
  }

  /**
    * Converts this DataWord to an int, checking for lost information.
    * If this DataWord is out of the possible range for an int result
    * then an ArithmeticException is thrown.
    *
    * @return this DataWord converted to an int.
    * @throws ArithmeticException - if this will not fit in an int.
    */
  def intValue: Int = {
    var intVal = 0
    for (aData <- data) {
      intVal = (intVal << 8) + (aData & 0xff)
    }
    intVal
  }

  /**
    * In case of int overflow returns Integer.MAX_VALUE
    * otherwise works as #intValue()
    */
  def intValueSafe: Int = {
    if (bytesOccupied > 4) {
      Int.MaxValue
    } else {
      val value = intValue
      if (value < 0) {
        Int.MaxValue
      } else {
        value
      }
    }
  }

  /**
    * Converts this DataWord to a long, checking for lost information.
    * If this DataWord is out of the possible range for a long result
    * then an ArithmeticException is thrown.
    *
    * @return this DataWord converted to a long.
    * @throws ArithmeticException - if this will not fit in a long.
    */
  def longValue: Long = {
    var longVal = 0
    for (aData <- data) {
      longVal = (longVal << 8) + (aData & 0xff)
    }
    longVal
  }

  /**
    * In case of long overflow returns Long.MAX_VALUE
    * otherwise works as #longValue()
    */
  def longValueSafe: Long = {
    if (bytesOccupied > 8) {
      Long.MaxValue
    } else {
      val longVal = longValue
      if (longVal < 0) {
        Long.MaxValue
      } else {
        longVal
      }
    }
  }

  def sValue = BigInt(data)

  def isZero: Boolean = {
    equals(ZERO)
  }

  // only in case of signed operation
  // when the number is explicit defined
  // as negative
  def isNegative: Boolean = {
    val result = data(0) & 0x80
    result == 0x80
  }

  def and(word: DataWord): DataWord = {
    val newData = copyData
    for (i <- 0 to data.length - 1) {
      newData(i) = (newData(i) & word.data(i)).toByte
    }
    new DataWord(newData)
  }

  def or(word: DataWord): DataWord = {
    val newData = copyData
    for (i <- 0 to data.length - 1) {
      newData(i) = (newData(i) | word.data(i)).toByte
    }
    new DataWord(newData)
  }

  def xor(word: DataWord): DataWord = {
    val newData = copyData
    for (i <- 0 to data.length - 1) {
      newData(i) = (newData(i) ^ word.data(i)).toByte
    }
    Byte
    new DataWord(newData)
  }

  def negate: DataWord = {
    if (isZero) ZERO else bnot.add(ONE)
  }

  def bnot: DataWord = {
    new DataWord((if (isZero) MAX_VALUE else MAX_VALUE - value).toBytes)
  }

  // By   : Holger
  // From : http://stackoverflow.com/a/24023466/459349
  def add(word: DataWord): DataWord = {
    var overflow = 0
    val newData = new Array[Byte](32)
    for (i <- 31 to 0 by -1) {
      val v = (data(i) & 0xff) + (word.data(i) & 0xff) + overflow
      newData(i) = v.toByte
      overflow = v >>> 8
    }
    new DataWord(newData)
  }

  // old add-method with BigInteger quick hack
  def add2(word: DataWord): DataWord = {
    new DataWord((value + word.value & MAX_VALUE).toBytes)
  }

  // TODO: mul can be done in more efficient way
  // TODO:     with shift left shift right trick
  // TODO      without BigInteger quick hack
  def mul(word: DataWord): DataWord = {
    new DataWord((value * word.value & MAX_VALUE).toBytes)
  }

  // TODO: improve with no BigInteger
  def div(word: DataWord): DataWord = {
    if (word.isZero) {
      ZERO
    } else {
      new DataWord((value / word.value & MAX_VALUE).toBytes)
    }
  }

  def sDiv(word: DataWord): DataWord = {
    if (word.isZero) {
      ZERO
    } else {
      new DataWord((sValue / word.sValue & MAX_VALUE).toBytes)
    }
  }

  // TODO: improve with no BigInteger
  def sub(word: DataWord): DataWord = {
    val result = ((value - word.value) & MAX_VALUE).toBytes
    new DataWord(result)
  }

  // TODO: improve with no BigInteger
  def exp(word: DataWord): DataWord = {
    new DataWord(value.modPow(word.value, _2_256).toBytes)
  }

  // TODO: improve with no BigInteger
  def mod(word: DataWord): DataWord = {
    if (word.isZero) {
      ZERO
    } else {
      new DataWord((value.mod(word.value) & MAX_VALUE).toBytes)
    }
  }

  def sMod(word: DataWord): DataWord = {
    if (word.isZero) {
      ZERO
    } else {
      var result = sValue.abs.mod(word.sValue.abs)
      result = if (sValue.signum == -1) -result else result
      new DataWord((result & MAX_VALUE).toBytes)
    }
  }

  def addmod(word1: DataWord, word2: DataWord): DataWord = {
    if (word2.isZero) {
      ZERO
    } else {
      val result = (value + word1.value).mod(word2.value)
      new DataWord((result & MAX_VALUE).toBytes)
    }
  }

  def mulmod(word1: DataWord, word2: DataWord): DataWord = {
    if (isZero || word1.isZero || word2.isZero) {
      ZERO
    } else {
      val result = (value * word1.value).mod(word2.value)
      new DataWord((result & MAX_VALUE).toBytes)
    }
  }

  /**
    * Shift left, both this and input arg are treated as unsigned
    *
    * @param arg
    * @return this << arg
    */
  def shiftLeft(arg: DataWord): DataWord = {
    if (arg.value >= BigInt(MAX_POW)) {
      ZERO
    } else {
      val result = value << arg.intValueSafe
      new DataWord((result & MAX_VALUE).toBytes)
    }
  }

  /**
    * Shift right, both this and input arg are treated as unsigned
    *
    * @param arg
    * @return this >> arg
    */
  def shiftRight(arg: DataWord): DataWord = {
    if (arg.value >= BigInt(MAX_POW)) {
      ZERO
    } else {
      val result = value >> arg.intValueSafe
      new DataWord((result & MAX_VALUE).toBytes)
    }
  }

  /**
    * Shift right, this is signed, while input arg is treated as unsigned
    *
    * @param arg
    * @return this >> arg
    */
  def shiftRightSigned(arg: DataWord): DataWord = {
    if (arg.value >= BigInt(MAX_POW)) {
      if (isNegative) {
        ONE.negate
      } else {
        ZERO
      }
    } else {
      val result = value >> arg.intValueSafe
      new DataWord((result & MAX_VALUE).toBytes)
    }
  }

  def signExtend(k: Byte): DataWord = {
    if (0 > k || k > 31) {
      throw new IndexOutOfBoundsException
    }
    val mask = if (sValue.testBit((k * 8) + 7)) 0xff.toByte else 0.toByte
    val newData = copyData
    for (i <- 31 to k + 1 by -1) {
      newData(31 - i) = mask
    }
    new DataWord(newData)
  }

  def getNoLeadZeroesData: Array[Byte] = {
    copyData.stripLeadingZeroes
  }

  def getLast20Bytes: Array[Byte] = {
    Arrays.copyOfRange(data, 12, data.length)
  }

  def bytesOccupied: Int = {
    val firstNonZero = data.indexWhere(_ != 0)
    if (firstNonZero == -1) 0 else 31 - firstNonZero + 1
  }

  /**
    * Returns instance data
    * Actually copy of internal byte array is provided
    * in order to protect DataWord immutability
    *
    * @return instance data
    */
  def getData: Array[Byte] = util.Arrays.copyOf(data, data.length)

  def toPrefixString: String = {
    val pref = getNoLeadZeroesData
    pref.length match {
      case 0 => ""
      case i if i < 7 => pref.toHex
      case _ => pref.toHex.substring(0, 6)
    }
  }

  def shortHex: String = {
    val hexValue = getNoLeadZeroesData.toHex.toUpperCase
    "0x" + hexValue.replaceFirst("^0+(?!$)", "")
  }

  def isHex(hex: String): Boolean = data.toHex == hex

  def asString = new String(getNoLeadZeroesData)

  override def toString: String = data.toHex

  override def hashCode: Int = java.util.Arrays.hashCode(data)

  override def equals(obj: scala.Any): Boolean = {
    super.equals(obj) || (obj match {
      case that: DataWord => data.sameElements(that.data)
      case _ => false
    })
  }

  /**
    * Returns copy of instance data
    *
    * @return copy of instance data
    */
  private def copyData = util.Arrays.copyOf(data, data.length)
}

object DataWord {
  private val encoder = new HexEncoder

  /* Maximum value of the DataWord */
  val MAX_POW = 256
  val _2_256 = BigInt(2).pow(MAX_POW)
  val MAX_VALUE = _2_256 - BigInt(1)
  val ZERO = new DataWord(new Array[Byte](32))
  val ONE: DataWord = DataWord.of(1.toByte)

  def of(data: String): DataWord = {
    //    val bOut = new ByteArrayOutputStream
    //    try {
    //      encoder.decode(data, bOut)
    //    } catch {
    //      case e: Exception =>
    //        throw new Exception(s"exception decoding Hex string: ${e.getMessage}", e)
    //    }
    //
    //    of(bOut.toByteArray.toHex)
    of(Hex.decode(data))
  }

  def of(num: Byte): DataWord = {
    val bb = new Array[Byte](32)
    bb(31) = num
    new DataWord(bb)
  }

  def of(num: Long): DataWord = {
    of(num.toBytes)
  }

  def of(num: BigInt): DataWord = {
    of(num.toByteArray)
  }

  def of(data: Array[Byte]): DataWord = {
    if (data == null || data.length == 0) {
      ZERO
    } else {
      val leadingZeroBits = data.numberOfLeadingZeros
      val valueBits = 8 * data.length - leadingZeroBits
      if (valueBits <= 8 && data(data.length - 1) == 0) {
        ZERO
      } else if (valueBits <= 8 && data(data.length - 1) == 0) {
        ONE
      } else {
        if (data.length > 32) {
          throw new RuntimeException(s"Data word can't exceed 32 bytes: 0x${data.toHex}")
        } else if (data.length == 32) {
          new DataWord(util.Arrays.copyOf(data, data.length))
        } else {
          val bytes = new Array[Byte](32)
          System.arraycopy(data, 0, bytes, 32 - data.length, data.length)
          new DataWord(bytes)
        }
      }
    }
  }
}
