/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: package.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-28 下午6:53@version: 1.0
 *
 */

package com.apex

import java.nio.ByteBuffer

import org.bouncycastle.util.encoders.Hex

package object vm {

  implicit class ByteExtension(value: Byte) {
    def toHex = {
      val retVal = Integer.toString(value & 0xFF, 16)
      if (retVal.length == 1) {
        s"0$retVal"
      } else {
        retVal
      }
    }
  }

  implicit class ByteArrayExtension(data: Array[Byte]) {
    def firstNonZeroByte: Int = {
      var j = 0
      for (i <- 0 to data.length - 1 if data(i) == 0) {
        j += 1
      }
      if (j < data.length) j else -1
    }

    def numberOfLeadingZeros: Int = {
      val i = data.firstNonZeroByte
      if (i == -1) {
        data.length * 8
      } else {
        val byteLeadingZeros = Integer.numberOfLeadingZeros(data(i).toInt & 0xff) - 24
        i * 8 + byteLeadingZeros
      }
    }

    def stripLeadingZeroes: Array[Byte] = {
      if (data == null) {
        null
      } else {
        val firstNonZero = data.firstNonZeroByte
        firstNonZero match {
          case -1 => Array.empty
          case 0 => data
          case _ =>
            val result = new Array[Byte](data.length - firstNonZero)
            System.arraycopy(data, firstNonZero, result, 0, data.length - firstNonZero)
            result
        }
      }
    }

    /**
      * Parses fixed number of bytes starting from offset in data array.
      * If data has not enough bytes return array will be right padded with zero bytes.
      * I.e. if offset is higher than data.length then zero byte array of length len will be returned
      */
    def parseBytes(offset: Int, len: Int): Array[Byte] = {
      if (offset >= data.length || len == 0) {
        Array.empty
      } else {
        val bytes = new Array[Byte](len)
        System.arraycopy(data, offset, bytes, 0, Math.min(data.length - offset, len))
        bytes
      }
    }

    /**
      * Parses 32-bytes word from given input.
      * Uses {@link #parseBytes(byte[], int, int)} method,
      * thus, result will be right-padded with zero bytes if there is not enough bytes in {@code input}
      *
      */
    def parseWord(idx: Int): Array[Byte] = data.parseBytes(32 * idx, 32)

    /**
      * Cast hex encoded value from byte[] to BigInteger
      * null is parsed like byte[0]
      */
    def toBigInt: BigInt = {
      if (data == null || data.length == 0) {
        BigInt(0)
      } else {
        BigInt(1, data)
      }
    }

    def toHex: String = {
      if (data != null) {
        Hex.toHexString(data)
      } else {
        ""
      }
    }
  }

  implicit class LongExtension(value: Long) {
    def toBytes: Array[Byte] = {
      ByteBuffer.allocate(8).putLong(value).array
    }
  }

  implicit class BigIntExtension(value: BigInt) {
    def toBytes: Array[Byte] = {
      val src = value.toByteArray
      val dest = new Array[Byte](32)
      if (src.length != 1 && src(0) == 0) {
        val len = src.length - 1
        System.arraycopy(src, 1, dest, dest.length - len, len);
      } else {
        System.arraycopy(src, 0, dest, dest.length - src.length, src.length);
      }
      dest
    }

  }

}
