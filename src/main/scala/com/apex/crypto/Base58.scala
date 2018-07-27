/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Base58.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.crypto

import scala.util.Try

object Base58 {

  private val Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  private val Base = BigInt(58)

  def encode(input: Array[Byte]): String = {
    var bi = BigInt(1, input)
    val s = new StringBuilder()
    if (bi > 0) {
      while (bi >= Base) {
        val mod = bi.mod(Base)
        s.insert(0, Alphabet.charAt(mod.intValue()))
        bi = (bi - mod) / Base
      }
      s.insert(0, Alphabet.charAt(bi.intValue()))
    }

    input.takeWhile(_ == 0).foldLeft(s) { case (ss, _) =>
      ss.insert(0, Alphabet.charAt(0))
    }.toString()
  }

  def decode(input: String): Try[Array[Byte]] = Try {
    require(input.length > 0, "Empty input for Base58.decode")

    val decoded = decodeToBigInteger(input)

    val bytes: Array[Byte] = if (decoded == BigInt(0)) Array.empty else decoded.toByteArray

    val stripSignByte = bytes.length > 1 && bytes.head == 0 && bytes(1) < 0
    val stripSignBytePos = if (stripSignByte) 1 else 0

    val leadingZeros = input.takeWhile(_ == Alphabet.charAt(0)).length

    val tmp = new Array[Byte](bytes.length - stripSignBytePos + leadingZeros)
    System.arraycopy(bytes, stripSignBytePos, tmp, leadingZeros, tmp.length - leadingZeros)
    tmp
  }

  private def decodeToBigInteger(input: String): BigInt =

    input.foldRight((BigInt(0), input.length - 1)) { case (ch, (bi, i)) =>
      val alphaIndex = Alphabet.indexOf(ch)
        .ensuring(_ != -1, "wrong char")
      (bi + BigInt(alphaIndex) * Base.pow(input.length - 1 - i), i - 1)
    }._1
}
