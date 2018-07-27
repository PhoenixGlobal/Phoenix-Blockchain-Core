/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: package.scala
 *
 * @author: shan.huang@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex

import java.math.BigInteger

import com.apex.crypto.Ecdsa.PublicKey
import org.bouncycastle.util.encoders.Hex


package object crypto {

  def toHexString(blob: BinaryData) = Hex.toHexString(blob)

  def fromHexString(hex: String): BinaryData = Hex.decode(hex.stripPrefix("0x"))

  implicit def string2binaryData(input: String): BinaryData = BinaryData(fromHexString(input))

  implicit def seq2binaryData(input: Seq[Byte]): BinaryData = BinaryData(input)

  implicit def array2binaryData(input: Array[Byte]): BinaryData = BinaryData(input)

  implicit def binaryData2array(input: BinaryData): Array[Byte] = input.data.toArray

  implicit def binaryData2Seq(input: BinaryData): Seq[Byte] = input.data


}
