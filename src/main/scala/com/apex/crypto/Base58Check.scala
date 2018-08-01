/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Base58Check.scala
 *
 * @author: shan.huang@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.crypto

import scala.util.Try

object Base58Check {

   def encode(prefix: Byte, data: Array[Byte]): String = {
      encode(Array(prefix), data)
   }

   def encode(prefix: Array[Byte], data: Array[Byte]): String = {
      val prefixAndData = prefix ++ data
      encode(prefixAndData)
   }
   
   def encode(all: Array[Byte]): String = {
      Base58.encode(all ++ checksum(all))
   }

   def decode(input: String): Try[Array[Byte]] = Try {
      require(input.length > 0, "Empty input for Base58Check.decode")
      val raw = Base58.decode(input).getOrElse(Array[Byte]())
      require(raw.length > 4, "Error input for Base58Check.decode")
      val versionAndHash = raw.dropRight(4)
      val checksum = raw.takeRight(4)
      require(checksum.sameElements(Base58Check.checksum(versionAndHash)), "invalid Base58Check data checksum")

      versionAndHash
   }

   private def checksum(input: Array[Byte]): Array[Byte] = {
      Crypto.hash256(input).take(4)
   }

}
