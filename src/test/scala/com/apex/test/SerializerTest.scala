/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: SerializerTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午1:39@version: 1.0
 */

package com.apex.test

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.crypto.{Crypto, UInt160, UInt256}

class SerializerTest[T <: com.apex.common.Serializable](deserializer: DataInputStream => T, eqComparer: (T, T) => Boolean = (x: T, y: T) => x.equals(y)) {
  def test(value: T) = {
    SerializerTest.test(value, deserializer, eqComparer)
  }
}

object SerializerTest {
  def testHash256(str: String = "test") = UInt256.fromBytes(Crypto.hash256(str.getBytes("UTF-8")))

  def testHash160(str: String = "test") = UInt160.fromBytes(Crypto.hash160(str.getBytes("UTF-8")))

  def test[T <: com.apex.common.Serializable](value: T, deserializer: DataInputStream => T, eqComparer: (T, T) => Boolean) = {
    val bos = new ByteArrayOutputStream
    val os = new DataOutputStream(bos)
    os.write(value)
    val ba = bos.toByteArray
    val bis = new ByteArrayInputStream(ba)
    val is = new DataInputStream(bis)
    import com.apex.common.Serializable._
    assert(eqComparer(is.readObj(deserializer), value))
  }
}
