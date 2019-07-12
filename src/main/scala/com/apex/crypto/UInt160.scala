/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: UInt160.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.crypto

import java.io.DataInputStream
import org.bouncycastle.util.encoders.Hex
import play.api.libs.json.{JsValue, Json, Writes}

class UInt160(data: Array[Byte]) extends UIntBase(UInt160.Size, data) with Ordered[UInt160] {

  override def compare(that: UInt160): Int = UIntBase.compare(this, that)

  def address: String = Base58Check.encode(UInt160.addressPrefixBin, data)

  def neoAddress: String = Base58Check.encode(UInt160.addressPrefixBinNeo, data)

  def shortAddr: String = address.substring(0, 8)
}

object UInt160 {
  final val Size: Int = 20
  final val Zero: UInt160 = UInt160.fromBytes(Array.fill(Size)(0.toByte))

  implicit val deserializer: DataInputStream => UInt160 = deserialize

  private val addressPrefixString = "AP"
  private val addressPrefixBin = BinaryData("0548") // "0548" is for the "AP" prefix

  private val addressPrefixBinNeo = BinaryData("17")

  def fromBytes(bytes: Array[Byte]): UInt160 = {
    require(bytes.length == 20)
    new UInt160(bytes)
  }

  def parse(str: String): Option[UInt160] = {
    if (str == null || str.isEmpty) {
      None
    } else {
      val s = if (str.startsWith("0x")) str.substring(2) else str
      if (s.length != 40) {
        None
      } else {
        Some(UInt160.fromBytes(Hex.decode(s)))
      }
    }
  }

  def fromAddress(address: String): Option[UInt160] = {
    var publicKeyHash: Option[UInt160] = None
    if (address.startsWith(addressPrefixString) && address.length == 35) {
      val decode = Base58Check.decode(address).getOrElse(Array[Byte]())
      // 2 bytes prefix + 20 bytes data (+ 4 bytes checksum)
      if (decode.length == 22 && BinaryData(decode.slice(0, 2)) == addressPrefixBin)
        publicKeyHash = Some(UInt160.fromBytes(decode.slice(2, 22)))
    }
    publicKeyHash
  }

  def fromNeoAddress(address: String): Option[UInt160] = {
    var publicKeyHash: Option[UInt160] = None
    val decode = Base58Check.decode(address).getOrElse(Array[Byte]())
    // 1 bytes prefix + 20 bytes data (+ 4 bytes checksum)
    if (decode.length == 21 && BinaryData(decode.slice(0, 1)) == addressPrefixBinNeo)
      publicKeyHash = Some(UInt160.fromBytes(decode.slice(1, 21)))
    publicKeyHash
  }

  def deserialize(is: DataInputStream): UInt160 = {
    val data = Array.fill(UInt160.Size)(0.toByte)
    is.read(data, 0, UInt160.Size)
    UInt160.fromBytes(data)
  }

  implicit val uint160Writes = new Writes[UInt160] {
    override def writes(o: UInt160): JsValue = {
      Json.obj(
        "address" -> o.address
      )
    }
  }

}