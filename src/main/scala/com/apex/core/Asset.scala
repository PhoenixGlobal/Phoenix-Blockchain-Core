//package com.apex.core
//
//import java.io.{ByteArrayOutputStream, DataInputStream, DataOutputStream}
//
//import com.apex.crypto.Ecdsa.Point
//import com.apex.crypto.{Crypto, Fixed8, UInt160, UInt256}
//
//class Asset(val assetType: AssetType.Value,
//            val issuer: UInt160,
//            val name: String,
//            val amount: Fixed8,
//            val available: Fixed8,
//            val precision: Byte,
//            val fee: Fixed8,
//            val active: Boolean,
//            val version: Int = 0x01,
//            override protected var _id: UInt256 = null)
//  extends Identifier[UInt256] {
//
//  override def serialize(os: DataOutputStream): Unit = {
//    serializeExcludeId(os)
//    os.write(id)
//  }
//
//  override protected def genId(): UInt256 = {
//    val bs = new ByteArrayOutputStream()
//    val os = new DataOutputStream(bs)
//    serializeExcludeId(os)
//    UInt256.fromBytes(Crypto.hash256(bs.toByteArray))
//  }
//
//  private def serializeExcludeId(os: DataOutputStream): Unit = {
//    import com.apex.common.Serializable._
//    os.writeInt(version)
//    os.writeByte(assetType.toByte)
//    os.write(issuer)
//    os.writeString(name)
//    os.write(amount)
//    os.write(available)
//    os.writeByte(precision)
//    os.write(fee)
//    os.writeBoolean(active)
//  }
//}
//
//object Asset {
//  def deserialize(is: DataInputStream): Asset = {
//    import com.apex.common.Serializable._
//    val version = is.readInt
//    new Asset(
//      assetType = AssetType(is.readByte),
//      issuer = is.readObj(UInt160.deserialize),
//      name = is.readString,
//      amount = is.readObj(Fixed8.deserialize),
//      available = is.readObj(Fixed8.deserialize),
//      precision = is.readByte,
//      fee = is.readObj(Fixed8.deserialize),
//      active = is.readBoolean,
//      version = version,
//      is.readObj(UInt256.deserialize)
//    )
//  }
//}
