package com.apex.core

import java.io.{ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.crypto.{Crypto, Ecdsa, FixedNumber, UInt160, UInt256}
import play.api.libs.json.{JsValue, Json, Writes}
import com.apex.common.Serializable
import org.bouncycastle.util.encoders.Hex

class Account(val pubKeyHash: UInt160,
              val active: Boolean,
              val name: String,
              val balance: FixedNumber,
              val nextNonce: Long,
              val codeHash: Array[Byte] = Array.empty,
              val version: Int = 0x01) extends com.apex.common.Serializable {

  //TODO check balance and code
  def isEmpty: Boolean = balance.isZero

  def address: String = Ecdsa.PublicKeyHash.toAddress(pubKeyHash.data)

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeInt(version)
    os.write(pubKeyHash)
    os.writeBoolean(active)
    os.writeString(name)
    os.write(balance)
    os.writeLong(nextNonce)
    os.writeByteArray(codeHash)
  }
}

object Account {
  def deserialize(is: DataInputStream): Account = {
    import com.apex.common.Serializable._
    val version = is.readInt
    val pubKeyHash = is.readObj[UInt160]
    val active = is.readBoolean
    val name = is.readString
    val balance = is.readObj[FixedNumber]
    val nextNonce = is.readLong
    val codeHash = is.readByteArray

    new Account(
      pubKeyHash = pubKeyHash,
      active = active,
      name = name,
      balance = balance,
      nextNonce = nextNonce,
      codeHash = codeHash,
      version = version
    )
  }

  implicit val accountWrites = new Writes[Account] {
    override def writes(o: Account): JsValue = {
      Json.obj(
        "address" -> o.address,
        "active" -> o.active,
        "name" -> o.name,
        "balance" -> o.balance.toString,
        "nextNonce" -> o.nextNonce,
        "codeHash" -> Hex.toHexString(o.codeHash),
        "version" -> o.version
      )
    }
  }
}