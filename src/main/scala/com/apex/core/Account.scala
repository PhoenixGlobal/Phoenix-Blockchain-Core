package com.apex.core

import java.io.{ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.crypto.{Crypto, Ecdsa, FixedNumber, UInt160, UInt256}
import play.api.libs.json.{JsValue, Json, Writes}
import com.apex.common.Serializable

class Account(val pubKeyHash: UInt160,
              val active: Boolean,
              val name: String,
              val balance: FixedNumber,
              val nextNonce: Long,
              val version: Int = 0x01) extends com.apex.common.Serializable {

  def address: String = Ecdsa.PublicKeyHash.toAddress(pubKeyHash.data)

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeInt(version)
    os.write(pubKeyHash)
    os.writeBoolean(active)
    os.writeString(name)
    os.write(balance)
    os.writeLong(nextNonce)
  }
}

object Account {
  def deserialize(is: DataInputStream): Account = {
    import com.apex.common.Serializable._
    val version = is.readInt
    val pubKeyHash = UInt160.deserialize(is)
    val active = is.readBoolean
    val name = is.readString
    val balance = FixedNumber.deserialize(is)
    val nextNonce = is.readLong

    new Account(
      pubKeyHash = pubKeyHash,
      active = active,
      name = name,
      balance = balance,
      nextNonce = nextNonce,
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
        "version" -> o.version
      )
    }
  }
}