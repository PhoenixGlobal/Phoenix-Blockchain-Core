package com.apex.core

import java.io.{ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.crypto.{Crypto, Ecdsa, Fixed8, UInt160, UInt256}
import play.api.libs.json.{JsValue, Json, Writes}
import com.apex.common.Serializable

class Account(val pubKeyHash: UInt160,
              val active: Boolean,
              val name: String,
              val balances: Map[UInt256, Fixed8],
              val nextNonce: Long,
              val version: Int = 0x01) extends com.apex.common.Serializable {

  def getBalance(assetID: UInt256): Fixed8 = {
    balances.getOrElse(assetID, Fixed8.Zero)
  }

  def address: String = Ecdsa.PublicKeyHash.toAddress(pubKeyHash.data)

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeInt(version)
    os.write(pubKeyHash)
    os.writeBoolean(active)
    os.writeString(name)
    os.writeMap(balances.filter(_._2 > Fixed8.Zero))
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
    val balances = is.readMap(UInt256.deserialize, Fixed8.deserialize)
    val nextNonce = is.readLong

    new Account(
      pubKeyHash = pubKeyHash,
      active = active,
      name = name,
      balances = balances,
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
        "balances" -> o.balances.map(p => (p._1.toString -> p._2.toString)),
        "nextNonce" -> o.nextNonce,
        "version" -> o.version
      )
    }
  }
}