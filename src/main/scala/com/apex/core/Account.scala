package com.apex.core

import java.io.{ByteArrayOutputStream, DataInputStream, DataOutputStream}
import com.apex.common.Serializable.DataInputStreamExtension
import com.apex.crypto.{Crypto, Fixed8, UInt160, UInt256}

class Account(val active: Boolean,
              val balances: Map[UInt256, Fixed8],
              val nextNonce: Long,
              val version: Int = 0x01,
              override protected var _id: UInt160 = null)
  extends Identifier[UInt160] {

  def getBalance(assetID: UInt256): Fixed8 = {
    balances.getOrElse(assetID, Fixed8.Zero)
  }

  override def serialize(os: DataOutputStream): Unit = {
    serializeExcludeId(os)
    os.write(id)
  }

  override def genId(): UInt160 = {
    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    serializeExcludeId(os)
    UInt160.fromBytes(Crypto.hash160(bs.toByteArray))
  }

  private def serializeExcludeId(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeInt(version)
    os.writeBoolean(active)
    os.writeMap(balances.filter(_._2 > Fixed8.Zero))
    os.writeLong(nextNonce)
  }
}

object Account {
  def deserialize(is: DataInputStream): Account = {
    import com.apex.common.Serializable._
    val version = is.readInt
    val active = is.readBoolean
    val balances = is.readMap(UInt256.deserialize, Fixed8.deserialize)
    val nextNonce = is.readLong
    val id = is.readObj(UInt160.deserialize)
    new Account(
      active = active,
      balances = balances,
      nextNonce = nextNonce,
      version = version,
      id
    )
  }
}