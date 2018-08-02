package com.apex.core

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.{Fixed8, UInt160, UInt256}
import play.api.libs.json.{JsValue, Json, Writes}

case class TransactionOutput(val address: UInt160,
                             val assetId: UInt256,
                             val amount: Fixed8,
                             val version: Int = 0x01) extends Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    os.writeInt(version)
    os.write(address)
    os.write(assetId)
    os.write(amount)
  }
}

object TransactionOutput {
  implicit val transactionOutputWrites = new Writes[TransactionOutput] {
    override def writes(o: TransactionOutput): JsValue = {
      Json.obj(
        "address" -> o.address.toString,
        "assetId" -> o.assetId.toString,
        "amount" -> o.amount.value,
        "version" -> o.version
      )
    }
  }

  def deserialize(is: DataInputStream): TransactionOutput = {
    val version = is.readInt
    new TransactionOutput(
      address = UInt160.deserialize(is),
      assetId = UInt256.deserialize(is),
      amount = Fixed8.deserialize(is),
      version = version)
  }

  def fromBytes(bytes: Array[Byte]): TransactionOutput = {
    val bs = new ByteArrayInputStream(bytes)
    val is = new DataInputStream(bs)
    deserialize(is)
  }
}
