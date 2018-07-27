package com.apex.core

import java.io.{DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.UInt256
import play.api.libs.json.{JsValue, Json, Writes}

case class TransactionInput(val blockId: UInt256,
                            val index: Int) extends Serializable {
  override def hashCode(): Int = {
    blockId.hashCode() + index.hashCode()
  }

  override def serialize(os: DataOutputStream): Unit = {
    os.write(blockId)
    os.writeInt(index)
  }
}

object TransactionInput {
  implicit val transactionInputWrites = new Writes[TransactionInput] {
    override def writes(o: TransactionInput): JsValue = {
      Json.obj(
        "blockId" -> o.blockId.toString,
        "index" -> o.index
      )
    }
  }

  def deserialize(is: DataInputStream): TransactionInput = {
    import com.apex.common.Serializable._
    return new TransactionInput(
      blockId = is.readObj(UInt256.deserialize),
      index = is.readInt
    )
  }
}
