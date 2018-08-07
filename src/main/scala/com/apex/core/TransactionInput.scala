package com.apex.core

import java.io.{DataInputStream, DataOutputStream}

import com.apex.common.{Serializable}
import com.apex.crypto.{UInt256, BinaryData}
import play.api.libs.json.{JsValue, Json, Writes}

case class TransactionInput(val txId: UInt256,
                            val index: Int,
                            var signatureScript: BinaryData) extends Serializable {
  override def hashCode(): Int = {
    //signatureScript ?
    txId.hashCode() + index.hashCode()
  }

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.write(txId)
    os.writeInt(index)
    os.writeByteArray(signatureScript)
  }

  def serializeForSign(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.write(txId)
    os.writeInt(index)
    // exclude the signatureScript
    // previousOutputScript and sighashType ... ?
  }
}

object TransactionInput {
  implicit val transactionInputWrites = new Writes[TransactionInput] {
    override def writes(o: TransactionInput): JsValue = {
      Json.obj(
        "txId" -> o.txId.toString,
        "index" -> o.index,
        "signatureScript" -> o.signatureScript.toString
      )
    }
  }

  def deserialize(is: DataInputStream): TransactionInput = {
    import com.apex.common.Serializable._
    return new TransactionInput(
      txId = is.readObj(UInt256.deserialize),
      index = is.readInt,
      signatureScript = is.readByteArray
    )
  }
}
