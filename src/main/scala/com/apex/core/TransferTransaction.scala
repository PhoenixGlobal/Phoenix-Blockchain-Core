package com.apex.core

import java.io.{DataInputStream, DataOutputStream}

import com.apex.crypto.UInt256
import play.api.libs.json.{JsValue, Json, Writes}

class TransferTransaction(inputs: Seq[TransactionInput],
                          outputs: Seq[TransactionOutput],
                          val note: String,
                          version: Int = 0x01,
                          override protected var _id: UInt256 = null)
  extends Transaction(
    TransactionType.Transfer,
    inputs, outputs, version) {

  override protected def serializeExtraData(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeString(note)
  }
}

object TransferTransaction {
  implicit val transactionWrites = new Writes[TransferTransaction] {
    override def writes(o: TransferTransaction): JsValue = Json.obj(
      "id" -> o.id.toString,
      "type" -> o.txType.toString,
      "inputs" -> o.inputs,
      "outputs" -> o.outputs,
      "note" -> o.note,
      "version" -> o.version
    )
  }

  def deserialize(is: DataInputStream): TransferTransaction = {
    import com.apex.common.Serializable._
    val version = is.readInt
    val inputs = is.readSeq(TransactionInput.deserialize)
    val outputs = is.readSeq(TransactionOutput.deserialize)
    val note = is.readString
    val id = is.readObj(UInt256.deserialize)
    new TransferTransaction(
      inputs,
      outputs,
      note,
      version,
      id
    )
  }
}