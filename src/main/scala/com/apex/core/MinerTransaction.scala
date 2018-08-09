package com.apex.core

import java.io.{DataInputStream, DataOutputStream}

import com.apex.crypto.UInt256
import play.api.libs.json.{JsValue, Json, Writes}

class MinerTransaction(outputs: Seq[TransactionOutput], version: Int = 0x01, override protected var _id: UInt256 = null)
  extends Transaction(TransactionType.Miner, Seq.empty, outputs, version) {

  override protected def serializeExtraData(os: DataOutputStream): Unit = {
  }
}

object MinerTransaction {
  implicit val transactionWrites = new Writes[MinerTransaction] {
    override def writes(o: MinerTransaction): JsValue = Json.obj(
      "id" -> o.id.toString,
      "type" -> o.txType.toString,
      "inputs" -> o.inputs,
      "outputs" -> o.outputs,
      "version" -> o.version
    )
  }

  def deserialize(is: DataInputStream): MinerTransaction = {
    import com.apex.common.Serializable._
    val version = is.readInt
    val _ = is.readSeq(TransactionInput.deserialize)
    val outputs = is.readSeq(TransactionOutput.deserialize)
    val id = is.readObj(UInt256.deserialize)
    new MinerTransaction(outputs, version, id)
  }
}