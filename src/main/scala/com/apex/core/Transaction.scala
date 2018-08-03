package com.apex.core

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.{Crypto, Fixed8, UInt256}
import play.api.libs.json.{JsValue, Json, Writes}

abstract class Transaction(val txType: TransactionType.Value,
                           val inputs: Seq[TransactionInput],
                           val outputs: Seq[TransactionOutput],
                           val version: Int = 0x01)
  extends Identifier[UInt256] with Serializable {

  //TODO: read settings
  def fee: Fixed8 = Fixed8.Zero

  override def serialize(os: DataOutputStream): Unit = {
    serializeExcludeId(os)
    os.write(id)
  }

  override protected def genId(): UInt256 = {
    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    serializeExcludeId(os)
    UInt256.fromBytes(Crypto.hash256(bs.toByteArray))
  }

  private def serializeExcludeId(os: DataOutputStream) = {
    import com.apex.common.Serializable._
    os.writeByte(txType.toByte)
    os.writeInt(version)
    os.writeSeq(inputs)
    os.writeSeq(outputs)
    serializeExtraData(os)
  }

  protected def serializeExtraData(os: DataOutputStream): Unit

}

object Transaction {
  implicit val transactionWrites = new Writes[Transaction] {
    override def writes(o: Transaction): JsValue = {
      o.txType match {
        case TransactionType.Transfer =>
          TransferTransaction.transactionWrites.writes(o.asInstanceOf)
        case _ => throw new NotImplementedError()
      }
    }
  }

  def deserialize(is: DataInputStream): Transaction = {
    TransactionType(is.readByte) match {
      case TransactionType.Transfer =>
        return TransferTransaction.deserialize(is)
      case _ => throw new NotImplementedError
    }
  }
}