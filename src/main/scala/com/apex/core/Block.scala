package com.apex.core

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.UInt256
import play.api.libs.json.{JsValue, Json, Writes}

import scala.util.Try

class Block(val header: BlockHeader,
            val transactions: Seq[Transaction]) extends Serializable {
  private val txMp: Map[UInt256, Transaction] = transactions.map(tx => tx.id -> tx).toMap

  def id(): UInt256 = {
    header.id
  }

  def getTransaction(id: UInt256): Option[Transaction] = {
    txMp.get(id)
  }

  def getTransaction(index: Int): Option[Transaction] = {
    if (index < 0 || index >= transactions.length) return null
    return Some(transactions(index))
  }

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.write(header)
    os.writeSeq(transactions)
  }
}

object Block {
  implicit val blockWrites = new Writes[Block] {
    override def writes(o: Block): JsValue = Json.obj(
      "header" -> o.header,
      "transactions" -> o.transactions
    )
  }

  def build(header: BlockHeader, txs: Seq[Transaction]): Block = {
    return new Block(header, txs)
  }

  def deserialize(is: DataInputStream): Block = {
    import com.apex.common.Serializable._
    val header = is.readObj(BlockHeader.deserialize)
    val txs = is.readSeq(Transaction.deserialize)
    return new Block(header, txs)
  }

  def fromBytes(data: Array[Byte]): Block = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }
}