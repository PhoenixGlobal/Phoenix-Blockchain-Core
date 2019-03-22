package com.apex.core

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.{MerkleTree, UInt160, UInt256}
import play.api.libs.json.{JsValue, Json, Writes}

import scala.util.Try

class Block(val header: BlockHeader,
            val transactions: Seq[Transaction]) extends Serializable {
  private val txMp: Map[UInt256, Transaction] = transactions.map(tx => tx.id -> tx).toMap

  def producer(): UInt160 = header.producer

  def id(): UInt256 = {
    header.id
  }

  def shortId(): String = {
    header.shortId()
  }

  def height(): Long = {
    header.index
  }

  def prev(): UInt256 = {
    header.prevBlock
  }

  def timeStamp(): Long = {
    header.timeStamp
  }

  def merkleRoot(): UInt256 = {
    MerkleTree.root(transactions.map(_.id))
  }

  def getTransaction(id: UInt256): Option[Transaction] = {
    txMp.get(id)
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

  implicit val deserializer: (DataInputStream) => Block = deserialize

  def build(header: BlockHeader, txs: Seq[Transaction]): Block = {
    new Block(header, txs)
  }

  def deserialize(is: DataInputStream): Block = {
    import com.apex.common.Serializable._
    val header = is.readObj(BlockHeader.deserialize)
    val txs = is.readSeq(Transaction.deserialize)
    new Block(header, txs)
  }

  def fromBytes(data: Array[Byte]): Block = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }
}