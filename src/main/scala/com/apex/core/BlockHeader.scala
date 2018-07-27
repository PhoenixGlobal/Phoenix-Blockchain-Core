package com.apex.core

import java.io.{ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.crypto.{Crypto, UInt256}
import play.api.libs.json.{JsValue, Json, Writes}

class BlockHeader(val index: Int,
                  val timeStamp: Long,
                  val merkleRoot: UInt256,
                  val prevBlock: UInt256,
                  val version: Int = 0x01,
                  override protected var _id: UInt256 = null) extends Identifier[UInt256] {

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: BlockHeader => id.equals(that.id)
      case _ => false
    }
  }

  override def hashCode(): Int = {
    return id.hashCode()
  }

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
    os.writeInt(version)
    os.writeInt(index)
    os.writeLong(timeStamp)
    os.write(merkleRoot)
    os.write(prevBlock)
  }
}

object BlockHeader {
  implicit val blockHeaderWrites = new Writes[BlockHeader] {
    override def writes(o: BlockHeader): JsValue = Json.obj(
      "id" -> o.id.toString,
      "index" -> o.index,
      "timeStamp" -> o.timeStamp,
      "merkleRoot" -> o.merkleRoot.toString,
      "prevBlock" -> o.prevBlock.toString,
      "version" -> o.version
    )
  }

  def build(index: Int, timeStamp: Long, merkleRoot: UInt256, prevBlock: UInt256): BlockHeader = {
    new BlockHeader(index, timeStamp, merkleRoot, prevBlock)
  }

  def deserialize(is: DataInputStream): BlockHeader = {
    import com.apex.common.Serializable._
    val version = is.readInt
    new BlockHeader(
      index = is.readInt,
      timeStamp = is.readLong,
      merkleRoot = is.readObj(UInt256.deserialize),
      prevBlock = is.readObj(UInt256.deserialize),
      version = version,
      is.readObj(UInt256.deserialize))
  }
}
