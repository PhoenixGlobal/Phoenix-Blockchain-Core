package com.apex.core

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.crypto.Ecdsa.PrivateKey
import com.apex.crypto.{BinaryData, Crypto, UInt256}
import play.api.libs.json.{JsValue, Json, Writes}

class BlockHeader(val index: Int,
                  val timeStamp: Long,
                  val merkleRoot: UInt256,
                  val prevBlock: UInt256,
                  val producer: BinaryData,   // 33 bytes pub key
                  var producerSig: BinaryData,
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

  private def serializeForSign(os: DataOutputStream) = {
    import com.apex.common.Serializable._
    os.writeInt(version)
    os.writeInt(index)
    os.writeLong(timeStamp)
    os.write(merkleRoot)
    os.write(prevBlock)
    os.writeByteArray(producer)
    // skip the producerSig
  }

  private def serializeExcludeId(os: DataOutputStream) = {
    import com.apex.common.Serializable._
    serializeForSign(os)
    os.writeByteArray(producerSig)
  }

  private def getSigTargetData(): Array[Byte] = {
    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    serializeForSign(os)
    bs.toByteArray
  }

  def sign(privKey: PrivateKey) = {
    producerSig = Crypto.sign(getSigTargetData, privKey)
  }

  def verifySig(): Boolean = {
    Crypto.verifySignature(getSigTargetData, producerSig, producer)
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
      "producer" -> o.producer.toString,
      "producerSig" -> o.producerSig.toString,
      "version" -> o.version
    )
  }

  def build(index: Int, timeStamp: Long, merkleRoot: UInt256, prevBlock: UInt256,
    producer: BinaryData, privateKey: PrivateKey): BlockHeader = {
    assert(producer.length == 33)
    val header = new BlockHeader(index, timeStamp, merkleRoot, prevBlock, producer, BinaryData.empty)
    header.sign(privateKey)
    header
  }

  def deserialize(is: DataInputStream): BlockHeader = {
    import com.apex.common.Serializable._
    val version = is.readInt
    new BlockHeader(
      index = is.readInt,
      timeStamp = is.readLong,
      merkleRoot = is.readObj(UInt256.deserialize),
      prevBlock = is.readObj(UInt256.deserialize),
      producer = is.readByteArray,
      producerSig = is.readByteArray,
      version = version,
      is.readObj(UInt256.deserialize)
    )
  }

  def fromBytes(data: Array[Byte]): BlockHeader = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }
}
