/*
 *
 *
 *
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Message.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-8-17 下午2:59@version: 1.0
 */

package com.apex.network

import java.io.{DataInputStream, DataOutputStream, ByteArrayInputStream}
import java.net.{InetAddress, InetSocketAddress}

import com.apex.core.Block
import com.apex.crypto.UInt256

object MessageType extends Enumeration {
  val Version = Value(0)
  val BlockProduced = Value(1)
  val GetBlocks = Value(2)
  val Block = Value(3)
  val Blocks = Value(4)
  val Inventory = Value(5)
  val Getdata = Value(6)
}

trait PackMessage {
  def pack(): MessagePack
}

case class MessagePack(messageType: MessageType.Value, data: Array[Byte], address: Option[InetSocketAddress] = None)

abstract class Message(val messageType: MessageType.Value) extends PackMessage

case class VersionMessage(height: Int) extends Message(MessageType.Version) {
  override def pack(): MessagePack = {
    MessagePack(messageType, BigInt(height).toByteArray)
  }
}

case class GetBlocksMessage(blockHashs: GetBlocksPayload) extends Message(MessageType.GetBlocks) {
  override def pack(): MessagePack = {
    MessagePack(messageType, blockHashs.toBytes)
  }
}

case class BlockMessage(block: Block) extends Message(MessageType.Block) {
  override def pack(): MessagePack = {
    MessagePack(messageType, block.toBytes)
  }
}

case class BlocksMessage(blocks: BlocksPayload) extends Message(MessageType.Blocks) {
  override def pack(): MessagePack = {
    MessagePack(messageType, blocks.toBytes)
  }
}

case class InventoryMessage(inv: InventoryPayload) extends Message(MessageType.Inventory) {
  override def pack(): MessagePack = {
    MessagePack(messageType, inv.toBytes)
  }
}

case class GetDataMessage(inv: InventoryPayload) extends Message(MessageType.Getdata) {
  override def pack(): MessagePack = {
    MessagePack(messageType, inv.toBytes)
  }
}

object InventoryType extends Enumeration {
  val Block = Value(0x00)
  val Tx = Value(0x01)

  implicit class Extension(val value: InventoryType.Value) {
    def toByte: Byte = value.id.toByte
  }
}

class InventoryPayload(val invType: InventoryType.Value,
                val hashs: Seq[UInt256]) extends com.apex.common.Serializable {
  def serialize(os: DataOutputStream) = {
    import com.apex.common.Serializable._
    os.writeByte(invType.toByte)
    os.writeSeq(hashs)
  }
}

object InventoryPayload {
  def deserialize(is: DataInputStream): InventoryPayload = {
    import com.apex.common.Serializable._
    val invType = InventoryType(is.readByte())
    val hashs = is.readSeq(UInt256.deserialize)
    new InventoryPayload(invType, hashs)
  }

  def fromBytes(data: Array[Byte]): InventoryPayload = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }
}

class BlocksPayload(val blocks: Seq[Block]) extends com.apex.common.Serializable {
  def serialize(os: DataOutputStream) = {
    import com.apex.common.Serializable._
    os.writeSeq(blocks)
  }
}

object BlocksPayload {
  def deserialize(is: DataInputStream): BlocksPayload = {
    import com.apex.common.Serializable._
    val blocks = is.readSeq(Block.deserialize)
    new BlocksPayload(blocks)
  }
  def fromBytes(data: Array[Byte]): BlocksPayload = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }
}

class GetBlocksPayload(val hashStart: Seq[UInt256],
                       val hashStop: UInt256) extends com.apex.common.Serializable {
  def serialize(os: DataOutputStream) = {
    import com.apex.common.Serializable._
    os.writeSeq(hashStart)
    os.write(hashStop)
  }
}

object GetBlocksPayload {
  def deserialize(is: DataInputStream): GetBlocksPayload = {
    import com.apex.common.Serializable._
    val hashStart = is.readSeq(UInt256.deserialize)
    val hashStop = is.readObj(UInt256.deserialize)
    new GetBlocksPayload(hashStart, hashStop)
  }
  def fromBytes(data: Array[Byte]): GetBlocksPayload = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }
}

object MessagePack {
  def fromBytes(bytes: Array[Byte], addr: InetSocketAddress = null): Message = {
    val messageType = MessageType(bytes(0))
    val data = bytes.drop(1)
    messageType match {
      case MessageType.Version => {
        VersionMessage(BigInt(data).toInt)
      }
      case MessageType.GetBlocks => {
        GetBlocksMessage(GetBlocksPayload.fromBytes(data))
      }
      case MessageType.Block => {
        BlockMessage(Block.fromBytes(data))
      }
      case MessageType.Blocks => {
        BlocksMessage(BlocksPayload.fromBytes(data))
      }
      case MessageType.Inventory => {
        InventoryMessage(InventoryPayload.fromBytes(data))
      }
      case MessageType.Getdata => {
        GetDataMessage(InventoryPayload.fromBytes(data))
      }
    }
  }
}

