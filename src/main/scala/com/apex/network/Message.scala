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

import java.net.{InetAddress, InetSocketAddress}

import com.apex.core.Block

object MessageType extends Enumeration {
  val Version = Value(0)
  val BlockProduced = Value(1)
  val GetBlock = Value(2)
  val Block = Value(3)
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

case class GetBlockMessage(height: Int) extends Message(MessageType.GetBlock) {
  override def pack(): MessagePack = {
    MessagePack(messageType, BigInt(height).toByteArray)
  }
}

case class BlockMessage(block: Block) extends Message(MessageType.Block) {
  override def pack(): MessagePack = {
    MessagePack(messageType, block.toBytes)
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
      case MessageType.GetBlock => {
        GetBlockMessage(BigInt(data).toInt)
      }
      case MessageType.Block => {
        BlockMessage(Block.fromBytes(data))
      }
    }
  }
}

