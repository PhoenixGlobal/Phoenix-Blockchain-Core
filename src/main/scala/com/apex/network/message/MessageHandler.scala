package com.apex.network.message

import java.nio.ByteBuffer
import com.apex.network.ConnectedPeer
import com.apex.core.hash.Blake2b256
import scala.language.existentials
import scala.util.Try

import scala.Left


case class MessageHandler(specs: Seq[MessageSpec[_]]) {

  import Message._

  private val specsMap = Map(specs.map(s => s.messageCode -> s): _*)
    .ensuring(m => m.size == specs.size, "重复消息代码")

  def parseBytes(bytes: ByteBuffer, sourceOpt: Option[ConnectedPeer]): Try[Message[_]] = Try {
    val magic = new Array[Byte](MagicLength)
    bytes.get(magic)

    require(magic.sameElements(Message.MAGIC), "错误的魔法字节" + magic.mkString)

    val msgCode = bytes.get

    val length = bytes.getInt
    require(length >= 0, "数据长度为负数!")

    val msgData: Array[Byte] = if (length > 0) {
      val data = new Array[Byte](length)
      val checksum = new Array[Byte](Message.ChecksumLength)
      bytes.get(checksum)

      bytes.get(data)

      val digest = Blake2b256.hash(data).take(Message.ChecksumLength)

      if(!checksum.sameElements(digest)) throw new Error(s"校验和错误的数据长度 = $length")
      data
    }
    else Array()

    val spec = specsMap.get(msgCode) match {
      case Some(h) => h
      case None => throw new Error(s"没有找到消息处理程序  $msgCode")
    }

    Message(spec, Left(msgData), sourceOpt)
  }
}
