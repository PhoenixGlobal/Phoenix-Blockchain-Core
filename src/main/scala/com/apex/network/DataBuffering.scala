package com.apex.network

import java.nio.ByteOrder
import akka.util.ByteString
import com.apex.core.settings.NetworkSettings
import scala.annotation.tailrec


trait DataBuffering {

  def settings: NetworkSettings

  def getPacket(data: ByteString): (List[ByteString], ByteString) = {

    val headerSize = 4

    @tailrec
    def multiPacket(packets: List[ByteString], current: ByteString): (List[ByteString], ByteString) = {
      if (current.length < headerSize) {
        (packets.reverse, current)
      } else {
        val len = current.iterator.getInt(ByteOrder.BIG_ENDIAN)
        if (len > settings.maxPacketSize || len < 0) throw new Exception(s"无效数据包长度: $len")
        if (current.length < len + headerSize) {
          (packets.reverse, current)
        } else {
          val rem = current drop headerSize 
          val (front, back) = rem.splitAt(len) 
          multiPacket(front :: packets, back)
        }
      }
    }
    multiPacket(List[ByteString](), data)
  }
}
