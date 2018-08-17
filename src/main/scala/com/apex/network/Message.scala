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

object MessageType extends Enumeration {
  val BlockProduced = Value(0)
  val GetBlock = Value(1)
}

case class Message(messageType: MessageType.Value, data: Array[Byte])

