package com.apex.hybrid

import com.apex.core.utils.ApexEncoding
import akka.actor.{Actor,ActorRef, ActorSystem, Props}
import com.apex.common.ApexLogging
import com.apex.network.NodeViewSynchronizer.ReceivableMessages.SuccessfulStructureMessageInfo
import com.apex.network.message.Message

/**
 * 广播消息
 */
class BroadcastMessage
  extends Actor with ApexLogging{
  
  import BroadcastMessage.ReceivableMessages.LocallyGeneratedMessageInfo
  def receive = {
    case LocallyGeneratedMessageInfo(msg) =>
      txModify(msg)
    case a: Any => log.error("异常的输入: " + a)
  }

  //对外广播交易
  protected def txModify(message: Message[_]): Unit = {
      context.system.eventStream.publish(SuccessfulStructureMessageInfo(message))
  }
}

object BroadcastMessage {
  object ReceivableMessages {
    case class LocallyGeneratedMessageInfo(message: Message[_])
  }
}

object BroadcastMessageRef {
  def props(): Props = Props(new BroadcastMessage())

  def apply()
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props())

  def apply(name: String)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(), name)
}