package com.apex.hybrid

import com.apex.core.utils.ApexEncoding
import akka.actor.{Actor,ActorRef, ActorSystem, Props}
import com.apex.core.PersistentNodeViewModifier
import com.apex.common.ApexLogging
import com.apex.core.network.NodeViewSynchronizer.ReceivableMessages.SuccessfulStructureStr
import com.apex.network.message.Message

class BroadcastMessage
  extends Actor with ApexLogging{
  
  import BroadcastMessage.ReceivableMessages._
  import BroadcastMessage._
  def receive = {
    case LocallyGeneratedStructureTest(msg) =>
      txModify(msg)
    case a: Any => log.error("Strange input: " + a)
  }

  //对外广播交易
  protected def txModify(message: Message[_]): Unit = {
      context.system.eventStream.publish(SuccessfulStructureStr(message))
  }
}

object BroadcastMessage {
  object ReceivableMessages {
    case class LocallyGeneratedStructureTest(message: Message[_])
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