package com.apex.hybrid

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.apex.common.ApexLogging
import com.apex.network.message.Message
import com.apex.hybrid.BroadcastMessage.ReceivableMessages.LocallyGeneratedMessageInfo

import scala.concurrent.ExecutionContext

/**
 * 转发消息
 */
class MessageEntry(viewHolderRef: ActorRef) extends Actor with ApexLogging {

  import MessageEntry.ReceivableMessages.GeneratorInfo
  
  def receive = {
      case GeneratorInfo(msg) =>
        msg match {
          case Message(invSpec, Right(msgBytes), None) =>
            viewHolderRef ! LocallyGeneratedMessageInfo(msg)
          case _ =>
        }
  }
}

object MessageEntry {
  object ReceivableMessages {
    case class GeneratorInfo(message: Message[_])
  }
}

object MessageEntryRef {
  def props(viewHolderRef: ActorRef): Props =
    Props(new MessageEntry(viewHolderRef))

  def apply(viewHolderRef: ActorRef)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(viewHolderRef))

  def apply(name: String, viewHolderRef: ActorRef)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(viewHolderRef), name)
}
