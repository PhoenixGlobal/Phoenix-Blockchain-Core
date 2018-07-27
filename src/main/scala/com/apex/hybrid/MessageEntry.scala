package com.apex.hybrid

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.apex.common.ApexLogging
import com.apex.network.message.Message
import com.apex.hybrid.BroadcastMessage.ReceivableMessages.LocallyGeneratedStructureTest

import scala.concurrent.ExecutionContext

class MessageEntry(viewHolderRef: ActorRef) extends Actor with ApexLogging {

  import MessageEntry.ReceivableMessages._
  
  def receive = {
      case GeneratorInfoTest(msg) =>
        msg match {
          case Message(invSpec, Right(msgBytes), None) =>
            viewHolderRef ! LocallyGeneratedStructureTest(msg)
          case _ =>
        }
  }
}

object MessageEntry {
  object ReceivableMessages {
    case class GeneratorInfoTest(message: Message[_])
  }
}

object MessageEntryRef {
  def props(viewHolderRef: ActorRef)(implicit ec: ExecutionContext): Props =
    Props(new MessageEntry(viewHolderRef))

  def apply(viewHolderRef: ActorRef)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = system.actorOf(props(viewHolderRef))

  def apply(name: String, viewHolderRef: ActorRef)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = system.actorOf(props(viewHolderRef), name)
}
