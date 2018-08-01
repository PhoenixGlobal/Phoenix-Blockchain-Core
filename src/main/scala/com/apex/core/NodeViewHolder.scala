package com.apex.core

import akka.actor.Actor
import com.apex.common.ApexLogging
import com.apex.network.ConnectedPeer
import com.apex.network.NodeViewSynchronizer.ReceivableMessages.NodeViewHolderEvent
import com.apex.core.serialization.Serializer
import com.apex.core.settings.ApexSettings
import com.apex.core.utils.ApexEncoding
import com.apex.exceptions.RecoverableModifierError

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import com.apex.network.message.StructureMessage
import com.apex.core.utils.NetworkTimeProvider
import akka.actor.Props
import com.apex.hybrid.HybridSettings
import akka.actor.ActorSystem
import akka.actor.ActorRef


class NodeViewHolder
  extends Actor with ApexLogging with ApexEncoding {

  import NodeViewHolderRef.ReceivableMessages._
  import com.apex.network.NodeViewSynchronizer.ReceivableMessages._



  def receive = {
    case CompareViews(peer, modifierTypeId, modifierIds) =>
//      val ids = modifierTypeId match {
//        case typeId: ModifierTypeId if typeId == com.apex.core.structure.StructureMessage.ModifierTypeId =>
//          memoryPool().notIn(modifierIds)
//        case _ =>
//          modifierIds.filterNot(mid => history().contains(mid) || modifiersCache.contains(key(mid)))
//      }
      sender() ! RequestFromLocal(peer, modifierTypeId, /*ids*/modifierIds)
  }
}


object NodeViewHolderRef {
  def props(): Props =
    Props(new NodeViewHolder())

  def apply()(implicit system: ActorSystem): ActorRef = system.actorOf(props())

  def apply(name: String)(implicit system: ActorSystem): ActorRef = system.actorOf(props(), name)
  
  object ReceivableMessages {
    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])
  }
}
