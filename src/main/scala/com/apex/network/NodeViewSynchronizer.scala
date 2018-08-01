package com.apex.network


import java.net.InetSocketAddress
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.apex.common.ApexLogging
import com.apex.network.message.BasicMsgDataTypes._
import com.apex.network.message.{InvSpec, RequestModifierSpec, _}
import com.apex.core.settings.NetworkSettings
import com.apex.network.message.StructureMessage
import com.apex.core.utils.{ApexEncoding, NetworkTimeProvider}
import com.apex.core.{_}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps
import com.apex.network.message.{GetPeersSpec, Message, PeersSpec}
import com.apex.network.message.MessageSpec

/**
 * 节点之间消息处理
 */
class NodeViewSynchronizer(networkControllerRef: ActorRef,
                         viewHolderRef: ActorRef,
                         networkSettings: NetworkSettings,
                         timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext) extends Actor
  with ApexLogging with ApexEncoding {

  import NodeViewSynchronizer.ReceivableMessages._
  import com.apex.core.NodeViewHolderRef.ReceivableMessages.CompareViews
  import com.apex.network.NetworkController.ReceivableMessages.{RegisterMessagesHandler, SendToNetwork}
  import com.apex.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer

  protected val invSpec = new InvSpec(networkSettings.maxInvObjects)
  protected val requestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)


  override def preStart(): Unit = {
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, ModifiersSpec)
    networkControllerRef ! RegisterMessagesHandler(messageSpecs, self)

    context.system.eventStream.subscribe(self, classOf[HandshakedPeer])
    context.system.eventStream.subscribe(self, classOf[DisconnectedPeer])

    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])

  }

  protected def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit = {
    val msg = Message(invSpec, Right(m.modifierTypeId -> Seq(m.id)), None)
    networkControllerRef ! SendToNetwork(msg, Broadcast)
  }

   protected def broadcastModifierInvMessageInfo(message: Message[_]): Unit = {
    networkControllerRef ! SendToNetwork(message, Broadcast)
  }
   
  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  protected def viewHolderEvents: Receive = {
//    case SuccessfulStructure(tx) =>
//      broadcastModifierInv(tx)
    
    case SuccessfulStructureMessageInfo(msg) =>
      broadcastModifierInvMessageInfo(msg)
      
//    case FailedStructure(tx, throwable) =>

  }

  protected def peerManagerEvents: Receive = {
    case HandshakedPeer(remote) =>
//      statusTracker.updateStatus(remote, Unknown)

    case DisconnectedPeer(remote) =>
//      statusTracker.clearStatus(remote)
  }


  protected def processInv: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == InvSpec.MessageCode =>
        viewHolderRef ! CompareViews(remote, invData._1, invData._2)
  }

  protected def modifiersReq: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == RequestModifierSpec.MessageCode =>
       log.info("ResponseFromLocal(remote, invData._1, objs)="+remote, invData._1)
  }


  protected def requestFromLocal: Receive = {
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      if (modifierIds.nonEmpty) {
        val msg = Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
        peer.handlerRef ! msg
      }
  }


  protected def penalizeNonDeliveringPeer(peer: ConnectedPeer): Unit = {
    // networkControllerRef ! Blacklist(peer)
  }

  protected def penalizeSpammingPeer(peer: ConnectedPeer): Unit = {
    // networkControllerRef ! Blacklist(peer)
  }

  protected def responseFromLocal: Receive = {
    case ResponseFromLocal(peer, _, modifiers: Seq[NodeViewModifier]) =>
      if (modifiers.nonEmpty) {
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val modType = modifiers.head.modifierTypeId
        val m = modType -> modifiers.map(m => m.id -> m.bytes).toMap
        val msg = Message(ModifiersSpec, Right(m), None)
        peer.handlerRef ! msg
      }
  }

  override def receive: Receive =
      processInv orElse
      modifiersReq orElse
      requestFromLocal orElse
      responseFromLocal orElse
      viewHolderEvents orElse
      peerManagerEvents orElse{
      case a: Any => log.error("Strange input: " + a)
    }
}

object NodeViewSynchronizer {
  import com.apex.network.{ConnectedPeer,ConnectionType}

  object ReceivableMessages {

    case class RequestFromLocal(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    case class ResponseFromLocal[M <: NodeViewModifier](source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

    trait PeerManagerEvent

    case class HandshakedPeer(remote: ConnectedPeer) extends PeerManagerEvent

    case class DisconnectedPeer(remote: InetSocketAddress) extends PeerManagerEvent

    trait NodeViewHolderEvent

    trait ModificationOutcome extends NodeViewHolderEvent

//    case class FailedStructure[TX <: StructureMessage](structure: TX, error: Throwable) extends ModificationOutcome
//
//    case class SuccessfulStructure[TX <: StructureMessage](structure: TX) extends ModificationOutcome
    
    case class SuccessfulStructureMessageInfo(message: Message[_]) extends ModificationOutcome

  }

}

object NodeViewSynchronizerRef {
  def props(networkControllerRef: ActorRef,
                           viewHolderRef: ActorRef,
                           networkSettings: NetworkSettings,
                           timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext): Props =
    Props(new NodeViewSynchronizer(networkControllerRef,viewHolderRef,
      networkSettings, timeProvider))

  def apply(networkControllerRef: ActorRef,
                           viewHolderRef: ActorRef,
                           networkSettings: NetworkSettings,
                           timeProvider: NetworkTimeProvider)
                          (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(networkControllerRef,viewHolderRef, networkSettings, timeProvider))

  def apply(name: String,
                           networkControllerRef: ActorRef,
                           viewHolderRef: ActorRef,
                           networkSettings: NetworkSettings,
                           timeProvider: NetworkTimeProvider)
                          (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(networkControllerRef,viewHolderRef, networkSettings, timeProvider), name)
}