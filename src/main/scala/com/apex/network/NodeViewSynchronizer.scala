package com.apex.core.network


import java.net.InetSocketAddress
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.apex.common.ApexLogging
import com.apex.core.consensus.{History, HistoryReader, SyncInfo}
import com.apex.network.message.BasicMsgDataTypes._
import com.apex.network.message.{InvSpec, RequestModifierSpec, _}
import com.apex.core.settings.NetworkSettings
import com.apex.core.structure.state.StateReader
import com.apex.core.structure.wallet.VaultReader
import com.apex.core.structure.{MempoolReader, StructureMessage}
import com.apex.core.utils.{ApexEncoding, NetworkTimeProvider}
import com.apex.core.{PersistentNodeViewModifier, _}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps
import com.apex.network.message.{GetPeersSpec, Message, PeersSpec}
import com.apex.network.message.MessageSpec
import com.apex.network.SendToPeer
import com.apex.network.Broadcast
import com.apex.network.SyncTracker
import com.apex.network.DeliveryTracker
import com.apex.network.{ConnectedPeer,ConnectionType}

class NodeViewSynchronizer[
SI <: SyncInfo,
SIS <: SyncInfoMessageSpec[SI]](networkControllerRef: ActorRef,
                         syncInfoSpec: SIS,
                         networkSettings: NetworkSettings,
                         timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext) extends Actor
  with ApexLogging with ApexEncoding {

  import com.apex.core.consensus.History._
  import NodeViewSynchronizer.ReceivableMessages._
  import com.apex.core.NodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges, ModifiersFromRemote}
  import com.apex.network.NetworkController.ReceivableMessages.{RegisterMessagesHandler, SendToNetwork}
  import com.apex.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer

  protected val deliveryTimeout: FiniteDuration = networkSettings.deliveryTimeout
  protected val maxDeliveryChecks: Int = networkSettings.maxDeliveryChecks
  protected val invSpec = new InvSpec(networkSettings.maxInvObjects)
  protected val requestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)

  protected val deliveryTracker = new DeliveryTracker(context.system, deliveryTimeout, maxDeliveryChecks, self)
  protected val statusTracker = new SyncTracker(self, context, networkSettings, timeProvider)


  override def preStart(): Unit = {
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, ModifiersSpec, syncInfoSpec)
    networkControllerRef ! RegisterMessagesHandler(messageSpecs, self)

    context.system.eventStream.subscribe(self, classOf[HandshakedPeer])
    context.system.eventStream.subscribe(self, classOf[DisconnectedPeer])

    //context.system.eventStream.subscribe(self, classOf[ChangedHistory[HR]])
//    context.system.eventStream.subscribe(self, classOf[ChangedMempool[MR]])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    //viewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)

    statusTracker.scheduleSendSyncInfo()
  }

  //private def readersOpt: Option[(HR, MR)] = historyReaderOpt.flatMap(h => mempoolReaderOpt.map(mp => (h, mp)))

  protected def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit = {
    val msg = Message(invSpec, Right(m.modifierTypeId -> Seq(m.id)), None)
    networkControllerRef ! SendToNetwork(msg, Broadcast)
  }

   protected def broadcastModifierInvStr(message: Message[_]): Unit = {
    networkControllerRef ! SendToNetwork(message, Broadcast)
  }
   
  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  protected def viewHolderEvents: Receive = {
    case SuccessfulStructure(tx) =>
      broadcastModifierInv(tx)
    
    case SuccessfulStructureStr(msg) =>
      broadcastModifierInvStr(msg)
      
    case FailedStructure(tx, throwable) =>

    case SyntacticallySuccessfulModifier(mod) =>
    case SyntacticallyFailedModification(mod, throwable) =>

    case SemanticallySuccessfulModifier(mod) =>
      broadcastModifierInv(mod)

    case SemanticallyFailedModification(mod, throwable) =>

//    case ChangedHistory(reader: HR@unchecked) if reader.isInstanceOf[HR] =>
//      historyReaderOpt = Some(reader)

//    case ChangedMempool(reader: MR@unchecked) if reader.isInstanceOf[MR] =>
//      mempoolReaderOpt = Some(reader)
  }

  protected def peerManagerEvents: Receive = {
    case HandshakedPeer(remote) =>
      statusTracker.updateStatus(remote, Unknown)

    case DisconnectedPeer(remote) =>
      statusTracker.clearStatus(remote)
  }

//  protected def getLocalSyncInfo: Receive = {
//    case SendLocalSyncInfo =>
//      historyReaderOpt.foreach(r => sendSync(statusTracker, r))
//  }

//  protected def sendSync(syncTracker: SyncTracker, history: HR): Unit = {
//    val peers = statusTracker.peersToSyncWith()
//    if (peers.nonEmpty) {
//      networkControllerRef ! SendToNetwork(Message(syncInfoSpec, Right(history.syncInfo), None), SendToPeers(peers))
//    }
//  }

//  protected def processSync: Receive = {
//    case DataFromPeer(spec, syncInfo: SI@unchecked, remote)
//      if spec.messageCode == syncInfoSpec.messageCode =>
//
//      historyReaderOpt match {
//        case Some(historyReader) =>
//          val extensionOpt = historyReader.continuationIds(syncInfo, networkSettings.maxInvObjects)
//          val ext = extensionOpt.getOrElse(Seq())
//          val comparison = historyReader.compare(syncInfo)
//          log.debug(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
//            s"Comparison result is $comparison. Sending extension of length ${ext.length}")
//          log.trace(s"Extension ids: ${idsToString(ext)}")
//
//          if (!(extensionOpt.nonEmpty || comparison != Younger)) {
//            log.warn("Extension is empty while comparison is younger")
//          }
//
//          self ! OtherNodeSyncingStatus(remote, comparison, extensionOpt)
//        case _ =>
//      }
//  }


  def sendExtension(remote: ConnectedPeer,
                    status: HistoryComparisonResult,
                    extOpt: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit = extOpt match {
    case None => log.warn(s"extOpt is empty for: $remote. Its status is: $status.")
    case Some(ext) =>
      ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
        case (mid, mods) =>
          networkControllerRef ! SendToNetwork(Message(invSpec, Right(mid -> mods), None), SendToPeer(remote))
      }
  }

  protected def processSyncStatus: Receive = {
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      statusTracker.updateStatus(remote, status)

      status match {
        case Unknown =>
          //todo: should we ban peer if its status is unknown after getting info from it?
          log.warn("Peer status is still unknown")
        case Nonsense =>
          log.warn("Got nonsense")
        case Younger =>
          sendExtension(remote, status, extOpt)
        case _ => 
      }
  }

//  protected def processInv: Receive = {
//    case DataFromPeer(spec, invData: InvData@unchecked, remote)
//      if spec.messageCode == InvSpec.MessageCode =>
//
//      //TODO can't replace viewHolderRef with a reader because of modifiers cache
//      viewHolderRef ! CompareViews(remote, invData._1, invData._2)
//  }

//  protected def modifiersReq: Receive = {
//    case DataFromPeer(spec, invData: InvData@unchecked, remote)
//      if spec.messageCode == RequestModifierSpec.MessageCode =>
//
//      readersOpt.foreach { readers =>
//        val objs: Seq[NodeViewModifier] = invData._1 match {
//          case typeId: ModifierTypeId if typeId == StructureMessage.ModifierTypeId =>
//            readers._2.getAll(invData._2)
//          case _: ModifierTypeId =>
//            invData._2.flatMap(id => readers._1.modifierById(id))
//        }
//
//        log.debug(s"Requested ${invData._2.length} modifiers ${idsToString(invData)}, " +
//          s"sending ${objs.length} modifiers ${idsToString(invData._1, objs.map(_.id))} ")
//        self ! ResponseFromLocal(remote, invData._1, objs)
//      }
//  }

  protected def modifiersFromRemote: Receive = {
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote)
      if spec.messageCode == ModifiersSpec.messageCode =>

      val typeId = data._1
      val modifiers = data._2

      log.info(s"Got modifiers of type $typeId from remote connected peer: $remote")
      log.trace(s"Received modifier ids ${data._2.keySet.map(encoder.encode).mkString(",")}")

      modifiers.foreach { case (id, _) => deliveryTracker.onReceive(typeId, id, remote) }

      val (spam, fm) = modifiers.partition { case (id, _) => deliveryTracker.isSpam(id) }

      if (spam.nonEmpty) {
        log.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(encoder.encode)}")
        penalizeSpammingPeer(remote)
        val mids = spam.keys.toSeq
        deliveryTracker.deleteSpam(mids)
      }

//      if (fm.nonEmpty) {
//        val mods = fm.values.toSeq
//        viewHolderRef ! ModifiersFromRemote(remote, typeId, mods)
//      }
  }

  protected def requestFromLocal: Receive = {
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>

      if (modifierIds.nonEmpty) {
        val msg = Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
        peer.handlerRef ! msg
      }
      deliveryTracker.expect(peer, modifierTypeId, modifierIds)
  }

  @SuppressWarnings(Array("org.wartremover.warts.JavaSerializable"))
  protected def checkDelivery: Receive = {
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      if (deliveryTracker.peerWhoDelivered(modifierId).contains(peer)) {
        deliveryTracker.delete(modifierId)
      } else {
        log.info(s"Peer $peer has not delivered asked modifier ${encoder.encode(modifierId)} on time")
        penalizeNonDeliveringPeer(peer)
        deliveryTracker.reexpect(peer, modifierTypeId, modifierId)
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
//    getLocalSyncInfo orElse
//      processSync orElse
      processSyncStatus orElse
//      processInv orElse
//      modifiersReq orElse
      requestFromLocal orElse
      responseFromLocal orElse
      modifiersFromRemote orElse
      viewHolderEvents orElse
      peerManagerEvents orElse
      checkDelivery orElse {
      case a: Any => log.error("Strange input: " + a)
    }
}

object NodeViewSynchronizer {
  import com.apex.network.{ConnectedPeer,ConnectionType}
  object Events {

    trait NodeViewSynchronizerEvent

    case object NoBetterNeighbour extends NodeViewSynchronizerEvent

    case object BetterNeighbourAppeared extends NodeViewSynchronizerEvent

  }

  object ReceivableMessages {

    // getLocalSyncInfo messages
    case object SendLocalSyncInfo

    case class RequestFromLocal(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    case class ResponseFromLocal[M <: NodeViewModifier](source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

    case class CheckDelivery(source: ConnectedPeer,
                             modifierTypeId: ModifierTypeId,
                             modifierId: ModifierId)

    case class OtherNodeSyncingStatus[SI <: SyncInfo](remote: ConnectedPeer,
                                                      status: History.HistoryComparisonResult,
                                                      extension: Option[Seq[(ModifierTypeId, ModifierId)]])

    trait PeerManagerEvent

    case class HandshakedPeer(remote: ConnectedPeer) extends PeerManagerEvent

    case class DisconnectedPeer(remote: InetSocketAddress) extends PeerManagerEvent

    trait NodeViewHolderEvent

    trait NodeViewChange extends NodeViewHolderEvent

    case class ChangedHistory[HR <: HistoryReader[_ <: PersistentNodeViewModifier, _ <: SyncInfo]](reader: HR) extends NodeViewChange

    case class ChangedMempool[MR <: MempoolReader[_ <: StructureMessage]](mempool: MR) extends NodeViewChange

    case class ChangedVault[VR <: VaultReader](reader: VR) extends NodeViewChange

    case class ChangedState[SR <: StateReader](reader: SR) extends NodeViewChange


    case object RollbackFailed extends NodeViewHolderEvent

    case class NewOpenSurface(newSurface: Seq[ModifierId]) extends NodeViewHolderEvent

    case class StartingPersistentModifierApplication[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends NodeViewHolderEvent

    trait ModificationOutcome extends NodeViewHolderEvent

    case class FailedStructure[TX <: StructureMessage](structure: TX, error: Throwable) extends ModificationOutcome

    case class SuccessfulStructure[TX <: StructureMessage](structure: TX) extends ModificationOutcome
    
    case class SuccessfulStructureStr(message: Message[_]) extends ModificationOutcome

    case class SyntacticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome

    case class SemanticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome

    case class SyntacticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

    case class SemanticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

  }

}

object NodeViewSynchronizerRef {
  def props[
  SI <: SyncInfo,
  SIS <: SyncInfoMessageSpec[SI]](networkControllerRef: ActorRef,
                           syncInfoSpec: SIS,
                           networkSettings: NetworkSettings,
                           timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext): Props =
    Props(new NodeViewSynchronizer[SI, SIS](networkControllerRef,syncInfoSpec,
      networkSettings, timeProvider))

  def apply[
  SI <: SyncInfo,
  SIS <: SyncInfoMessageSpec[SI]](networkControllerRef: ActorRef,
                           viewHolderRef: ActorRef,
                           syncInfoSpec: SIS,
                           networkSettings: NetworkSettings,
                           timeProvider: NetworkTimeProvider)
                          (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props[SI, SIS](networkControllerRef,syncInfoSpec, networkSettings, timeProvider))

  def apply[
  SI <: SyncInfo,
  SIS <: SyncInfoMessageSpec[SI]](name: String,
                           networkControllerRef: ActorRef,
                           viewHolderRef: ActorRef,
                           syncInfoSpec: SIS,
                           networkSettings: NetworkSettings,
                           timeProvider: NetworkTimeProvider)
                          (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props[SI, SIS](networkControllerRef,syncInfoSpec, networkSettings, timeProvider), name)
}