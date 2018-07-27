package com.apex.network

import java.net.InetSocketAddress
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.apex.common.ApexLogging
import com.apex.network.NetworkController.ReceivableMessages.{RegisterMessagesHandler, SendToNetwork}
import com.apex.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
import com.apex.network.message.{GetPeersSpec, Message, PeersSpec}
import com.apex.core.network.peer.PeerManager.ReceivableMessages.{AddOrUpdatePeer, RandomPeers}
import com.apex.core.settings.NetworkSettings
import shapeless.syntax.typeable._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps
import akka.actor.actorRef2Scala
import scala.Right
import scala.collection.Seq

class PeerSynchronizer(val networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings)
                      (implicit ec: ExecutionContext) extends Actor with ApexLogging {

  private implicit val timeout: Timeout = Timeout(settings.syncTimeout.getOrElse(5 seconds))

  override def preStart: Unit = {
    super.preStart()

    networkControllerRef ! RegisterMessagesHandler(Seq(GetPeersSpec, PeersSpec), self)

    val msg = Message[Unit](GetPeersSpec, Right(Unit), None)
    val stn = SendToNetwork(msg, SendToRandom)
    context.system.scheduler.schedule(2.seconds, 10.seconds)(networkControllerRef ! stn)
  }

  override def receive: Receive = {
    case DataFromPeer(spec, peers: Seq[InetSocketAddress]@unchecked, remote)
      if spec.messageCode == PeersSpec.messageCode && peers.cast[Seq[InetSocketAddress]].isDefined =>

      peers.foreach(isa => peerManager ! AddOrUpdatePeer(isa, None, Some(remote.direction)))

    case DataFromPeer(spec, _, remote) if spec.messageCode == GetPeersSpec.messageCode =>

      //外部编号，检查接收
      (peerManager ? RandomPeers(3))
        .mapTo[Seq[InetSocketAddress]]
        .foreach { peers =>
          val msg = Message(PeersSpec, Right(peers), None)
          networkControllerRef ! SendToNetwork(msg, SendToPeers(Seq(remote)))
        }

    case nonsense: Any => log.warn(s"PeerSynchronizer: got something strange $nonsense")
  }
}

object PeerSynchronizerRef {
  def props(networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings)
           (implicit ec: ExecutionContext): Props =
    Props(new PeerSynchronizer(networkControllerRef, peerManager, settings))

  def apply(networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(networkControllerRef, peerManager, settings))

  def apply(name: String, networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(networkControllerRef, peerManager, settings), name)
}