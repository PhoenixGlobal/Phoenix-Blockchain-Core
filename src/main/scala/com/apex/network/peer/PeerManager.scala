package com.apex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.apex.common.ApexLogging
import com.apex.network._
import com.apex.core.settings.ApexSettings
import com.apex.core.utils.NetworkTimeProvider

import scala.collection.mutable
import scala.util.Random
import com.apex.network.SendingStrategy
import com.apex.network.Handshake
import com.apex.network.{Incoming,Outgoing}
import com.apex.network.peer.PeerDatabaseImpl

import com.apex.network.peer.PeerInfo

class PeerManager(settings: ApexSettings, timeProvider: NetworkTimeProvider) extends Actor with ApexLogging {

  import PeerManager.ReceivableMessages._
  import com.apex.network.NetworkController.ReceivableMessages.ConnectTo
  import com.apex.core.network.NodeViewSynchronizer.ReceivableMessages.{DisconnectedPeer, HandshakedPeer}
  import com.apex.network.PeerConnectionHandler.ReceivableMessages.{CloseConnection, StartInteraction}
  import com.apex.network.{ConnectedPeer,ConnectionType}
  //握手成功
  private val connectedPeers = mutable.Map[InetSocketAddress, ConnectedPeer]()

  //握手前
  private val connectingPeers = mutable.Set[InetSocketAddress]()

  private lazy val peerDatabase = new PeerDatabaseImpl(Some(settings.dataDir + "/peers.dat"))

  if (peerDatabase.isEmpty()) {
    settings.network.knownPeers.foreach { address =>
      if (!isSelf(address, None)) {
        val defaultPeerInfo = PeerInfo(timeProvider.time(), None)
        peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
      }
    }
  }

  private def randomPeer(): Option[InetSocketAddress] = {
    val peers = peerDatabase.knownPeers().keys.toSeq
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  private def peerListOperations: Receive = {
    case AddOrUpdatePeer(address, peerNameOpt, connTypeOpt) =>
      if (!isSelf(address, None)) {
        val peerInfo = PeerInfo(timeProvider.time(), peerNameOpt, connTypeOpt)
        peerDatabase.addOrUpdateKnownPeer(address, peerInfo)
      }

    case KnownPeers =>
      sender() ! peerDatabase.knownPeers().keys.toSeq

    case RandomPeer =>
      sender() ! randomPeer()

    case RandomPeers(howMany: Int) =>
      sender() ! Random.shuffle(peerDatabase.knownPeers().keys.toSeq).take(howMany)

    case FilterPeers(sendingStrategy: SendingStrategy) =>
      sender() ! sendingStrategy.choose(connectedPeers.values.toSeq)
  }

  private def apiInterface: Receive = {
    case GetConnectedPeers =>
      sender() ! (connectedPeers.values.map(_.handshake).toSeq: Seq[Handshake])

    case GetAllPeers =>
      sender() ! peerDatabase.knownPeers()

    case GetBlacklistedPeers =>
      sender() ! peerDatabase.blacklistedPeers()

//    case Subscribe(listener, events) =>
//      events.foreach { evt =>
//        val current = subscribers.getOrElse(evt, Seq())
//        subscribers.put(evt, current :+ listener)
//      }
  }


  private def isSelf(address: InetSocketAddress, declaredAddress: Option[InetSocketAddress]): Boolean = {
    settings.network.bindAddress == address ||
      settings.network.declaredAddress.exists(da => declaredAddress.contains(da)) ||
      declaredAddress.contains(settings.network.bindAddress) ||
      settings.network.declaredAddress.contains(address)
  }

  private var lastIdUsed = 0

  private def peerCycle: Receive = connecting orElse handshaked orElse disconnected

  private def connecting: Receive = {
    case DoConnecting(remote, direction) =>
      if (peerDatabase.isBlacklisted(remote)) {
        log.info(s"Got incoming connection from blacklisted $remote")
      } else {
        val peerHandlerRef = sender
        val isIncoming = direction == Incoming
        val isAlreadyConnecting = connectingPeers.contains(remote)
        if (isAlreadyConnecting && !isIncoming) {
          log.info(s"Trying to connect twice to $remote, going to drop the duplicate connection")
          peerHandlerRef ! CloseConnection
        } else {
          if (!isIncoming) {
            log.info(s"Connecting to $remote")
            connectingPeers += remote
          }
          peerHandlerRef ! StartInteraction
          lastIdUsed += 1
        }
      }
  }


  private def handshaked: Receive = {
    case Handshaked(peer) =>
      if (peerDatabase.isBlacklisted(peer.socketAddress)) {
        log.info(s"Got handshake from blacklisted ${peer.socketAddress}")
      } else {
        if (peer.direction == Outgoing && isSelf(peer.socketAddress, peer.handshake.declaredAddress)) {
          peer.handlerRef ! CloseConnection
        } else {
          if (peer.publicPeer) {
            self ! AddOrUpdatePeer(peer.socketAddress, Some(peer.handshake.nodeName), Some(peer.direction))
          } else {
            peerDatabase.remove(peer.socketAddress)
          }
          connectedPeers += peer.socketAddress -> peer
          context.system.eventStream.publish(HandshakedPeer(peer))
        }
      }
  }

  private def disconnected: Receive = {
    case Disconnected(remote) =>
      connectedPeers -= remote
      connectingPeers -= remote
      context.system.eventStream.publish(DisconnectedPeer(remote))
  }

  override def receive: Receive = ({
    case CheckPeers =>
      if (connectedPeers.size + connectingPeers.size < settings.network.maxConnections) {
        randomPeer().foreach { address =>
          if (!connectedPeers.exists(_._1 == address) &&
            !connectingPeers.exists(_.getHostName == address.getHostName)) {
            sender() ! ConnectTo(address)
          }
        }
      }

    case AddToBlacklist(peer) =>
      log.info(s"Blacklist peer $peer")
      peerDatabase.blacklistPeer(peer, timeProvider.time())
  }: Receive) orElse peerListOperations orElse apiInterface orElse peerCycle
}

object PeerManager {
  import com.apex.network.{ConnectedPeer,ConnectionType}
  object ReceivableMessages {
    case object CheckPeers
    case class AddToBlacklist(remote: InetSocketAddress)

    case class AddOrUpdatePeer(address: InetSocketAddress, peerName: Option[String], direction: Option[ConnectionType])
    case object KnownPeers
    case object RandomPeer
    case class RandomPeers(hawMany: Int)
    case class FilterPeers(sendingStrategy: SendingStrategy)

    case object GetConnectedPeers
    case object GetAllPeers
    case object GetBlacklistedPeers

    case class DoConnecting(remote: InetSocketAddress, direction: ConnectionType)
    case class Handshaked(peer: ConnectedPeer)
    case class Disconnected(remote: InetSocketAddress)
  }
}

object PeerManagerRef {
  def props(settings: ApexSettings, timeProvider: NetworkTimeProvider): Props =
    Props(new PeerManager(settings, timeProvider))

  def apply(settings: ApexSettings, timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(settings, timeProvider))

  def apply(name: String, settings: ApexSettings, timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(settings, timeProvider), name)
}
