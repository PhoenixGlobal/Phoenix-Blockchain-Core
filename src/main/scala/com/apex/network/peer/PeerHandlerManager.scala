package com.apex.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import akka.util.Timeout
import com.apex.common.ApexLogging
import com.apex.core.NewBlockProducedNotify
import com.apex.settings.NetworkSettings
import com.apex.utils.NetworkTimeProvider
import com.apex.network._

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.Random
import scala.concurrent.duration.DurationInt

class PeerHandlerManager(settings: NetworkSettings, timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext) extends Actor with ApexLogging {

  import PeerHandlerManager.ReceivableMessages._
  import com.apex.network.ConnectedPeer
  import com.apex.network.PeerConnectionManager.ReceivableMessages.{CloseConnection, StartInteraction}

  private implicit val timeout: Timeout = Timeout(5 seconds)

  //握手成功
  private val connectedPeers = mutable.Map[InetSocketAddress, ConnectedPeer]()

  //握手前
  private val connectingPeers = mutable.Set[InetSocketAddress]()

  private lazy val peerDatabase = new PeerDatabaseImpl(settings.peersDB)

  override def preStart: Unit = {
    peerDatabase.loadFromDB()

    settings.seedPeers.foreach { address =>
      if (!isSelf(address, None)) {
        val defaultPeerInfo = new NodeInfo(address.getAddress.getHostAddress, address.getPort, NodeType.SEED)
        peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
      }
    }

    settings.knownPeers.foreach { address =>
      if (!isSelf(address, None)) {
        val defaultPeerInfo = new NodeInfo(address.getAddress.getHostAddress, address.getPort, NodeType.TRUST)
        peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
      }
    }

    //syn up peerInfoMessage with other connected-peers every one minute
    context.system.scheduler.schedule(1.minute, 1.minute) {
      val knowPeerSer = peerDatabase.selectPeersByRandom(settings.peerSyncNumber)
      //TODO don't needs to broadcast , it can be other strategy
      self ! SendToNetWork((PeerInfoMessage(new PeerInfoPayload(knowPeerSer))).pack(), Broadcast)
    }
  }

  override def postStop(): Unit = {
    peerDatabase.flush2DB()
  }

  private def peerListOperations: Receive = {
    case AddOrUpdatePeer(peer, nodeInfo) =>
      peerDatabase.addOrUpdateKnownPeer(peer, nodeInfo)
    case RemovePeer(peerInfo) =>
      peerDatabase.remove(peerInfo)
  }

  private def apiInterface: Receive = {
    case GetConnectedPeers =>
      sender() ! (connectedPeers.values.toSeq)
    case GetBroadCastPeers(data) =>
      sender() ! BroadCastPeers(data, connectedPeers.values.toSeq)
    case GetAllPeers =>
      sender() ! peerDatabase.knownPeers()

    case GetBlacklistedPeers =>
      sender() ! peerDatabase.blacklistedPeers()
  }


  private def isSelf(address: InetSocketAddress, declaredAddress: Option[InetSocketAddress]): Boolean = {
    settings.bindAddress == address ||
      settings.declaredAddress.exists(da => declaredAddress.contains(da)) ||
      declaredAddress.contains(settings.bindAddress) ||
      settings.declaredAddress.contains(address)
  }

  private var lastIdUsed = 0

  private def peerCycle: Receive = connecting orElse handshaked orElse disconnected

  private def connecting: Receive = {
    case DoConnecting(remote, direction) =>
      if (peerDatabase.isBlacklisted(remote)) {
        log.info(s"got connection from blacklist $remote")
      } else {
        val peerHandlerRef = sender
        val isIncoming = direction == Incoming
        val isAlreadyConnecting = connectingPeers.contains(remote)
        if (isAlreadyConnecting && !isIncoming) {
          peerHandlerRef ! CloseConnection
        } else {
          if (!isIncoming) {
            log.info(s"remote connection $remote")
            connectingPeers += remote
          }
          peerHandlerRef ! StartInteraction
          lastIdUsed += 1
        }
      }

    case PeerHandler(handler) => {
      //      handler ! startSync()
      //      log.info("连接成功后获取的PeerConnectionManager链接="+handler)
      //      //获取远程hangler，测发送消息
      //      val msg = Message[Unit](GetPeersSpec, Right(Unit), None)
      //      handler! ms
    }
    //      log.info("连接成功后获取的PeerConnectionManager链接="+handler)
    //      //获取远程hangler，测发送消息
    //      val msg = Message[Unit](GetPeersSpec, Right(Unit), None)
    //      handler! msg

    case MessagePack(a, b, c) =>
      c match {
        case Some(address) => {
          connectedPeers.get(address) match {
            case Some(peer) => peer.connectionRef ! MessagePack(a, b)
            case None => log.error(s"peer($address) not exists")
          }
        }
        case None => {
          connectedPeers.values.foreach(_.connectionRef ! MessagePack(a, b))
        }
      }
  }

  private def handshaked: Receive = {
    case Handshaked(peer) =>
      if (peerDatabase.isBlacklisted(peer.socketAddress)) {
        log.info(s"got handshake from blacklist ${peer.socketAddress}")
      } else {
        if (peer.direction == Outgoing && isSelf(peer.socketAddress, peer.handshake.declaredAddress)) {
          log.info("send CloseConnection")
          peer.connectionRef ! CloseConnection
        } else {
          if (peer.publicPeer) {
            log.info("add or update peer:" + peer.handshake.declaredAddress.get)
            val declaredAddress = peer.handshake.declaredAddress.get
            val nodeInfo = new NodeInfo(declaredAddress.getAddress.getHostAddress, declaredAddress.getPort)
            //val peerInfo = PeerInfo(declaredAddress.getHostName, declaredAddress.getPort, timeProvider.time(), Some(peer.handshake.nodeName), Some(peer.direction))
            self ! AddOrUpdatePeer(declaredAddress, nodeInfo)
          }

          connectedPeers += peer.socketAddress -> peer
          log.info(s"connected ${connectedPeers.size} peers: " + connectedPeers)
          // Once connected, try get the peer's latest block to sync
          Thread.sleep(50) // to avoid peer mess with the "handshakeDone"
          peer.connectionRef ! VersionMessage(0).pack
        }
      }
    case NewBlockProducedNotify(block) => {
      self ! SendToNetWork(BlockMessage(block).pack(), Broadcast)
    }
    case msg: InventoryMessage => {
      self ! SendToNetWork(msg.pack(), Broadcast)
    }
  }

  private def disconnected: Receive = {
    case Disconnected(remote) =>
      connectedPeers -= remote
      connectingPeers -= remote
  }

  override def receive: Receive = ({
    case AddToBlacklist(peer) =>
      log.info(s"add into black-name-list: $peer")
      peerDatabase.addBlacklistPeer(peer, timeProvider.time())
      val connectedPeer = connectedPeers.get(peer);
      if (connectedPeer.isDefined) {
        connectedPeer.get.connectionRef ! CloseConnection //close connection if it is in black name list
      }
    case ReceivedPeers(peers) => {
      if (peerDatabase.peerSize() < settings.peerDatabaseMax) {
        peers.knownPeers.foreach(knPeer => {
          val address = new InetSocketAddress(knPeer.address, knPeer.port)
          // log.info("received:" + knPeer.address + ":" + knPeer.port)
          if (!isSelf(address, None)) {
            peerDatabase.addPeerIfEmpty(address, knPeer)
          }
        })
      }
    }
    case SendToNetWork(messagePack, sendingStrategy) => {
      sendingStrategy.choose(connectedPeers.values.toSeq).foreach(peer => {
        peer.connectionRef ! messagePack
      })
    }
    case get: GetPeers[_] => {
      //first filter already connected peer
      val candidates = peerDatabase.knownPeers().filterNot { entry =>
        connectedPeers.keys.exists(e => {
          e.getAddress.getHostAddress.equals(entry._2.address)
        }
        )
      }
      if (connectedPeers.size <= settings.maxConnections)
        sender() ! get.choose(candidates)
    }

  }: Receive) orElse peerListOperations orElse apiInterface orElse peerCycle
}


object PeerHandlerManager {

  import com.apex.network.{ConnectedPeer, ConnectionType}

  object ReceivableMessages {

    case class AddToBlacklist(remote: InetSocketAddress)

    case class AddOrUpdatePeer(address: InetSocketAddress, nodeInfo: NodeInfo)

    case class RemovePeer(address: InetSocketAddress)

    case object GetConnectedPeers

    case object GetBlacklistedPeers

    case class DoConnecting(remote: InetSocketAddress, direction: ConnectionType)

    case class Handshaked(peer: ConnectedPeer)

    case class PeerHandler(handlerRef: ActorRef)

    case class Disconnected(remote: InetSocketAddress)

    case class GetBroadCastPeers(data: Array[Byte])

    case class BroadCastPeers(data: Array[Byte], peers: Seq[ConnectedPeer])

    case class ReceivedPeers(peerInfoPayload: PeerInfoPayload)

    case class SendToNetWork(messagePack: MessagePack, sendingStrategy: SendingStrategy)

    /**
      * Message to get peers from known peers map filtered by `choose` function
      */
    trait GetPeers[T] {
      def choose(knownPeers: Map[InetSocketAddress, NodeInfo]): T
    }


    case object GetAllPeers extends GetPeers[Map[InetSocketAddress, NodeInfo]] {
      override def choose(knownPeers: Map[InetSocketAddress, NodeInfo]): Map[InetSocketAddress, NodeInfo] = knownPeers
    }

    case class RandomPeerExcluding(excludedPeers: Seq[InetSocketAddress]) extends GetPeers[Option[NodeInfo]] {

      override def choose(knownPeers: Map[InetSocketAddress, NodeInfo]): Option[NodeInfo] = {
        val candidates = knownPeers.values.filterNot { p =>
          excludedPeers.exists(e => {
            e.getAddress.getHostAddress.equals(p.address) && e.getPort == p.port
          })
        }.toSeq
        if (candidates.nonEmpty) Some(candidates(Random.nextInt(candidates.size)))
        else None
      }
    }

    case object RandomNodeInfo extends GetPeers[Option[NodeInfo]] {

      override def choose(knownPeers: Map[InetSocketAddress, NodeInfo]): Option[NodeInfo] = {
        if (knownPeers.nonEmpty) {
          val nodes = knownPeers.values.toSeq
          Some(nodes(Random.nextInt(knownPeers.size)))
        }
        else None
      }
    }

    case object RandomTrustNodeInfo extends GetPeers[Option[NodeInfo]] {

      override def choose(knownPeers: Map[InetSocketAddress, NodeInfo]): Option[NodeInfo] = {
        val candidates = knownPeers.values.filter({ p => p.nodeType != NodeType.UNKNOWN }).toSeq

        if (candidates.nonEmpty) Some(candidates(Random.nextInt(candidates.size)))
        else None
      }


    }

  }

}

object PeerHandlerManagerRef {
  def props(settings: NetworkSettings, timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext): Props =
    Props(new PeerHandlerManager(settings, timeProvider))

  def apply(settings: NetworkSettings, timeProvider: NetworkTimeProvider)
           (implicit system: ActorContext, ec: ExecutionContext
           ): ActorRef = system.actorOf(props(settings, timeProvider))

  def apply(name: String, settings: NetworkSettings, timeProvider: NetworkTimeProvider)
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef = system.actorOf(props(settings, timeProvider), name)
}
