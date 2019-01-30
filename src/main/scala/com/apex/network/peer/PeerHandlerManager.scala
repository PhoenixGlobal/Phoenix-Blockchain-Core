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

  private lazy val peerDatabase = new PeerDatabaseImpl(Some(settings.peersDB + "/peers.dat"))

  if (peerDatabase.isEmpty()) {
    settings.knownPeers.foreach { address =>
      if (!isSelf(address, None)) {
        val defaultPeerInfo = PeerInfo(timeProvider.time(), Some(settings.nodeName), None)
        peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
      }
    }
  }

  override def preStart: Unit = {
    //30s后，每隔30s与其它peer交换一次peerDatabase
    context.system.scheduler.schedule(30.seconds, 30.seconds) {
      var knowPeerSer = Seq[InetSocketAddressSer]()
      peerDatabase.knownPeers().keys.foreach(address => {
        knowPeerSer = knowPeerSer :+ new InetSocketAddressSer(address.getHostName, address.getPort)
      })

      if (knowPeerSer.size > 0) {
        connectedPeers.values.foreach(connectedPeer => {
          connectedPeer.connectionRef ! PeerInfoMessage(new PeerInfoPayload(knowPeerSer)).pack()
        })
      }
    }
  }

  private def peerListOperations: Receive = {
    case AddOrUpdatePeer(address, peerNameOpt, connTypeOpt) =>
      if (!isSelf(address, None)) {
        val peerInfo = PeerInfo(timeProvider.time(), peerNameOpt, connTypeOpt)
        peerDatabase.addOrUpdateKnownPeer(address, peerInfo)
      }
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
        log.info(s"从黑名单中获得传入连接 $remote")
      } else {
        val peerHandlerRef = sender
        val isIncoming = direction == Incoming
        val isAlreadyConnecting = connectingPeers.contains(remote)
        if (isAlreadyConnecting && !isIncoming) {
          peerHandlerRef ! CloseConnection
        } else {
          if (!isIncoming) {
            log.info(s"远程链接 $remote")
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
        log.info(s"从黑名单中得到握手 ${peer.socketAddress}")
      } else {
        if (peer.direction == Outgoing && isSelf(peer.socketAddress, peer.handshake.declaredAddress)) {
          log.info("send CloseConnection")
          peer.connectionRef ! CloseConnection
        } else {
          if (peer.publicPeer) {
            log.info("add or update peer:" + peer.handshake.declaredAddress.get)
            self ! AddOrUpdatePeer(peer.handshake.declaredAddress.get, Some(peer.handshake.nodeName), Some(peer.direction))
          }

          connectedPeers += peer.socketAddress -> peer
          log.info("更新本节点连接的节点=" + connectedPeers)
          // Once connected, try get the peer's latest block to sync
          Thread.sleep(50) // to avoid peer mess with the "handshakeDone"
          peer.connectionRef ! VersionMessage(0).pack
        }
      }
    case NewBlockProducedNotify(block) => {
      //log.info("broadcasting the new produced Block")
      connectedPeers.values.foreach(peer => {
        //log.debug(s"send block #${block.height} (${block.id}) to ${peer.toString}")
        peer.connectionRef ! BlockMessage(block).pack()
      })
    }
    case msg: InventoryMessage => {
      //log.info("broadcasting InventoryMessage")
      connectedPeers.values.foreach(peer => {
        //log.info(s"send INV to ${peer.toString}")
        peer.connectionRef ! msg.pack()
      })
    }
  }

  private def disconnected: Receive = {
    case Disconnected(remote) =>
      connectedPeers -= remote
      connectingPeers -= remote
  }


  /**
    * 返回一个随机节点
    *
    * @return
    */
  private def randomPeer: Receive = {
    case RandomPeerToConnect() => {
      // log.info("now, select a peer to connected if can found.")
      if (connectedPeers.size <= settings.maxConnections)
        sender() ! randomPeerExcluded()

    }
  }


  //从peerDatabase中，随机选 出一个peer,且不在connectedPeers
  private def randomPeerExcluded(): Option[InetSocketAddress] = {
    val candidates = peerDatabase.knownPeers().keys.filterNot { p =>
      connectedPeers.keys.exists(e => {
        p.getHostName.equals(e.getHostName) /*&& p.getPort == e.getPort */
      }
      )
    }.toSeq

    randomSelectedPeer(candidates)
  }

  private def randomSelectedPeer(peers: Seq[InetSocketAddress]): Option[InetSocketAddress] = {
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  override def receive: Receive = ({
    case AddToBlacklist(peer) =>
      log.info(s"黑名单  $peer")
      peerDatabase.blacklistPeer(peer, timeProvider.time())
    case ReceivedPeers(peers) => {
      peers.knownPeers.foreach(knPeer => {
        val address = new InetSocketAddress(knPeer.address, knPeer.port)
        log.info("收到peer:" + knPeer.toString)
        if (!isSelf(address, None)) {
          val defaultPeerInfo = PeerInfo(timeProvider.time(), Some(settings.nodeName), None)
          peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
        }
      })

    }
  }: Receive) orElse peerListOperations orElse apiInterface orElse randomPeer orElse peerCycle
}

object PeerHandlerManager {

  import com.apex.network.{ConnectedPeer, ConnectionType}

  object ReceivableMessages {

    case class AddToBlacklist(remote: InetSocketAddress)

    case class AddOrUpdatePeer(address: InetSocketAddress, peerName: Option[String], direction: Option[ConnectionType])

    case object GetConnectedPeers

    case object GetAllPeers

    case object GetBlacklistedPeers

    case class DoConnecting(remote: InetSocketAddress, direction: ConnectionType)

    case class Handshaked(peer: ConnectedPeer)

    case class PeerHandler(handlerRef: ActorRef)

    case class Disconnected(remote: InetSocketAddress)

    case class GetBroadCastPeers(data: Array[Byte])

    case class BroadCastPeers(data: Array[Byte], peers: Seq[ConnectedPeer])

    case class RandomPeerToConnect()

    case class ReceivedPeers(peerInfoPayload: PeerInfoPayload)

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
