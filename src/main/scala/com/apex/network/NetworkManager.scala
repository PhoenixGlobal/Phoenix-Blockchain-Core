package com.apex.network

import java.net.InetSocketAddress

import akka.actor.Status.{Failure, Success}
import akka.actor.{Actor, ActorContext, ActorRef, ActorSystem, Props, actorRef2Scala}
import akka.io.Tcp.SO.KeepAlive
import akka.pattern.ask
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.Timeout
import com.apex.common.ApexLogging
import com.apex.core.ChainInfo
import com.apex.network.peer.PeerHandlerManager.ReceivableMessages.{GetConnectedPeers, PeerHandler, RandomPeerToConnect}
import com.apex.settings.NetworkSettings
import com.apex.utils.NetworkTimeProvider

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.DurationInt


/**
  * 控制所有网络交互
  * 必须是单例
  */
class NetworkManager(settings: NetworkSettings,
                     chainInfo: ChainInfo,
                     timeProvider: NetworkTimeProvider,
                     peerHandlerManager: ActorRef)
                    (implicit ec: ExecutionContext) extends Actor with ApexLogging {

  import NetworkManager.ReceivableMessages._
  import PeerConnectionManager.ReceivableMessages.{CloseConnection, GetHandlerToPeerConnectionManager}
  import com.apex.network.peer.PeerHandlerManager.ReceivableMessages.Disconnected

  private implicit val system: ActorSystem = context.system

  private implicit val timeout: Timeout = Timeout(settings.controllerTimeout.getOrElse(5 seconds))

  private val node: ActorRef = context.parent

  private val localAddress = settings.bindAddress

  //发送给peers的地址
  private val externalSocketAddress: Option[InetSocketAddress] = None

  private val connTimeout = Some(settings.connectionTimeout)

  private val tcpManager = IO(Tcp)

  //绑定来侦听传入的连接
  tcpManager ! Bind(self, localAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  private def bindingLogic: Receive = {
    case Bound(_) =>
      log.info("成功绑定到端口 " + settings.bindAddress.getPort)
      scheduleConnectToPeer();
    case CommandFailed(_: Bind) =>
      log.error("端口 " + settings.bindAddress.getPort + " already in use!")
      context stop self
  }


  private val outgoing = mutable.Set[InetSocketAddress]()
  var handler: ActorRef = system.actorOf(Props.empty)

  //首次启动连接远程节点
  def peerLogic: Receive = {
    case ConnectTo(remote) =>
      log.info(s"Connecting to: $remote")
      outgoing += remote
      tcpManager ! Connect(remote,
        localAddress = externalSocketAddress,
        options = KeepAlive(true) :: Nil,
        timeout = connTimeout,
        pullMode = true)
    case DisconnectFrom(peer) =>
      log.info(s"Disconnected from ${peer.socketAddress}")
      peer.connectionRef ! CloseConnection
      peerHandlerManager ! Disconnected(peer.socketAddress)

    //    case Blacklist(peer) =>
    //      peer.handlerRef ! PeerConnectionmanager.ReceivableMessages.Blacklist
    //      peerHandlerManagerRef ! Disconnected(peer.socketAddress)

    //远程连接绑定到本地端口,所有连接的节点同时互相绑定
    case Connected(remote, local) =>
      val direction: ConnectionType = if (outgoing.contains(remote)) Outgoing else Incoming
      val logMsg = direction match {
        case Incoming => s"incoming:远程连接 $remote 绑定到本地 $local"
        case Outgoing => s"outgoing:远程连接 $remote 绑定到本地 $local"
      }
      log.info(logMsg)
      val connection = sender()
      val handlerProps: Props = PeerConnectionManagerRef.props(settings, peerHandlerManager, node, chainInfo, connection, direction, externalSocketAddress, remote, timeProvider)
      handler = context.actorOf(handlerProps)
      outgoing -= remote

    case CommandFailed(c: Connect) =>
      outgoing -= c.remoteAddress
      log.info("未能连接到 : " + c.remoteAddress)
      peerHandlerManager ! Disconnected(c.remoteAddress)
  }

  def getHandler: Receive = {
    case GetHandlerToPeerConnectionManager =>
      peerHandlerManager ! PeerHandler(handler) //handler做为消息发送到peerHandlerManager，由peerHandlerManager统一管理
  }

  //  def interfaceCalls: Receive = {
  //    case ShutdownNetwork =>
  //      log.info("关闭所有连接和解除绑定端口")
  //      (peerManagerRef ? FilterPeers(Broadcast))
  //        .map(_.asInstanceOf[Seq[ConnectedPeer]])
  //        .foreach(_.foreach(_.handlerRef ! CloseConnection))
  //      self ! Unbind
  //      context stop self
  //  }

  override def receive: Receive = bindingLogic orElse peerLogic orElse getHandler orElse {
    case CommandFailed(cmd: Tcp.Command) =>
      log.info("执行命令失败 : " + cmd)

    case nonsense: Any =>
      log.warn(s"NetworkController: 未知的错误  $nonsense")
  }

  /**
    * Schedule a periodic connection to a random known peer
    */
  private def scheduleConnectToPeer(): Unit = {
    context.system.scheduler.schedule(5.seconds, 5.seconds) {
      val randomPeerF = peerHandlerManager ? RandomPeerToConnect()
      randomPeerF.mapTo[Option[InetSocketAddress]].foreach { peerInfoOpt =>
        peerInfoOpt.foreach(peerInfo => {
          log.info("found peerInfo:" + peerInfo.getAddress + ":" + peerInfo.getPort)
          self ! ConnectTo(peerInfo)
        }
        )
      }
    }
  }
}

object NetworkManager {

  object ReceivableMessages {

    case class ConnectTo(address: InetSocketAddress)

    case class DisconnectFrom(peer: ConnectedPeer)

    case class Blacklist(peer: ConnectedPeer)

  }

}

object NetworkManagerRef {
  def props(settings: NetworkSettings, chainInfo: ChainInfo, timeProvider: NetworkTimeProvider, peerHandlerManagerRef: ActorRef)(implicit ec: ExecutionContext): Props =
    Props(new NetworkManager(settings, chainInfo, timeProvider, peerHandlerManagerRef))

  def apply(settings: NetworkSettings, chainInfo: ChainInfo, timeProvider: NetworkTimeProvider, peerHandlerManagerRef: ActorRef)(implicit system: ActorContext, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, chainInfo, timeProvider, peerHandlerManagerRef))

  def apply(name: String, settings: NetworkSettings, chainInfo: ChainInfo, timeProvider: NetworkTimeProvider, peerHandlerManagerRef: ActorRef)(implicit system: ActorContext, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, chainInfo, timeProvider, peerHandlerManagerRef), name)
}