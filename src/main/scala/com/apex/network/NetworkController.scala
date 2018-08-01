package com.apex.network

import java.net.{InetAddress, InetSocketAddress, NetworkInterface, URI}
import akka.actor._
import akka.io.Tcp.SO.KeepAlive
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import com.apex.common.ApexLogging
import com.apex.network.message.{Message, MessageHandler}
import com.apex.network.message.MessageSpec
import com.apex.core.settings.NetworkSettings
import com.apex.core.utils.NetworkTimeProvider
import com.apex.network.upnp.UPnP
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.universe.TypeTag


/**
  * 控制所有网络交互
  * 必须是单例
  */
class NetworkController(settings: NetworkSettings,
                        messageHandler: MessageHandler,
                        upnp: UPnP,
                        peerManagerRef: ActorRef,
                        timeProvider: NetworkTimeProvider
                       )(implicit ec: ExecutionContext) extends Actor with ApexLogging {

  import NetworkController.ReceivableMessages._
  import NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
  import PeerConnectionHandler.ReceivableMessages.CloseConnection
  import com.apex.network.peer.PeerManager.ReceivableMessages.{CheckPeers, Disconnected, FilterPeers}

  private implicit val system: ActorSystem = context.system

  private implicit val timeout: Timeout = Timeout(settings.controllerTimeout.getOrElse(5 seconds))

  private val peerSynchronizer: ActorRef = PeerSynchronizerRef("PeerSynchronizer", self, peerManagerRef, settings)
  
  private val messageHandlers = mutable.Map[Seq[Message.MessageCode], ActorRef]()

  private val tcpManager = IO(Tcp)

  if (!settings.localOnly) {
    settings.declaredAddress.foreach { myAddress =>
      Try {
        val uri = new URI("http://" + myAddress)
        val myHost = uri.getHost
        val myAddrs = InetAddress.getAllByName(myHost)

        NetworkInterface.getNetworkInterfaces.asScala.exists { intf =>
          intf.getInterfaceAddresses.asScala.exists { intfAddr =>
            val extAddr = intfAddr.getAddress
            myAddrs.contains(extAddr)
          }
        } || (settings.upnpEnabled && myAddrs.exists(_ == upnp.externalAddress))
      } recover { case t: Throwable =>
        log.error("声明的地址验证失败: ", t)
      }
    }
  }

  lazy val localAddress = settings.bindAddress

  //发送给peers的地址
  lazy val externalSocketAddress: Option[InetSocketAddress] = {
    settings.declaredAddress orElse {
      if (settings.upnpEnabled) {
        upnp.externalAddress.map(a => new InetSocketAddress(a, settings.bindAddress.getPort))
      } else None
    }
  }

  log.info(s"Declared address: $externalSocketAddress")


  lazy val connTimeout = Some(settings.connectionTimeout)

  //绑定来侦听传入的连接
  tcpManager ! Bind(self, localAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  private def bindingLogic: Receive = {
    case Bound(_) =>
      log.info("成功绑定到端口 " + settings.bindAddress.getPort)
      context.system.scheduler.schedule(600.millis, 5.seconds)(peerManagerRef ! CheckPeers)

    case CommandFailed(_: Bind) =>
      log.error("端口 " + settings.bindAddress.getPort + " already in use!")
      context stop self
    //TODO catch?
  }

  def businessLogic: Receive = {
    //来自另一个peer的消息
    case Message(spec, Left(msgBytes), Some(remote)) =>
      val msgId = spec.messageCode
      //messageHandlers=Map(List(1, 2) -> Actor[akka://2-Hop/user/PeerSynchronizer#-544478922])
      spec.parseBytes(msgBytes) match {
        case Success(content) =>
          messageHandlers.find(_._1.contains(msgId)).map(_._2) match {
            case Some(handler) =>
              handler ! DataFromPeer(spec, content, remote)

            case None =>
              log.error("没有操作者处理消息: " + msgId)
          }
        case Failure(e) =>
          log.error("反序列化数据失败: ", e)
      }

    case SendToNetwork(message, sendingStrategy) =>
      (peerManagerRef ? FilterPeers(sendingStrategy))
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! message))
  }

  private val outgoing = mutable.Set[InetSocketAddress]()

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
      peer.handlerRef ! CloseConnection
      peerManagerRef ! Disconnected(peer.socketAddress)

    case Blacklist(peer) =>
      peer.handlerRef ! PeerConnectionHandler.ReceivableMessages.Blacklist
      peerManagerRef ! Disconnected(peer.socketAddress)

    //远程连接绑定到本地端口,所有连接的节点同时互相绑定
    case Connected(remote, local) =>
      val direction: ConnectionType = if(outgoing.contains(remote)) Outgoing else Incoming
      val logMsg = direction match {
        case Incoming => s"传入的远程连接 $remote 绑定到本地 $local"
        case Outgoing => s"输入的远程连接 $remote 绑定到本地 $local"
      }
      log.info(logMsg)
      val connection = sender()
      val handlerProps: Props = PeerConnectionHandlerRef.props(settings, self, peerManagerRef,
        messageHandler, connection, direction, externalSocketAddress, remote, timeProvider)
      context.actorOf(handlerProps) 
      outgoing -= remote

    case CommandFailed(c: Connect) =>
      outgoing -= c.remoteAddress
      log.info("未能连接到 : " + c.remoteAddress)
      peerManagerRef ! Disconnected(c.remoteAddress)
  }

  def interfaceCalls: Receive = {
    case ShutdownNetwork =>
      log.info("关闭所有连接和解除绑定端口")
      (peerManagerRef ? FilterPeers(Broadcast))
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! CloseConnection))
      self ! Unbind
      context stop self
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse interfaceCalls orElse {
    case RegisterMessagesHandler(specs, handler) =>
      log.info(s"Registering handlers for ${specs.map(s => s.messageCode -> s.messageName)}") //注册处理程序
      messageHandlers += specs.map(_.messageCode) -> handler

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("执行命令失败 : " + cmd)

    case nonsense: Any =>
      log.warn(s"NetworkController: 未知的错误  $nonsense")
  }
}

object NetworkController {
  object ReceivableMessages {
    case class RegisterMessagesHandler(specs: Seq[MessageSpec[_]], handler: ActorRef)
    case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)
    case object ShutdownNetwork
    case class ConnectTo(address: InetSocketAddress)
    case class DisconnectFrom(peer: ConnectedPeer)
    case class Blacklist(peer: ConnectedPeer)
  }
}


object NetworkControllerSharedMessages {
  object ReceivableMessages {
    case class DataFromPeer[DT: TypeTag](spec: MessageSpec[DT], data: DT, source: ConnectedPeer)
  }
}

object NetworkControllerRef {
  def props(settings: NetworkSettings,
            messageHandler: MessageHandler,
            upnp: UPnP,
            peerManagerRef: ActorRef,
            timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext): Props =
    Props(new NetworkController(settings, messageHandler, upnp, peerManagerRef, timeProvider))

  def apply(settings: NetworkSettings,
            messageHandler: MessageHandler,
            upnp: UPnP,
            peerManagerRef: ActorRef,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, messageHandler, upnp, peerManagerRef, timeProvider))

  def apply(name: String,
            settings: NetworkSettings,
            messageHandler: MessageHandler,
            upnp: UPnP,
            peerManagerRef: ActorRef,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, messageHandler, upnp, peerManagerRef, timeProvider), name)
}