package com.apex.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorContext, ActorRef, ActorSystem, Cancellable, Props}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import com.apex.common.ApexLogging
import com.apex.core.{Blockchain, ChainInfo}
import com.apex.settings.NetworkSettings
import com.apex.utils.{NetworkTimeProvider, Version}
import com.apex.network.PeerConnectionManager.{AwaitingHandshake, WorkingCycle}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

sealed trait ConnectionType

case object Incoming extends ConnectionType

case object Outgoing extends ConnectionType

case class ConnectedPeer(socketAddress: InetSocketAddress,
                         connectionRef: ActorRef,
                         direction: ConnectionType,
                         handshake: Handshake) {

  import shapeless.syntax.typeable._

  def publicPeer: Boolean = handshake.declaredAddress.contains(socketAddress)

  override def hashCode(): Int = socketAddress.hashCode()

  override def equals(obj: Any): Boolean =
    obj.cast[ConnectedPeer].exists(p => p.socketAddress == this.socketAddress && p.direction == this.direction)

  override def toString: String = s"ConnectedPeer($socketAddress)"
}

case object Ack extends Event

class PeerConnectionManager(settings: NetworkSettings,
                            peerHandlerManagerRef: ActorRef,
                            nodeRef: ActorRef,
                            chainInfo: ChainInfo,
                            connection: ActorRef,
                            direction: ConnectionType,
                            ownSocketAddress: Option[InetSocketAddress],
                            remote: InetSocketAddress,
                            timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext)
  extends Actor with ApexLogging {

  import PeerConnectionManager.ReceivableMessages._
  import com.apex.network.peer.PeerHandlerManager.ReceivableMessages.{Disconnected, DoConnecting, Handshaked}

  context watch connection

  private val networkManager = context.parent

  private var receivedHandshake = Option.empty[Handshake]

  private var selfPeer = Option.empty[ConnectedPeer]

  private var handshakeTimeoutCancellableOpt = Option.empty[Cancellable]

  private var chunksBuffer = CompactByteString()

  private var handshakeSent = false

  private def handshakeGot = {
    receivedHandshake.isDefined
  }

  private def constructHandshakeMsg: Handshake = {
    val headerNum = 0 // not used

    Handshake(settings.agentName, Version(settings.appVersion), settings.nodeName,
      ownSocketAddress, chainInfo.id, headerNum.toString,
      System.currentTimeMillis())
  }

  private def handshake: Receive =
    startInteraction orElse
      receivedData orElse
      handshakeTimeout orElse
      handshakeDone orElse
      processErrors(AwaitingHandshake.toString)

  private def processErrors(stateName: String): Receive = {
    case CommandFailed(w: Write) =>
      log.warn(s"写入失败 :$w " + remote + s" 状态 $stateName")
      connection ! Close
      connection ! ResumeReading
      connection ! ResumeWriting

    case cc: ConnectionClosed =>
      peerHandlerManagerRef ! Disconnected(remote)
      log.info("链接关闭 : " + remote + ": " + cc.getErrorCause + s" in state $stateName")
      context stop self

    case CloseConnection =>
      log.info(s"强制中止通信: " + remote + s" 状态  $stateName")
      connection ! Close

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("执行命令失败 : " + cmd + s" 状态 $stateName")
      connection ! ResumeReading
  }

  private def startInteraction: Receive = {
    case StartInteraction =>
      val hb = constructHandshakeMsg.bytes
      connection ! Tcp.Write(ByteString(hb))
      log.info(s"发送握手到: $remote")
      handshakeSent = true
      if (handshakeGot && handshakeSent) self ! HandshakeDone
  }

  private def receivedData: Receive = {
    case Received(data) =>
      if (!handshakeGot) {
        HandshakeSerializer.parseBytes(data.toArray) match {
          case Success(handshake) => handleHandshake(handshake)
          case Failure(t) =>
            log.info(s"解析握手时的错误", t)
            self ! CloseConnection
        }
      }
      else {
        // if the peer send some data immediately after handshake done, we might got it before HandshakeDone
        log.error(s"expect HandshakeDone, but recv other data, ignore")
      }
  }

  private def handleHandshake(handshakeMsg: Handshake): Unit = {
    if (!chainInfo.id.equals(handshakeMsg.chainId)) {
      log.error(f"Peer on a different chain. Closing connection")
      self ! CloseConnection
    } else {
      val myTime = System.currentTimeMillis()
      val timeGap = math.abs(handshakeMsg.time - myTime)
      log.info(s"peer timeGap = $timeGap")
      if (timeGap > settings.peerMaxTimeGap) {
        log.error(s"peer timeGap too large $timeGap  Closing connection")
        self ! CloseConnection
      } else {
        receivedHandshake = Some(handshakeMsg)
        log.info(s"获得握手: $remote")
        connection ! ResumeReading
        networkManager ! GetHandlerToPeerConnectionManager //握手成功后，向PeerConnectionManager发送远程handler
        if (handshakeGot && handshakeSent) {
          self ! HandshakeDone
        }
      }
    }
  }

  private def handshakeTimeout: Receive = {
    case HandshakeTimeout =>
      log.info(s"与远程 $remote 握手超时, 将删除连接")
      self ! CloseConnection
  }

  private def handshakeDone: Receive = {
    case HandshakeDone =>
      require(receivedHandshake.isDefined)

      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val peer = ConnectedPeer(remote, self, direction, receivedHandshake.get)
      selfPeer = Some(peer)

      peerHandlerManagerRef ! Handshaked(peer)
      handshakeTimeoutCancellableOpt.map(_.cancel())
      connection ! ResumeReading
      context become workingCycle
  }

  private val msgBuffer = ArrayBuffer.empty[MessagePack]
  private var waitForAck: Boolean = false

  private def acknowledge(): Unit = {
    if (msgBuffer.size > 0) {
      val msg = msgBuffer.remove(0)
      connection ! Write(ByteString(Array(msg.messageType.id.toByte) ++ msg.data), Ack)
      waitForAck = true
    }
    else
      waitForAck = false
  }

  //发送消息
  def workingCycleLocalInterface: Receive = {
    case msg: MessagePack =>
      //log.info(s"PeerConnectionManager try send $msg")
      if (waitForAck) {
        msgBuffer.append(msg)
        log.info(s"msgBuffer size ${msgBuffer.size}")
      }
      else {
        connection ! Write(ByteString(Array(msg.messageType.id.toByte) ++ msg.data), Ack)
        waitForAck = true
      }

    //context.become({
    //  case Ack            ⇒ acknowledge()
    //}, discardOld = false)

    case Ack =>
      acknowledge()

    //    case Blacklist =>
    //      log.info(s"加入黑名单 " + remote)
    //      peerManagerRef ! AddToBlacklist(remote)
    //      connection ! Close
  }

  //接收消息
  def workingCycleRemoteInterface: Receive = {
    case Received(data) =>
      connection ! ResumeReading
//      log.info(s"PeerConnectionManager recv Message")
      nodeRef ! MessagePack.fromBytes(data.toArray)
  }

  private def reportStrangeInput: Receive = {
    case nonsense: Any =>
      log.warn(s"未知的错误: $nonsense")
  }

  def workingCycle: Receive =
    workingCycleLocalInterface orElse
      workingCycleRemoteInterface orElse
      processErrors(WorkingCycle.toString) orElse
      reportStrangeInput

  override def preStart: Unit = {
    peerHandlerManagerRef ! DoConnecting(remote, direction)
    handshakeTimeoutCancellableOpt = Some(context.system.scheduler.scheduleOnce(settings.handshakeTimeout)
    (self ! HandshakeTimeout))
    connection ! Register(self, keepOpenOnPeerClosed = false, useResumeWriting = true)
    connection ! ResumeReading
  }

  override def receive: Receive = handshake

  override def postStop(): Unit = log.info(s"Peer handler to $remote 销毁")

}

object PeerConnectionManager {

  sealed trait CommunicationState

  case object AwaitingHandshake extends CommunicationState

  case object WorkingCycle extends CommunicationState

  object ReceivableMessages {

    private[PeerConnectionManager] object HandshakeDone

    case object StartInteraction

    case object HandshakeTimeout

    case object CloseConnection

    case object GetHandlerToPeerConnectionManager

    //    case object Blacklist
  }

}

object PeerConnectionManagerRef {
  def props(settings: NetworkSettings, peerHandlerManagerRef: ActorRef, nodeRef: ActorRef, chainInfo: ChainInfo, connection: ActorRef, direction: ConnectionType, ownSocketAddress: Option[InetSocketAddress], remote: InetSocketAddress, timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext): Props =
    Props(new PeerConnectionManager(settings, peerHandlerManagerRef, nodeRef, chainInfo, connection, direction, ownSocketAddress, remote, timeProvider))

  def apply(settings: NetworkSettings, peerHandlerManagerRef: ActorRef, nodeRef: ActorRef, chainInfo: ChainInfo, connection: ActorRef, direction: ConnectionType, ownSocketAddress: Option[InetSocketAddress], remote: InetSocketAddress, timeProvider: NetworkTimeProvider)
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, peerHandlerManagerRef, nodeRef, chainInfo, connection, direction, ownSocketAddress, remote, timeProvider))

  def apply(name: String, settings: NetworkSettings, peerHandlerManagerRef: ActorRef, nodeRef: ActorRef, chainInfo: ChainInfo, connection: ActorRef, direction: ConnectionType, ownSocketAddress: Option[InetSocketAddress], remote: InetSocketAddress, timeProvider: NetworkTimeProvider)
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, peerHandlerManagerRef, nodeRef, chainInfo, connection, direction, ownSocketAddress, remote, timeProvider), name)
}
