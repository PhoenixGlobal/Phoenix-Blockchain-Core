package com.apex.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Cancellable, Props}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import com.apex.common.ApexLogging
import com.apex.core.Blockchain
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

class PeerConnectionManager(val settings: NetworkSettings,
                            val peerHandlerManagerRef: ActorRef,
                            val nodeRef: ActorRef,
                            connection: ActorRef,
                            direction: ConnectionType,
                            ownSocketAddress: Option[InetSocketAddress],
                            remote: InetSocketAddress,
                            networkManager: ActorRef,
                            timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext)
  extends Actor with DataBuffering with ApexLogging {

  import PeerConnectionManager.ReceivableMessages._
  import com.apex.network.peer.PeerHandlerManager.ReceivableMessages.{Disconnected, DoConnecting, Handshaked}

  context watch connection

  private var receivedHandshake: Option[Handshake] = None

  private var selfPeer: Option[ConnectedPeer] = None

  private def handshakeGot = receivedHandshake.isDefined

  private var handshakeTimeoutCancellableOpt: Option[Cancellable] = None

  private var handshakeSent = false

  private var chunksBuffer: ByteString = CompactByteString()

  private def constructHandshakeMsg: Handshake = {
    val chain = Blockchain.getLevelDBBlockchain
    val headerNum = chain.getHeight()
    val chainId = chain.getGenesisBlockChainId
    Handshake(settings.agentName,
            Version(settings.appVersion), settings.nodeName,
            ownSocketAddress,chainId,headerNum.toString,  timeProvider.time())
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
          case Success(handshake) => handleMsg(handshake)
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

  private def handleMsg(handshakeMsg: Handshake): Unit ={
    val localChain = Blockchain.getLevelDBBlockchain
    if(localChain.getGenesisBlockChainId != handshakeMsg.chainId){
      log.error(f"Peer on a different chain. Closing connection")
      self ! CloseConnection
      return
    }
    if (localChain.getHeight() > handshakeMsg.headerNum.toInt){
      //log.error(f"Peer on a lower chain. Closing connection")
      //self ! CloseConnection
      //return
    }

    recvHandshake(handshakeMsg)
  }

  private def recvHandshake(handshakeMsg: Handshake): Unit ={
    receivedHandshake = Some(handshakeMsg)
    log.info(s"获得握手: $remote")
    connection ! ResumeReading
    networkManager ! GetHandlerToPeerConnectionManager //握手成功后，向PeerConnectionManager发送远程handler
    if (handshakeGot && handshakeSent) self ! HandshakeDone
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
      //log.info("PeerConnectionManager try send Message")
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
      //log.info(s"PeerConnectionManager recv Message")
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
  def props(settings: NetworkSettings, peerHandlerManagerRef: ActorRef, nodeRef: ActorRef, connection: ActorRef, direction: ConnectionType, ownSocketAddress: Option[InetSocketAddress], remote: InetSocketAddress, networkManager: ActorRef, timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext): Props =
    Props(new PeerConnectionManager(settings, peerHandlerManagerRef, nodeRef, connection, direction, ownSocketAddress, remote, networkManager, timeProvider))

  def apply(settings: NetworkSettings, peerHandlerManagerRef: ActorRef, nodeRef: ActorRef, connection: ActorRef, direction: ConnectionType, ownSocketAddress: Option[InetSocketAddress], remote: InetSocketAddress, networkManager: ActorRef, timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, peerHandlerManagerRef, nodeRef, connection, direction, ownSocketAddress, remote, networkManager, timeProvider))

  def apply(name: String, settings: NetworkSettings, peerHandlerManagerRef: ActorRef, nodeRef: ActorRef, connection: ActorRef, direction: ConnectionType, ownSocketAddress: Option[InetSocketAddress], remote: InetSocketAddress, networkManager: ActorRef, timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, peerHandlerManagerRef, nodeRef, connection, direction, ownSocketAddress, remote, networkManager, timeProvider), name)
}
