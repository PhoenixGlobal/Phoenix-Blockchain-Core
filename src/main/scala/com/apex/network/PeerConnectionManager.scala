package com.apex.network

import java.net.InetSocketAddress

import scala.concurrent.ExecutionContext

import com.apex.common.ApexLogging
import com.apex.core.settings.NetworkSettings
import com.apex.network.message.Message
import com.google.common.primitives.Ints
import com.apex.core.utils.Version
import com.apex.network.PeerConnectionManager.{AwaitingHandshake, WorkingCycle}

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.ByteString
import akka.util.CompactByteString
import akka.actor.Cancellable
import scala.util.{Failure, Random, Success}
import com.apex.core.utils.NetworkTimeProvider
import com.apex.network.message.GetPeersSpec

sealed trait ConnectionType
case object Incoming extends ConnectionType
case object Outgoing extends ConnectionType

case class ConnectedPeer(socketAddress: InetSocketAddress,
        handlerRef: ActorRef,
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

class PeerConnectionManager(val settings: NetworkSettings,peerHandlerManagerRef:ActorRef,
                            connection: ActorRef,
                            direction: ConnectionType,
                            ownSocketAddress: Option[InetSocketAddress],
                            remote: InetSocketAddress,networkManager:ActorRef,timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext)
  extends Actor with DataBuffering with ApexLogging {

  import PeerConnectionManager.ReceivableMessages._
  import com.apex.network.peer.PeerHandlerManager.ReceivableMessages.{AddToBlacklist, Disconnected, DoConnecting, Handshaked}
     
  context watch connection
  
  private var receivedHandshake: Option[Handshake] = None
  
  private var selfPeer: Option[ConnectedPeer] = None
  
  
  private def handshakeGot = receivedHandshake.isDefined
      
  private var handshakeTimeoutCancellableOpt: Option[Cancellable] = None
  
  private var handshakeSent = false
  
  private var chunksBuffer: ByteString = CompactByteString()
  
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
      log.info("连接关闭 : " + remote + ": " + cc.getErrorCause + s" 状态  $stateName")
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
	    val hb = Handshake(settings.agentName,
	      Version(settings.appVersion), settings.nodeName,
	      ownSocketAddress, timeProvider.time()).bytes
	    connection ! Tcp.Write(ByteString(hb))
	    log.info(s"发送握手到: $remote")
	    handshakeSent = true
	    if (handshakeGot && handshakeSent) self ! HandshakeDone
  }
	
  private def receivedData: Receive = {
	  case Received(data) =>
	    HandshakeSerializer.parseBytes(data.toArray) match {
	      case Success(handshake) =>
	        receivedHandshake = Some(handshake)
	        log.info(s"获得握手: $remote")
	        connection ! ResumeReading
	        networkManager ! GetHandlerToPeerConnectionManager //握手成功后，向PeerConnectionManager发送远程handler
	        if (handshakeGot && handshakeSent) self ! HandshakeDone
	      case Failure(t) =>
	        log.info(s"解析握手时的错误", t)
	        self ! CloseConnection
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
	        
  //发送消息
  def workingCycleLocalInterface: Receive = {
    case msg: message.Message[_] =>
        val bytes = msg.bytes
        log.info("发送消息 " + msg.spec + " 到 " + remote)
        connection ! Write(ByteString(Ints.toByteArray(bytes.length) ++ bytes))
        
//    case Blacklist =>
//      log.info(s"加入黑名单 " + remote)
//      peerManagerRef ! AddToBlacklist(remote)
//      connection ! Close
  }

  //接收消息
  def workingCycleRemoteInterface: Receive = {
    case Received(data) =>

      val t = getPacket(chunksBuffer ++ data)
      chunksBuffer = t._2
      log.info("接收到的消息="+t._1+":"+t._2)
      
//      t._1.find { packet =>
//        messagesHandler.parseBytes(packet.toByteBuffer, selfPeer) match {
//          case Success(message) =>
//            log.info(" 从 " + remote + "收到消息 " + message.spec )
//            networkControllerRef ! message
//            false
//
//          case Failure(e) =>
//            log.info(s"损坏的数据来自: " + remote, e)
//            true
//        }
//      }
      
      connection ! ResumeReading
      
      Thread.sleep(2000)
      val msg = Message[Unit](GetPeersSpec, Right(Unit), None)
		  self ! msg
  }
  
  private def reportStrangeInput: Receive= {
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
  def props(settings: NetworkSettings,peerHandlerManagerRef:ActorRef,connection: ActorRef,direction: ConnectionType,ownSocketAddress: Option[InetSocketAddress],remote: InetSocketAddress,networkManager:ActorRef,timeProvider: NetworkTimeProvider)(implicit ec: ExecutionContext): Props =
    Props(new PeerConnectionManager(settings, peerHandlerManagerRef,connection, direction,ownSocketAddress,remote,networkManager,timeProvider))

  def apply(settings: NetworkSettings,peerHandlerManagerRef:ActorRef,connection: ActorRef,direction: ConnectionType,ownSocketAddress: Option[InetSocketAddress],remote: InetSocketAddress,networkManager:ActorRef,timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, peerHandlerManagerRef,connection, direction,ownSocketAddress,remote,networkManager,timeProvider))

  def apply(name: String,settings: NetworkSettings,peerHandlerManagerRef:ActorRef,connection: ActorRef,direction: ConnectionType,ownSocketAddress: Option[InetSocketAddress],remote: InetSocketAddress,networkManager:ActorRef,timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, peerHandlerManagerRef,connection, direction,ownSocketAddress,remote,networkManager,timeProvider), name)
}
