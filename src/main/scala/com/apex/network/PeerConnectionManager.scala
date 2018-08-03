package com.apex.network

import java.net.InetSocketAddress

import scala.concurrent.ExecutionContext

import com.apex.common.ApexLogging
import com.apex.core.ModifierId
import com.apex.core.hash.Blake2b256
import com.apex.core.settings.NetworkSettings
import com.apex.network.PeerConnectionHandler.AwaitingHandshake
import com.apex.network.message.InvSpec
import com.apex.network.message.Message
import com.apex.network.message.StructureMessage
import com.google.common.primitives.Ints

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.io.Tcp
import akka.io.Tcp.Close
import akka.io.Tcp.CommandFailed
import akka.io.Tcp.ConnectionClosed
import akka.io.Tcp.Received
import akka.io.Tcp.Register
import akka.io.Tcp.ResumeReading
import akka.io.Tcp.ResumeWriting
import akka.io.Tcp.Write
import akka.util.ByteString
import akka.util.CompactByteString


class PeerConnectionManager(val settings: NetworkSettings,
                            connection: ActorRef,
                            remote: InetSocketAddress)(implicit ec: ExecutionContext)
  extends Actor with DataBuffering with ApexLogging {

  private def handshake: Receive =
		  workingCycleLocalInterface orElse
      processErrors(AwaitingHandshake.toString) orElse
      workingCycleRemoteInterface
      
  private var chunksBuffer: ByteString = CompactByteString()

  private def processErrors(stateName: String): Receive = {
    case CommandFailed(w: Write) =>
      log.warn(s"写入失败 :$w " + remote + s" 状态 $stateName")
      connection ! Close
      connection ! ResumeReading
      connection ! ResumeWriting

    case cc: ConnectionClosed =>
      log.info("连接关闭 : " + remote + ": " + cc.getErrorCause + s" 状态  $stateName")
      context stop self


    case CommandFailed(cmd: Tcp.Command) =>
      log.info("执行命令失败 : " + cmd + s" 状态 $stateName")
      connection ! ResumeReading
  }


  private def reportStrangeInput: Receive= {
      case nonsense: Any =>
        log.warn(s"未知的错误: $nonsense")
  }
  
  //发送消息
  def workingCycleLocalInterface: Receive = {
    case msg: message.Message[_] =>
        val bytes = msg.bytes
        log.info("发送消息 " + msg.spec + " 到 " + remote)
        connection ! Write(ByteString(Ints.toByteArray(bytes.length) ++ bytes))
  }

  //接收消息
  def workingCycleRemoteInterface: Receive = {
    case Received(data) =>

      val t = getPacket(chunksBuffer ++ data)
      chunksBuffer = t._2
      println("接收到的消息="+t._1+":"+t._2)
      
      connection ! ResumeReading
      
      Thread.sleep(2000)
      val id: ModifierId = ModifierId @@ Blake2b256(Array(1.toByte, 1.toByte, 1.toByte))
		  val msg = Message(new InvSpec(20), Right(StructureMessage.ModifierTypeId -> Seq(id)), None)
		  self ! msg
  }
      
  override def preStart: Unit = {
    connection ! Register(self, keepOpenOnPeerClosed = false, useResumeWriting = true)
    connection ! ResumeReading
  }

  override def receive: Receive = handshake

  override def postStop(): Unit = log.info(s"Peer handler to $remote destroyed")
  
}

object PeerConnectionManagerRef {
  def props(settings: NetworkSettings,connection: ActorRef,remote: InetSocketAddress)(implicit ec: ExecutionContext): Props =
    Props(new PeerConnectionManager(settings, connection, remote))

  def apply(settings: NetworkSettings,connection: ActorRef,remote: InetSocketAddress)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, connection, remote))

  def apply(name: String,settings: NetworkSettings,connection: ActorRef,remote: InetSocketAddress)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, connection, remote), name)
}
