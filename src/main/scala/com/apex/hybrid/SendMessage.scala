package com.apex.hybrid

import com.apex.core.settings.ApexSettings
import com.apex.network.ConnectedPeer
import com.apex.network.message.InvSpec
import com.apex.core.hash.Blake2b256
import com.apex.network.message.Message
import com.apex.network.message.StructureMessage
import com.apex.core.{ModifierId, ModifierTypeId}
import akka.actor.Props
import akka.actor.ActorSystem
import com.apex.core.utils.NetworkTimeProvider
import akka.actor.ActorRef
import com.apex.hybrid.MessageEntry.ReceivableMessages.GeneratorInfo

/**
 * 根据传入的消息组织消息结构
 */
class SendMessage(settings: ApexSettings){
  protected val invSpec = new InvSpec(settings.network.maxInvObjects)
  
  def sendMessage(input: Array[Byte],source: Option[ConnectedPeer])(implicit system: ActorSystem)={
			val id: ModifierId = ModifierId @@ Blake2b256(input)
		  val msg = Message(invSpec, Right(StructureMessage.ModifierTypeId -> Seq(id)), source)
		  val broadcastMessageRef: ActorRef = BroadcastMessageRef()
      val generator: ActorRef = MessageEntryRef(broadcastMessageRef)
      generator ! GeneratorInfo(msg)
  }
}

object SendMessageRef {
  def apply(settings: ApexSettings)(implicit system: ActorSystem) = new SendMessage(settings)
}