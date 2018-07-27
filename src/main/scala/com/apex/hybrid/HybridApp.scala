package com.apex.hybrid

import akka.actor.ActorRef
import com.apex.common.{SimpleBoxStructure, SimpleBoxStructureMemPool}
import com.apex.hybrid.history.{HybridSyncInfo, HybridSyncInfoMessageSpec}
import com.apex.hybrid.mining._
import com.apex.hybrid.MessageEntry.ReceivableMessages.GeneratorInfoTest
import com.apex.core.app.Application
import com.apex.core.network.NodeViewSynchronizerRef
import com.apex.network.message.MessageSpec
import com.apex.core.serialization.SerializerRegistry
import com.apex.core.serialization.SerializerRegistry.SerializerRecord
import com.apex.core.settings.ApexSettings

import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps
import java.net.InetSocketAddress
import com.apex.network.message.{GetPeersSpec, Message, MessageSpec, PeersSpec}
import com.apex.network.message.InvSpec
import com.apex.core.{EphemerealNodeViewModifier, ModifierId, ModifierTypeId}
import com.apex.core.hash.Blake2b256

class HybridApp(val settingsFilename: String) extends Application {

  //加载节点文件
  private val hybridSettings = HybridSettings.read(Some(settingsFilename))
  implicit override lazy val settings: ApexSettings = HybridSettings.read(Some(settingsFilename)).apexSettings
  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(NodeViewSynchronizerRef.props[HybridSyncInfo, HybridSyncInfoMessageSpec.type]
                                                     (networkControllerRef, HybridSyncInfoMessageSpec, settings.network, timeProvider))
  protected val invSpec = new InvSpec(settings.network.maxInvObjects)
  val messageToSign: Array[Byte]= Array(1.toByte, 1.toByte, 1.toByte)
  val id: ModifierId = ModifierId @@ Blake2b256(messageToSign)
  if (settings.network.nodeName.startsWith("generatorNode")) {
    log.info("Starting structures generation")
    val nodeViewHolderRef: ActorRef = BroadcastMessageRef()
    val generator: ActorRef = MessageEntryRef(nodeViewHolderRef)
    //generator ! StartGeneration(10 seconds)
    
    val msg = Message(invSpec, Right(MessageType.ModifierTypeId -> Seq(id)), None)
    generator ! GeneratorInfoTest(msg)
  }
}
object MessageType {
  val ModifierTypeId: com.apex.core.ModifierTypeId = com.apex.core.ModifierTypeId @@ 10.toByte
}

object HybridApp extends App {
  private val settingsFilename = args.headOption.getOrElse("settings2.conf")
  new HybridApp(settingsFilename).run()
  
}
