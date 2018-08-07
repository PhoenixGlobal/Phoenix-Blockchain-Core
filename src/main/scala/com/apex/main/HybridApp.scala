package com.apex.hybrid

import com.apex.core.NodeViewHolderRef
import com.apex.core.app.Application
import com.apex.core.settings.ApexSettings
import com.apex.network.NodeViewSynchronizerRef

import akka.actor.ActorRef

class HybridApp(val settingsFilename: String) extends Application {

  //加载节点文件
  private val hybridSettings = HybridSettings.read(Some(settingsFilename))
  implicit override lazy val settings: ApexSettings = HybridSettings.read(Some(settingsFilename)).apexSettings
  override val nodeViewHolderRef: ActorRef = NodeViewHolderRef()
  
  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(NodeViewSynchronizerRef.props(networkControllerRef, nodeViewHolderRef, settings.network, timeProvider))
  val message: Array[Byte] = Array(1.toByte, 1.toByte, 1.toByte)

  if (settings.network.nodeName.startsWith("generatorNode")) {
    log.info("Starting structures generation")
    Thread.sleep(20000)
    SendMessageRef(settings).sendMessage(message, None)
  }
}

object HybridApp extends App {
  private val settingsFilename = args.headOption.getOrElse("settings2.conf")
  new HybridApp(settingsFilename).run()

}
