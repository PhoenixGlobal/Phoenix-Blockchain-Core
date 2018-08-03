package com.apex.hybrid

import com.apex.core.NodeViewHolderRef
import com.apex.core.settings.ApexSettings
import com.apex.network.NodeViewSynchronizerRef

import akka.actor.ActorRef
import akka.actor.ActorSystem
import com.apex.network.upnp.UPnP
import scala.concurrent.ExecutionContext
import akka.actor.Actor
import akka.actor.Props
import scala.concurrent.Promise
import com.apex.network.message.GetPeersSpec
import com.apex.network.message.Message
import com.apex.network.NetworkManagerRef

class HybridApp2(val settingsFilename: String){

  //加载节点文件
  private val hybridSettings = HybridSettings.read(Some(settingsFilename))
  val settings: ApexSettings = hybridSettings.apexSettings
  
  val ApplicationNameLimit = 50
  protected implicit lazy val actorSystem = ActorSystem(settings.network.agentName)
  implicit val executionContext: ExecutionContext = actorSystem.dispatchers.lookup("apex.executionContext")

  //p2p
  lazy val upnp = new UPnP(settings.network)
  
  val ip:String = "127.0.0.1"
  val port:Int = 9084
  
  val promise = Promise[(Boolean, ActorRef)]()
  val networkControllerRef: ActorRef = NetworkManagerRef("networkController",settings.network,upnp,ip,port,promise)
    promise.future.map(x => {
      if(x._1){
        val msg = Message[Unit](GetPeersSpec, Right(Unit), None)
        x._2! msg
      }
    })

  def run(): Unit = {
    require(settings.network.agentName.length <= ApplicationNameLimit)
  }
}

object HybridApp2 extends App {
  private val settingsFilename = args.headOption.getOrElse("settings2.conf")
  new HybridApp2(settingsFilename).run()
}
