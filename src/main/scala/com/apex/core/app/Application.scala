package com.apex.core.app

import akka.actor.{ActorRef, ActorSystem}
import com.apex.common.ApexLogging
import com.apex.network.NetworkControllerRef
import com.apex.network.message._
import com.apex.network.peer.PeerManagerRef
import com.apex.core.settings.ApexSettings
import com.apex.core.utils.NetworkTimeProvider
import com.apex.network.upnp.UPnP
import scala.concurrent.ExecutionContext
import com.apex.network.message.MessageHandler

trait Application extends ApexLogging {

  val ApplicationNameLimit = 50
  implicit val settings: ApexSettings
  protected implicit lazy val actorSystem = ActorSystem(settings.network.agentName)
  implicit val executionContext: ExecutionContext = actorSystem.dispatchers.lookup("apex.executionContext")

  val nodeViewHolderRef: ActorRef
  //p2p
  lazy val upnp = new UPnP(settings.network)

  private lazy val basicSpecs = {
    val invSpec = new InvSpec(settings.network.maxInvObjects)
    val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
    Seq(GetPeersSpec,PeersSpec,invSpec,requestModifierSpec,ModifiersSpec)
  }

  lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs)

  val nodeViewSynchronizer: ActorRef

  val timeProvider = new NetworkTimeProvider(settings.ntp)

  val peerManagerRef = PeerManagerRef(settings, timeProvider)
//
  val networkControllerRef: ActorRef = NetworkControllerRef("networkController",settings.network,
                                                            messagesHandler, upnp, peerManagerRef, timeProvider)

  //lazy val combinedRoute = CompositeHttpService(actorSystem, apiRoutes, settings.restApi, swaggerConfig).compositeRoute

  def run(): Unit = {
    require(settings.network.agentName.length <= ApplicationNameLimit)

    //implicit val materializer = ActorMaterializer()
    val bindAddress = settings.restApi.bindAddress

    //Http().bindAndHandle(combinedRoute, bindAddress.getAddress.getHostAddress, bindAddress.getPort)

//    Runtime.getRuntime.addShutdownHook(new Thread() {
//      override def run() {
//        stopAll()
//      }
//    })
  }

//  def stopAll(): Unit = synchronized {
//    if (settings.network.upnpEnabled) upnp.deletePort(settings.network.bindAddress.getPort)
//    networkControllerRef ! ShutdownNetwork
//
//    actorSystem.terminate().onComplete { _ =>
//
//      System.exit(0)
//    }
//  }
}
