package com.apex.network.upnp

import java.net.InetAddress

import com.apex.common.ApexLogging

import scala.util.Try

import scala.collection.JavaConversions._
import com.apex.core.settings.NetworkSettings

class UPnP(settings: NetworkSettings) extends ApexLogging{

  private var gateway: Option[GatewayDevice] = None

  lazy val localAddress = gateway.map(_.localAddress)
  lazy val externalAddress = gateway.map(_.externalIPAddress).get.map(InetAddress.getByName(_))

  Try {
    log.info("Looking for UPnP gateway device...")
    //TODO: read from configuration
    val defaultHttpReadTimeout =  100
    //GatewayDevice.setHttpReadTimeout(defaultHttpReadTimeout)
    val discover = GatewayDiscover()

    //TODO: read from configuration
    //val defaultDiscoverTimeout = 100
    //discover.setTimeout(defaultDiscoverTimeout)
    //val gatewayMap = Option(discover.discover).map(_.toMap).getOrElse(Map())
    if (discover.isEmpty) {
      log.info("There are no UPnP gateway devices")
    } else {
      Option(discover.getValidGateway) match {
        case None => log.debug("There is no connected UPnP gateway device")
        case Some(device) =>
          gateway = Some(device.get)
          log.debug("Using UPnP gateway device on " + localAddress.map(_.getHostAddress).getOrElse("err"))
          log.info("External IP address is " + externalAddress.map(_.getHostAddress).getOrElse("err"))
      }
      
      
      
      val activeGW = discover.getValidGateway.get;
      activeGW.getAllPortMappingEntries foreach (x => println("Portmapping retrieved (" + x.internalClient + ":" + x.externalPort + ")"));//检索的端口映射
      
      
      
      
    }
  }.recover { case t: Throwable =>
    log.error("Unable to discover UPnP gateway devices: " + t.toString)
  }
  
  if(Option(gateway.get.getSpecificPortMappingEntry(settings.bindAddress.getPort,"TCP"))!=None){
    deletePort(settings.bindAddress.getPort)
  }
  addPort(settings.bindAddress.getPort)
  def addPort(port: Int): Try[Unit] = Try {
    if (gateway.get.addPortMapping(port, port, localAddress.get.getHostAddress, "TCP", "Scorex")) {
      log.info("Mapped port [" + localAddress.get.getHostAddress + "]:" + port)
    } else {
      log.info("Unable to map port " + port)
    }
  }.recover { case t: Throwable =>
    log.error("Unable to map port " + port + ": " + t.toString)
  }

  def deletePort(port: Int): Try[Unit] = Try {
    if (gateway.get.deletePortMapping(port, "TCP")) {
      log.info("Mapping deleted for port " + port)
    } else {
      log.info("Unable to delete mapping for port " + port)
    }
  }.recover { case t: Throwable =>
    log.error("Unable to delete mapping for port " + port + ": " + t.toString)
  }
}

