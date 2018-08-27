package com.apex.network.upnp

import java.net.InetAddress

import com.apex.common.ApexLogging

import scala.util.Try

import scala.collection.JavaConversions._
import com.apex.settings.NetworkSettings

class UPnP(settings: NetworkSettings) extends ApexLogging{

  private var gateway: Option[GatewayDevice] = None

  lazy val localAddress = gateway.map(_.localAddress)
  lazy val externalAddress = gateway.map(_.externalIPAddress).get.map(InetAddress.getByName(_))

  Try {
    log.info("寻找UPnP网关设备...")
    val discover = GatewayDiscover()

    if (discover.isEmpty) {
      log.info("没有UPnP网关设备")
    } else {
      Option(discover.getValidGateway) match {
        case None => log.debug("没有连接的UPnP网关设备")
        case Some(device) =>
          gateway = Some(device.get)
          log.debug("使用UPnP网关设备 " + localAddress.map(_.getHostAddress).getOrElse("err"))
          log.info("外部IP地址是 " + externalAddress.map(_.getHostAddress).getOrElse("err"))
      }
    }
  }.recover { case t: Throwable =>
    log.error("无法发现UPnP网关设备: " + t.toString)
  }
  
  if(Option(gateway.get.getSpecificPortMappingEntry(settings.bindAddress.getPort,"TCP"))!=None){
    deletePort(settings.bindAddress.getPort)
  }
  addPort(settings.bindAddress.getPort)
  def addPort(port: Int): Try[Unit] = Try {
    if (gateway.get.addPortMapping(port, port, localAddress.get.getHostAddress, "TCP", "Apex")) {
      log.info("映射端口 [" + localAddress.get.getHostAddress + "]:" + port)
    } else {
      log.info("无法映射端口 " + port)
    }
  }.recover { case t: Throwable =>
    log.error("无法映射端口 " + port + ": " + t.toString)
  }

  def deletePort(port: Int): Try[Unit] = Try {
    if (gateway.get.deletePortMapping(port, "TCP")) {
      log.info("删除端口映射 " + port)
    } else {
      log.info("无法删除端口映射 " + port)
    }
  }.recover { case t: Throwable =>
    log.error("无法删除端口映射 " + port + ": " + t.toString)
  }
}

