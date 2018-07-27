package com.apex.network
import java.net.InetAddress
import scala.xml.NodeSeq

package object upnp {
import GatewayDiscover._
    implicit def map2GatewayDeviceMap(o: Map[InetAddress, GatewayDevice]): GatewayDeviceMap = new GatewayDeviceMap(o)
	implicit def gatewayDeviceMap2Map(o: GatewayDeviceMap): Map[InetAddress, GatewayDevice] = o.devices
	
	  implicit class XmlStringParser(nodeSeq:NodeSeq) {
    def textOption = Option( nodeSeq.text ).filter(_.trim.nonEmpty)
  }
	
}