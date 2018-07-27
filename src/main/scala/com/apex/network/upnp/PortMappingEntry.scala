package com.apex.network.upnp

case class PortMappingEntry private[upnp] (
internalPort: Option[Int], externalPort: Option[Int], 
remoteHost: Option[String], internalClient: Option[String],
protocol: Option[String], enabled: Option[String], portMappingDescription: Option[String])
