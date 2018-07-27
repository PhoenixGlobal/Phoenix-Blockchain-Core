package com.apex.network.peer

import java.net.InetSocketAddress
import com.apex.network.ConnectionType
import com.apex.core.utils.NetworkTime

case class PeerInfo(lastSeen: Long, nodeName: Option[String] = None, connectionType: Option[ConnectionType] = None)

trait PeerDatabase {

  def isEmpty():Boolean

  def addOrUpdateKnownPeer(peer: InetSocketAddress, peerInfo: PeerInfo): Unit

  def knownPeers(): Map[InetSocketAddress, PeerInfo]

  def blacklistPeer(peer: InetSocketAddress, time: NetworkTime.Time): Unit

  def blacklistedPeers(): Seq[String]

  def isBlacklisted(address: InetSocketAddress): Boolean

  def remove(address: InetSocketAddress): Boolean
}

