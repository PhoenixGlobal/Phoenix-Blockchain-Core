package com.apex.network.peer

import java.net.InetSocketAddress

import com.apex.network.{ConnectionType, NodeInfo}
import com.apex.utils.NetworkTime

case class PeerInfo(address: String, port: Int, lastSeen: Long, nodeName: Option[String] = None, connectionType: Option[ConnectionType] = None)

trait PeerDatabase {

  def isEmpty(): Boolean

  def knownPeers(): Map[InetSocketAddress, NodeInfo]

  def addBlacklistPeer(peer: InetSocketAddress, time: NetworkTime.Time): Unit

  def blacklistedPeers(): Seq[String]

  def isBlacklisted(address: InetSocketAddress): Boolean

  def remove(address: InetSocketAddress): Boolean

  def selectPeersByRandom(number: Int): Seq[NodeInfo]

  def peerSize(): Int
}

