package com.apex.network.peer

import java.net.InetSocketAddress

import com.apex.utils.NetworkTime

import scala.collection.mutable
import com.apex.common.ApexLogging

import scala.util.Random


class PeerDatabaseImpl(filename: Option[String]) extends PeerDatabase {

  private val whitelistPersistence = mutable.Map[InetSocketAddress, PeerInfo]()

  private val blacklist = mutable.Map[String, NetworkTime.Time]()

  override def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    val updatedPeerInfo = whitelistPersistence.get(address).fold(peerInfo) { dbPeerInfo =>
      val nodeNameOpt = peerInfo.nodeName orElse dbPeerInfo.nodeName
      val connTypeOpt = peerInfo.connectionType orElse dbPeerInfo.connectionType
      PeerInfo(peerInfo.lastSeen, nodeNameOpt, connTypeOpt)
    }
    whitelistPersistence.put(address, updatedPeerInfo)
  }

  override def blacklistPeer(address: InetSocketAddress, time: NetworkTime.Time): Unit = {
    whitelistPersistence.remove(address)
    if (!isBlacklisted(address)) blacklist += address.getHostName -> time
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.synchronized(blacklist.contains(address.getHostName))
  }

  override def knownPeers(): Map[InetSocketAddress, PeerInfo] = {
    whitelistPersistence.keys.flatMap(k => whitelistPersistence.get(k).map(v => k -> v)).toMap
  }

  override def blacklistedPeers(): Seq[String] = blacklist.keys.toSeq

  override def isEmpty(): Boolean = whitelistPersistence.isEmpty

  override def remove(address: InetSocketAddress): Boolean = whitelistPersistence.remove(address).nonEmpty

  //从whitelist里随机选出number个peer
  override def selectPeersByRandom(number: Long): Seq[InetSocketAddress] = {

    val allSeq = whitelistPersistence.keys.toSeq
    if (allSeq.size < number)
      return allSeq

    var ret = Seq[InetSocketAddress]()
    val size: Int = whitelistPersistence.size
    var rand: Int = Random.nextInt(size)

    while (ret.size < number && rand < size) {
      ret = ret :+ allSeq(rand)
      rand = rand + 1
    }

    rand =0
    while(ret.size < number){
      ret = ret :+ allSeq(rand)
      rand = rand + 1
    }

    ret
  }

  override def peerSize():Int ={ whitelistPersistence.size }

}
