package com.apex.network.peer

import java.io.{File, PrintWriter}
import java.net.InetSocketAddress

import com.apex.utils.NetworkTime

import scala.collection.mutable
import com.apex.common.ApexLogging
import com.apex.network.{NodeInfo, NodeType}

import scala.io.Source
import scala.util.Random


class PeerDatabaseImpl(filename: String) extends PeerDatabase with ApexLogging {

  /**
    * key: raw IP address
    * value: NodeInfo class
    */
  private val knownPeersMap = mutable.Map[InetSocketAddress, NodeInfo]()

  private val blacklist = mutable.Map[String, NetworkTime.Time]()


  def flush2DB(): Unit = {
    val file = new File(filename)
    if (!file.exists() && !file.isDirectory()) {
      file.mkdir()
    }
    val whileListFile = filename + "/knowPeers.txt"
    val fw = new PrintWriter(whileListFile)
    knownPeersMap.values.foreach(node => {
      fw.println(node.address + ":" + node.port + ":" + node.nodeType.toByte)
    })
    log.info("write while list into file " + whileListFile)
    fw.close()

    val blackListFile = filename + "/blackListPeers.txt"

    val fw2 = new PrintWriter(blackListFile)
    blacklist.keys.foreach(address => {
      fw2.println(address)
    })

    fw2.close()
    log.info("write black list into file " + blackListFile)
  }

  def loadFromDB(): Unit = {
    val whileListFile = filename + "/knowPeers.txt"
    val wlf = new File(whileListFile)
    if (!wlf.exists()) {
      return
    }
    val file = Source.fromFile(whileListFile)
    val it = file.getLines
    while (it.hasNext) {
      val line: Array[String] = it.next().toString.split(":")
      addOrUpdateKnownPeer(new InetSocketAddress(line(0), line(1).toInt), new NodeInfo(line(0), line(1).toInt, NodeType(line(2).toInt)))
    }

    val blackFilePath = filename + "/blackListPeers.txt"
    val bfp = new File(blackFilePath)
    if (!bfp.exists()) {
      return
    }
    val bfile = Source.fromFile(blackFilePath)
    val it2 = bfile.getLines
    while (it2.hasNext) {
      val line: Array[String] = it2.next().toString.split(":")
      val address = new InetSocketAddress(line(0), line(1).toInt)
      addBlacklistPeer(address, System.currentTimeMillis())
    }
  }

  def addPeerIfEmpty(address: InetSocketAddress, peerInfo: NodeInfo): Unit = {
    if (!knownPeersMap.contains(address))
      knownPeersMap.put(address, peerInfo)
  }

  def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: NodeInfo): Unit = {
    knownPeersMap.put(address, peerInfo)
  }

  override def addBlacklistPeer(address: InetSocketAddress, time: NetworkTime.Time): Unit = {
    knownPeersMap.remove(address)
    if (!isBlacklisted(address)) blacklist += address.getAddress.getHostAddress -> time
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.synchronized(blacklist.contains(address.getAddress.getHostAddress))
  }

  override def knownPeers(): Map[InetSocketAddress, NodeInfo] = {
    knownPeersMap.keys.flatMap(k => knownPeersMap.get(k).map(v => k -> v)).toMap
  }

  override def blacklistedPeers(): Seq[String] = blacklist.keys.toSeq

  override def isEmpty(): Boolean = knownPeersMap.isEmpty

  override def remove(address: InetSocketAddress): Boolean = {
    log.info("remove peer:" + address)
    knownPeersMap.remove(address).nonEmpty
  }


  //从whitelist里随机选出number个peer
  override def selectPeersByRandom(number: Int): Seq[NodeInfo] = {

    val allSeq = knownPeersMap.values.toSeq
    if (allSeq.size < number)
      return allSeq

    val ret = Random.shuffle(allSeq).take(number)

    ret
  }

  override def peerSize(): Int = {
    knownPeersMap.size
  }

}
