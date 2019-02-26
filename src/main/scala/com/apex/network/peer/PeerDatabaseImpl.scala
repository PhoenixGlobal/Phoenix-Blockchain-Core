package com.apex.network.peer

import java.io.{File, PrintWriter}
import java.net.InetSocketAddress

import com.apex.utils.NetworkTime
import scala.collection.mutable
import com.apex.common.ApexLogging
import com.apex.network.NodeInfo

import scala.io.Source
import scala.util.Random


class PeerDatabaseImpl(filename: String) extends PeerDatabase with ApexLogging {

  /**
    * key: raw IP address
    * value: NodeInfo class
    */
  private val whitelistPersistence = mutable.Map[String, NodeInfo]()

  private val blacklist = mutable.Map[String, NetworkTime.Time]()


  def flush2DB(): Unit = {
    val file = new File(filename)
    if (!file.exists() && !file.isDirectory()) {
      file.mkdir()
    }
    val whileListFile = filename + "/white_list.txt"
    val fw = new PrintWriter(whileListFile)
    whitelistPersistence.values.foreach(node => {
      fw.println(node.address + ":" + node.port)
    })
    log.info("write while list into file " + whileListFile)
    fw.close()

    val blackListFile = filename + "/black_list.txt"

    val fw2 = new PrintWriter(blackListFile)
    blacklist.keys.foreach(address => {
      fw2.println(address)
    })

    fw2.close()
    log.info("write black list into file " + blackListFile)
  }

  def loadFromDB(): Unit = {
    val whileListFile = filename + "/white_list.txt"
    val wlf = new File(whileListFile)
    if (!wlf.exists()) {
      return
    }
    val file = Source.fromFile(whileListFile)
    val it = file.getLines
    while (it.hasNext) {
      val line: Array[String] = it.next().toString.split(":")
      addOrUpdateKnownPeer(line(0), new NodeInfo(line(0), line(1).toInt))
    }

    val blackFilePath = filename + "/black_list.txt"
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

  override def addOrUpdateKnownPeer(address: String, peerInfo: NodeInfo): Unit = {
    whitelistPersistence.put(address, peerInfo)
  }

  def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: NodeInfo): Unit = {
    addOrUpdateKnownPeer(address.getAddress.getHostAddress, peerInfo)
  }

  override def addBlacklistPeer(address: InetSocketAddress, time: NetworkTime.Time): Unit = {
    whitelistPersistence.remove(address.getAddress.getHostAddress)
    if (!isBlacklisted(address)) blacklist += address.getAddress.getHostAddress -> time
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.synchronized(blacklist.contains(address.getAddress.getHostAddress))
  }

  override def knownPeers(): Map[String, NodeInfo] = {
    whitelistPersistence.keys.flatMap(k => whitelistPersistence.get(k).map(v => k -> v)).toMap
  }

  override def blacklistedPeers(): Seq[String] = blacklist.keys.toSeq

  override def isEmpty(): Boolean = whitelistPersistence.isEmpty

  override def remove(address: InetSocketAddress): Boolean = whitelistPersistence.remove(address.getAddress.getHostAddress).nonEmpty

  //从whitelist里随机选出number个peer
  override def selectPeersByRandom(number: Long): Seq[NodeInfo] = {

    val allSeq = whitelistPersistence.values.toSeq
    if (allSeq.size < number)
      return allSeq

    var ret = Seq[NodeInfo]()
    val size: Int = whitelistPersistence.size
    var rand: Int = Random.nextInt(size)

    while (ret.size < number && rand < size) {
      ret = ret :+ allSeq(rand)
      rand = rand + 1
    }

    rand = 0
    while (ret.size < number) {
      ret = ret :+ allSeq(rand)
      rand = rand + 1
    }

    ret
  }

  override def peerSize(): Int = {
    whitelistPersistence.size
  }

}
