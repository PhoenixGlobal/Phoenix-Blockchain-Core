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

  private val knowPeersFileName = filename + "/" + "knowPeers.txt"
  private val blackListFileName = filename + "/" + "blackListPeers.txt"

  def flush2DB(): Unit = {
    try {
      val file = new File(filename)
      if (!file.exists() && !file.isDirectory()) {
        file.mkdir()
      }

      val fw = new PrintWriter(knowPeersFileName)
      knownPeersMap.values.foreach(node => {
        fw.println(node.address + ":" + node.port + ":" + node.nodeType.toByte)
      })
      log.info("write peers into file " + knowPeersFileName)
      fw.close()

      val fw2 = new PrintWriter(blackListFileName)
      blacklist.keys.foreach(address => {
        fw2.println(address)
      })

      fw2.close()
      log.info("write black list into file " + blackListFileName)
    } catch {
      case e: Exception =>
        log.info(s"cann't write ${knowPeersFileName},${blackListFileName}")
        e.printStackTrace()
    }
  }

  def loadFromDB(): Unit = {
    try {
      val knowPeers = loadFromFile(knowPeersFileName)
      knowPeers.foreach { lineStr =>
        val line: Array[String] = lineStr.toString.split(":")
        addOrUpdateKnownPeer(new InetSocketAddress(line(0), line(1).toInt), new NodeInfo(line(0), line(1).toInt, NodeType(line(2).toInt)))
      }

      val blackLists = loadFromFile(blackListFileName)
      blackLists.foreach { lineStr =>
        val line: Array[String] = lineStr.toString.split(":")
        val address = new InetSocketAddress(line(0), 0)
        addBlacklistPeer(address, System.currentTimeMillis())
      }
    } catch {
      case e: Exception =>
        log.error(e.getMessage)
        e.printStackTrace().toString
    }
  }

  private def loadFromFile(path: String): Seq[String] = {
    try {
      val bfp = new File(path)
      if (!bfp.exists())
        Seq()
      else {
        val source = Source.fromFile(path)
        val lines = source.getLines.toList
        source.close()
        lines
      }
    } catch {
      case e: Exception =>
        log.error(s"init failed, cannot read from ${path}.")
        e.printStackTrace()
        Seq()
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
