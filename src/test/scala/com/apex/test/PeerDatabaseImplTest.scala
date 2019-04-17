package com.apex.test

import java.net.InetSocketAddress

import com.apex.network.NodeInfo
import com.apex.network.peer.PeerDatabaseImpl
import org.junit.Test

@Test
class PeerDatabaseImplTest {

  @Test
  def addOrUpdateKnownPeer() {
    val peerDatabase = new PeerDatabaseImpl("test")

    val node = new NodeInfo("localhost", 1000)
    val address =new InetSocketAddress("localhost",1000)
    peerDatabase.addOrUpdateKnownPeer(address, node)

    val kn = peerDatabase.knownPeers()
    val node2 = kn.get(address).get
    assert(node == node2)
  }

  @Test
  def blacklistedPeers(): Unit = {
    val peerDatabase = new PeerDatabaseImpl("test")
    val address = new InetSocketAddress("localhost", 1000)
    peerDatabase.addOrUpdateKnownPeer(address, new NodeInfo("localhost", 1000))
    assert(peerDatabase.knownPeers().size == 1)

    peerDatabase.addBlacklistPeer(address, System.currentTimeMillis())
    assert(peerDatabase.knownPeers().size == 0)
    assert(peerDatabase.isBlacklisted(address))
  }

  @Test
  def remove(): Unit = {
    val peerDatabase = new PeerDatabaseImpl("test")
    val address = new InetSocketAddress("localhost", 1000)

    val node = new NodeInfo("localhost", 1000)
    peerDatabase.addOrUpdateKnownPeer(address, node)
    assert(peerDatabase.knownPeers().size == 1)

    peerDatabase.remove(address)

    val kn = peerDatabase.knownPeers()
    val node2 = kn.get(address)
    assert(node2 == None)
  }

}
