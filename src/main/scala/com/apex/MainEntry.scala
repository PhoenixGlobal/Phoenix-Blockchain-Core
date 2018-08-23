package com.apex

import akka.actor.ActorSystem
import com.apex.core.settings.ApexSettings
import com.apex.core.utils.NetworkTimeProvider
import com.apex.main.HybridSettings
import com.apex.network.peer.PeerHandlerManagerRef
import com.apex.network.rpc.RpcServer
import com.apex.network.upnp.UPnP
import com.apex.network.{LocalNode, LocalNodeRef, NetworkManagerRef}
import com.apex.wallets.Wallet

import scala.concurrent.ExecutionContext
import scala.io.StdIn


object MainEntry {

  def main(args: Array[String]): Unit = {

    //    val block1 = Blockchain.Current.produceBlock(Seq.empty)

    Wallet.importPrivKeyFromWIF("Kx45GeUBSMPReYQwgXiKhG9FzNXrnCeutJp4yjTd5kKxCitadm3C")
    //val tx = Wallet.makeTransaction("APQKUqPcJEUwRdwoxpoGQnkrRGstSXkgebk", UInt256.Zero, new Fixed8(230000L)).get

    //LocalNode.default.addTransaction(tx)
    //    val block2 = Blockchain.Current.produceBlock(LocalNode.default.getMemoryPool())
    val settingsFilename = args.headOption.getOrElse("settings.conf")
    val hybridSettings = HybridSettings.read(Some(settingsFilename))
    val settings: ApexSettings = hybridSettings.apexSettings

    implicit val actorSystem = ActorSystem(settings.network.agentName)
    implicit val executionContext: ExecutionContext = actorSystem.dispatchers.lookup("apex.executionContext")

    //p2p
    val upnp = new UPnP(settings.network)

    val timeProvider = new NetworkTimeProvider(settings.ntp)
//    val node = LocalNode.launch(settings, timeProvider)

    val peerManagerRef = PeerHandlerManagerRef(settings, timeProvider)
    val localNodeRef = LocalNodeRef(peerManagerRef)
    val networkControllerRef = NetworkManagerRef(settings.network, upnp, timeProvider, peerManagerRef, localNodeRef)
    LocalNode.beginProduce(localNodeRef, settings.genesisConfig)
//    val task = node.beginProduce(Genesis.config)
    //    producer.wait()
    //    val block3 = Blockchain.Current.produceBlock(Seq.empty)

    RpcServer.run()

    //    val block = new Block()
    //
    //    System.out.print("Hello BlockChain: " + block.getClass.toString)
    //    System.out.println(" #" + block.header.index)
    System.out.println("Press RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    RpcServer.stop() //System.out.println("main end...")
  }
}
