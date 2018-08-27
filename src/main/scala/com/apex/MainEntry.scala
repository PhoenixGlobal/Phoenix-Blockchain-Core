package com.apex

import java.nio.file.{Path, Paths}
import java.security.MessageDigest

import akka.actor.ActorSystem
import com.apex.common.ApexLogging
import com.apex.core.settings.ApexSettings
import com.apex.core.utils.NetworkTimeProvider
import com.apex.main.HybridSettings
import com.apex.network.peer.PeerHandlerManagerRef
import com.apex.network.rpc.RpcServer
import com.apex.network.upnp.UPnP
import com.apex.network.{LocalNode, LocalNodeRef, NetworkManagerRef}
import com.apex.wallets.Wallet
import net.sourceforge.argparse4j.ArgumentParsers
import net.sourceforge.argparse4j.inf.{ArgumentParser, ArgumentParserException, Namespace}

import scala.concurrent.ExecutionContext
import scala.io.StdIn


object MainEntry extends ApexLogging{

  def main(args: Array[String]): Unit = {

    val ns = parseArgs(args)
    val hybridSettings = getApexSettings(ns)
    val settings: ApexSettings = hybridSettings.apexSettings
    //    val block1 = Blockchain.Current.produceBlock(Seq.empty)

    Wallet.importPrivKeyFromWIF("Kx45GeUBSMPReYQwgXiKhG9FzNXrnCeutJp4yjTd5kKxCitadm3C")
    //val tx = Wallet.makeTransaction("APQKUqPcJEUwRdwoxpoGQnkrRGstSXkgebk", UInt256.Zero, new Fixed8(230000L)).get

    //LocalNode.default.addTransaction(tx)
    //    val block2 = Blockchain.Current.produceBlock(LocalNode.default.getMemoryPool())

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

  private def parseArgs(args: Array[String]): Namespace ={
    val parser: ArgumentParser  = ArgumentParsers.newFor("MainEntry").build().defaultHelp(true)
      .description("check cli params")
    parser.addArgument("configFile").nargs("*").help("files for configuration")
    var ns: Namespace = null
    try{
      ns = parser.parseArgs(args)
    }
    catch{
      case e: ArgumentParserException => {
        parser.handleError(e)
        System.exit(1)
      }
    }
    ns
  }

  private def getApexSettings(ns: Namespace): HybridSettings = {
    val digest: MessageDigest = null
    val files = ns.getList[String]("configFile")
    if(files.size() > 0){
      val conf = files.toArray().head.toString
      getConfig(conf)
    }
    else getConfig()

  }

  private def getConfig(file: String = ""): HybridSettings ={
    if(file.isEmpty){
      val defaultConf = "src/main/resources/settings.conf"
      return HybridSettings.read(Some(defaultConf))
    }
    HybridSettings.read(Some(file))
  }
}
