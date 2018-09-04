/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: MainEntry.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-8-27 下午7:56@version: 1.0
 */

package com.apex

import akka.actor.ActorSystem
import com.apex.common.ApexLogging
import com.apex.consensus.ProducerRef
import com.apex.core.Blockchain
import com.apex.network.peer.PeerHandlerManagerRef
import com.apex.network.rpc.RpcServer
import com.apex.network.upnp.UPnP
import com.apex.network.{NetworkManagerRef, NodeRef}
import com.apex.settings.ApexSettings
import com.apex.utils.NetworkTimeProvider
import net.sourceforge.argparse4j.ArgumentParsers
import net.sourceforge.argparse4j.inf.{ArgumentParser, ArgumentParserException, Namespace}

import scala.concurrent.ExecutionContext
import scala.io.StdIn


object MainEntry extends ApexLogging{

  def main(args: Array[String]): Unit = {

    val ns = parseArgs(args)
    val settings = getApexSettings(ns)
    val chain = Blockchain.populate(settings.chain)

//    Wallet.importPrivKeyFromWIF("Kx45GeUBSMPReYQwgXiKhG9FzNXrnCeutJp4yjTd5kKxCitadm3C")
    //val tx = Wallet.makeTransaction("APQKUqPcJEUwRdwoxpoGQnkrRGstSXkgebk", UInt256.Zero, new Fixed8(230000L)).get

    //LocalNode.default.addTransaction(tx)
    //    val block2 = Blockchain.Current.produceBlock(LocalNode.default.getMemoryPool())

    implicit val system = ActorSystem("APEX-NETWORK")
    implicit val executionContext: ExecutionContext = system.dispatcher

    //p2p
    val upnp = new UPnP(settings.network)

    val timeProvider = new NetworkTimeProvider(settings.ntp)
//    val node = LocalNode.launch(settings, timeProvider)

    val peerManagerRef = PeerHandlerManagerRef(settings, timeProvider)
    val nodeRef = NodeRef(chain, peerManagerRef)
    val producer = ProducerRef(settings.consensus, chain, peerManagerRef)
//    val rpcRef = RpcServerRef(settings.rpcSettings, nodeRef)
    val networkControllerRef = NetworkManagerRef(settings.network, upnp, timeProvider, peerManagerRef, nodeRef)
//    Node.beginProduce(nodeRef, settings.consensus)
//    val task = node.beginProduce(Genesis.config)
    RpcServer.run(settings.rpc, nodeRef, producer)
    //    producer.wait()
    //    val block3 = Blockchain.Current.produceBlock(Seq.empty)

    //    val block = new Block()
    //
    //    System.out.print("Hello BlockChain: " + block.getClass.toString)
    //    System.out.println(" #" + block.header.index)
    System.out.println("Press RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
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

  private def getApexSettings(ns: Namespace): ApexSettings = {
//    val digest: MessageDigest = null
    val files = ns.getList[String]("configFile")
    if(files.size() > 0){
      val conf = files.toArray().head.toString
      getConfig(conf)
    }
    else getConfig()

  }

  private def getConfig(file: String = "settings.conf"): ApexSettings ={
    if(file.isEmpty){
      val defaultConf = "src/main/resources/settings.conf"
      return ApexSettings.read(defaultConf)
    }
    ApexSettings.read(file)
  }
}
