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
import com.apex.consensus.{ProducerRef, ProducerStopMessage}
import com.apex.core.Blockchain
import com.apex.network.peer.PeerHandlerManagerRef
import com.apex.network.rpc.RpcServer
import com.apex.network.upnp.UPnP
import com.apex.network.{NetworkManagerRef, NodeRef, NodeStopMessage}
import com.apex.settings.{ApexSettings, GlobalConfig}
import com.apex.utils.NetworkTimeProvider
import net.sourceforge.argparse4j.ArgumentParsers
import net.sourceforge.argparse4j.inf.{ArgumentParser, ArgumentParserException, Namespace}

import scala.concurrent.ExecutionContext
import scala.io.StdIn


object MainEntry extends ApexLogging {

  def main(args: Array[String]): Unit = {
    val ns = parseArgs(args)
    val settings = getApexSettings(ns)

    implicit val system = ActorSystem("APEX-NETWORK")
    implicit val executionContext: ExecutionContext = system.dispatcher

    val node = NodeRef(settings)

    if (settings.rpc.enabled) {
      RpcServer.run(settings.rpc, node)
    }

    System.out.println("Press RETURN to stop...")
    StdIn.readLine() // let it run until user presses return

    if (settings.rpc.enabled) {
      log.info("stopping rpc server")
      RpcServer.stop()
    }

    log.info("stopping node")
    node ! NodeStopMessage()
    // TODO: close network ...
    Thread.sleep(1000) // TODO
    log.info("quit")
    System.exit(0)
  }

  private def parseArgs(args: Array[String]): Namespace = {
    val parser: ArgumentParser = ArgumentParsers.newFor("MainEntry").build().defaultHelp(true)
      .description("check cli params")
    parser.addArgument("configFile").nargs("*").help("files for configuration")
    var ns: Namespace = null
    try {
      ns = parser.parseArgs(args)
    }
    catch {
      case e: ArgumentParserException => {
        parser.handleError(e)
        System.exit(1)
      }
    }
    ns
  }

  private def getApexSettings(ns: Namespace): ApexSettings = {
    //val digest: MessageDigest = null
    val files = ns.getList[String]("configFile")
    if (files.size() > 0) {
      val conf = files.toArray().head.toString
      getConfig(conf)
    }
    else getConfig()

  }

  private def getConfig(file: String = "settings.conf"): ApexSettings = {
    if (file.isEmpty) {
      val defaultConf = "src/main/resources/settings.conf"
      return ApexSettings.read(defaultConf)
    }
    ApexSettings.read(file)
  }
}
