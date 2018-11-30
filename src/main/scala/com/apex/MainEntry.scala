/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: MainEntry.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-8-27 下午7:56@version: 1.0
 */

package com.apex

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.util.Timeout
import com.apex.common.ApexLogging
import com.apex.network.NodeRef
import com.apex.network.rpc.RpcServer
import com.apex.settings.ApexSettings
import net.sourceforge.argparse4j.ArgumentParsers
import net.sourceforge.argparse4j.inf.{ArgumentParser, ArgumentParserException, Namespace}

import scala.concurrent.ExecutionContext
import scala.io.StdIn


object MainEntry extends ApexLogging {

  def main(args: Array[String]): Unit = {
    val code = Array[Byte](96, 96, 96, 64, 82, 96, 4, 54, 16, 97, 0, 98, 87, 96, 0, 53, 124, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -112, 4, 99, -1, -1, -1, -1, 22, -128, 99, 44, 4, 71, 121, 20, 97, 0, 103, 87, -128, 99, 69, 112, -108, -52, 20, 97, 0, -68, 87, -128, 99, 78, 112, -79, -36, 20, 97, 0, -47, 87, -128, 99, 96, -2, 71, -79, 20, 97, 0, -6, 87, 91, 96, 0, -128, -3, 91, 52, 21, 97, 0, 114, 87, 96, 0, -128, -3, 91, 97, 0, 122, 97, 1, 29, 86, 91, 96, 64, 81, -128, -126, 115, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 22, 115, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 22, -127, 82, 96, 32, 1, -111, 80, 80, 96, 64, 81, -128, -111, 3, -112, -13, 91, 52, 21, 97, 0, -57, 87, 96, 0, -128, -3, 91, 97, 0, -49, 97, 1, 37, 86, 91, 0, 91, 52, 21, 97, 0, -36, 87, 96, 0, -128, -3, 91, 97, 0, -28, 97, 1, -113, 86, 91, 96, 64, 81, -128, -126, -127, 82, 96, 32, 1, -111, 80, 80, 96, 64, 81, -128, -111, 3, -112, -13, 91, 52, 21, 97, 1, 5, 87, 96, 0, -128, -3, 91, 97, 1, 27, 96, 4, -128, -128, 53, -112, 96, 32, 1, -112, -111, -112, 80, 80, 97, 1, -107, 86, 91, 0, 91, 96, 0, 51, -112, 80, -112, 86, 91, 127, 5, -57, 102, -47, -59, -22, 111, 64, -81, -61, -116, -40, -30, 115, 8, -62, 54, -60, -110, -5, -49, -93, 43, 69, -115, 39, 85, -49, 118, -20, 30, 33, 96, 64, 81, -128, -128, 96, 32, 1, -126, -127, 3, -126, 82, 96, 4, -127, 82, 96, 32, 1, -128, 127, 102, 105, 114, 101, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -127, 82, 80, 96, 32, 1, -111, 80, 80, 96, 64, 81, -128, -111, 3, -112, -95, 86, 91, 96, 0, 84, -127, 86, 91, -128, 96, 0, -127, -112, 85, 80, 97, 34, 34, 96, 1, 2, 97, 17, 17, 96, 64, 81, -128, -126, 96, 1, 2, 96, 0, 25, 22, -127, 82, 96, 32, 1, -111, 80, 80, 96, 64, 81, -128, -111, 3, -112, -95, 80, 86, 0, -95, 101, 98, 122, 122, 114, 48, 88, 32, -121, -35, 34, 93, 122, -19, 95, -92, -1, -29, 108, -128, -54, -55, -79, -52, 23, 125, -42, 54, 38, -119, -44, -87, 61, -57, -70, 101, -45, 77, 11, -73, 0, 41)

    val ns = parseArgs(args)
    val settings = getApexSettings(ns)
    println(settings.chain.genesis.timeStamp.toEpochMilli)
    println(java.time.Instant.now.toEpochMilli)

    implicit val system = ActorSystem("APEX-NETWORK")
    implicit val executionContext: ExecutionContext = system.dispatcher

    val node = NodeRef(settings)

    if (settings.rpc.enabled) {
      RpcServer.run(settings.rpc, node)
    }

    //temp
    while (true) {
      Thread.sleep(50000000)
    }

    if (settings.rpc.enabled) {
      // stopping rpc server
      RpcServer.stop()
    }

    //  stopping node
    implicit val timeout = Timeout(30, TimeUnit.MICROSECONDS)
    system.terminate.foreach(_ => log.info("quit"))
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
      ApexSettings.read(defaultConf)
    }
    else
      ApexSettings.read(file)
  }
}
