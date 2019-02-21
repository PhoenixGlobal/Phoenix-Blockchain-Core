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
import com.typesafe.config.{Config, ConfigFactory}
import net.sourceforge.argparse4j.ArgumentParsers
import net.sourceforge.argparse4j.inf.{ArgumentParser, ArgumentParserException, Namespace}

import scala.concurrent.ExecutionContext

object MainEntry extends ApexLogging {

  def main(args: Array[String]): Unit = {
    val ns = parseArgs(args)
    val (settings, config) = getApexSettings(ns)
    println(settings.chain.genesis.timeStamp.toEpochMilli)
    println(java.time.Instant.now.toEpochMilli)

    implicit val system = ActorSystem("APEX-NETWORK", config)
    implicit val executionContext: ExecutionContext = system.dispatcher

    val node = NodeRef(settings)

    //temp
    while (true) {
      Thread.sleep(50000000)
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

  private def getApexSettings(ns: Namespace): (ApexSettings, Config) = {
    //val digest: MessageDigest = null
    val files = ns.getList[String]("configFile")
    if (files.size() > 0) {
      val conf = files.toArray().head.toString
      getConfig(conf)
    }
    else
      getConfig()

  }

  private def getConfig(file: String = "settings.conf"): (ApexSettings, Config) = {
    if (file.isEmpty) {
      val defaultConf = "src/main/resources/settings.conf"
      ApexSettings.read(defaultConf)
    }
    else
      ApexSettings.read(file)
  }
}
