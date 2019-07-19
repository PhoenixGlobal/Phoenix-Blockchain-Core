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
import com.apex.settings.ApexSettings
import com.typesafe.config.Config
import net.sourceforge.argparse4j.ArgumentParsers
import net.sourceforge.argparse4j.inf.{ArgumentParser, ArgumentParserException, Namespace}

import scala.concurrent.ExecutionContext

object MainEntry extends ApexLogging {

  import java.lang.management.ManagementFactory
  import java.lang.management.RuntimeMXBean

  private def getProcessID: Int = {
    val runtimeMXBean = ManagementFactory.getRuntimeMXBean
    System.out.println(runtimeMXBean.getName)
    Integer.valueOf(runtimeMXBean.getName.split("@")(0)).intValue
  }

  def main(args: Array[String]): Unit = {

    //val pid: Long = Seq("sh", "-c", "echo $PPID").!!.trim.toLong

    log.info(s"version=2019.07.19.D   main pid ${getProcessID}")

    Thread.setDefaultUncaughtExceptionHandler((t, e) => {
      log.error(s"Thread [${t.getId}], there is an unhandled exception", e)
    })

    val (settings, config) = getApexSettings(args)
    log.debug(s"genesis: ${settings.chain.genesis.timeStamp.toEpochMilli}, now: ${java.time.Instant.now.toEpochMilli}")

    implicit val system = ActorSystem("APEX-NETWORK", config)
    implicit val executionContext: ExecutionContext = system.dispatcher
    system.registerOnTermination({
      log.info("node terminated")
      System.exit(0)
    })

    NodeRef(settings, config)
    //log.info("main() end")
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

  private def getApexSettings(args: Array[String]): (ApexSettings, Config) = {
    //val digest: MessageDigest = null
    val ns = parseArgs(args)
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
