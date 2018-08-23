/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Genesis.scala
 *
 * @author: shan.huang@chinapex.com: 2018-08-15 下午4:06@version: 1.0
 */

package com.apex.consensus

import java.io.File

import com.apex.crypto.BinaryData
import com.apex.crypto.Ecdsa.{Point, PrivateKey, PublicKey}
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader

//case class ConsesusConfig(produceInterval: Int,
//                          acceptableTimeError: Int,
//                          initialWitness: Array[Witness])
//
//case class Witness(name: String,
//                   pubkey: PublicKey,
//                   privkey: Option[PrivateKey])

//object Consesus {
//
//  implicit val publicKeyReader: ValueReader[PublicKey] = (cfg, path) => new PublicKey(Point(cfg.getString(path)))
//  implicit val privateKeyReader: ValueReader[PrivateKey] = (cfg, path) => new PrivateKey(BinaryData(cfg.getString(path)))
//
//  private def parseConfig(): ConsesusConfig = {
//
//    // FIXME: file path
//    val cfg = ConfigFactory.parseFile(new File("src/main/resources/genesis.conf"))
//
//    val config = cfg.as[ConsesusConfig]("genesisConfig")
//
//    config
//  }
//
//  final val config = parseConfig()
//}


