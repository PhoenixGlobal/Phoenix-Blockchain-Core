package com.apex.settings

import java.io.{ByteArrayOutputStream, DataOutputStream, File}
import java.net.InetSocketAddress
import java.text.SimpleDateFormat
import java.time.Instant
import java.util.TimeZone

import com.apex.common.ApexLogging
import com.apex.crypto.BinaryData
import com.apex.crypto.Crypto.hash256
import com.apex.crypto.Ecdsa.{Point, PrivateKey, PublicKey}
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader

import scala.concurrent.duration._

case class RPCSettings(enabled: Boolean, host: String, port: Int)

case class NetworkSettings(nodeName: String,
                           addedMaxDelay: Option[FiniteDuration],
                           localOnly: Boolean,
                           peersDB: String,
                           knownPeers: Seq[InetSocketAddress],
                           bindAddress: InetSocketAddress,
                           maxConnections: Int,
                           connectionTimeout: FiniteDuration,
                           upnpEnabled: Boolean,
                           upnpGatewayTimeout: Option[FiniteDuration],
                           upnpDiscoverTimeout: Option[FiniteDuration],
                           declaredAddress: Option[InetSocketAddress],
                           handshakeTimeout: FiniteDuration,
                           appVersion: String,
                           agentName: String,
                           maxPacketSize: Int,
                           controllerTimeout: Option[FiniteDuration],
                           peerMaxTimeGap: Int)

case class NetworkTimeProviderSettings(server: String, updateEvery: FiniteDuration, timeout: FiniteDuration)


case class ApexSettings(network: NetworkSettings,
                        ntp: NetworkTimeProviderSettings,
                        consensus: ConsensusSettings,
                        chain: ChainSettings,
                        rpc: RPCSettings)

case class BlockBaseSettings(dir: String, cacheEnabled: Boolean, cacheSize: Int)

case class DataBaseSettings(dir: String, cacheEnabled: Boolean, cacheSize: Int)

case class ForkBaseSettings(dir: String, cacheEnabled: Boolean, cacheSize: Int)

case class GenesisSettings(timeStamp: Instant,
                           publicKey: String,
                           privateKey: String,
                           genesisCoinAirdrop: Array[CoinAirdrop])

case class ChainSettings(blockBase: BlockBaseSettings,
                         dataBase: DataBaseSettings,
                         forkBase: ForkBaseSettings,
                         minerAward: Double,
                         genesis: GenesisSettings)

case class ConsensusSettings(produceInterval: Int,
                             acceptableTimeError: Int,
                             producerRepetitions: Int,
                             initialWitness: Array[Witness]) {
  def fingerprint(): BinaryData = {
    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    os.writeInt(produceInterval)
    os.writeInt(acceptableTimeError)
    os.writeInt(producerRepetitions)
    initialWitness.foreach(w => {
      os.writeBytes(w.name)
      os.writeBytes(w.pubkey.toString)
      // do not include privkey
    })
    hash256(bs.toByteArray)
  }
}

case class CoinAirdrop(addr: String,
                      coins: Double)

case class Witness(name: String,
                   pubkey: PublicKey,
                   privkey: Option[PrivateKey])

object ApexSettings extends SettingsReaders with ApexLogging {
  protected val configPath: String = "apex"
  //
  //  implicit val valueReader: ValueReader[ApexSettings] =
  //    (cfg: Config, path: String) => cfg.as[ApexSettings](path)


  implicit val publicKeyReader: ValueReader[PublicKey] = (cfg, path) => new PublicKey(Point(cfg.getString(path)))

  implicit val privateKeyReader: ValueReader[PrivateKey] = (cfg, path) => new PrivateKey(BinaryData(cfg.getString(path)))

  implicit val dateReader: ValueReader[Instant] = (cfg, path) => {
    val dateStr = cfg.getString(path)
    val fmt = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    fmt.setTimeZone(TimeZone.getTimeZone("UTC"))
    fmt.parse(dateStr).toInstant
  }

  def read(configFilePath: String): ApexSettings = {
    val conf = readConfigFromPath(Some(configFilePath), configPath)
    conf.as[ApexSettings](configPath)
  }

  def readConfigFromPath(userConfigPath: Option[String], configPath: String): Config = {

    val maybeConfigFile: Option[File] = userConfigPath.map(filename => new File(filename)).filter(_.exists())
      .orElse(userConfigPath.flatMap(filename => Option(getClass.getClassLoader.getResource(filename))).
        map(r => new File(r.toURI)).filter(_.exists()))

    val config = maybeConfigFile match {
      case None =>
        log.warn("NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS FOR TESTNET!")
        ConfigFactory.load()
      case Some(file) =>
        val cfg = ConfigFactory.parseFile(file)
        if (!cfg.hasPath(configPath)) {
          throw new Error("Malformed configuration file was provided! Aborting!")
        }
        ConfigFactory
          .defaultOverrides()
          .withFallback(cfg) // 
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference()) // 加载"src/main/resources/reference.conf"
          .resolve()
    }

    config
  }
}