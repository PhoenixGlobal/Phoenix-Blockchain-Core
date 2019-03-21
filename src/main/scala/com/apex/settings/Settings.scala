package com.apex.settings

import java.io.{ByteArrayOutputStream, DataOutputStream, File}
import java.net.InetSocketAddress
import java.text.SimpleDateFormat
import java.time.Instant
import java.util.TimeZone

import com.apex.common.ApexLogging
import com.apex.crypto.{BinaryData, FixedNumber, UInt160}
import com.apex.crypto.Crypto.hash256
import com.apex.crypto.Ecdsa.{Point, PrivateKey, PublicKey}
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader

import scala.concurrent.duration._

case class ContractSettings(dumpBlock: Long,
                            vmTrace: Boolean,
                            maxContractSize: Int,
                            registerSpend: FixedNumber = FixedNumber.One,
                            refundDelay: Long = 24 * 60 * 60 * 1000)

case class RPCSettings(enabled: Boolean, host: String, port: Int)

case class SecretRPCSettings(enabled: Boolean, host: String, port: Int)

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
                           peerMaxTimeGap: Int,
                           peerSyncNumber: Int,
                           peerDatabaseMax: Int)

case class NetworkTimeProviderSettings(server: String, updateEvery: FiniteDuration, timeout: FiniteDuration)


case class ApexSettings(network: NetworkSettings,
                        ntp: NetworkTimeProviderSettings,
                        chain: ChainSettings,
                        rpc: RPCSettings,
                        secretRpc: SecretRPCSettings,
                        consensus: ConsensusSettings,
                        miner: MinerSettings,
                        plugins: PluginsSettings,
                        runtimeParas: RuntimeParas)


object DBType extends Enumeration {
  val Memory = Value(0)
  val LevelDB = Value(1)
  val RocksDB = Value(2)
}

case class ScheduleBaseSettings(dir: String, cacheEnabled: Boolean, cacheSize: Int, dbType: DBType.Value)

case class BlockBaseSettings(dir: String, cacheEnabled: Boolean, cacheSize: Int, dbType: DBType.Value)

case class DataBaseSettings(dir: String, cacheEnabled: Boolean, cacheSize: Int, dbType: DBType.Value)

case class ForkBaseSettings(dir: String, cacheEnabled: Boolean, cacheSize: Int, dbType: DBType.Value)

case class GenesisSettings(timeStamp: Instant,
                           privateKey: String,
                           genesisCoinAirdrop: Array[CoinAirdrop])

case class ChainSettings(blockBase: BlockBaseSettings,
                         dataBase: DataBaseSettings,
                         forkBase: ForkBaseSettings,
                         minerAward: Double,
                         genesis: GenesisSettings)

case class PluginsSettings(mongodb: MongodbSettings)

case class RuntimeParas(stopProcessTxTimeSlot: Int, txAcceptGasLimit: Long)

case class MongodbSettings(enabled: Boolean, uri: String)

case class MinerSettings(privKeys: Array[PrivateKey]) {

  def findPrivKey(miner: UInt160): Option[PrivateKey] = {
    privKeys.find(p => p.publicKey.pubKeyHash == miner)
  }

  def findPrivKey(miner: Witness): Option[PrivateKey] = {
    findPrivKey(miner.pubkeyHash)
  }
}

case class ConsensusSettings(produceInterval: Int,
                             acceptableTimeError: Int,
                             producerRepetitions: Int,
                             witnessNum: Int,
                             electeTime: Long,
                             initialWitness: Array[Witness]) {

  require(initialWitness.size == witnessNum)
  require(electeTime >= witnessNum * producerRepetitions * produceInterval)

  def fingerprint(): BinaryData = {
    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    os.writeInt(produceInterval)
    os.writeInt(acceptableTimeError)
    os.writeInt(producerRepetitions)
    os.writeInt(witnessNum)
    os.writeLong(electeTime)
    initialWitness.foreach(w => {
      os.writeBytes(w.name)
      os.writeBytes(w.pubkeyHash.toString)
      // do not include privkey
    })
    hash256(bs.toByteArray)
  }
}

case class CoinAirdrop(addr: String,
                       coins: Double)

case class Witness(name: String,
                   pubkeyHash: UInt160)

object ApexSettings extends SettingsReaders with ApexLogging {
  protected val configPath: String = "apex"
  //
  //  implicit val valueReader: ValueReader[ApexSettings] =
  //    (cfg: Config, path: String) => cfg.as[ApexSettings](path)

  implicit val uInt160Reader: ValueReader[UInt160] = (cfg, path) => UInt160.parse(cfg.getString(path)).get

  implicit val publicKeyReader: ValueReader[PublicKey] = (cfg, path) => new PublicKey(Point(cfg.getString(path)))

  implicit val privateKeyReader: ValueReader[PrivateKey] = (cfg, path) => new PrivateKey(BinaryData(cfg.getString(path)))

  implicit val dateReader: ValueReader[Instant] = (cfg, path) => {
    val dateStr = cfg.getString(path)
    val fmt = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    fmt.setTimeZone(TimeZone.getTimeZone("UTC"))
    fmt.parse(dateStr).toInstant
  }

  implicit val dbTypeReader: ValueReader[DBType.Value] = (cfg, path) => {
    val conf = cfg.getString(path)
    DBType(conf.toInt)
  }

  def read(configFilePath: String): (ApexSettings, Config) = {
    val conf = readConfigFromPath(Some(configFilePath), configPath)
    (conf.as[ApexSettings](configPath), conf)
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