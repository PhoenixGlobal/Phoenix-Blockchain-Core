package com.apex.hybrid

import com.apex.common.ApexLogging
import com.apex.core.settings.ApexSettings.readConfigFromPath
import com.apex.core.settings._
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader
import scala.concurrent.duration._
import scala.util.Random

case class HybridSettings(apexSettings: ApexSettings)

object HybridSettings extends ApexLogging with SettingsReaders {
  def read(userConfigPath: Option[String]): HybridSettings = {
    fromConfig(readConfigFromPath(userConfigPath, "apex"))
  }

  implicit val networkSettingsValueReader: ValueReader[HybridSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  private def fromConfig(config: Config): HybridSettings = {
    val apexSettings = config.as[ApexSettings]("apex")
    
//    val discover = GatewayDiscover()
//    if (discover.isEmpty) {
//      log.info("There are no UPnP gateway devices")
//    } else {
//      Option(discover.getValidGateway) match {
//        case None =>
//          log.info("There is no connected UPnP gateway device")
//          apexSettings.network.knownPeers = Nil
//        case Some(device) =>
//          var inetSocketAddressMap = new scala.collection.mutable.HashMap[Int,String]
//          val activeGW = discover.getValidGateway.get;
//          activeGW.getAllPortMappingEntries foreach (x => println("Portmapping retrieved (" + x.internalClient + ":" + x.externalPort + ")"));//检索的端口映射
//          //所有添加到upnp的端口放入map
//          var allPortMappingEntries = activeGW.getAllPortMappingEntries
//          for(i <- 0 until allPortMappingEntries.length){
//            var j = 0;
//            if(apexSettings.network.bindAddress.getPort != allPortMappingEntries(i).externalPort.get){//不是本次添加的端口，才可以加入map，避免链接自己
//              inetSocketAddressMap.put(j, /*allPortMappingEntries(i).internalClient.get*/"/127.0.0.1" + ":" + allPortMappingEntries(i).externalPort.get)
//              j = j+1
//            }
//          }
//          Option(inetSocketAddressMap) match{
//            case Some(map) if map.size == 0 =>
//              log.info("没有需要链接的节点")
//            case _ =>
//              var inetSocketAddress = Seq[InetSocketAddress]()
//              //如果配置的需要链接数大于现在upnp上添加的节点数量，就获取upnp上配置的，否则根据配置的需要链接数从upnp中随机获取
//              if(apexSettings.network.linkNumber>inetSocketAddressMap.size){
//            	  for(i <- 0 until inetSocketAddressMap.size){
//            		  inetSocketAddress = inetSocketAddress :+ new InetSocketAddress(inetSocketAddressMap.get(i).get.split(":")(0),inetSocketAddressMap.get(i).get.split(":")(1).toInt)
//            				  println(apexSettings.network.linkNumber>allPortMappingEntries.length)
//            	  }
//              }else{
//            	  val randomList = randomNew(apexSettings.network.linkNumber,inetSocketAddressMap.size)
//            			  for(i <- 0 until randomList.length){
//            				  inetSocketAddress = inetSocketAddress :+ new InetSocketAddress(inetSocketAddressMap.get(randomList(i)).get.split(":")(0),inetSocketAddressMap.get(randomList(i)).get.split(":")(1).toInt)
//            			  }
//              }
//              apexSettings.network.knownPeers = inetSocketAddress
//          }
//      }
//    }
    HybridSettings(apexSettings)
  }
    
  //在m中找到n个随机数
  def randomNew(n:Int,m:Int)={
    var resultList:List[Int] = Nil
    while(resultList.length<n){
      val randomNum = (new Random).nextInt(m)
      if(!resultList.exists(s => s==randomNum)){
        resultList = resultList:::List(randomNum)
      }
    }
    resultList
  }
}

