package com.apex.network.upnp

import java.io.IOException
import java.net._
import java.util.StringTokenizer

import javax.xml.parsers.ParserConfigurationException
import org.xml.sax.SAXException

import scala.collection.immutable.HashMap
import scala.collection.JavaConverters._

object GatewayDiscover {
  
  val PORT = 1900;

  val IP = "239.255.255.250";

  def SendDiscoveryThread(localAddress: InetAddress, searchMessage: String) = {

    /**
     * The timeout to set for the initial broadcast request
     */
    val TIMEOUT = 3000;

    val ssdp : Option[(java.net.DatagramSocket, Option[(java.net.InetAddress,  com.apex.network.upnp.GatewayDevice)])] =  try {
      val ssdp_try = new DatagramSocket(new InetSocketAddress(localAddress, 0))
      val searchMessageBytes = searchMessage.getBytes;
      val ssdpDiscoverPacket = new DatagramPacket(searchMessageBytes, searchMessageBytes.length);
      ssdpDiscoverPacket.setAddress(InetAddress.getByName(IP));
      ssdpDiscoverPacket.setPort(PORT);

      ssdp_try.send(ssdpDiscoverPacket);
      ssdp_try.setSoTimeout(TIMEOUT);

      val discoveredGatewayDeviceOption : Option[(InetAddress,GatewayDevice)] = {
        val receivePacket = new DatagramPacket(new Array[Byte](1536), 1536);

        try {
          ssdp_try.receive(receivePacket);
          val receivedData = new Array[Byte](receivePacket.getLength);
          Array.copy(receivePacket.getData(), 0, receivedData, 0, receivePacket.getLength);
          
          val (locationOption,deviceSearchTargetOption) = parseMSearchReply(receivedData);
          val gateDev: Option[GatewayDevice] = locationOption.map(location => new GatewayDevice(location,localAddress));

         gateDev.map(gd =>(localAddress, gd));
        } catch {
          case (ste: SocketTimeoutException) ⇒
            None
        }
      }
      Some((ssdp_try, discoveredGatewayDeviceOption))

    } catch {
      case (e: Exception) ⇒
      println(s"WARNING: EXCEPTION HIT: " + e.printStackTrace())
      None;
    }
    
    ssdp.foreach(_._1.close); //close DatagramSocket
    ssdp.flatMap(_._2) //return discoveredGatewayDeviceOption
  }

  @throws(classOf[SocketException])
  @throws(classOf[UnknownHostException])
  @throws(classOf[IOException])
  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  def apply(): GatewayDeviceMap = {

    val ips = getLocalInetAddresses(true, false, false);

    val SEARCHTYPES: Array[String] = Array(
      "urn:schemas-upnp-org:device:InternetGatewayDevice:1",
      "urn:schemas-upnp-org:service:WANIPConnection:1", 
      "urn:schemas-upnp-org:service:WANPPPConnection:1"
      );

    def searchLoop(i: Int, m: Map[InetAddress, GatewayDevice]): Map[InetAddress, GatewayDevice] = { //flatMap { i =>

      val searchMessage: String = "M-SEARCH * HTTP/1.1\r\n" +
        "HOST: " + IP + ":" + PORT + "\r\n" +
        "ST: " + SEARCHTYPES(i) + "\r\n" +
        "MAN: \"ssdp:discover\"\r\n" +
        "MX: 2\r\n" + // seconds to delay response
        "\r\n";

      val deviceList = ips.par flatMap { x ⇒
        {
          SendDiscoveryThread(x, searchMessage)
        }
      }
      val next = i - 1
      if (deviceList.isDefinedAt(0) || next < 0) return m ++ deviceList else return searchLoop(next, m ++ deviceList)
    } 
    val devs = searchLoop(SEARCHTYPES.length - 1, new HashMap[InetAddress, GatewayDevice]())

    return devs.toMap
  }

  def parseMSearchReply(reply: Array[Byte]): (Option[String],Option[String] ) = {
    val replyString: String = new String(reply);
    val st: StringTokenizer = new StringTokenizer(replyString, "\n");

    var deviceLocation : Option[String] = None
    var deviceSearchTarget : Option[String] = None
    
    while (st.hasMoreTokens()) {
      val line: String = st.nextToken().trim();

      if (!(line.isEmpty || line.startsWith("HTTP/1."))) {
        val key: String = line.substring(0, line.indexOf(':')).trim;
        val value: Option[String] = if (line.length() > key.length() + 1) Some(line.substring(key.length() + 1)) else None;
        val trimmedValue = value map (_.trim)

        if (key.compareToIgnoreCase("location") == 0) {
          deviceLocation = trimmedValue;

        } else if (key.compareToIgnoreCase("st") == 0) { // Search Target
          deviceSearchTarget = trimmedValue;
        }
      }
    }

    return (deviceLocation,deviceSearchTarget);

  }

  def getLocalInetAddresses(getIPv4: Boolean, getIPv6: Boolean, sortIPv4BeforeIPv6: Boolean): Seq[InetAddress] = {
   def getAllNetworkInterfaces: Seq[NetworkInterface] = {
      try {
        val n = NetworkInterface.getNetworkInterfaces
        if (n == null) Nil else n.asScala.toSeq;
      } catch {
        case e : Exception => e.printStackTrace(); Nil
      }
    }

    def getIPAddressesOfInterface(card: NetworkInterface): Seq[InetAddress] = {
      try {
        if (card.isLoopback() || card.isPointToPoint() ||
          card.isVirtual() || !card.isUp())
          return Nil;
        else {
          val a = card.getInetAddresses();
          if (a == null) Nil else a.asScala.toSeq
        }
      } catch {
          case e : Exception => e.printStackTrace(); Nil
      }

    }

    def getAddressesWhichMatchQuery(inetaddr: InetAddress): Boolean = {
      inetaddr match {
        case i: Inet4Address ⇒ getIPv4
        case i: Inet6Address ⇒ getIPv6
      }
    }

    def IPv4BeforeIPv6(one: InetAddress, two: InetAddress): Boolean = {
      def value(x: InetAddress) = x match {
        case i: Inet4Address ⇒ 1
        case i: Inet6Address ⇒ 2
      }
      value(one) < value(two)
    }

    val networkInterfaces = getAllNetworkInterfaces

    val addresses = networkInterfaces.flatMap(getIPAddressesOfInterface(_))
    val filteredAddresses = addresses filter getAddressesWhichMatchQuery
    if (sortIPv4BeforeIPv6) filteredAddresses sortWith IPv4BeforeIPv6 else filteredAddresses
  }

  class GatewayDeviceMap private[upnp] (val devices: Map[InetAddress, GatewayDevice]) {
    def getValidGateway(): Option[GatewayDevice] = {
      val d = devices.map(_._2) filter (_.isConnected)
      if (!d.isEmpty) Some(d.head) else None

    }
  }
}