package com.apex.network.upnp
import java.net.DatagramSocket
import java.net.InetAddress
import scala.collection.immutable.HashMap
import java.net.InetSocketAddress
import java.net.DatagramPacket
import java.net.SocketTimeoutException
import java.net.SocketException
import java.io.IOException
import javax.xml.parsers.ParserConfigurationException
import org.xml.sax.SAXException
import java.net.UnknownHostException
import scala.collection.JavaConversions._
import java.net.NetworkInterface
import java.net.Inet4Address
import java.util.ArrayList
import java.util.Enumeration
import java.net.Inet6Address
import java.util.StringTokenizer
import util.control.Breaks._
import scala.collection.parallel.immutable.ParMap
import com.apex.network.upnp.GatewayDiscover.GatewayDeviceMap
object GatewayDiscover {
  
  val PORT = 1900;

  val IP = "239.255.255.250";

  //case class DiscoveredGatewayDevice(inet: InetAddress, gwd: GatewayDevice)
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

      //  def receive(iter: Int): Option[Tuple2[InetAddress, GatewayDevice]] = {
      val discoveredGatewayDeviceOption : Option[(InetAddress,GatewayDevice)] = {
        val receivePacket = new DatagramPacket(new Array[Byte](1536), 1536);

        try {
            //println("Trying to receive the packet for : " + searchMessage)
          ssdp_try.receive(receivePacket);
          val receivedData = new Array[Byte](receivePacket.getLength);
          // println("Packet received. Copying.")
          //I'm trying to use this in the same way as System.arraycopy. This may be a problem.s
          // println("Array.copy")
          Array.copy(receivePacket.getData(), 0, receivedData, 0, receivePacket.getLength);
          // println("Copied. Parsing.")
          
          val (locationOption,deviceSearchTargetOption) = parseMSearchReply(receivedData);
          val gateDev: Option[GatewayDevice] = locationOption.map(location => new GatewayDevice(location,localAddress));

          // println("Loading description.")
         // gateDev.loadDescription();
          // println("Now going into the lock.")
          //TODO: Not sure if this is synchronizing properly.
          //Need to look up scala synchronization.

          //println("In the lock.")
         gateDev.map(gd =>(localAddress, gd));
        } catch {
          case (ste: SocketTimeoutException) ⇒
            // if (iter < 2) receive(iter + 1) else {
            //println("Scala: Socket timed out while attempting to discover gateway.")
            None
          //}
        }
      }
      // val discoveredGatewayDeviceOption = receive(0)
      //println("Scala: SUCCESS, Socket did not time out while attempting to discover gateway.")
      Some((ssdp_try, discoveredGatewayDeviceOption))

    } catch {
      case (e: Exception) ⇒
      println(s"WARNING: EXCEPTION HIT: " + e.printStackTrace())
      None;
    }
    
    ssdp.foreach(_._1.close); //close DatagramSocket
    println(ssdp.flatMap(_._2))
    ssdp.flatMap(_._2) //return discoveredGatewayDeviceOption
  }

  @throws(classOf[SocketException])
  @throws(classOf[UnknownHostException])
  @throws(classOf[IOException])
  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  def apply(): GatewayDeviceMap = {

    val ips = getLocalInetAddresses(true, false, false);

    // ST parameter: Search Targets
    val SEARCHTYPES: Array[String] = Array(
      "urn:schemas-upnp-org:device:InternetGatewayDevice:1",
      "urn:schemas-upnp-org:service:WANIPConnection:1", 
      "urn:schemas-upnp-org:service:WANPPPConnection:1"
      );

    //val devs = (0 until SEARCHTYPES.length) flatMap { i =>
    def searchLoop(i: Int, m: Map[InetAddress, GatewayDevice]): Map[InetAddress, GatewayDevice] = { //flatMap { i =>

      val searchMessage: String = "M-SEARCH * HTTP/1.1\r\n" +
        "HOST: " + IP + ":" + PORT + "\r\n" +
        "ST: " + SEARCHTYPES(i) + "\r\n" +
        "MAN: \"ssdp:discover\"\r\n" +
        "MX: 2\r\n" + // seconds to delay response
        "\r\n";

      // perform search requests for multiple network adapters concurrently
      val deviceList = ips.par flatMap { x ⇒
        {
          SendDiscoveryThread(x, searchMessage)
        }
      }
      val next = i - 1
      //if we have a a device defined or we have tried all search types then return
      if (deviceList.isDefinedAt(0) || next < 0) return m ++ deviceList else return searchLoop(next, m ++ deviceList)
    } // loop SEARCHTYPES
    // (0 until SEARCHTYPES.length)
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

  /**
   * Retrieves all local IP addresses from all present network devices on the local PC
   *
   * @param getIPv4            boolean flag if IPv4 addresses shall be retrieved
   * @param getIPv6            boolean flag if IPv6 addresses shall be retrieved
   * @param sortIPv4BeforeIPv6 if true, IPv4 addresses will be sorted before IPv6 addresses
   * @return Collection if {@link InetAddress}es
   */
  //Private removed for testing
  def getLocalInetAddresses(getIPv4: Boolean, getIPv6: Boolean, sortIPv4BeforeIPv6: Boolean): Seq[InetAddress] = {
    def getAllNetworkInterfaces: Seq[NetworkInterface] = {
      try {
        val n = NetworkInterface.getNetworkInterfaces
        if (n == null) Nil else n.toSeq;
      } catch {
       // case (e: SocketException) ⇒ Nil
        case e : Exception => e.printStackTrace(); Nil
      }
    }

    def getIPAddressesOfInterface(card: NetworkInterface): Seq[InetAddress] = {
      try {
        // skip devices, not suitable to search gateways for
        if (card.isLoopback() || card.isPointToPoint() ||
          card.isVirtual() || !card.isUp())
          return Nil;
        else {
          val a = card.getInetAddresses();
          if (a == null) Nil
          else
            a.toList
        }
      } catch {
        //case (e: SocketException) ⇒ return Nil;
          case e : Exception => e.printStackTrace(); Nil
      }

    }

    def getAddressesWhichMatchQuery(inetaddr: InetAddress): Boolean = {
      inetaddr match {
        case i: Inet4Address ⇒ getIPv4
        case i: Inet6Address ⇒ getIPv6
      }
    }

    /*
     * sort IPv4 addresses before IPv6 addresses
     */
    def IPv4BeforeIPv6(one: InetAddress, two: InetAddress): Boolean = {
      def value(x: InetAddress) = x match {
        case i: Inet4Address ⇒ 1
        case i: Inet6Address ⇒ 2
      }
      value(one) < value(two)
    }

    val networkInterfaces = getAllNetworkInterfaces

    // For every suitable network interface, get all IP addresses
    val addresses = networkInterfaces.flatMap(getIPAddressesOfInterface(_))
    //get the list of address which match query of getIPv4 and getIPv6
    val filteredAddresses = addresses filter getAddressesWhichMatchQuery
    //sortIPv4BeforeIPv6 if required then return
    if (sortIPv4BeforeIPv6) filteredAddresses sortWith IPv4BeforeIPv6 else filteredAddresses
  }

  //wrap Map[InetAddress, GatewayDevice] with the getValidGateway function
  class GatewayDeviceMap private[upnp] (val devices: Map[InetAddress, GatewayDevice]) {
    /**
     * Gets the first connected gateway
     *
     * @return the first GatewayDevice which is connected to the network, or None
     */
    def getValidGateway(): Option[GatewayDevice] = {
      val d = devices.map(_._2) filter (_.isConnected)
      if (!d.isEmpty) Some(d.head) else None

    }
  }
}