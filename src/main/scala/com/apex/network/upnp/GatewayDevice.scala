package com.apex.network.upnp
import java.io.IOException
import org.xml.sax.SAXException
import java.net.URL
import java.net.HttpURLConnection
import org.xml.sax.helpers.XMLReaderFactory
import org.xml.sax.InputSource
import org.xml.sax.XMLReader
import java.net.URLConnection
import org.xml.sax.helpers.DefaultHandler
import java.net.InetAddress
import scala.collection.JavaConversions._
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.Reader
import java.io.InputStreamReader
import scala.xml.Attribute
import scala.xml.Unparsed
import scala.xml.Group
import scala.xml.Elem
import scala.xml.TopScope
import scala.xml.NodeSeq
import scala.xml.Node
import java.io.InputStream
import scala.xml.XML


class GatewayDevice(val location: String, val localAddress: InetAddress) {
  import GatewayDevice.Commands._

  //  def parseXml(inputStream: InputStream) = {
  //   val bufferedSource =  scala.io.Source.fromInputStream(inputStream)
  //   bufferedSource.getLines foreach println
  //  }

  //  @throws(classOf[SAXException])
  //  @throws(classOf[IOException])
  //  def loadDescription() = {
  //TODO: Fix the use of options here.
  //Using all of them in a match statement won't work.
  private val urlConn: URLConnection = new URL(location).openConnection();
  urlConn.setReadTimeout(GatewayDevice.HTTP_RECEIVE_TIMEOUT);

  // val parser: XMLReader = XMLReaderFactory.createXMLReader();
  // parser.setContentHandler(new GatewayDeviceHandler(this));
  private val inputStream = urlConn.getInputStream();
  //  parseXml(inputStream);
  //parser.parse(new InputSource(urlConn.getInputStream()));
  //  parser.parse(new InputSource(inputStream));

  private val xmlFromInputStream = scala.xml.XML.load(inputStream)
  val urlBase: Option[String] = (xmlFromInputStream \ "URLBase").textOption

  val ipConDescURL = urlBase.filter(_.trim.length > 0).orElse(Some(location)).map { descUrl =>
    val lastSlashIndex: Int = descUrl.indexOf('/', 7)

    if (lastSlashIndex > 0) {
      descUrl.substring(0, lastSlashIndex);
    } else descUrl

  }

  object RootDevice {
    private val rootDeviceNodeSeq = (xmlFromInputStream \ "device")

    /**
     * The friendly (human readable) name associated with this device
     */
    val friendlyName: Option[String] = (rootDeviceNodeSeq \ "friendlyName").textOption

    /**
     * The device manufacturer name
     */
    val manufacturer: Option[String] = (rootDeviceNodeSeq \ "manufacturer").textOption

    /**
     * The model description as a string
     */
    val modelDescription: Option[String] = (rootDeviceNodeSeq \ "modelDescription").textOption;

    /**
     * The URL that can be used to access the IGD interface
     */
    val presentationURL: Option[String] = copyOrCatUrl(ipConDescURL, (rootDeviceNodeSeq \ "presentationURL").textOption)

    /**
     * The model number (used by the manufacturer to identify the product)
     */
    val modelNumber: Option[String] = (rootDeviceNodeSeq \ "modelNumber").textOption;

    /**
     * The model name
     */
    val modelName: Option[String] = (rootDeviceNodeSeq \ "modelName").textOption;

    private val allDevices = rootDeviceNodeSeq \\ "device"

    private val allServices = rootDeviceNodeSeq \\ "service"

    def deviceXml(deviceType: String) = allDevices.filter(d => (d \ "deviceType").text.contains(deviceType)).headOption

    def serviceXml(serviceType: String) = allServices.filter(d => (d \ "serviceType").text.contains(serviceType)).headOption

    def innerNode(nodeOption: Option[Node], name: String) = nodeOption.flatMap(x => (x \ name).textOption)

    object WANDevice {
      private val wanDeviceXML = deviceXml("urn:schemas-upnp-org:device:WANDevice")
      val deviceType: Option[String] = innerNode(wanDeviceXML, "deviceType")
      object WANCommonInterfaceConfig { //CIF
        private val wanCommonInterfaceConfigXml = serviceXml("urn:schemas-upnp-org:service:WANCommonInterfaceConfig")
        val serviceType: Option[String] = innerNode(wanCommonInterfaceConfigXml, "serviceType")
        val controlURL: Option[String] = copyOrCatUrl(ipConDescURL, innerNode(wanCommonInterfaceConfigXml, "controlURL"))
        val eventSubURL: Option[String] = innerNode(wanCommonInterfaceConfigXml, "eventSubURL")
        val SCPDURL: Option[String] = innerNode(wanCommonInterfaceConfigXml, "SCPDURL")
      }

      object WANConnectionDevice {
        private val wanConnectionDeviceXml = deviceXml("urn:schemas-upnp-org:device:WANConnectionDevice")
        val deviceType: Option[String] = innerNode(wanConnectionDeviceXml, "deviceType")
        object WANIPConnection {
          private val wanIPConnectionXml = serviceXml("urn:schemas-upnp-org:service:WANIPConnection")
          val serviceType: Option[String] = innerNode(wanIPConnectionXml, "serviceType")
          val controlURL: Option[String] = copyOrCatUrl(ipConDescURL, innerNode(wanIPConnectionXml, "controlURL"))
          val eventSubURL: Option[String] = innerNode(wanIPConnectionXml, "eventSubURL")
          val SCPDURL: Option[String] = copyOrCatUrl(ipConDescURL, innerNode(wanIPConnectionXml, "SCPDURL"))
        }
      }

    }
  }
  import RootDevice.WANDevice.WANConnectionDevice.WANIPConnection
  //  import RootDevice.WANDevice.WANCommonInterfaceConfig

  // WANIPConnection.SCPDURL = copyOrCatUrl(ipConDescURL, WANIPConnection.SCPDURL);
  // WANIPConnection.controlURL = copyOrCatUrl(ipConDescURL, WANIPConnection.controlURL);

  //  RootDevice.presentationURL = copyOrCatUrl(ipConDescURL, RootDevice.presentationURL);   
  //WANCommonInterfaceConfig.controlURL = copyOrCatUrl(ipConDescURL, WANCommonInterfaceConfig.controlURL);

  //}

  /**
   * Retrieves the connection status of this device
   *
   * @return true if connected, false otherwise
   * @throws IOException
   * @throws SAXException
   * @see #simpleUPnPcommand(java.lang.String, java.lang.String,
   * java.lang.String, java.util.Map)
   */
  @throws(classOf[IOException])
  @throws(classOf[SAXException])
  lazy val isConnected: Boolean = {
    (WANIPConnection.controlURL, WANIPConnection.serviceType) match {
      case (Some(controlURLValue), Some(serviceTypeValue)) => {
        val nameValue: Map[String, String] = GatewayDevice.simpleUPnPcommand(controlURLValue,
          serviceTypeValue, GetStatusInfo);
        //Needs to be optimized as an option
        val t = nameValue.get("NewConnectionStatus").
          filter(_.equalsIgnoreCase("Connected")).
          map(_ => true)

        t.getOrElse(false)
      }
      case _ => false;
    }

  }

  /**
   * Retrieves the external IP address associated with this device
   * <p/>
   * The external address is the address that can be used to connect to the
   * GatewayDevice from the external network
   *
   * @return the external IP
   * @throws IOException
   * @throws SAXException
   * @see #simpleUPnPcommand(java.lang.String, java.lang.String,
   * java.lang.String, java.util.Map)
   */
  lazy val externalIPAddress: Option[String] = {
    //TODO: Print out if controlUrl or ServiceType are None.
    (WANIPConnection.controlURL, WANIPConnection.serviceType) match {
      case (Some(controlURLValue), Some(serviceTypeValue)) => {
        val nameValue: Map[String, String] = GatewayDevice.simpleUPnPcommand(controlURLValue,
          serviceTypeValue, GetExternalIPAddress);
        nameValue.get("NewExternalIPAddress");
      }
      case (_) => {
        None;
      }
    }
  }

  /**
   * Adds a new port mapping to the GatewayDevices using the supplied
   * parameters.
   *
   * @param externalPort   the external associated with the new mapping
   * @param internalPort   the internal port associated with the new mapping
   * @param internalClient the internal client associated with the new mapping
   * @param protocol       the protocol associated with the new mapping
   * @param description    the mapping description
   * @return true if the mapping was succesfully added, false otherwise
   * @throws IOException
   * @throws SAXException
   * @see #simpleUPnPcommand(java.lang.String, java.lang.String,
   * java.lang.String, java.util.Map)
   * @see PortMappingEntry
   */
  @throws(classOf[IOException])
  @throws(classOf[SAXException])
  def addPortMapping(externalPort: Int, internalPort: Int,
    internalClient: String, protocol: String, description: String, leaseDuration: Int = 0): Boolean = {
    //TODO: Print out if controlUrl or ServiceType are None.
    (WANIPConnection.controlURL, WANIPConnection.serviceType) match {
      case (Some(controlURLValue), Some(serviceTypeValue)) =>
        val args: Map[String, String] = Map("NewRemoteHost" -> "", //wildcard, any remote host matches
          "NewExternalPort" -> Integer.toString(externalPort),
          "NewProtocol" -> protocol,
          "NewInternalPort" -> Integer.toString(internalPort),
          "NewInternalClient" -> internalClient,
          "NewEnabled" -> "1",
          "NewPortMappingDescription" -> description,
          "NewLeaseDuration" -> Integer.toString(leaseDuration));

        val nameValue: Map[String, String] = GatewayDevice.simpleUPnPcommand(controlURLValue,
          serviceTypeValue, AddPortMapping, args);

        nameValue.get("errorCode").isEmpty
      case _ => false;
    }
  }

  def getAllPortMappingEntriesOf(description: String) = {
    getAllPortMappingEntries filter (_.portMappingDescription == Some(description))
  }
  def getAllPortMappingEntriesOf(internalClientAddress: InetAddress) = {
    getAllPortMappingEntries filter (_.internalClient == Some(internalClientAddress.getHostAddress))
  }
  def getAllPortMappingEntriesOf(internalClientAddress: InetAddress, description: String) = {
    getAllPortMappingEntries filter (e => e.internalClient == Some(internalClientAddress.getHostAddress) && e.portMappingDescription == Some(description))
  }

  def getAllPortMappingEntries = {
    def looped(entries: List[PortMappingEntry] = Nil): List[PortMappingEntry] = {
      this.getGenericPortMappingEntry(entries.size) match {
        case Some(portMapping) => looped(portMapping :: entries)
        case None => return entries;
      }
    }
    looped()
  }

  private def portQueryToPortMappingEntry(nameValue: Map[String, String]): Option[PortMappingEntry] = {
    if (nameValue.isEmpty || nameValue.contains("errorCode"))
      return None;
    Some(PortMappingEntry(
      nameValue.get("NewInternalPort") map (_.toInt),
      nameValue.get("NewExternalPort") map (_.toInt),
      nameValue.get("NewRemoteHost"),
      nameValue.get("NewInternalClient"),
      nameValue.get("NewProtocol"),
      nameValue.get("NewEnabled"),
      nameValue.get("NewPortMappingDescription")));
  }

  /**
   * Queries the GatewayDevice to retrieve a specific port mapping entry,
   * corresponding to specified criteria, if present.
   * <p/>
   * Retrieves the <tt>PortMappingEntry</tt> associated with
   * <tt>externalPort</tt> and <tt>protocol</tt>, if present.
   *
   * @param externalPort     the external port
   * @param protocol         the protocol (TCP or UDP)
   * @return Some(PortMappingEntry) if a valid mapping is found, else None
   * @throws IOException
   * @throws SAXException
   * @todo consider refactoring this method to make it consistent with
   * Java practices (return the port mapping)
   * @see #simpleUPnPcommand(java.lang.String, java.lang.String,
   * java.lang.String, java.util.Map)
   * @see PortMappingEntry
   */
  @throws(classOf[IOException])
  @throws(classOf[SAXException])
  def getSpecificPortMappingEntry(externalPort: Int,
    protocol: String): Option[PortMappingEntry] = {

    (WANIPConnection.controlURL, WANIPConnection.serviceType) match {
      case (Some(controlURLValue), Some(serviceTypeValue)) =>
        val args: Map[String, String] = Map(
          "NewRemoteHost" -> "", // wildcard, any remote host matches
          "NewExternalPort" -> Integer.toString(externalPort),
          "NewProtocol" -> protocol);

        val nameValue: Map[String, String] = GatewayDevice.simpleUPnPcommand(controlURLValue,
          serviceTypeValue, GetSpecificPortMappingEntry, args);

        if (!nameValue.contains("NewInternalClient") || !nameValue.contains("NewInternalPort")) {
          None;
        } else {
          portQueryToPortMappingEntry(args ++ nameValue)
        }
      case _ => None;

    }
  }

  /**
   * Returns a specific port mapping entry, depending on a the supplied index.
   *
   * @param index            the index of the desired port mapping
   * @return Some(PortMappingEntry) if a valid mapping is found, else None
   * @throws IOException
   * @throws SAXException
   * @todo consider refactoring this method to make it consistent with
   * Java practices (return the port mapping)
   * @see #simpleUPnPcommand(java.lang.String, java.lang.String,
   * java.lang.String, java.util.Map)
   * @see PortMappingEntry
   */
  @throws(classOf[IOException])
  @throws(classOf[SAXException])
  def getGenericPortMappingEntry(index: Int): Option[PortMappingEntry] = {

    //TODO: There appears to be a lot of reuse from getSpecificPortMappingEntry.
    //Combine the two methods to a degree?

    (WANIPConnection.controlURL, WANIPConnection.serviceType) match {
      case (Some(controlURLValue), Some(serviceTypeValue)) =>
        val args: Map[String, String] = Map("NewPortMappingIndex" -> Integer.toString(index));

        val nameValue: Map[String, String] = GatewayDevice.simpleUPnPcommand(controlURLValue,
          serviceTypeValue, GetGenericPortMappingEntry, args);

        portQueryToPortMappingEntry(nameValue)

      case _ => None;

    }
  }

  /**
   * Retrieves the number of port mappings that are registered on the
   * GatewayDevice.
   *
   * @return the number of port mappings
   * @throws IOException
   * @throws SAXException
   */
  @throws(classOf[IOException])
  @throws(classOf[SAXException])
  def getPortMappingNumberOfEntries(): Int = {
    (WANIPConnection.controlURL, WANIPConnection.serviceType) match {
      case (Some(controlURLValue), Some(serviceTypeValue)) =>
        val nameValue: Map[String, String] = GatewayDevice.simpleUPnPcommand(controlURLValue,
          serviceTypeValue, GetPortMappingNumberOfEntries);

        //This originally used Integer.valueOf. Not sure if .toInt will mess it up.
        nameValue.get("NewPortMappingNumberOfEntries").getOrElse("0").toInt;
      case _ => 0
    }
  }

  /**
   * Deletes the port mapping associated to <tt>externalPort</tt> and
   * <tt>protocol</tt>
   *
   * @param externalPort the external port
   * @param protocol     the protocol
   * @return true if removal was successful
   * @throws IOException
   * @throws SAXException
   */
  @throws(classOf[IOException])
  @throws(classOf[SAXException])
  def deletePortMapping(externalPort: Int, protocol: String): Boolean = {
    //TODO: You know, this doesn't actually confirm that it worked...
    //It just kind of returns true if it tried to do it.
    (WANIPConnection.controlURL, WANIPConnection.serviceType) match {
      case (Some(controlURLValue), Some(serviceTypeValue)) =>
        val args: Map[String, String] = Map(
          "NewRemoteHost" -> "",
          "NewExternalPort" -> Integer.toString(externalPort),
          "NewProtocol" -> protocol);
        val nameValue: Map[String, String] = GatewayDevice.simpleUPnPcommand(controlURLValue,
          serviceTypeValue, DeletePortMapping, args);
        true;
      case _ => false;
    }
  }

  // private methods

  private def copyOrCatUrl(dstOption: Option[String], srcOption: Option[String]): Option[String] = {
    srcOption.flatMap { src =>
      if (src.startsWith("http://"))
        Some(src)
      else
        dstOption.map(_ + (if (!src.startsWith("/")) "/" else "") + src);
    }
  }
}
object GatewayDevice {
  object Commands extends Enumeration {
    // type Commands = Value
    val GetStatusInfo, GetExternalIPAddress, AddPortMapping, GetSpecificPortMappingEntry, GetGenericPortMappingEntry, GetPortMappingNumberOfEntries, DeletePortMapping = Value
  }
  val HTTP_RECEIVE_TIMEOUT = 7000;

  private def makeSoap(action: Commands.Value, args: Map[String, String], service: String) = {
    def buildArgs: collection.immutable.Seq[Node] = {
      args.map {
        case (key, value) => {

          scala.xml.Elem(null, key, scala.xml.Null, TopScope, true, scala.xml.Text(value))
          // soapBody.append("<" + key + ">" + value + "</" + key + ">");
        }
        // }
      }.toSeq
    }

    val header = Unparsed("""<?xml version="1.0"?>""")
    val soapBody = <SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
                     <SOAP-ENV:Body>
                       { Elem("m", action.toString, Attribute("xmlns", "m", service, scala.xml.Null), TopScope, true, buildArgs: _*) }
                     </SOAP-ENV:Body>
                   </SOAP-ENV:Envelope>;
    Group(List(header, soapBody)).toString();
  }

  private def issueUPnpCommand(url: String, service: String, action: Commands.Value, args: Map[String, String])(doParse: InputSource => Map[String, String]): Map[String, String] = {
    val soapAction: String = "\"" + service + "#" + action + "\""
    //   val soapBody = new StringBuilder();
    //<m:{action} xmlns:m={ service }/>
    //    soapBody.append("" +
    val soapXmlBytes = makeSoap(action, args, service).getBytes();

    val postUrl = new URL(url);

    try {
      val conn = postUrl.openConnection().asInstanceOf[HttpURLConnection];
      conn.setRequestMethod("POST");
      conn.setReadTimeout(HTTP_RECEIVE_TIMEOUT);
      conn.setDoOutput(true);
      conn.setRequestProperty("Content-Type", "text/xml");
      conn.setRequestProperty("SOAPAction", soapAction);
      conn.setRequestProperty("Connection", "Close");

      conn.setRequestProperty("Content-Length", String.valueOf(soapXmlBytes.length));

      conn.getOutputStream().write(soapXmlBytes);

      val stream = if (conn.getResponseCode() == HttpURLConnection.HTTP_INTERNAL_ERROR)
        conn.getErrorStream
      else
        conn.getInputStream
      val mappedXml = doParse(new InputSource(stream));
      conn.disconnect();
      mappedXml
    } catch {
      case e: Exception =>
        e.printStackTrace()
        Map.empty[String, String]
    }

  }

  /**
   * Issues UPnP commands to a GatewayDevice that can be reached at the
   * specified <tt>url</tt>
   * <p/>
   * The command is identified by a <tt>service</tt> and an <tt>action</tt>
   * and can receive arguments
   *
   * @param url     the url to use to contact the device
   * @param service the service to invoke
   * @param action  the specific action to perform
   * @param args    the command arguments
   * @return the response to the performed command, as a name-value map.
   * In case errors occur, the returned map will be <i>empty.</i>
   * @throws IOException  on communication errors
   * @throws SAXException if errors occur while parsing the response
   */
  @throws(classOf[IOException])
  @throws(classOf[SAXException])
  def simpleUPnPcommand(url: String, service: String, action: Commands.Value, args: Map[String, String]): Map[String, String] = {

    //handles parsing the result of the UPnP command into a HashMap
    //    val parseIt: InputSource => Unit = { inputSource =>
    //      val parser = XMLReaderFactory.createXMLReader();
    //      parser.setContentHandler(new NameValueHandler(nameValue));
    //      try {
    //        parser.parse(inputSource);
    //      } catch {
    //        case e: SAXException => {}
    //        case e => throw e;
    //      }
    //    }
    issueUPnpCommand(url, service, action, args) { 
      inputSource => (XML.load(inputSource) \\ "_").map(x => (x.label, x.text)).toMap
    }

  }

  /**
   * Issues UPnP commands to a GatewayDevice that can be reached at the
   * specified <tt>url</tt>
   * <p/>
   * The command is identified by a <tt>service</tt> and an <tt>action</tt>
   * and can receive arguments
   *
   * @param url     the url to use to contact the device
   * @param service the service to invoke
   * @param action  the specific action to perform
   * @return the response to the performed command, as a name-value map.
   * In case errors occur, the returned map will be <i>empty.</i>
   * @throws IOException  on communication errors
   * @throws SAXException if errors occur while parsing the response
   */
  @throws(classOf[IOException])
  @throws(classOf[SAXException])
  def simpleUPnPcommand(url: String, service: String, action: Commands.Value): Map[String, String] = {
    simpleUPnPcommand(url, service, action, Map.empty)
  }

  /**
   * Issues a simpleUPnPcommand, but instead of parsing the result into a HashMap it places the result in an XML file.  The XML can then be used for testing, etc.
   */
  def simpleUPnPcommand_createFile(url: String, service: String, action: Commands.Value, args: Map[String, String], filename: String): Unit = {
    //handles parsing the result of the UPnP command into a file
    val parseIt: InputSource => Map[String, String] = {
      inputSource =>
      val body = new InputStreamReader(inputSource.getByteStream());
      var c: Int = 0;
      def red = { c = body.read(); c }
      try {
        val f = new FileWriter(filename);
        while (red > -1) {
          f.write(c);
        }
        f.flush()
        f.close();
      } catch {
        case e: Exception => {
          System.err.println("Exception: " + e.getMessage());
          e.printStackTrace();
        }
      }
      Map.empty[String, String]
    }

    issueUPnpCommand(url, service, action, args)(parseIt)
  }

}
