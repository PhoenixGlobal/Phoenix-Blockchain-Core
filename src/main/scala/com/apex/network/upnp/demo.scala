/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: demo.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-27 下午3:16@version: 1.0
 */

package com.apex.network.upnp

import java.lang.Thread

import scala.collection.JavaConversions.mapAsScalaMap
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable
object Main extends App {
  val SAMPLE_PORT = 6991;
  val WAIT_TIME = 10;
  val LISTALLMAPPINGS = true;
  var counter = 0;

  println("Starting psyonik-upnp");
  // val gatewayDiscover = new GatewayDiscover();

  println("Looking for Gateway Devices...");

  val gateways = GatewayDiscover();

  if (gateways.isEmpty) {
    println("No gateways found");
    println("Stopping weupnp");
    sys.exit();
  }
  println(gateways.size + " gateway(s) found");
  gateways foreach {
    case (key, gw) =>
      {
        counter = counter + 1;
        println("Listing gateway details of device #" + counter +
          "\n\tFriendly name: " + gw.RootDevice.friendlyName +
          "\n\tPresentation URL: " + gw.RootDevice.presentationURL +
          "\n\tModel name: " + gw.RootDevice.modelName +
          "\n\tModel number: " + gw.RootDevice.modelNumber +
          "\n\tLocal interface address: " + gw.localAddress.getHostAddress() + "\n");
      }
  }

  val activeGWOption = gateways.getValidGateway();

  activeGWOption match {
    case Some(gw) => println("Using gateway: " + gw.RootDevice.friendlyName);
    case None =>
      println("No active gateway device found");
      println("exiting");
      sys.exit();
  }
  val activeGW = activeGWOption.get;

  val portMapCount = activeGW.getPortMappingNumberOfEntries();
  println("GetPortMappingNumberOfEntries=" + (if (portMapCount != 0) portMapCount.toString else "(unsupported)"));

  if (LISTALLMAPPINGS) {
    activeGW.getAllPortMappingEntries foreach (x => println("Portmapping retrieved (" + x.portMappingDescription + ":" + x.externalPort + ")"));//检索的端口映射
    
     activeGW.getGenericPortMappingEntry(3) match {
      case Some(portMapping) => println("Portmapping #0 successfully retrieved (Description: " + portMapping.portMappingDescription.getOrElse(" ") + ", externalPort: " + portMapping.externalPort.getOrElse(" ") + ")");
      case None => println("Portmapping #0 retrival failed");
    }
  } else {
    activeGW.getGenericPortMappingEntry(0) match {
      case Some(portMapping) => println("Portmapping #0 successfully retrieved (Description: " + portMapping.portMappingDescription.getOrElse(" ") + ", externalPort: " + portMapping.externalPort.getOrElse(" ") + ")");
      case None => println("Portmapping #0 retrival failed");
    }
  }

  println("retrieving only port mappings with description: psyonik")//仅用描述检索端口映射
  activeGW.getAllPortMappingEntriesOf("psyonik") foreach (x => println("Portmapping retrieved (" + x.portMappingDescription + ":" + x.externalPort + ")"));

  val localAddress = activeGW.localAddress;
  println("Using local address: " + localAddress.getHostAddress());
  val externalIPAddress = activeGW.externalIPAddress;
  println("External address: " + externalIPAddress);

  println("Querying device to see if a port mapping already exists for port: " + SAMPLE_PORT);

//  activeGW.getSpecificPortMappingEntry(SAMPLE_PORT, "TCP") match {
//    case Some(portMapping) =>
//      println("Port " + SAMPLE_PORT + " is already mapped. Aborting test.");
//      sys.exit();
//    case None =>
//      println("Mapping free. Sending port mapping request for port " + SAMPLE_PORT);

      if (activeGW.addPortMapping(SAMPLE_PORT, SAMPLE_PORT, localAddress.getHostAddress(), "TCP", "test")) {
        println("Mapping Successful. Waiting " + WAIT_TIME + " seconds before removing mapping...");
        Thread.sleep(1009 * WAIT_TIME);

        // if (activeGW.deletePortMapping(SAMPLE_PORT, "TCP"))
        //   println("Port mapping removed, test SUCCESSFUL");
        // else
        //   println("Port mapping removal FAILED");
      }
//  }
  println("Stopping psyonik-upnp");
}