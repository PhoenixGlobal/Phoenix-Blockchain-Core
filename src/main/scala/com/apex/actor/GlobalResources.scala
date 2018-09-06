package com.apex.actor

import akka.actor.{ActorRef, ActorSystem, Props}
import com.apex.network.peer.PeerHandlerManager
import com.typesafe.config.ConfigFactory

class GlobalResources {

}

object GlobalResources{
  var system: ActorSystem =  null

  var peerManagerActor: ActorRef = null
  var nodeActor: ActorRef = null
  var networkActor: ActorRef = null

  def actorOf(): Unit ={
    system = ActorSystem("ResourceSystem", ConfigFactory.load("AkkaActor.conf"))
    peerManagerActor = system.actorOf(Props(new PeerManagerHandlerActor()), name = "peerManagerHandlerActor")
  }
}
