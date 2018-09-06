package com.apex.actor

import akka.actor.Actor
import PeerManagerHandlerActor._

case class TestMessage()
class PeerManagerHandlerActor extends Actor{

  override def receive = {
    case TestMessage() => test()
    case _ => None
  }

  override def preStart(): Unit ={
    super.preStart()
    println(f"for test if actor is start")
  }
}



object PeerManagerHandlerActor{
  def test(){
    println("hello world")
  }
}
