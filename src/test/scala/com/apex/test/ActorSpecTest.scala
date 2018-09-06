package com.apex.test

import com.apex.actor.{GlobalResources, TestMessage}
import org.junit.Test

@Test
class ActorSpecTest {
  @Test
  def actorTest{
    GlobalResources.actorOf()
    GlobalResources.peerManagerActor ! TestMessage()
  }
}
