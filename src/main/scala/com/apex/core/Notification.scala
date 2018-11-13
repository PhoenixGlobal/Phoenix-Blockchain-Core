package com.apex.core

import akka.actor.ActorRef
import com.apex.common.ApexLogging

import scala.collection.mutable.ArrayBuffer

trait NotifyMessage

case class NewBlockProducedNotify(block: Block) extends NotifyMessage
case class BlockConfirmedNotify(block: Block) extends NotifyMessage

case class Notification() extends ApexLogging {
  private val listeners = ArrayBuffer.empty[ActorRef]

  def register(actorRef : ActorRef) = {
    listeners.append(actorRef)
  }

  def onNewBlockProduced(block: Block): Unit = {
    log.info(s"block (${block.height}, ${block.timeStamp}) ${block.shortId} produced by ${block.header.producer.address.substring(0, 7)}")
    listeners.foreach(actor => actor ! NewBlockProducedNotify(block))
  }

  def onBlockConfirmed(block: Block) = {

  }
}