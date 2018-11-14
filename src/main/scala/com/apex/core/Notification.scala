package com.apex.core

import akka.actor.ActorRef
import com.apex.common.ApexLogging

import scala.collection.mutable.ArrayBuffer

trait NotifyMessage

case class NewBlockProducedNotify(block: Block) extends NotifyMessage
case class BlockAddedToHead(block: Block) extends NotifyMessage
case class BlockConfirmedNotify(block: Block) extends NotifyMessage
case class AddTransactionNotify(tx: Transaction) extends NotifyMessage

case class Notification() extends ApexLogging {
  private val listeners = ArrayBuffer.empty[ActorRef]

  def register(actorRef : ActorRef) = {
    listeners.append(actorRef)
  }

  def send(notify: NotifyMessage) = {
    listeners.foreach(actor => actor ! notify)
  }
}