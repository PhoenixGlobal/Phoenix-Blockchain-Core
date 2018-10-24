/*
 *
 *
 *
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Producer.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-8-27 下午7:17@version: 1.0
 */

package com.apex.consensus

import java.math.BigInteger
import java.time.{Duration, Instant}

import akka.actor.{Actor, ActorContext, ActorRef, ActorSystem, Props}
import com.apex.common.ApexLogging
import com.apex.core.BlockHeader
import com.apex.crypto.Ecdsa.PublicKey
import com.apex.settings.{ConsensusSettings, Witness}

import scala.concurrent.ExecutionContext

trait ProduceState

case class NotSynced(nextProduceTime: Long, currTime: Long) extends ProduceState

case class NotYet(nextProduceTime: Long, currTime: Long) extends ProduceState

case class NotMyTurn(producer: String, pubKey: PublicKey) extends ProduceState

case class TimeMissed(thisProduceTime: Long, currTime: Long) extends ProduceState

case class Success(time: Long) extends ProduceState

case class Failed(e: Throwable) extends ProduceState

trait ProducerMessage

case class ProducerStopMessage() extends ProducerMessage
case class NodeIsAliveMessage(node: ActorRef) extends ProducerMessage
case class BlockStartProduceMessage(witness: Witness) extends ProducerMessage
case class BlockFinalizeProduceMessage(witness: Witness, timeStamp: Long) extends ProducerMessage
case class LatestHeaderMessage(header: BlockHeader) extends ProducerMessage

class ProduceTask(val producer: Producer,
                  val peerHandlerManager: ActorRef,
                  private var cancelled: Boolean = false)
  extends Runnable with ApexLogging {

  def cancel(): Unit = {
    cancelled = true
  }

  override def run(): Unit = {
    Thread.sleep(100) // make sure Producer.nodeRef is not null
    while (!cancelled) {
      var sleep = 500 - Instant.now.toEpochMilli % 500
      sleep = if (sleep < 10) sleep + 500 else sleep
      Thread.sleep(sleep)

      if (!cancelled) {
        producer.produce() match {
          case NotSynced(_, _) => log.debug(s"not synced")
          case NotYet(npt, ct) => log.debug(s"Not yet, next produce time: $npt, current time: $ct")
          case TimeMissed(tpt, ct) => log.debug(s"missed, this produce time: $tpt, current time: $ct")
          case NotMyTurn(name, _) => log.debug(s"not my turn, ($name)")
          case Failed(e) => log.error("error occurred when producing block", e)
          case Success(time) => log.debug(s"BlockFinalizeProduceMessage sent. $time")
        }
      }
    }
  }
}

class Producer(settings: ConsensusSettings,
               peerHandlerManager: ActorRef)
              (implicit ec: ExecutionContext) extends Actor with ApexLogging {

  private val task = new ProduceTask(this, peerHandlerManager)
  private val nodeRef: ActorRef = context.parent

  private var latestHeader: BlockHeader = null
  private var blockProducing = false
  private var canProduce = true

  context.system.scheduler.scheduleOnce(Duration.ZERO, task)

  override def receive: Receive = {
    case ProducerStopMessage() => {
      log.info("stopping producer task")
      task.cancel()
      context.stop(self)
    }
    case LatestHeaderMessage(header) => {
      if (latestHeader != null && latestHeader.id.equals(header.id) == false) {
        blockProducing = false
      }
      latestHeader = header
      tryStartProduce(Instant.now.toEpochMilli)
    }
    case a: Any => {
      log.info(s"${sender().toString}, ${a.toString}")
    }
  }

  def produce(): ProduceState = {
    try {
      val now: Long = Instant.now.toEpochMilli

      if (canProduce) {
        tryProduce(now)
      }
      else {
        val next = nextBlockTime()
        if (next >= now) {
          canProduce = true
          tryProduce(now)
        }
        else {
          NotSynced(next, now)
        }
      }
    } catch {
      case e: Throwable => Failed(e)
    }
  }

  private def tryStartProduce(now: Long) = {
    if (canProduce && blockProducing == false) {
      val witness = ProducerUtil.getWitness(nextProduceTime(now, nextBlockTime()), settings)
      if (witness.privkey.isDefined) {
        log.debug(s"send BlockStartProduceMessage to Node. witness name is ${witness.name}")
        nodeRef ! BlockStartProduceMessage(witness)
        blockProducing = true
      }
    }
  }

  private def tryProduce(now: Long) = {
    val next = nextBlockTime()
    tryStartProduce(now)
    if (now + settings.acceptableTimeError < next) {
      NotYet(next, now)
    }
    else {
      if (nextProduceTime(now, next) > next) {
        //println(s"some blocks skipped")
      }
      val witness = ProducerUtil.getWitness(nextProduceTime(now, next), settings)
      if (witness.privkey.isEmpty) {
        NotMyTurn(witness.name, witness.pubkey)
      }
      else {
        nodeRef ! BlockFinalizeProduceMessage(witness, nextProduceTime(now, next))
        Success(now)
      }
    }
  }

  // the nextBlockTime is the expected time of next block based on current latest block
  private def nextBlockTime(nextN: Int = 1): Long = {
    val headTime = latestHeader.timeStamp
    var slot = headTime / settings.produceInterval
    slot += nextN
    slot * settings.produceInterval
  }

  // the nextProduceTime() maybe > nextBlockTime() in case some producer didn't produce,
  // then there are missing gaps in the blocks
  private def nextProduceTime(now: Long, next: Long): Long = {
    if (now <= next) {
      next
    }
    else {
      val slot = now / settings.produceInterval
      slot * settings.produceInterval
    }
  }

  private def updateWitnessSchedule(nowSec: Long, witnesses: Array[Witness]): Array[Witness] = {
    var newWitness = witnesses.clone()

    val nowHi = new BigInteger(nowSec.toString).shiftLeft(32)
    val param = new BigInteger("2685821657736338717")

    for (i <- 0 to newWitness.size - 1) {
      val ii = BigInteger.valueOf(i)
      var k = ii.multiply(param).add(nowHi)
      k = k.xor(k.shiftRight(12))
      k = k.xor(k.shiftLeft(25))
      k = k.xor(k.shiftRight(27))
      k = k.multiply(param)

      val jmax = newWitness.size - i;
      val j = k.remainder(BigInteger.valueOf(jmax)).add(ii).intValue()

      val a = newWitness(i)
      val b = newWitness(j)
      newWitness.update(i, b)
      newWitness.update(j, a)
    }
    newWitness
  }
}

object ProducerUtil {

  // "timeMs": time from 1970 in ms, should be divided evenly with no remainder by settings.produceInterval
  def getWitness(timeMs: Long, settings: ConsensusSettings): Witness = {
    val slot = timeMs / settings.produceInterval
    var index = slot % (settings.initialWitness.size * settings.producerRepetitions)
    index /= settings.producerRepetitions
    settings.initialWitness(index.toInt)
  }

  def isTimeStampValid(timeStamp: Long, settings: ConsensusSettings): Boolean = {
    if (timeStamp % settings.produceInterval == 0)
      true
    else
      false
  }

  def isProducerValid(timeStamp: Long, producer: PublicKey, settings: ConsensusSettings): Boolean = {
    var isValid = false
    if (getWitness(timeStamp, settings).pubkey.toBin sameElements producer.toBin) {
      if (isTimeStampValid(timeStamp, settings)) {
        isValid = true
      }
    }
    isValid
  }
}

object ProducerRef {
  def props(settings: ConsensusSettings,
            peerHandlerManager: ActorRef)
           (implicit ec: ExecutionContext): Props = {
    Props(new Producer(settings, peerHandlerManager))
  }

  def apply(settings: ConsensusSettings,
            peerHandlerManager: ActorRef)
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef = {
    system.actorOf(props(settings, peerHandlerManager))
  }

  def apply(settings: ConsensusSettings,
            peerHandlerManager: ActorRef, name: String)
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef = {
    system.actorOf(props(settings, peerHandlerManager), name)
  }
}