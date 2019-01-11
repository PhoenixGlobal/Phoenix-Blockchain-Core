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

import java.time.Instant
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import com.apex.common.ApexLogging
import com.apex.core.Blockchain
import com.apex.crypto.Ecdsa.PublicKey
import com.apex.network.ProduceTask
import com.apex.settings.{ApexSettings, ConsensusSettings, Witness}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

trait ProduceState

case class NotSynced(nextProduceTime: Long, currTime: Long) extends ProduceState
case class NotYet(nextProduceTime: Long, currTime: Long) extends ProduceState
case class NotMyTurn(producer: String, pubKey: PublicKey) extends ProduceState
case class TimeMissed(thisProduceTime: Long, currTime: Long) extends ProduceState
case class Success(time: Long) extends ProduceState
case class Failed(e: Throwable) extends ProduceState

trait ProducerMessage

case class ProducerStopMessage() extends ProducerMessage
//case class NodeIsAliveMessage(node: ActorRef) extends ProducerMessage
//case class BlockStartProduceMessage(witness: Witness) extends ProducerMessage
//case class BlockFinalizeProduceMessage(witness: Witness, timeStamp: Long) extends ProducerMessage

class Producer(apexSettings: ApexSettings)
              (implicit ec: ExecutionContext) extends Actor with ApexLogging {

  private val settings = apexSettings.consensus

  private val scheduler = context.system.scheduler
  private val node = context.parent

  private val minProducingTime = settings.produceInterval / 10
  private val earlyMS = settings.produceInterval / 5

  private val delayOneBlock = blockDuration(1)
  private val noDelay = blockDuration(0)

  private var enableProduce = false

  scheduleBegin()

  override def receive: Receive = {
    case ProducerStopMessage() => {
      log.info("stopping producer task")
      context.stop(self)
    }
    case a: Any => {
      log.info(s"${sender().toString}, ${a.toString}")
    }
  }

  private def scheduleBegin(duration: Option[FiniteDuration] = None): Unit = {
    val dur = duration.orElse(noDelay).get
    scheduler.scheduleOnce(dur, node, ProduceTask(beginProduce))
  }

  private def scheduleEnd(duration: Option[FiniteDuration] = None): Unit = {
    val dur = duration.orElse(noDelay).get
    scheduler.scheduleOnce(dur, node, ProduceTask(endProduce))
  }

  private def beginProduce(chain: Blockchain): Unit = {
    if (!maybeProduce(chain)) {
      scheduleBegin(delayOneBlock)
    }
  }

  private def endProduce(chain: Blockchain): Unit = {
    chain.produceBlockFinalize()
    scheduleBegin()
  }

  private def maybeProduce(chain: Blockchain) = {
    try {
      if (enableProduce) {
        producing(chain)
        true
      } else if (Instant.now.toEpochMilli <= ProducerUtil.nextTime(chain.getHeadTime, settings.produceInterval)) {
        log.info("now set enableProduce to true")
        enableProduce = true
        producing(chain)
        true
      } else {
//        val curr = chain.getHeadTime
//        val next = nextTime(curr)
//        println(s"not synced, ${Instant.ofEpochMilli(curr).toString} ${Instant.ofEpochMilli(next).toString}")
        false
      }
    } catch {
      case e: Throwable => {
        log.error("begin produce failed", e)
        false
      }
    }
  }

  private def producing(chain: Blockchain) = {
    val headTime = chain.getHeadTime
    val now = Instant.now.toEpochMilli
    if (headTime - now > settings.produceInterval) {
      scheduleBegin(delayOneBlock)
    } else {
      val next = ProducerUtil.nextBlockTime(headTime, now, minProducingTime, settings.produceInterval)
      //println(s"head: $headTime, now: $now, next: $next, delta: ${headTime - now}")
      val witness = ProducerUtil.getWitness(next, settings)
      val myTurn = witness.privkey.isDefined
      if (!myTurn) {
        val now = Instant.now.toEpochMilli
        val delay = calcDelay(now, next, myTurn)
        //println(delay)
        scheduleBegin(delay)
      } else {
        chain.startProduceBlock(witness, next, next - apexSettings.runtimeParas.stopProcessTxTimeSlot)
        val now = Instant.now.toEpochMilli
        val delay = calcDelay(now, next, myTurn)
        scheduleEnd(delay)
      }
    }
  }

  private def calcDelay(now: Long, next: Long, myTurn: Boolean = false): Option[FiniteDuration] = {
    var delay = next - now
    // produce last block in advance
    val rest = restBlocks(next)
    if (myTurn && rest == 1) {
        delay = if (delay < earlyMS) 0 else delay - earlyMS
    }
    //log.info(s"now: $now, next: $next, delay: $delay, delta: ${next - now}, rest: $rest")
    calcDuration(delay.toInt)
  }

  private def restBlocks(time: Long): Int = {
    (settings.producerRepetitions - time / settings.produceInterval % settings.producerRepetitions).toInt
  }

  private def blockDuration(blocks: Int = 1) = {
    Some(FiniteDuration(blocks * settings.produceInterval * 1000, TimeUnit.MICROSECONDS))
  }

  private def calcDuration(delay: Int) = {
    Some(FiniteDuration(delay * 1000, TimeUnit.MICROSECONDS))
  }
}

object ProducerUtil {

  // the nextBlockTime is the expected time of next block based on current latest block time and current time
  def nextBlockTime(headTime: Long, now: Long, minProducingTime: Int, produceInterval: Int): Long = {
    var next = nextTime(math.max(headTime, now), produceInterval)
    if (next - now < minProducingTime) {
      next += produceInterval
    }
    next
  }

  def nextTime(time: Long, produceInterval: Int) = {
    time + produceInterval - time % produceInterval
  }

  // "timeMs": time from 1970 in ms, should be divided evenly with no remainder by settings.produceInterval
  def getWitness(timeMs: Long, settings: ConsensusSettings): Witness = {
    val slot = timeMs / settings.produceInterval
    var index = slot % (settings.initialWitness.size * settings.producerRepetitions)
    index /= settings.producerRepetitions
    settings.initialWitness(index.toInt)
  }

  def isTimeStampValid(timeStamp: Long, produceInterval: Int): Boolean = {
    if (timeStamp % produceInterval == 0)
      true
    else
      false
  }

  def isProducerValid(timeStamp: Long, producer: PublicKey, settings: ConsensusSettings): Boolean = {
    var isValid = false
    if (getWitness(timeStamp, settings).pubkey.toBin sameElements producer.toBin) {
      if (isTimeStampValid(timeStamp, settings.produceInterval)) {
        isValid = true
      }
    }
    isValid
  }
}

object ProducerRef {
  def props(apexSettings: ApexSettings)
           (implicit ec: ExecutionContext): Props = {
    Props(new Producer(apexSettings))
  }

  def apply(apexSettings: ApexSettings)
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef = {
    system.actorOf(props(apexSettings))
  }

  def apply(apexSettings: ApexSettings, name: String)
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef = {
    system.actorOf(props(apexSettings), name)
  }
}