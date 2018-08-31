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

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.apex.common.ApexLogging
import com.apex.core._
import com.apex.crypto.Ecdsa.PublicKey
import com.apex.crypto.UInt256
import com.apex.network.{MessagePack, MessageType, Node}
import com.apex.settings.{ConsensusSettings, Witness}

import scala.collection.mutable.Map
import scala.concurrent.ExecutionContext

trait ProduceState

case class NotSynced(nextProduceTime: Long, currTime: Long) extends ProduceState

case class NotYet(nextProduceTime: Long, currTime: Long) extends ProduceState

case class NotMyTurn(producer: String, pubKey: PublicKey) extends ProduceState

case class TimeMissed(thisProduceTime: Long, currTime: Long) extends ProduceState

case class Success(block: Option[Block], producer: String, time: Long) extends ProduceState

case class Failed(e: Throwable) extends ProduceState

class ProduceTask(val producer: Producer,
                  val peerManager: ActorRef,
                  private var cancelled: Boolean = false)
  extends Runnable with ApexLogging {

  def cancel(): Unit = {
    cancelled = true
  }

  override def run(): Unit = {
    while (!cancelled) {
      var sleep = 500 - Instant.now.toEpochMilli % 500
      sleep = if (sleep < 10) sleep + 500 else sleep
      Thread.sleep(sleep)

      if (!cancelled) {
        producer.produce() match {
          case NotSynced(_, _) => log.info(s"not synced")
          case NotYet(npt, ct) => log.debug(s"Not yet, next produce time: $npt, current time: $ct")
          case TimeMissed(tpt, ct) => log.debug(s"missed, this produce time: $tpt, current time: $ct")
          case NotMyTurn(name, _) => log.debug(s"not my turn, ($name)")
          case Failed(e) => log.error("error occurred when producing block", e)
          case Success(block, producer, time) => block match {
            case Some(blk) => {
              peerManager ! MessagePack(MessageType.BlockProduced, blk.id.data)
              log.info(s"block (${blk.height}, ${blk.timeStamp}) produced by $producer on $time")
            }
            case None => log.error("produce block failed")
          }
        }
      }
    }
  }
}

class Producer(settings: ConsensusSettings,
               chain: Blockchain, peerManager: ActorRef)
              (implicit system: ActorSystem) extends Actor with ApexLogging {

  implicit val executionContext: ExecutionContext = system.dispatcher

  private val txPool: Map[UInt256, Transaction] = Map.empty

  private var canProduce = false

  private val task = new ProduceTask(this, peerManager)
  system.scheduler.scheduleOnce(Duration.ZERO, task)

  def produce(): ProduceState = {
    try {
      val now: Long = Instant.now.toEpochMilli

      if (canProduce) {
        tryProduce(now)
      } else {
        val next = nextBlockTime()
        if (next >= now) {
          canProduce = true
          tryProduce(now)
        } else {
          NotSynced(next, now)
        }
      }
    } catch {
      case e: Throwable => Failed(e)
    }
  }

  private def tryProduce(now: Long) = {
    val next = nextBlockTime()
    //val (distance, timeToProduce) = getDistanceAndProduceTime(now + config.acceptableTimeError)
    if (now + settings.acceptableTimeError < next) { // if (distance == 0) {
      NotYet(next, now)
    } else {
      if (nextProduceTime(now, next) > next) {
        //println(s"some blocks skipped")
      }
      val witness = getWitness(nextProduceTime(now, next))
      if (witness.privkey.isEmpty) {
        NotMyTurn(witness.name, witness.pubkey)
      } else {
        var timeToProduce: Long = 0
        if (next + settings.acceptableTimeError < now)
          timeToProduce = now
        else
          timeToProduce = next
        val txs = txPool.values.toSeq
        val block = chain.produceBlock(
          witness.pubkey,
          witness.privkey.get,
          nextProduceTime(now, next),
          txs)
        Success(block, witness.name, now)
      }
    }
  }

  //  private def getDistanceAndProduceTime(curr: Long): (Long, Long) = {
  //    val next = nextProduceTime()
  //    if (curr < next) {
  //      (0, next)
  //    } else {
  //      val distance = (curr - next) / config.produceInterval
  //      val time = next + distance * config.produceInterval
  //      (distance + 1, time)
  //    }
  //  }

  private def nextBlockTime(nextN: Int = 1): Long = {
    val headTime = chain.getHeadTime
    var slot = headTime / settings.produceInterval
    slot += nextN
    slot * settings.produceInterval //   (headTime / produceInterval + nextN) * produceInterval
  }

  private def nextProduceTime(now: Long, next: Long): Long = {
    if (now <= next) {
      next
    }
    else {
      val slot = now / settings.produceInterval
      slot * settings.produceInterval
    }
  }

  // timeMs: time from 1970 in ms
  private def getWitness(timeMs: Long): Witness = {
    val slot = timeMs / settings.produceInterval
    var index = slot % (settings.initialWitness.size * settings.producerRepetitions)
    index /= settings.producerRepetitions
    settings.initialWitness(index.toInt)
  }

  def updateWitnessSchedule(nowSec: Long, witnesses: Array[Witness]): Array[Witness] = {
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

  override def receive: Receive = {
    case a: Any => {
      log.info(s"${sender().toString}, ${a.toString}")
    }
  }
}

object ProducerRef {
  def props(settings: ConsensusSettings,
            chain: Blockchain, peerManager: ActorRef)
           (implicit system: ActorSystem): Props = {
    Props(new Producer(settings, chain, peerManager))
  }

  def apply(settings: ConsensusSettings,
            chain: Blockchain, peerManager: ActorRef)
           (implicit system: ActorSystem): ActorRef = {
    system.actorOf(props(settings, chain, peerManager))
  }

  def apply(settings: ConsensusSettings,
            chain: Blockchain, peerManager: ActorRef, name: String)
           (implicit system: ActorSystem): ActorRef = {
    system.actorOf(props(settings, chain, peerManager), name)
  }
}