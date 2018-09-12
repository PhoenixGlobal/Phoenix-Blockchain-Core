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
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.apex.common.ApexLogging
import com.apex.core._
import com.apex.crypto.Ecdsa.PublicKey
import com.apex.crypto.UInt256
import com.apex.network.rpc.SendRawTransactionCmd
import com.apex.network._
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
          case NotSynced(_, _) => log.debug(s"not synced")
          case NotYet(npt, ct) => log.debug(s"Not yet, next produce time: $npt, current time: $ct")
          case TimeMissed(tpt, ct) => log.debug(s"missed, this produce time: $tpt, current time: $ct")
          case NotMyTurn(name, _) => log.debug(s"not my turn, ($name)")
          case Failed(e) => log.error("error occurred when producing block", e)
          case Success(block, producer, time) => block match {
            case Some(blk) => {
              log.info(s"block (${blk.height}, ${blk.timeStamp}) produced by $producer on $time")
              peerManager ! BlockMessage(blk)
              //peerManager ! InventoryMessage(new Inventory(InventoryType.Block, Seq(blk.id())))
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

  private val txPool: scala.collection.mutable.Map[UInt256, Transaction] = scala.collection.mutable.Map.empty

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
    if (now + settings.acceptableTimeError < next) {
      NotYet(next, now)
    } else {
      if (nextProduceTime(now, next) > next) {
        //println(s"some blocks skipped")
      }
      val witness = getWitness(nextProduceTime(now, next))
      if (witness.privkey.isEmpty) {
        NotMyTurn(witness.name, witness.pubkey)
      } else {
        val txs = txPool.values.toSeq

        /*
         * the timestamp in every block header
         * should be divided evenly with no remainder by settings.produceInterval
         */
        val block = chain.produceBlock(
          witness.pubkey,
          witness.privkey.get,
          nextProduceTime(now, next),
          txs)
        Success(block, witness.name, now)
      }
    }
  }

  // the nextBlockTime is the expected time of next block based on current latest block
  private def nextBlockTime(nextN: Int = 1): Long = {
    val headTime = chain.getLatestHeader.timeStamp
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

  // "timeMs": time from 1970 in ms, should be divided evenly with no remainder by settings.produceInterval
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
    case SendRawTransactionCmd(rawTx) => {
      val is = new DataInputStream(new ByteArrayInputStream(rawTx))
      val tx = Transaction.deserialize(is)
      if (tx.verifySignature()) {
        txPool += (tx.id -> tx)
        sender() ! true
      }
      else {
        sender() ! false
      }
    }
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