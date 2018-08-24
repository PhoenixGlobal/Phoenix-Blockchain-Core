/*
 *
 *
 *
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: BlockProducer.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-8-15 下午2:27@version: 1.0
 */

package com.apex.core

import java.math.BigInteger
import java.time.Instant

import com.apex.core.settings.{ConsesusConfig, Witness}
import com.apex.crypto.Ecdsa.PublicKey

trait ProduceState

case class NotSynced(nextProduceTime: Long, currTime: Long) extends ProduceState

case class NotYet(nextProduceTime: Long, currTime: Long) extends ProduceState

case class TimeMissed(thisProduceTime: Long, currTime: Long) extends ProduceState

case class NotMyTurn(producer: String, pubKey: PublicKey) extends ProduceState

case class Success(block: Option[Block], producer: String, time: Long) extends ProduceState

case class Failed(e: Throwable) extends ProduceState

class BlockProducer(config: ConsesusConfig) {

  private var canProduce = false

  def produce(txs: Seq[Transaction]): ProduceState = {
    try {
      val now: Long = Instant.now.toEpochMilli

      if (canProduce) {
        tryProduce(txs, now)
      } else {
        val next = nextBlockTime()
        if (next >= now) {
          canProduce = true
          tryProduce(txs, now)
        } else {
          NotSynced(next, now)
        }
      }
    } catch {
      case e: Throwable => Failed(e)
    }
  }

  private def tryProduce(txs: Seq[Transaction], now: Long) = {
    val next = nextBlockTime()
    //val (distance, timeToProduce) = getDistanceAndProduceTime(now + config.acceptableTimeError)
    if (now + config.acceptableTimeError < next) {    // if (distance == 0) {
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
        if (next + config.acceptableTimeError < now)
          timeToProduce = now
        else
          timeToProduce = next
        val block = Blockchain.Current.produceBlock(
              witness.pubkey.toBin,
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
    val headTime = Blockchain.Current.getHeadTime
    var slot = headTime / config.produceInterval
    slot += nextN
    slot * config.produceInterval   //   (headTime / produceInterval + nextN) * produceInterval
  }

  private def nextProduceTime(now: Long, next: Long): Long = {
    if (now <= next) {
      next
    }
    else {
      val slot = now / config.produceInterval
      slot * config.produceInterval
    }
  }

  // timeMs: time from 1970 in ms
  private def getWitness(timeMs: Long): Witness = {
    val slot = timeMs / config.produceInterval
    var index = slot % (config.initialWitness.size * config.producerRepetitions)
    index /= config.producerRepetitions
    config.initialWitness(index.toInt)
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

}
