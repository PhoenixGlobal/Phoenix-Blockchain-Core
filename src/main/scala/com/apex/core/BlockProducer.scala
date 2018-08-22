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

import java.time.Instant
import java.math.BigInteger

import com.apex.consensus.Witness
import com.apex.crypto.Ecdsa.PublicKey

trait ProduceState

case class NotSynced(nextProduceTime: Long, currTime: Long) extends ProduceState

case class NotYet(nextProduceTime: Long, currTime: Long) extends ProduceState

case class TimeMissed(thisProduceTime: Long, currTime: Long) extends ProduceState

case class NotMyTurn(producer: String, pubKey: PublicKey) extends ProduceState

case class Success(block: Option[Block]) extends ProduceState

case class Failed(e: Throwable) extends ProduceState

class BlockProducer(val witnesses: Array[Witness],
                    val produceInterval: Int,
                    val acceptableTimeError: Int) {

  private var canProduce = false

  def produce(txs: Seq[Transaction]): ProduceState = {
    try {
      val now: Long = Instant.now.toEpochMilli

      if (canProduce) {
        tryProduce(txs, now)
      } else {
        val next = nextProduceTime()
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
    val (distance, timeToProduce) = getDistanceAndProduceTime(now + acceptableTimeError)
    if (distance == 0) {
      NotYet(timeToProduce, now)
    } else {
      if ((now - timeToProduce).abs > acceptableTimeError) {
        TimeMissed(timeToProduce, now)
      } else {
        val witness = getWitness(distance)
        if (witness.privkey.isEmpty) {
          NotMyTurn(witness.name, witness.pubkey)
        } else {
          val block = Blockchain.Current.produceBlock(
            witness.pubkey.toBin,
            witness.privkey.get,
            now + acceptableTimeError,
            distance,
            txs
          )
          Success(block)
        }
      }
    }
  }

  private def getDistanceAndProduceTime(curr: Long): (Long, Long) = {
    val next = nextProduceTime()
    if (curr < next) {
      (0, next)
    } else {
      val distance = (curr - next) / produceInterval
      val time = next + distance * produceInterval
      (distance + 1, time)
    }
  }

  private def nextProduceTime(nextN: Int = 1): Long = {
    val headTime = Blockchain.Current.getHeadTime
    (headTime / produceInterval + nextN) * produceInterval
  }

  private def getWitness(relativeDistance: Long): Witness = {
    val dis = Blockchain.Current.getDistance + relativeDistance
    val pos = dis % witnesses.length
    witnesses(pos.toInt)
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
