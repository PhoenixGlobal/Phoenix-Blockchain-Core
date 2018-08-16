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

import com.apex.consensus.Witness
import com.apex.crypto.Ecdsa.PublicKey

trait ProduceState

case class NotYet(nextProduceTime: Long, currTime: Long) extends ProduceState

case class TimeMissed(thisProduceTime: Long, currTime: Long) extends ProduceState

case class NotMyTurn(producer: String, pubKey: PublicKey) extends ProduceState

case class Success(block: Option[Block]) extends ProduceState

case class Failed(e: Throwable) extends ProduceState

class BlockProducer(val witnesses: Array[Witness],
                    val produceInterval: Int,
                    val acceptableTimeError: Int) {

  def produce(txs: Seq[Transaction]): ProduceState = {
    try {
      val now: Long = Instant.now.toEpochMilli
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
    } catch {
      case e: Throwable => Failed(e)
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
}
