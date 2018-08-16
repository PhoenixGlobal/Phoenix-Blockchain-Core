/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: LocalNode.scala
 *
 * @author: shan.huang@chinapex.com: 2018-7-25 下午1:06@version: 1.0
 */

package com.apex.network

import java.time.{Duration, Instant}

import akka.actor.{Actor, ActorSystem, Cancellable}

import collection.mutable.Map
import com.apex.common.ApexLogging
import com.apex.consensus.GenesisConfig
import com.apex.crypto.UInt256
import com.apex.core._

class LocalNode(val blockProducer: BlockProducer) extends ApexLogging {
  def this() = this(null)

  private val memPool: Map[UInt256, Transaction] = Map.empty

  private val connectedMax: Int = 10

  private var cancelled = false

  def beginProduce(config: GenesisConfig): Cancellable = {
    val actorSystem = ActorSystem()
    val scheduler = actorSystem.scheduler
    implicit val executor = actorSystem.dispatcher

    val blockProducer = new BlockProducer(
      config.initialWitness,
      config.produceInterval,
      config.acceptableTimeError)

    scheduler.scheduleOnce(Duration.ZERO, new Runnable {
      override def run(): Unit = {
        while (true) {
          blockProducer.produce(memPool.values.toSeq) match {
            case NotYet(npt, ct) => log.info(s"next produce time: $npt, current time: $ct")
            case TimeMissed(tpt, ct) => log.info(s"missed, this produce time: $tpt, current time: $ct")
            case NotMyTurn(name, _) => log.info(s"not my turn, ($name)")
            case Success(block) => block match {
              case Some(blk) => log.info(s"block ${blk.height} produced")
              case None => log.info("produce block failed")
            }
          }

          var sleep = 1000 - Instant.now.toEpochMilli % 1000
          sleep = if (sleep < 10) sleep + 1000 else sleep
          Thread.sleep(sleep)
        }
      }
    })
  }

  // connectedPeers
  // unconnectedPeers
  // badPeers

  //def AcceptPeers() = {  }

  def addTransaction(tx: Transaction): Boolean = {
    //lock (Blockchain.Default.PersistLock)
    //lock (mem_pool)
    if (memPool.contains(tx.id)) return false
    if (Blockchain.Current.containsTransaction(tx.id)) return false
    //if (!tx.Verify(mem_pool.Values)) return false;
    memPool.put(tx.id, tx)
    //CheckMemPool()
    true
  }

  def getMemoryPool(): Seq[Transaction] = {
    memPool.values.toSeq
  }

  def clearMemoryPool() = {
    memPool.clear()
  }

  def getTransaction(hash: UInt256): Option[Transaction] = {

    return memPool.get(hash)
  }

  def containsTransaction(tx: Transaction): Boolean = {

    return memPool.contains(tx.id)
  }

  def removeTransactionsInBlock(block: Block) = {
    //TODO
  }
}

object LocalNode {
  final val default = new LocalNode()
}
