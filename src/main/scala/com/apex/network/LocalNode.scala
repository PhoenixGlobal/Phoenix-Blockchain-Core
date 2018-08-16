/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: LocalNode.scala
 *
 * @author: shan.huang@chinapex.com: 2018-7-25 下午1:06@version: 1.0
 */

package com.apex.network

import java.time.{Duration, Instant}

import akka.actor.ActorSystem
import com.apex.common.ApexLogging
import com.apex.consensus.GenesisConfig
import com.apex.core._
import com.apex.crypto.UInt256

import scala.collection.mutable.Map

class LocalNode() extends ApexLogging {
  private val memPool: Map[UInt256, Transaction] = Map.empty

  private val connectedMax: Int = 10

  class ProduceTask(val producer: BlockProducer, private var cancelled: Boolean = false) extends Runnable {
    def cancel(): Unit = {
      cancelled = true
    }

    override def run(): Unit = {
      while (!cancelled) {
        producer.produce(memPool.values.toSeq) match {
          case NotYet(npt, ct) => log.debug(s"next produce time: $npt, current time: $ct")
          case TimeMissed(tpt, ct) => log.debug(s"missed, this produce time: $tpt, current time: $ct")
          case NotMyTurn(name, _) => log.info(s"not my turn, ($name)")
          case Failed(e) => log.error("error occurred when producing block", e)
          case Success(block) => block match {
            case Some(blk) => log.info(s"block (${blk.height}, ${blk.timeStamp}) produced")
            case None => log.error("produce block failed")
          }
        }
        if (!cancelled) {
          var sleep = 1000 - Instant.now.toEpochMilli % 1000
          sleep = if (sleep < 10) sleep + 1000 else sleep
          Thread.sleep(sleep)
        }
      }
    }
  }

  def beginProduce(config: GenesisConfig): ProduceTask = {
    val actorSystem = ActorSystem()
    val scheduler = actorSystem.scheduler
    implicit val executor = actorSystem.dispatcher

    val blockProducer = new BlockProducer(
      config.initialWitness,
      config.produceInterval,
      config.acceptableTimeError)

    val task = new ProduceTask(blockProducer)
    scheduler.scheduleOnce(Duration.ZERO, task)
    task
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
