/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Node.scala
 *
 * @author: shan.huang@chinapex.com: 2018-7-25 下午1:06@version: 1.0
 */

package com.apex.network

import java.time.{Duration, Instant}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.ByteString
import com.apex.common.ApexLogging
import com.apex.consensus.GenesisConfig
import com.apex.core._
import com.apex.core.settings.ApexSettings
import com.apex.core.utils.NetworkTimeProvider
import com.apex.crypto.UInt256
import com.apex.network.LocalNode._
import com.apex.network.peer.PeerHandlerManager.ReceivableMessages.{BroadCastPeers, GetBroadCastPeers, GetConnectedPeers}
import com.apex.network.peer.{PeerHandlerManager, PeerHandlerManagerRef}
import play.api.libs.json.Json

import scala.collection.mutable.Map

class LocalNode(val peerManager: ActorRef) extends Actor with ApexLogging {

  private val memPool: Map[UInt256, Transaction] = Map.empty

  private val connectedMax: Int = 10

  private var connectedPeers: Seq[ConnectedPeer] = Seq.empty

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
            case Some(blk) => {
              peerManager ! Message(MessageType.BlockProduced, blk.id.data)
              log.info(s"block (${blk.height}, ${blk.timeStamp}) produced")
            }
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

  def beginProduce(config: GenesisConfig): Unit = {
    val actorSystem = ActorSystem()
    val scheduler = actorSystem.scheduler
    implicit val executor = actorSystem.dispatcher

    val blockProducer = new BlockProducer(
      config.initialWitness,
      config.produceInterval,
      config.acceptableTimeError)

    val task = new ProduceTask(blockProducer)
    scheduler.scheduleOnce(Duration.ZERO, task)
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

  override def receive: Receive = {
    case data: ByteString => {
      println(data)
    }
    case blk: Block => {
      println(Json.toJson(blk))
    }
//    case BroadCastPeers(peers) => {
//      connectedPeers = peers
//    }
    case BeginProduce(config) => {
      beginProduce(config)
    }
    case BroadCastBlock(id) => {
      peerManager ! GetBroadCastPeers
    }
    case NewBlock(id) => {
      sender() ! GetBlock(id)
    }
    case GetBlock(id) => {
      Blockchain.Current.getBlock(id) match {
        case Some(block) => sender() ! block
        case None => sender() ! GetBlockFailed
      }
    }
    case unknown: Any => println(unknown)
  }
}

object LocalNode {

  case class NewBlock(id: UInt256)

  case class BroadCastBlock(id: UInt256)

  case class GetBlock(id: UInt256)

  case class GetBlockFailed()

  case class BeginProduce(config: GenesisConfig)

  def beginProduce(localNodeRef: ActorRef, config: GenesisConfig): Unit = {
    localNodeRef ! BeginProduce(config)
  }

  //  //  final val default = new LocalNode()
  //  def launch(settings: ApexSettings, timeProvider: NetworkTimeProvider): LocalNode = {
  //    new LocalNode(settings, timeProvider)
  //  }
}

object LocalNodeRef {
  def props(peerManager: ActorRef): Props = Props(new LocalNode(peerManager))

  def apply(peerManager: ActorRef)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(peerManager))

  def apply(peerManager: ActorRef, name: String)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(peerManager), name)

}
