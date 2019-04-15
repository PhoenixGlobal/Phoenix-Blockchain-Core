package com.apex.plugins.gasprice

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import com.apex.common.ApexLogging
import com.apex.core._
import com.apex.crypto.FixedNumber
import com.apex.rpc.GetAverageCmd
import com.apex.settings.ApexSettings

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.Try

class GasPricePlugin(settings: ApexSettings, mongodbPlugin: Option[ActorRef])
                    (implicit ec: ExecutionContext) extends Actor with ApexLogging {

  private val blocks = mutable.Queue[Block]()
  private var averageValue: FixedNumber = FixedNumber.Zero
  private var txNumber: BigInt = 0
  private val MAX_BLOCKS_IN_QUEUE: Int = 1000

  override def postStop(): Unit = {
    log.info("gasPrice plugin stopped")
    super.postStop()
  }

  override def receive: Receive = {
    case BlockAddedToHeadNotify(blockSummary) => {
      updateGasPrice(blockSummary)
      if (mongodbPlugin.isDefined) mongodbPlugin.get ! UpdateAverageGasPrice(averageValue.toString)
    }
    case GetAverageCmd => {
      val average = Try {
        averageValue.toString
      }
      sender() ! average
    }
    case a: Any => {}
  }

  def updateGasPrice(blockSummary: BlockSummary): Unit = {
    var removeBlock: Option[Block] = None
    if (blocks.size == MAX_BLOCKS_IN_QUEUE) {
      removeBlock = Some(blocks.dequeue)
    }
    blocks.enqueue(blockSummary.block) //add coming block into queue
    calculateAverage(removeBlock, blockSummary.block)
  }

  def calBlock(block: Block): (BigInt, FixedNumber) = {
    var txCount: BigInt = 0
    var totalGasPrice: FixedNumber = FixedNumber.Zero
    block.transactions.foreach(tx => {
      if (tx.txType != TransactionType.Miner) {
        totalGasPrice += tx.gasPrice
        txCount += 1
      }
    })
    (txCount, totalGasPrice)
  }

  def calculateAverage(removeBlock: Option[Block], addBlock: Block): Unit = {
    var sumPrice: FixedNumber = averageValue * txNumber // old value

    if (removeBlock.isDefined) {
      val (txCountRemove, totalGasPriceRemove) = calBlock(removeBlock.get)
      txNumber = txNumber - txCountRemove
      sumPrice = sumPrice - totalGasPriceRemove
    }

    val (txCountAdd, totalGasPriceAdd) = calBlock(addBlock)
    txNumber += txCountAdd
    sumPrice += totalGasPriceAdd

    if (txNumber > 0)
      averageValue = sumPrice / txNumber
    else
      averageValue = FixedNumber.Zero
  }
}

object GasPricePluginRef {
  def props(settings: ApexSettings, mongodbPlugin: Option[ActorRef])
           (implicit ec: ExecutionContext): Props = {
    Props(new GasPricePlugin(settings, mongodbPlugin))
  }

  def apply(settings: ApexSettings, mongodbPlugin: Option[ActorRef])
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef = {
    system.actorOf(props(settings, mongodbPlugin))
  }

  def apply(settings: ApexSettings, mongodbPlugin: Option[ActorRef],
            name: String)
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef = {
    system.actorOf(props(settings, mongodbPlugin), name)
  }
}
