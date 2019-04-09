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

class GasPricePlugin(settings: ApexSettings)
                   (implicit ec: ExecutionContext) extends Actor with ApexLogging {

  private val blocks = mutable.Queue[Block]()
  private var averageValue:FixedNumber = FixedNumber.Zero

  override def postStop(): Unit = {
    log.info("gasPrice plugin stopped")
    super.postStop()
  }

  override def receive: Receive = {
    case BlockAddedToHeadNotify(blockSummary) => {
      if(blocks.size > 1000) blocks.dequeue
      blocks.enqueue(blockSummary.block)
      calculateAverage()
    }
    case GetAverageCmd =>{

      val average = Try {
        if (averageValue != null) averageValue.toString
        else FixedNumber.Zero.toString
      }
      sender() ! average
    }
    case a: Any => {}
  }

  def calculateAverage(): Unit ={
    var sumPrice:FixedNumber = FixedNumber.Zero
    var sumTx:BigInt = 0
    blocks.foreach(block => {
      block.transactions.foreach(tx =>{
        sumPrice += tx.gasPrice
        sumTx += 1
      })
    })

    if(sumTx >0 ) averageValue = sumPrice/sumTx
  }

}

object GasPricePluginRef {
  def props(settings: ApexSettings)
           (implicit ec: ExecutionContext): Props = {
    Props(new GasPricePlugin(settings))
  }

  def apply(settings: ApexSettings)
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef = {
    system.actorOf(props(settings))
  }

  def apply(settings: ApexSettings,
            name: String)
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef = {
    system.actorOf(props(settings), name)
  }
}
