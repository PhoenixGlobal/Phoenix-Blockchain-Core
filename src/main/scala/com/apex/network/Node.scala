/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Node.scala
 *
 * @author: shan.huang@chinapex.com: 2018-7-25 下午1:06@version: 1.0
 */

package com.apex.network

import java.io.{ByteArrayInputStream, DataInputStream}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.apex.common.ApexLogging
import com.apex.consensus._
import com.apex.core._
import com.apex.crypto.UInt256
import com.apex.network.peer.PeerHandlerManagerRef
import com.apex.network.rpc._
import com.apex.settings.ApexSettings
import com.apex.utils.NetworkTimeProvider

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext

trait NodeMessage

trait AsyncTask

case class NodeStopMessage() extends NodeMessage

case class ProduceTask(task: Blockchain => Unit) extends AsyncTask

class Node(val settings: ApexSettings)
          (implicit ec: ExecutionContext)
  extends Actor with ApexLogging {

  private val notification = Notification(onBlock, onTransaction)

  private val chain = Blockchain.populate(settings.chain, settings.consensus, notification)

  private val timeProvider = new NetworkTimeProvider(settings.ntp)

  private val peerHandlerManager = PeerHandlerManagerRef(settings.network, timeProvider)

  private val networkManager = NetworkManagerRef(settings.network, chain.getChainInfo, timeProvider, peerHandlerManager)

  private val producer = ProducerRef(settings.consensus, peerHandlerManager)

  override def receive: Receive = {
    case task: AsyncTask => processAsyncTask(task)
    case message: NetworkMessage => processNetworkMessage(message)
    case cmd: RPCCommand => processRPCCommand(cmd)
    case prodMsg: ProducerMessage => processProducerMessage(prodMsg)
    case _: NodeStopMessage => {
      log.info("stopping node")
      //TODO close connections
      chain.close()
      context.stop(self)
    }
    case unknown: Any => {
      println("Unknown msg:")
      println(unknown)
    }
  }

  private def onBlock(block: Block): Unit = {
    log.info(s"block (${block.height}, ${block.timeStamp}) produced by ${block.header.producer.toAddress.substring(0, 6)} ${block.id.toString.substring(0, 6)}")
    peerHandlerManager ! BlockMessage(block)
  }

  private def onTransaction(trx: Transaction): Unit = {
    log.info(trx.toString)
  }

  private def processAsyncTask(asyncTask: AsyncTask): Unit = {
    asyncTask match {
      case ProduceTask(task) => task(chain)
      case _ => println(asyncTask)
    }
  }

  private def processProducerMessage(msg: ProducerMessage) = {
    log.info(msg.toString)
  }

  private def processRPCCommand(cmd: RPCCommand) = {
    cmd match {
      case GetBlockByIdCmd(id) => {
        sender() ! chain.getBlock(id)
      }
      case GetBlockByHeightCmd(height) => {
        sender() ! chain.getBlock(height)
      }
      case GetBlockCountCmd() => {
        sender() ! chain.getHeight()
      }
      case GetBlocksCmd() => {
        val blockNum = chain.getHeight()
        val blocks = ArrayBuffer.empty[Block]
        for (i <- 0 to blockNum) {
          blocks.append(chain.getBlock(i).get)
        }
        sender() ! blocks
      }
      case GetAccountCmd(address) => {
        sender() ! chain.getAccount(address)
      }
      case SendRawTransactionCmd(rawTx) => {
        val is = new DataInputStream(new ByteArrayInputStream(rawTx))
        val tx = Transaction.deserialize(is)
        if (tx.verifySignature()) {
          peerHandlerManager ! InventoryMessage(new InventoryPayload(InventoryType.Tx, Seq(tx.id)))
          if (chain.addTransaction(tx))
            sender() ! true
          else
            sender() ! false
        }
        else
          sender() ! false
      }
    }
  }

  private def processNetworkMessage(message: NetworkMessage) = {
    log.debug(s"Node processNetworkMessage $message")
    message match {
      case VersionMessage(height) => {
        processVersionMessage(message.asInstanceOf[VersionMessage])
      }
      case GetBlocksMessage(blockHashs) => {
        processGetBlocksMessage(message.asInstanceOf[GetBlocksMessage])
      }
      case BlockMessage(block) => {
        processBlockMessage(message.asInstanceOf[BlockMessage])
      }
      case BlocksMessage(blocksPayload) => {
        processBlocksMessage(message.asInstanceOf[BlocksMessage])
      }
      case TransactionsMessage(txsPayload) => {
        processTransactionsMessage(message.asInstanceOf[TransactionsMessage])
      }
      case InventoryMessage(inv) => {
        processInventoryMessage(message.asInstanceOf[InventoryMessage])
      }
      case GetDataMessage(inv) => {
        processGetDataMessage(message.asInstanceOf[GetDataMessage])
      }
    }
  }

  private def processVersionMessage(msg: VersionMessage) = {

  }

  private def processGetBlocksMessage(msg: GetBlocksMessage) = {
    //log.info("received GetBlocksMessage")
    val hashs = ArrayBuffer.empty[UInt256]
    val hash = msg.blockHashs.hashStart(0)
    if (hash.equals(UInt256.Zero)) {
      sender() ! InventoryMessage(new InventoryPayload(InventoryType.Block, Seq(chain.getLatestHeader.id))).pack()
    }
    else {
      hashs.append(hash)
      var next = chain.getNextBlockId(hash)
      while (next.isDefined) {
        hashs.append(next.get)
        next = chain.getNextBlockId(next.get)
      }
      //log.info("send InventoryMessage")
      sender() ! InventoryMessage(new InventoryPayload(InventoryType.Block, hashs.toSeq)).pack()
    }
  }

  private def processBlockMessage(msg: BlockMessage) = {
    //log.info(s"received a block #${block.height} (${block.id})")
    if (chain.tryInsertBlock(msg.block, true)) {
      peerHandlerManager ! InventoryMessage(new InventoryPayload(InventoryType.Block, Seq(msg.block.id())))
      log.info(s"success insert block #${msg.block.height} (${msg.block.id})")
    } else {
      log.error(s"failed insert block #${msg.block.height}, (${msg.block.id}) to db")
      if (msg.block.height() > chain.getLatestHeader.index) {
        // out of sync, try to get more blocks
        log.info(s"send GetBlocksMessage")
        sender() ! GetBlocksMessage(new GetBlocksPayload(Seq(chain.getLatestHeader.id), UInt256.Zero)).pack
      }
    }
  }

  private def processBlocksMessage(msg: BlocksMessage) = {
    log.info(s"received ${msg.blocks.blocks.size} blocks")
    msg.blocks.blocks.foreach(block => {
      if (chain.tryInsertBlock(block, true)) {
        // no need to send INV during sync
        //peerHandlerManager ! InventoryMessage(new Inventory(InventoryType.Block, Seq(block.id())))
        log.info(s"success insert block #${block.height} (${block.id})")
      } else {
        log.error(s"failed insert block #${block.height}, (${block.id}) to db")
      }
    })
    // try to get more blocks if have any
    sender() ! GetBlocksMessage(new GetBlocksPayload(Seq(chain.getLatestHeader.id), UInt256.Zero)).pack
  }

  private def processTransactionsMessage(msg: TransactionsMessage) = {
    log.info(s"received ${msg.txs.txs.size} transactions from network")
    //producer ! ReceivedNewTransactions(txsPayload.txs)
    msg.txs.txs.foreach(tx => {
      if (tx.verifySignature())
        chain.addTransaction(tx)
    })
    // TODO: for the new txs broadcast INV
  }

  private def processInventoryMessage(msg: InventoryMessage) = {
    //log.info(s"received Inventory")
    val inv = msg.inv
    if (inv.invType == InventoryType.Block) {
      val newBlocks = ArrayBuffer.empty[UInt256]
      inv.hashs.foreach(h => {
        if (chain.containBlock(h) == false)
          newBlocks.append(h)
      })
      if (newBlocks.size > 0) {
        log.info(s"send GetDataMessage to request ${newBlocks.size} new blocks.  $newBlocks")
        sender() ! GetDataMessage(new InventoryPayload(InventoryType.Block, newBlocks.toSeq)).pack
      }
    }
    else if (inv.invType == InventoryType.Tx) {
      // val newTxs = ArrayBuffer.empty[UInt256]
      // TODO: only request the txs that we don't have
      sender() ! GetDataMessage(new InventoryPayload(InventoryType.Tx, inv.hashs)).pack
    }
  }

  private def processGetDataMessage(msg: GetDataMessage) = {
    //log.info(s"received GetDataMessage")
    if (msg.inv.invType == InventoryType.Block) {
      val sendBlockNumMax: Int = 10
      var sentBlockNum: Int = 0
      val blocks = ArrayBuffer.empty[Block]
      msg.inv.hashs.foreach(h => {
        val block = chain.getBlock(h)
        if (block != None) {
          if (sentBlockNum < sendBlockNumMax) {
            //sender() ! BlockMessage(block.get).pack
            blocks.append(block.get)
            sentBlockNum += 1
          }
        }
        else
          log.error("received GetDataMessage but block not found")
      })
      if (blocks.size > 0) {
        sender() ! BlocksMessage(new BlocksPayload(blocks.toSeq)).pack
      }
    }
    else if (msg.inv.invType == InventoryType.Tx) {
      val txs = ArrayBuffer.empty[Transaction]
      msg.inv.hashs.foreach(h => {
        val tx = chain.getPendingTransaction(h)
        if (tx.isDefined) {
          txs.append(tx.get)
        }
      })
      if (txs.size > 0) {
        sender() ! TransactionsMessage(new TransactionsPayload(txs)).pack
      }
    }
  }
}

object NodeRef {

  def props(settings: ApexSettings)(implicit system: ActorSystem, ec: ExecutionContext): Props = Props(new Node(settings))

  def apply(settings: ApexSettings)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = system.actorOf(props(settings))

  def apply(settings: ApexSettings, name: String)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = system.actorOf(props(settings), name)

}
