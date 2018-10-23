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
import com.apex.core._
import com.apex.crypto.UInt256
import com.apex.network.rpc._
import com.apex.consensus._

import scala.collection.mutable.{ArrayBuffer, Map}

trait NodeMessage

case class NodeStopMessage() extends NodeMessage

class Node(val chain: Blockchain,
           val peerHandlerManager: ActorRef,
           val producer: ActorRef)
  extends Actor with ApexLogging {

  producer ! NodeIsAliveMessage(self)

  override def receive: Receive = {
    case message: Message => processNetworkMessage(message)
    case cmd: RPCCommand => processRPCCommand(cmd)
    case prodMsg: ProducerMessage => processProducerMessage(prodMsg)
    case msg: NodeStopMessage => {
      log.info("stopping node")
      chain.close()
      context.stop(self)
    }
    case unknown: Any => {
      println("Unknown msg:")
      println(unknown)
    }
  }

  private def processProducerMessage(msg: ProducerMessage) = {
    msg match {
      case BlockStartProduceMessage(witness) => {
        log.debug("got BlockStartProduceMessage")
        chain.startProduceBlock(witness.pubkey)
      }
      case BlockFinalizeProduceMessage(witness, timeStamp) => {
        val block = chain.produceBlockFinalize(witness.pubkey, witness.privkey.get, timeStamp)
        if (block.isDefined) {
          log.info(s"block (${block.get.height}, ${block.get.timeStamp}) produced by ${witness.name} ${block.get.id}")
          producer ! BlockAcceptedMessage(block.get)
          peerHandlerManager ! BlockMessage(block.get)
        }
        else {
          log.error(s"produceBlockFinalize Error, ${witness.name} $timeStamp")
        }
      }
    }
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

  private def processNetworkMessage(message: Message) = {
    //log.info(s"Node processNetworkMessage $message")
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
      log.info(s"success insert block #${msg.block.height} (${msg.block.id})")
      producer ! BlockAcceptedMessage(msg.block)
      peerHandlerManager ! InventoryMessage(new InventoryPayload(InventoryType.Block, Seq(msg.block.id())))
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
        log.info(s"success insert block #${block.height} (${block.id})")
        producer ! BlockAcceptedMessage(block)
        // no need to send INV during sync
        //peerHandlerManager ! InventoryMessage(new Inventory(InventoryType.Block, Seq(block.id())))
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
  def props(chain: Blockchain, peerHandlerManager: ActorRef, producer: ActorRef): Props = Props(new Node(chain, peerHandlerManager, producer))

  def apply(chain: Blockchain, peerHandlerManager: ActorRef, producer: ActorRef)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(chain, peerHandlerManager, producer))

  def apply(chain: Blockchain, peerHandlerManager: ActorRef, producer: ActorRef, name: String)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(chain, peerHandlerManager, producer), name)

}
