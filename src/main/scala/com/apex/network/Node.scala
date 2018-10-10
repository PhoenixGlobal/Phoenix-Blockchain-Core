/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Node.scala
 *
 * @author: shan.huang@chinapex.com: 2018-7-25 下午1:06@version: 1.0
 */

package com.apex.network

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.apex.common.ApexLogging
import com.apex.core._
import com.apex.crypto.UInt256
import com.apex.network.rpc.{GetBlockByHeightCmd, GetBlockByIdCmd, GetBlockCountCmd, GetBlocksCmd, GetAccountCmd, RPCCommand}
import com.apex.consensus.BlockAcceptedMessage

import scala.collection.mutable.{ArrayBuffer, Map}

class Node(val chain: Blockchain,
           val peerHandlerManager: ActorRef,
           val producer: ActorRef)
  extends Actor with ApexLogging {

//  def addTransaction(tx: Transaction): Boolean = {
//    //lock (Blockchain.Default.PersistLock)
//    //lock (mem_pool)
//    if (memPool.contains(tx.id)) return false
//    if (chain.containsTransaction(tx.id)) return false
//    //if (!tx.Verify(mem_pool.Values)) return false;
//    memPool.put(tx.id, tx)
//    //CheckMemPool()
//    true
//  }
//
//  def getMemoryPool(): Seq[Transaction] = {
//    memPool.values.toSeq
//  }
//
//  def clearMemoryPool() = {
//    memPool.clear()
//  }
//
//  def getTransaction(hash: UInt256): Option[Transaction] = {
//
//    return memPool.get(hash)
//  }
//
//  def containsTransaction(tx: Transaction): Boolean = {
//
//    return memPool.contains(tx.id)
//  }

  def removeTransactionsInBlock(block: Block) = {
    //TODO
  }

  override def receive: Receive = {
    case message: Message => processMessage(message)
    case cmd: RPCCommand => processRPCCommand(cmd)
    case unknown: Any => {
      println("Unknown msg:")
      println(unknown)
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
    }
  }

  private def processMessage(message: Message) = {
    message match {
      case VersionMessage(height) => {
        if (height < chain.getHeight) {
          //sender() ! GetBlockMessage(height).pack
        }
      }
      case GetBlocksMessage(blockHashs) => {
        //log.info("received GetBlocksMessage")
        val hashs = ArrayBuffer.empty[UInt256]
        val hash = blockHashs.hashStart(0)
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
      case BlockMessage(block) => {
        //log.info(s"received a block #${block.height} (${block.id})")
        if (chain.tryInsertBlock(block, true)) {
          log.info(s"success insert block #${block.height} (${block.id})")
          producer ! BlockAcceptedMessage(block)
          peerHandlerManager ! InventoryMessage(new InventoryPayload(InventoryType.Block, Seq(block.id())))
        } else {
          log.error(s"failed insert block #${block.height}, (${block.id}) to db")
          if (block.height() > chain.getLatestHeader.index) {
            // out of sync, try to get more blocks
            log.info(s"send GetBlocksMessage")
            sender() ! GetBlocksMessage(new GetBlocksPayload(Seq(chain.getLatestHeader.id), UInt256.Zero)).pack
          }
        }
      }
      case BlocksMessage(blocksPayload) => {
        log.info(s"received ${blocksPayload.blocks.size} blocks")
        blocksPayload.blocks.foreach(block => {
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
      case InventoryMessage(inv) => {
        //log.info(s"received Inventory")
        if (inv.invType == InventoryType.Block) {
          val newBlocks = ArrayBuffer.empty[UInt256]
          inv.hashs.foreach(h => {
            if (chain.getBlock(h) == None) {
              if (chain.getBlockInForkBase(h) == None) {
                newBlocks.append(h)
              }
            }
          })
          if (newBlocks.size > 0) {
            log.info(s"send GetDataMessage $newBlocks")
            sender() ! GetDataMessage(new InventoryPayload(InventoryType.Block, newBlocks.toSeq)).pack
          }
        }
      }
      case GetDataMessage(inv) => {
        //log.info(s"received GetDataMessage")
        if (inv.invType == InventoryType.Block) {
          val sendBlockNumMax: Int = 10
          var sentBlockNum: Int = 0
          val blocks = ArrayBuffer.empty[Block]
          inv.hashs.foreach(h => {
            var block = chain.getBlock(h)
            if (block == None) {
              block = chain.getBlockInForkBase(h)
            }
            if (block != None) {
              if (sentBlockNum < sendBlockNumMax) {
                //sender() ! BlockMessage(block.get).pack
                blocks.append(block.get)
                sentBlockNum += 1
              }
            }
            else {
              log.error("received GetDataMessage but block not found")
            }
          })
          if (blocks.size > 0) {
            sender() ! BlocksMessage(new BlocksPayload(blocks.toSeq)).pack
          }
        }
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
