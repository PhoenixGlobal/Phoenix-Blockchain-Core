package com.apex.core

import akka.http.scaladsl.model.DateTime
import com.apex.common.ApexLogging
import com.apex.crypto.{MerkleTree, UInt256}
import com.apex.storage.BlockChainStorage

import scala.collection.mutable.Map

trait BlockChain extends ApexLogging {
  def getHeader(id: UInt256): Option[BlockHeader]

  def getHeader(index: Int): Option[BlockHeader]

  def getBlock(height: Int): Option[Block]

  def containsBlock(id: UInt256): Boolean

  def produceBlock(transactions: Seq[Transaction]): Option[Block]

  def getTransaction(id: UInt256): Option[Transaction]

  def containsTransaction(id: UInt256): Boolean

  def verifyBlock(block: Block): Boolean

  def verifyTransaction(tx: Transaction): Boolean
}

object BlockChain {
  final val Current: BlockChain = new LevelDBBlockChain()

}

class LevelDBBlockChain extends BlockChain {
  private val storage: BlockChainStorage = new BlockChainStorage("test")
  private val genesisBlockHeader: BlockHeader = BlockHeader.build(0, 0, UInt256.Zero, UInt256.Zero)
  private val genesisBlock: Block = Block.build(genesisBlockHeader, Seq.empty)

  private val txMap: Map[UInt256, Transaction] = Map.empty
  private val blockIndexMap: Map[Int, Block] = Map.empty
  private val blockMap: Map[UInt256, Block] = Map.empty

  private var latestHeader: BlockHeader = genesisBlockHeader

  populate()

  override def getHeader(hash: UInt256): Option[BlockHeader] = {
    blockMap.get(hash) match {
      case Some(block) => Some(block.header)
      case _ => None
    }
  }

  override def getHeader(index: Int): Option[BlockHeader] = {
    blockIndexMap.get(index) match {
      case Some(block) => Some(block.header)
      case _ => None
    }
  }

  override def getBlock(index: Int): Option[Block] = {
    blockIndexMap.get(index)
  }

  override def containsBlock(id: UInt256): Boolean = {
    blockMap.contains(id)
  }

  override def produceBlock(transactions: Seq[Transaction]): Option[Block] = {
    val merkleRoot = MerkleTree.root(transactions.map(_.id))
    val header = BlockHeader.build(
      latestHeader.index + 1, DateTime.now.clicks,
      merkleRoot, latestHeader.id)
    val block = Block.build(header, transactions)

    if (storage.writeBlock(block).isSuccess) {
      latestHeader = header
      Some(block)
    } else {
      None
    }
  }

  override def getTransaction(id: UInt256): Option[Transaction] = {
    txMap.get(id)
  }

  override def containsTransaction(id: UInt256): Boolean = {
    txMap.contains(id)
  }

  override def verifyBlock(block: Block): Boolean = {
    if (verfyHeader(block.header)
      && verifyTxs(block.transactions)) {
      true
    } else {
      false
    }
  }

  override def verifyTransaction(tx: Transaction): Boolean = {
    if (tx.inputs.groupBy(i => (i.blockId, i.index)).exists(_._2.size > 0)) {
      false
    } else {
      true
    }
  }

  private def populate() = {
    storage.scan((_, value) => {
      val block = Block.parseFrom(value).toOption
      block match {
        case Some(blk) => {
          if (blk.header.index > latestHeader.index) {
            latestHeader = blk.header
          }
          blockIndexMap.put(blk.header.index, blk)
          blockMap.put(blk.id, blk)
          blk.transactions.foreach(tx => {
            txMap.put(tx.id, tx)
          })
        }
        case _ =>
      }
    })
  }

  private def verfyHeader(header: BlockHeader): Boolean = {
    if (header.index != latestHeader.index + 1
      || header.timeStamp < latestHeader.timeStamp
      || !header.id.equals(latestHeader.id)) {
      false
    } else {
      true
    }
  }

  private def verifyTxs(txs: Seq[Transaction]): Boolean = {
    txs.isEmpty || txs(0).txType == TransactionType.Miner
  }
}
