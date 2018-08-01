package com.apex.core

import akka.http.scaladsl.model.DateTime
import com.apex.common.ApexLogging
import com.apex.crypto.{MerkleTree, UInt256}
import com.apex.storage.BlockChainStorage

import scala.collection.mutable.Map
import scala.collection.mutable.Set

trait Blockchain extends Iterable[Block] with ApexLogging {
  def getLatestHeader: BlockHeader

  def getHeader(id: UInt256): Option[BlockHeader]

  def getHeader(index: Int): Option[BlockHeader]

  def getBlock(height: Int): Option[Block]

  def getBlock(id: UInt256): Option[Block]

  def containsBlock(id: UInt256): Boolean

  def produceBlock(transactions: Seq[Transaction]): Option[Block]

  def getTransaction(id: UInt256): Option[Transaction]

  def containsTransaction(id: UInt256): Boolean

  def verifyBlock(block: Block): Boolean

  def verifyTransaction(tx: Transaction): Boolean
}

object Blockchain {
  final val Current: Blockchain = new LevelDBBlockchain()

}

class LevelDBBlockchain extends Blockchain {
  private val storage: BlockChainStorage = new BlockChainStorage("test")
  private val genesisBlockHeader: BlockHeader = BlockHeader.build(0, 0, UInt256.Zero, UInt256.Zero)
  private val genesisBlock: Block = Block.build(genesisBlockHeader, Seq.empty)

  private val txBlockMap: Map[UInt256, UInt256] = Map.empty
  private val blockIndexMap: Map[Int, UInt256] = Map.empty
  private val utxoSet: Set[(UInt256, Int)] = Set.empty
  private val blkSet: Set[UInt256] = Set.empty

  private var latestHeader: BlockHeader = genesisBlockHeader

  populate()

  override def iterator: Iterator[Block] = new BlockchainIterator(this)

  override def getLatestHeader: BlockHeader = latestHeader

  override def getHeader(id: UInt256): Option[BlockHeader] = {
    storage.readBlock(id) match {
      case Some(blk) => Some(blk.header)
      case None => None
    }
  }

  override def getHeader(index: Int): Option[BlockHeader] = {
    blockIndexMap.get(index) match {
      case Some(id) => getHeader(id)
      case _ => None
    }
  }

  override def getBlock(id: UInt256): Option[Block] = {
    storage.readBlock(id) match {
      case Some(blk) => Some(blk)
      case None => None
    }
  }

  override def getBlock(index: Int): Option[Block] = {
    blockIndexMap.get(index) match {
      case Some(id) => getBlock(id)
      case _ => None
    }
  }

  override def containsBlock(id: UInt256): Boolean = {
    blkSet.contains(id)
  }

  override def produceBlock(transactions: Seq[Transaction]): Option[Block] = {
    val merkleRoot = MerkleTree.root(transactions.map(_.id))
    val header = BlockHeader.build(
      latestHeader.index + 1, DateTime.now.clicks,
      merkleRoot, latestHeader.id)
    val block = Block.build(header, transactions)

    if (storage.writeBlock(block).isSuccess) {
      updateUTXOSet(transactions)
      latestHeader = header
      blkSet.add(block.id)
      Some(block)
    } else {
      None
    }
  }

  override def getTransaction(id: UInt256): Option[Transaction] = {
    txBlockMap.get(id) match {
      case Some(blkId) => {
        getBlock(blkId) match {
          case Some(blk) => {
            blk.transactions.find(_.id.equals(id))
          }
          case None => None
        }
      }
      case None => None
    }
  }

  override def containsTransaction(id: UInt256): Boolean = {
    txBlockMap.contains(id)
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
    if (tx.inputs.groupBy(i => (i.txId, i.index)).exists(_._2.size > 0)) {
      false
    } else {
      true
    }
  }

  private def populate() = {
    val it = iterator
    while (it.hasNext) {
      val blk = it.next()
      blkSet.add(blk.id)
      blockIndexMap.put(blk.header.index, blk.id)
      updateUTXOSet(blk.transactions)
      for (tx <- blk.transactions) {
        txBlockMap.put(tx.id, blk.id)
      }
    }
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

  private def updateUTXOSet(transactions: Seq[Transaction]) = {
    for (tx <- transactions) {
      for (i <- tx.inputs) {
        utxoSet.remove(i.txId, i.index)
      }
      for (i <- 0 to tx.outputs.length - 1) {
        utxoSet.add(tx.id, i)
      }
    }
  }
}
