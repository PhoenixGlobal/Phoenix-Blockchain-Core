package com.apex.core

import java.io.{DataInputStream, DataOutputStream}

import akka.http.scaladsl.model.DateTime
import com.apex.common.{ApexLogging, Serializable}
import com.apex.crypto.{Fixed8, MerkleTree, UInt160, UInt256}
import com.apex.storage.LevelDbStorage

import scala.collection.mutable.{Map, Set}

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

  def getBalance(address: UInt160): Option[collection.immutable.Map[UInt256, Long]]

  def getUTXOSet: UTXOSet

  def getUTXOByAddress(address: UInt160): Option[Set[(UInt256, Int)]]
}

object Blockchain {
  final val Current: Blockchain = new LevelDBBlockchain()

}

class LevelDBBlockchain extends Blockchain {
  private val db: LevelDbStorage = LevelDbStorage.open("test_db")

  private val headerStore = new HeaderStore(db)
  private val heightStore = new HeightStore(db)
  private val txStore = new TransactionStore(db)
  private val accountStore = new AccountStore(db)
//  private val addressStore = new AddressStore(db)
  private val blkTxMappingStore = new BlkTxMappingStore(db)
  private val headBlkStore = new HeadBlockStore(db)
  private val utxoStore = new UTXOStore(db)

  private val genesisBlockHeader: BlockHeader = BlockHeader.build(0, 0, UInt256.Zero, UInt256.Zero)
  private val genesisBlock: Block = Block.build(genesisBlockHeader, Seq.empty)

  private val utxoSet: Set[(UInt256, Int)] = Set.empty

  private var latestHeader: BlockHeader = genesisBlockHeader

  populate()

  override def iterator: Iterator[Block] = new BlockchainIterator(this)

  override def getLatestHeader: BlockHeader = latestHeader

  override def getHeader(id: UInt256): Option[BlockHeader] = {
    headerStore.get(id) match {
      case Some(blk) => Some(blk)
      case None => None
    }
  }

  override def getHeader(index: Int): Option[BlockHeader] = {
    heightStore.get(index) match {
      case Some(id) => getHeader(id)
      case _ => None
    }
  }

  override def getBlock(id: UInt256): Option[Block] = {
    headerStore.get(id) match {
      case Some(header) => {
        var txs = Seq.empty[Transaction]
        val mapping = blkTxMappingStore.get(header.id)
        if (!mapping.isEmpty) {
          txs = mapping.get.txIds
            .map(txStore.get(_))
            .filterNot(_.isEmpty)
            .map(_.get)
        }
        Some(Block.build(header, txs))
      }
      case None => None
    }
  }

  override def getBlock(index: Int): Option[Block] = {
    heightStore.get(index) match {
      case Some(id) => getBlock(id)
      case _ => None
    }
  }

  override def containsBlock(id: UInt256): Boolean = {
    headerStore.contains(id)
  }

  override def produceBlock(transactions: Seq[Transaction]): Option[Block] = {
    def calcBalancesInBlock(balances: Map[UInt160, Map[UInt256, Fixed8]], output: TransactionOutput, spent: Boolean) = {
      val amount = if (spent) -output.amount else output.amount
      balances.get(output.address) match {
        case Some(balance) => {
          balance(output.assetId) += amount
        }
        case None => balances.put(output.address,
          Map((output.assetId, amount)))
      }
    }

    val txs = transactions.filter(verifyTransaction(_))
    val merkleRoot = MerkleTree.root(transactions.map(_.id))
    val header = BlockHeader.build(
      latestHeader.index + 1, DateTime.now.clicks,
      merkleRoot, latestHeader.id)
    val block = Block.build(header, txs)
    val ret = db.batchWrite(batch => {
      headBlkStore.set(header, batch)
      headerStore.set(header.id, header, batch)
      heightStore.set(header.index, header.id, batch)
      val blkTxMapping = BlkTxMapping(block.id, txs.map(_.id))
      blkTxMappingStore.set(block.id, blkTxMapping, batch)
      val balances = Map.empty[UInt160, Map[UInt256, Fixed8]]
      txs.foreach(tx => {
        txStore.set(tx.id, tx, batch)
        for (index <- 0 to tx.outputs.length - 1) {
          val key = UTXOKey(tx.id, index)
          val output = tx.outputs(index)
          utxoStore.set(key, output, batch)
          calcBalancesInBlock(balances, output, false)
        }
        for (i <- tx.inputs) {
          val key = UTXOKey(i.txId, i.index)
          utxoStore.delete(key, batch)
          val output = getTransaction(i.txId).get.outputs(i.index)
          calcBalancesInBlock(balances, output, true)
        }
      })
      balances.foreach(p => {
        accountStore.get(p._1) match {
          case Some(account) => {
            val merged = account.balances.toSeq ++ p._2.toSeq
            val balances = merged
              .groupBy(_._1)
              .map(p => (p._1, Fixed8.sum(p._2.map(_._2).sum)))
              .filter(_._2.value > 0)
            val a = new Account(account.active, balances, account.version)
            accountStore.set(p._1, a, batch)
          }
          case None => {
            val a = new Account(true, p._2.filter(_._2.value > 0).toMap)
            accountStore.set(p._1, a, batch)
          }
        }
      })
    })
    if (ret) {
      latestHeader = header
      Some(block)
    } else {
      None
    }
  }

  override def getTransaction(id: UInt256): Option[Transaction] = {
    txStore.get(id)
  }

  override def containsTransaction(id: UInt256): Boolean = {
    txStore.contains(id)
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

  override def getBalance(address: UInt160): Option[collection.immutable.Map[UInt256, Long]] = {
    accountStore.get(address) match {
      case Some(account) => {
        if (account.active) {
          Some(account.balances.map(b => b._1 -> b._2.value))
        } else {
          None
        }
      }
      case None => None
    }
  }

  override def getUTXOSet: UTXOSet = {
    new UTXOSet(utxoStore)
  }

  override def getUTXOByAddress(address: UInt160): Option[Set[(UInt256, Int)]] = {
    val utxoSet = Set.empty[(UInt256, Int)]
    utxoStore.foreach((k, v) => {
      if (v.address.equals(address)) {
        utxoSet.add(UTXOKey.unapply(k).get)
      }
    })
    if (utxoSet.isEmpty){
      None
    } else {
      Some(utxoSet)
    }
  }

  private def populate(): Unit = {
    headBlkStore.get match {
      case Some(headBlock) =>
        headerStore.get(headBlock.id) match {
          case Some(header) => latestHeader = header
          case None => db.batchWrite(batch => {
            headerStore.set(genesisBlockHeader.id, genesisBlockHeader, batch)
            headBlkStore.set(genesisBlockHeader, batch)
          })
        }
      case None => {
        headBlkStore.set(genesisBlockHeader)
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
    txs.exists(!_.inputs
      .map(i => UTXOKey(i.txId, i.index))
      .exists(!utxoStore.contains(_))
    )
  }
}


