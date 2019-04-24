/*
 * Copyright  2019 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: TransactionPool.scala
 *
 * @author: shan.huang@chinapex.com: 19-3-22 下午4:06@version: 1.0
 */

package com.apex.core

import java.time.Instant

import com.apex.common.ApexLogging
import com.apex.crypto.UInt256

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class TxEntry(tx: Transaction,
                   firstSeenTime: Long = 0) extends Ordered[TxEntry] {
  override def compare(that: TxEntry): Int = {
    var ret = 1
    if (tx.id == that.tx.id)
      ret = 0
    else if (tx.from == that.tx.from) {
      if (tx.nonce < that.tx.nonce)
        ret = -1
    }
    else if (tx.gasPrice > that.tx.gasPrice)
      ret = -1
    else if (firstSeenTime < that.firstSeenTime) // currently not used
      ret = -1
    ret
  }
}

class TransactionPool extends ApexLogging {

  val txsMap = mutable.LinkedHashMap.empty[UInt256, Transaction]
  var txsSorted = mutable.SortedSet.empty[TxEntry]

  private var txTotalSize: Long = 0
  private val txTotalSizeLimit: Long = 10000000000L // 10 MB
  private val maxKeepTime: Long = 600000 // 10 min

  def getSortedTxs(): Array[TxEntry] = {
    txsSorted.toArray.clone()
  }

  def get(txid: UInt256): Option[Transaction] = {
    txsMap.get(txid)
  }

  def add(tx: Transaction): Boolean = {
    if (!contains(tx) && txTotalSize + tx.approximateSize <= txTotalSizeLimit) {
      log.info(s"TransactionPool add tx ${tx.id.toString}  ${txsMap.size}  ${txsSorted.size}")
      txsMap += (tx.id -> tx)
      txsSorted.add(TxEntry(tx, Instant.now.toEpochMilli))
      txTotalSize += tx.approximateSize
      require(txsMap.size == txsSorted.size)
      true
    }
    else false
  }

  def contains(tx: Transaction): Boolean = {
    txsMap.contains(tx.id)
  }

  def remove(tx: Transaction) = {
    if (contains(tx)) {
      log.info(s"TransactionPool remove tx ${tx.id.toString}  ${txsMap.size}  ${txsSorted.size}")
      require(txsMap.size == txsSorted.size)
      txsMap.remove(tx.id)
      //txsSorted.remove(TxEntry(tx))
      txsSorted = txsSorted.filter(p => if (p.tx.id == tx.id) false else true)
      //txsSorted.retain(p => if (p.tx.id == tx.id) false else true)
      txTotalSize -= tx.approximateSize
      log.info(s"x TransactionPool removed tx ${txsMap.size}  ${txsSorted.size}")
      require(txsMap.size == txsSorted.size)
    }
  }

  def txNumber: Int = txsSorted.size

  def checkRemoveTimeoutTxs = {
    val nowTime = Instant.now.toEpochMilli
    val badTxs = ArrayBuffer.empty[Transaction]
    txsSorted.foreach(p => if (nowTime - p.firstSeenTime > maxKeepTime) badTxs.append(p.tx))
    badTxs.foreach(tx => remove(tx))
  }

}
