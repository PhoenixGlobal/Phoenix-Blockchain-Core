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

  val unapplyTxsMap = mutable.LinkedHashMap.empty[UInt256, Transaction]
  val unapplyTxsSorted = mutable.SortedSet.empty[TxEntry]

  var txTotalSize: Long = 0
  val txTotalSizeLimit: Long = 10000000000L // 10 MB
  val maxKeepTime: Long = 600000 // 10 min

  def get(txid: UInt256): Option[Transaction] = {
    unapplyTxsMap.get(txid)
  }

  def add(tx: Transaction) = {
    if (!contains(tx) && txTotalSize + tx.approximateSize <= txTotalSizeLimit) {
      log.info(s"TransactionPool add tx ${tx.id.toString}")
      unapplyTxsMap += (tx.id -> tx)
      unapplyTxsSorted.add(TxEntry(tx, Instant.now.toEpochMilli))
      txTotalSize += tx.approximateSize
      require(unapplyTxsMap.size == unapplyTxsSorted.size)
      true
    }
    else false
  }

  def contains(tx: Transaction): Boolean = {
    unapplyTxsMap.contains(tx.id)
  }

  def remove(tx: Transaction) = {
    if (contains(tx)) {
      unapplyTxsMap.remove(tx.id)
      unapplyTxsSorted.remove(TxEntry(tx))
      txTotalSize -= tx.approximateSize
      require(unapplyTxsMap.size == unapplyTxsSorted.size)
    }
  }

  def txNumber: Int = unapplyTxsSorted.size

  def checkRemoveTimeoutTxs = {
    val nowTime = Instant.now.toEpochMilli
    val badTxs = ArrayBuffer.empty[Transaction]
    unapplyTxsSorted.foreach(p => if (nowTime - p.firstSeenTime > maxKeepTime) badTxs.append(p.tx))
    badTxs.foreach(tx => remove(tx))
  }

}
