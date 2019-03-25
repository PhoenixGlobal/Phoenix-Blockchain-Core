/*
 * Copyright  2019 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: TxPoolTest.scala
 *
 * @author: shan.huang@chinapex.com: 19-3-18 下午4:06@version: 1.0
 */

package com.apex.test


import com.apex.core.{Transaction, TransactionPool, TransactionType, TxEntry}
import com.apex.crypto.{BinaryData, FixedNumber, UInt160}
import org.junit.Test

@Test
class TxPoolTest {   

  @Test
  def testAdd = {
     val tx1 = new Transaction(TransactionType.Call, UInt160.Zero, UInt160.Zero, FixedNumber.Zero, 1, BinaryData.empty,
       FixedNumber(1000), 100, BinaryData.empty, 1, 0)

    val tx1same = new Transaction(TransactionType.Call, UInt160.Zero, UInt160.Zero, FixedNumber.Zero, 1, BinaryData.empty,
      FixedNumber(1000), 100, BinaryData.empty, 1, 0)

    val tx2 = new Transaction(TransactionType.Call, UInt160.Zero, UInt160.Zero, FixedNumber.Zero, 2, BinaryData.empty,
      FixedNumber(1000), 100, BinaryData.empty, 1, 0)

    val txPool = new TransactionPool()

    txPool.add(tx1)

    assert(txPool.contains(tx1same))
    assert(txPool.contains(tx1))
    assert(txPool.unapplyTxsMap.contains(tx1.id))
    assert(txPool.unapplyTxsMap.contains(tx1same.id))
    assert(txPool.unapplyTxsSorted.contains(new TxEntry(tx1, 222)))
    assert(txPool.unapplyTxsSorted.contains(new TxEntry(tx1same, 333)))
    assert(!txPool.unapplyTxsSorted.contains(new TxEntry(tx2, 222)))
    assert(txPool.get(tx1same.id).isDefined)
    assert(!txPool.contains(tx2))
  }

  @Test
  def testRemove = {
    val tx1 = new Transaction(TransactionType.Call, UInt160.Zero, UInt160.Zero, FixedNumber.Zero, 1, BinaryData.empty,
      FixedNumber(1000), 100, BinaryData.empty, 1, 0)

    val tx1same = new Transaction(TransactionType.Call, UInt160.Zero, UInt160.Zero, FixedNumber.Zero, 1, BinaryData.empty,
      FixedNumber(1000), 100, BinaryData.empty, 1, 0)

    val tx2 = new Transaction(TransactionType.Call, UInt160.Zero, UInt160.Zero, FixedNumber.Zero, 2, BinaryData.empty,
      FixedNumber(1000), 100, BinaryData.empty, 1, 0)

    val txPool = new TransactionPool()

    txPool.add(tx1)
    txPool.add(tx2)
    assert(txPool.unapplyTxsMap.size == 2)
    assert(txPool.unapplyTxsSorted.size == 2)
    txPool.remove(tx1same)
    assert(txPool.unapplyTxsMap.size == 1)
    assert(txPool.unapplyTxsSorted.size == 1)

    assert(!txPool.contains(tx1same))
    assert(!txPool.contains(tx1))
    assert(txPool.contains(tx2))
    assert(!txPool.unapplyTxsMap.contains(tx1.id))
    assert(!txPool.unapplyTxsMap.contains(tx1same.id))
    assert(txPool.unapplyTxsMap.contains(tx2.id))
    assert(!txPool.unapplyTxsSorted.contains(new TxEntry(tx1, 222)))
    assert(!txPool.unapplyTxsSorted.contains(new TxEntry(tx1same, 333)))
    assert(txPool.unapplyTxsSorted.contains(new TxEntry(tx2, 222)))
    assert(txPool.get(tx1same.id).isEmpty)
    assert(txPool.get(tx1.id).isEmpty)
    assert(txPool.get(tx2.id).isDefined)
  }

}
