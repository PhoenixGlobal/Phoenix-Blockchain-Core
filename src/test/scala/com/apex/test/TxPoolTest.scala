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
import play.api.libs.json._

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
    assert(txPool.txsMap.contains(tx1.id))
    assert(txPool.txsMap.contains(tx1same.id))
    assert(txPool.txsSorted.contains(new TxEntry(tx1, 222)))
    assert(txPool.txsSorted.contains(new TxEntry(tx1same, 333)))
    assert(!txPool.txsSorted.contains(new TxEntry(tx2, 222)))
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

    val tx2same = new Transaction(TransactionType.Call, UInt160.Zero, UInt160.Zero, FixedNumber.Zero, 2, BinaryData.empty,
      FixedNumber(1000), 100, BinaryData.empty, 1, 0)

    val tx3 = new Transaction(TransactionType.Call, UInt160.Zero, UInt160.Zero, FixedNumber.Zero, 3, BinaryData.empty,
      FixedNumber(1000), 100, BinaryData.empty, 1, 0)

    val tx3same = new Transaction(TransactionType.Call, UInt160.Zero, UInt160.Zero, FixedNumber.Zero, 3, BinaryData.empty,
      FixedNumber(1000), 100, BinaryData.empty, 1, 0)

    val txPool = new TransactionPool()

    txPool.add(tx1)
    txPool.add(tx2)
    txPool.add(tx3)
    assert(txPool.txNumber == 3)
    assert(txPool.txsMap.size == 3)
    assert(txPool.txsSorted.size == 3)
    txPool.remove(tx1same)
    assert(txPool.txNumber == 2)
    assert(txPool.txsMap.size == 2)
    assert(txPool.txsSorted.size == 2)

    assert(!txPool.contains(tx1same))
    assert(!txPool.contains(tx1))
    assert(txPool.contains(tx2))
    assert(txPool.contains(tx2same))
    assert(txPool.contains(tx3))
    assert(txPool.contains(tx3same))

    txPool.remove(tx2same)
    assert(txPool.txNumber == 1)

    assert(!txPool.contains(tx1same))
    assert(!txPool.contains(tx1))
    assert(!txPool.contains(tx2))
    assert(!txPool.contains(tx2same))
    assert(txPool.contains(tx3))
    assert(txPool.contains(tx3same))

    assert(!txPool.txsMap.contains(tx1.id))
    assert(!txPool.txsMap.contains(tx1same.id))
    assert(!txPool.txsMap.contains(tx2.id))
    assert(txPool.txsMap.contains(tx3.id))
    assert(!txPool.txsSorted.contains(new TxEntry(tx1, 222)))
    assert(!txPool.txsSorted.contains(new TxEntry(tx1same, 333)))
    assert(!txPool.txsSorted.contains(new TxEntry(tx2, 222)))
    assert(txPool.txsSorted.contains(new TxEntry(tx3, 21232)))

    assert(txPool.get(tx1same.id).isEmpty)
    assert(txPool.get(tx1.id).isEmpty)
    assert(txPool.get(tx2.id).isEmpty)
    assert(txPool.get(tx3.id).isDefined)
  }

  @Test
  def testSort: Unit = {

    val addr1 = UInt160.parse("1212121212121212121212121212121212121211").get
    val addr2 = UInt160.parse("1212121212121212121212121212121212121212").get
    val addr3 = UInt160.parse("1212121212121212121212121212121212121213").get
    val addr4 = UInt160.parse("1212121212121212121212121212121212121214").get
    val addr5 = UInt160.parse("1212121212121212121212121212121212121215").get
    val addr6 = UInt160.parse("1212121212121212121212121212121212121216").get

    val tx1 = new Transaction(TransactionType.Call, addr1, UInt160.Zero, FixedNumber.Zero, 1, BinaryData.empty,
      FixedNumber(1000), 100, BinaryData.empty, 1, 0)

    val tx2 = new Transaction(TransactionType.Call, addr2, UInt160.Zero, FixedNumber.Zero, 1, BinaryData.empty,
      FixedNumber(2000), 100, BinaryData.empty, 1, 0)

    val tx3 = new Transaction(TransactionType.Call, addr3, UInt160.Zero, FixedNumber.Zero, 1, BinaryData.empty,
      FixedNumber(3000), 100, BinaryData.empty, 1, 0)

    val tx4 = new Transaction(TransactionType.Call, addr4, UInt160.Zero, FixedNumber.Zero, 1, BinaryData.empty,
      FixedNumber(500), 100, BinaryData.empty, 1, 0)

    val tx5 = new Transaction(TransactionType.Call, addr5, UInt160.Zero, FixedNumber.Zero, 1, BinaryData.empty,
      FixedNumber(800), 100, BinaryData.empty, 1, 0)

    // same addr6:

    val tx6 = new Transaction(TransactionType.Call, addr6, UInt160.Zero, FixedNumber.Zero, 9, BinaryData.empty,
      FixedNumber(600), 100, BinaryData.empty, 1, 0)

    val tx7 = new Transaction(TransactionType.Call, addr6, UInt160.Zero, FixedNumber.Zero, 7, BinaryData.empty,
      FixedNumber(501), 100, BinaryData.empty, 1, 0)

    val tx8 = new Transaction(TransactionType.Call, addr6, UInt160.Zero, FixedNumber.Zero, 6, BinaryData.empty,
      FixedNumber(400), 100, BinaryData.empty, 1, 0)

    val tx9 = new Transaction(TransactionType.Call, addr6, UInt160.Zero, FixedNumber.Zero, 10, BinaryData.empty,
      FixedNumber(700), 100, BinaryData.empty, 1, 0)

    val txPool = new TransactionPool()

    txPool.add(tx1)
    txPool.add(tx2)
    txPool.add(tx3)
    txPool.add(tx4)
    txPool.add(tx5)
    txPool.add(tx6)
    txPool.add(tx7)
    txPool.add(tx8)
    txPool.add(tx9)

    val sort = txPool.getSortedTxs()//txPool.unapplyTxsSorted.toArray

    //sort.foreach(f => println(Json.toJson(f.tx)))

    assert(sort(0).tx.id == tx3.id)
    assert(sort(1).tx.id == tx2.id)
    assert(sort(2).tx.id == tx1.id)
    assert(sort(3).tx.id == tx5.id)
    assert(sort(4).tx.id == tx8.id) // tx8 vs tx4
    assert(sort(5).tx.id == tx7.id)
    assert(sort(6).tx.id == tx6.id)
    assert(sort(7).tx.id == tx9.id)
    assert(sort(8).tx.id == tx4.id)
  }

}
