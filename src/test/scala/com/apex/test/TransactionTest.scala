/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: TransactionTest.scala
 *
 * @author: shan.huang@chinapex.com: 2018-09-03 下午4:06@version: 1.0
 */

package com.apex.test

import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, Ecdsa, FixedNumber, UInt256}
import org.junit.Test

@Test
class TransactionTest {

  @Test
  def testSize = {
    val tx = new Transaction(TransactionType.Transfer,
      Ecdsa.PublicKey(BinaryData("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322")).pubKeyHash,
      Ecdsa.PublicKeyHash.fromAddress("APGMmPKLYdtTNhiEkDGU6De8gNCk3bTsME9").get,
      FixedNumber.Ten,
      1,
      BinaryData("1234"),
      FixedNumber(567),
      789,
      BinaryData.empty, 2, 3)

    assert(tx.size == 81)
    assert(tx.approximateSize == 81)
  }

  @Test
  def testSerialize = {

    //    priv key raw with 0x01: d39d51a8d40336b0c73af180308fe0e4ee357e45a59e8afeebf6895ddf78aa2f01
    //    priv key WIF format:    L4K4YPBT7Z3aT7h1w9xApBJT1o2t4cEdtbYSdFj3YbKhKA8AZMsG
    //    pub key (compressed):   0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322
    //    pub key hash160:        e2a4b7c6582f4e837668504eb2f4eaa796e908e4
    //    Address:                APNctFxoeKJV9cXBzWarUmxmwoxxwfMXurX

    val privKey = new Ecdsa.PrivateKey(BinaryData("d39d51a8d40336b0c73af180308fe0e4ee357e45a59e8afeebf6895ddf78aa2f"))
    val privKeyWrong = new Ecdsa.PrivateKey(BinaryData("d39d51a8d40331b0c73af180208fe0e4ee357e45a59e8afeebf6895ddf78aa2f"))

    val tx = new Transaction(TransactionType.Transfer,
                    Ecdsa.PublicKey(BinaryData("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322")).pubKeyHash,
                    Ecdsa.PublicKeyHash.fromAddress("APGMmPKLYdtTNhiEkDGU6De8gNCk3bTsME9").get,
                    FixedNumber.Ten,
                    1,
                    BinaryData("1234"),
                    FixedNumber(567),
      789,
                    BinaryData.empty, 2, 3)

    tx.sign(privKeyWrong)
    assert(!tx.verifySignature())

    tx.sign(privKey)
    assert(tx.verifySignature())

    val o = new SerializerHelper[Transaction](
      Transaction.deserialize,
      (x, _) => x.id == tx.id
        && x.txType == tx.txType
        && x.from.data.sameElements(tx.from.data)
        && x.toPubKeyHash.data.sameElements(tx.toPubKeyHash.data)
        && x.amount.value == tx.amount.value
        && x.nonce == tx.nonce
        && x.data.sameElements(tx.data)
        && x.gasPrice == tx.gasPrice
        && x.gasLimit == tx.gasLimit
        && x.signature.sameElements(tx.signature)
        && x.version == tx.version
        && x.verifySignature() == true
        && x.executeTime == tx.executeTime)
    o.test(tx)
  }
  
  @Test
  def testZeroDataBytes = {
    val tx1 = new Transaction(TransactionType.Transfer,
      Ecdsa.PublicKey(BinaryData("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322")).pubKeyHash,
      Ecdsa.PublicKeyHash.fromAddress("APGMmPKLYdtTNhiEkDGU6De8gNCk3bTsME9").get,
      FixedNumber.Ten,1,
      BinaryData("1234"), FixedNumber(567),789, BinaryData.empty)

    assert(tx1.zeroDataBytes() == (0, 2))

    val tx2 = new Transaction(TransactionType.Transfer,
      Ecdsa.PublicKey(BinaryData("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322")).pubKeyHash,
      Ecdsa.PublicKeyHash.fromAddress("APGMmPKLYdtTNhiEkDGU6De8gNCk3bTsME9").get,
      FixedNumber.Ten,1,
      BinaryData("123400"), FixedNumber(567),789, BinaryData.empty)

    assert(tx2.zeroDataBytes() == (1, 2))

    val tx3 = new Transaction(TransactionType.Transfer,
      Ecdsa.PublicKey(BinaryData("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322")).pubKeyHash,
      Ecdsa.PublicKeyHash.fromAddress("APGMmPKLYdtTNhiEkDGU6De8gNCk3bTsME9").get,
      FixedNumber.Ten,1,
      BinaryData("120034"), FixedNumber(567),789, BinaryData.empty)

    assert(tx3.zeroDataBytes() == (1, 2))

    val tx4 = new Transaction(TransactionType.Transfer,
      Ecdsa.PublicKey(BinaryData("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322")).pubKeyHash,
      Ecdsa.PublicKeyHash.fromAddress("APGMmPKLYdtTNhiEkDGU6De8gNCk3bTsME9").get,
      FixedNumber.Ten,1,
      BinaryData("00123400"), FixedNumber(567),789, BinaryData.empty)

    assert(tx4.zeroDataBytes() == (2, 2))

    val tx5 = new Transaction(TransactionType.Transfer,
      Ecdsa.PublicKey(BinaryData("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322")).pubKeyHash,
      Ecdsa.PublicKeyHash.fromAddress("APGMmPKLYdtTNhiEkDGU6De8gNCk3bTsME9").get,
      FixedNumber.Ten, 1,
      BinaryData("00"), FixedNumber(567),789, BinaryData.empty)

    assert(tx5.zeroDataBytes() == (1, 0))

    val tx6 = new Transaction(TransactionType.Transfer,
      Ecdsa.PublicKey(BinaryData("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322")).pubKeyHash,
      Ecdsa.PublicKeyHash.fromAddress("APGMmPKLYdtTNhiEkDGU6De8gNCk3bTsME9").get,
      FixedNumber.Ten,1,
      BinaryData.empty, FixedNumber(567),789, BinaryData.empty)

    assert(tx6.zeroDataBytes() == (0, 0))

  }
}
