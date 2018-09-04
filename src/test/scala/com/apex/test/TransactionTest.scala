/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: TransactionTest.scala
 *
 * @author: shan.huang@chinapex.com: 2018-09-03 下午4:06@version: 1.0
 */

package com.apex.test

import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, Ecdsa, Fixed8, UInt256}
import org.junit.Test

@Test
class TransactionTest {
  @Test
  def testSerialize = {

    //    priv key raw with 0x01: d39d51a8d40336b0c73af180308fe0e4ee357e45a59e8afeebf6895ddf78aa2f01
    //    priv key WIF format:    L4K4YPBT7Z3aT7h1w9xApBJT1o2t4cEdtbYSdFj3YbKhKA8AZMsG
    //    pub key (compressed):   0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322
    //    pub key hash160:        e2a4b7c6582f4e837668504eb2f4eaa796e908e4
    //    Address:                APNctFxoeKJV9cXBzWarUmxmwoxxwfMXurX

    var privKey = new Ecdsa.PrivateKey(BinaryData("d39d51a8d40336b0c73af180308fe0e4ee357e45a59e8afeebf6895ddf78aa2f"))

    val tx = new Transaction(TransactionType.Transfer,
                    BinaryData("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322"),
                    Ecdsa.PublicKeyHash.fromAddress("APGMmPKLYdtTNhiEkDGU6De8gNCk3bTsME9").get,
                    "bob",
                    Fixed8.Ten,
                    UInt256.Zero,
                    1,
                    BinaryData("1234"),
                    BinaryData.empty)

    tx.sign(privKey)

    assert(tx.verifySignature() == true)

    val o = new SerializerTest[Transaction](
      Transaction.deserialize,
      (x, _) => x.id == tx.id
        && x.txType == tx.txType
        && x.from.sameElements(tx.from)
        && x.toPubKeyHash.data.sameElements(tx.toPubKeyHash.data)
        && x.toName == tx.toName
        && x.amount.value == tx.amount.value
        && x.assetId.data.sameElements(tx.assetId.data)
        && x.nonce == tx.nonce
        && x.data.sameElements(tx.data)
        && x.signature.sameElements(tx.signature)
        && x.version == tx.version

        && x.verifySignature() == true   )
    o.test(tx)
  }
}
