/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: AccountViewTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.test

import java.io.{ByteArrayOutputStream, DataOutputStream}

import com.apex.core._
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, FixedNumber, MerkleTree, UInt160, UInt256}
import org.bouncycastle.util.encoders.Hex
import org.junit.Test
import play.api.libs.json.Json

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

@Test
class BlockTest {
  @Test
  def testSerialize = {
    val privKey = Ecdsa.PrivateKey.fromWIF("KxFC1jmwwCoACiCAWZ3eXa96mBM6tb3TYzGmf6YwgdGWZgawvrtJ").get

    val minerTx = new Transaction(TransactionType.Miner, UInt160.Zero,
      UInt160.Zero, FixedNumber.fromDecimal(1),
      234,
      BinaryData(Crypto.randomBytes(8)), // add random bytes to distinct different blocks with same block index during debug in some cases
      FixedNumber.MinValue, 0, BinaryData.empty)

    val allTxs = ArrayBuffer.empty[Transaction]

    allTxs.append(minerTx)

    val header: BlockHeader = BlockHeader.build(234,
      123, MerkleTree.root(allTxs.map(_.id)),
      UInt256.Zero, privKey)

    val b = Block.build(header, allTxs)

    val o = new SerializerHelper[Block](
      Block.deserialize,
      (x, _) => x.header == b.header
      && x.transactions.size == b.transactions.size
      && x.transactions(0).id == b.transactions(0).id
    )
    o.test(b)


    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    b.serialize(os)

    val newBlock = Block.fromBytes(bs.toByteArray)

    assert(newBlock.header == b.header)
    assert(newBlock.transactions.size == 1)
    assert(newBlock.transactions(0).id == minerTx.id)

    val json = Json.toJson(b).toString

  }
}
