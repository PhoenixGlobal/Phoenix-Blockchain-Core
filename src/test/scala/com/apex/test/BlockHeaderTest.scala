/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: BlockHeaderTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午6:50@version: 1.0
 */

package com.apex.test

import akka.http.scaladsl.model.DateTime
import com.apex.core.BlockHeader
import com.apex.crypto.BinaryData
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import org.junit.Test
import play.api.libs.json.Json

@Test
class BlockHeaderTest {
  @Test
  def testSerialize = {
    val prevBlock = SerializerHelper.testHash256("prev")
    val merkleRoot = SerializerHelper.testHash256("root")
    val producerPrivKey = new PrivateKey(BinaryData("ec3b8408c99ee15cc39a9a2ae2d2c4b92a5878314f3eb7a25ce73958c5e8c73f"))
    val producerPrivKeyWrong = new PrivateKey(BinaryData("ec3b8408c99ee15cc39a9a2ae2d2c4b92a5878314f3eb7a25ce73958c5e8c75f"))
    val producer = producerPrivKey.publicKey
    val timeStamp = DateTime.now.clicks
    val a = new BlockHeader(0, timeStamp, merkleRoot,
      prevBlock, producer.pubKeyHash, BinaryData("0000"))
    a.sign(producerPrivKeyWrong)
    assert(a.verifySig() == false)
    a.sign(producerPrivKey)
    assert(a.verifySig() == true)

    val o = new SerializerHelper[BlockHeader](
      BlockHeader.deserialize,
      (x, _) => x.version.equals(a.version)
        && x.id.equals(a.id)
        && x.index.equals(a.index)
        && x.timeStamp.equals(a.timeStamp)
        && x.merkleRoot.equals(a.merkleRoot)
        && x.prevBlock.equals(a.prevBlock)
    )
    o.test(a)
  }

  @Test
  def testToJson = {
    val prevBlock = SerializerHelper.testHash256("prev")
    val merkleRoot = SerializerHelper.testHash256("root")
    val producerPrivKey = new PrivateKey(BinaryData("ec3b8408c99ee15cc39a9a2ae2d2c4b92a5878314f3eb7a25ce73958c5e8c73f"))
    val producer = producerPrivKey.publicKey
    val timeStamp = DateTime.now.clicks
    val a = new BlockHeader(0, timeStamp, merkleRoot, prevBlock, producer.pubKeyHash, BinaryData("0000"))
    val eefe = Json.toJson(a).toString
    assert(Json.toJson(a).toString.equals(
      s"""{"hash":"${a.id}","index":${a.index},"timeStamp":${a.timeStamp},"time":"${a.timeString()}","merkleRoot":"${a.merkleRoot}","prevBlock":"${a.prevBlock}","producer":"${a.producer.address}","producerSig":"${a.producerSig}","version":${a.version}}"""))
  }
}
