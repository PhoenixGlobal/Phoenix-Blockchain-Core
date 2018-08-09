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
import org.junit.Test
import play.api.libs.json.Json

@Test
class BlockHeaderTest {
  @Test
  def testSerialize = {
    val prevBlock = SerializerTest.testHash256("prev")
    val merkleRoot = SerializerTest.testHash256("root")
    val producer = SerializerTest.testHash256("producer")
    val timeStamp = DateTime.now.clicks
    val a = new BlockHeader(0, timeStamp, merkleRoot, prevBlock, producer)
    val o = new SerializerTest[BlockHeader](
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
    val prevBlock = SerializerTest.testHash256("prev")
    val merkleRoot = SerializerTest.testHash256("root")
    val producer = SerializerTest.testHash256("producer")
    val timeStamp = DateTime.now.clicks
    val a = new BlockHeader(0, timeStamp, merkleRoot, prevBlock, producer)
    assert(Json.toJson(a).toString.equals(
      s"""{"id":"${a.id}","index":${a.index},"timeStamp":${a.timeStamp},"merkleRoot":"${a.merkleRoot}","prevBlock":"${a.prevBlock}","producer":"${a.producer}","version":${a.version}}"""))
  }
}
