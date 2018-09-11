/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: SessionTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-9-11 下午5:32@version: 1.0
 *
 */

package com.apex.test

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.core.DbTrack
import com.apex.crypto.UInt256
import org.junit.Test

@Test
class SessionTest {
  @Test
  def testDbTrack = {
    import com.apex.common.Serializable._
    val a = new DbTrack[UInt256, UInt256]()
    a.insert.put(SerializerTest.testHash256("testKey1"), SerializerTest.testHash256("testValue1"))
    a.update.put(SerializerTest.testHash256("testKey2"), SerializerTest.testHash256("testValue2"))
    a.delete.put(SerializerTest.testHash256("testKey3"), SerializerTest.testHash256("testValue3"))
    val bs = new ByteArrayInputStream(a.toBytes)
    val is = new DataInputStream(bs)
    import com.apex.crypto.UInt256._
    val b = DbTrack.deserialize(is)
    assert(a.insert.size == b.insert.size)
  }
}
