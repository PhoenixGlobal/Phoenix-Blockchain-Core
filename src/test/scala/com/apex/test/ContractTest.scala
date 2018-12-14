/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: ContractViewTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-19 上午11:08@version: 1.0
 */

package com.apex.test

import scala.util.Random
import com.apex.core.Contract
import com.apex.crypto.BinaryData
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import org.junit.Test

@Test
class ContractTest {
  @Test
  def testSerialize = {
    val a = new Contract(PrivateKey(Random.nextString(32).getBytes.take(32)).publicKey, Array.fill(Random.nextInt(1024) + 65536)((Random.nextInt(256) - 128).toByte))
    val o = new SerializerHelper[Contract](
      Contract.deserialize,
      (x, _) =>
        x.account.equals(a.account)
          && x.code.sameElements(a.code)
          && x.name.equals(a.name)
          && x.author.equals(a.author)
          && x.email.equals(a.email)
          && x.description.equals(a.description)
    )
    o.test(a)
  }
}
