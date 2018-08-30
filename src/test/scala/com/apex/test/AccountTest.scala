/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: AccountViewTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.test

import java.io.{ByteArrayOutputStream, DataOutputStream}

import com.apex.core.Account
import com.apex.crypto.Fixed8
import org.junit.Test

@Test
class AccountTest {
  @Test
  def testSerialize = {
    val balances = (1 to 10)
      .map(i => SerializerTest.testHash256(s"test$i") -> new Fixed8(i))
      .toMap
    val a = new Account(false, "", balances, 0)
    val o = new SerializerTest[Account](
      Account.deserialize,
      (x, _) => x.id == a.id
        && x.active == a.active
        && x.balances.sameElements(a.balances))
    o.test(a)
  }
}
