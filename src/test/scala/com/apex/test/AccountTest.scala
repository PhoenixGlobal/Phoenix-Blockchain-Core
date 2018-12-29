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
import com.apex.crypto.{FixedNumber, UInt160}
import org.junit.Test

@Test
class AccountTest {
  @Test
  def testSerialize = {

    val a = new Account(UInt160.Zero, false, "iiruf", FixedNumber(567), 123)
    val o = new SerializerHelper[Account](
      Account.deserialize,
      (x, _) => x.pubKeyHash == a.pubKeyHash
        && x.active == a.active
        && x.name == a.name
        && x.balance == a.balance
        && x.nextNonce == a.nextNonce
        && x.version == a.version)
    o.test(a)
  }
}
