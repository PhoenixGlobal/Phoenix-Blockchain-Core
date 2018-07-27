/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: AccountViewTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.test

import com.apex.core.AccountView
import com.apex.crypto.Ecdsa.Point
import com.apex.crypto.{Crypto, Fixed8, UInt160, UInt256}
import org.junit.Test

@Test
class AccountViewTest {
  @Test
  def testSerialize = {
    val balances = (1 to 10)
      .map(i => SerializerTest.testHash256(s"test$i") -> new Fixed8(i))
      .toMap
    val a = new AccountView(false, Seq.empty[Point], balances)
    val o = new SerializerTest[AccountView](
      AccountView.deserialize,
      (x, _) => x.id == a.id
        && x.active == a.active
        && x.votes.sameElements(a.votes)
        && x.balances.sameElements(a.balances))
    o.test(a)
  }
}
