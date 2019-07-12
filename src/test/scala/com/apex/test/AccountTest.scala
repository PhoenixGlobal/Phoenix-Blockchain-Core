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
import com.apex.crypto.{Ecdsa, FixedNumber, UInt160}
import org.bouncycastle.util.encoders.Hex
import org.junit.Test
import play.api.libs.json.Json

import scala.util.Random

@Test
class AccountTest {
  @Test
  def testSerialize = {
    val codeHash = new Array[Byte](Random.nextInt(33))
    Random.nextBytes(codeHash)
    val a = new Account(UInt160.Zero, false, "iiruf", FixedNumber(567), 123, codeHash)

    assert(a.address == UInt160.Zero.address)
    assert(!a.isEmpty)

    val o = new SerializerHelper[Account](
      Account.deserialize,
      (x, _) => x.pubKeyHash == a.pubKeyHash
        && x.active == a.active
        && x.name == a.name
        && x.balance == a.balance
        && x.nextNonce == a.nextNonce
        && x.codeHash.sameElements(a.codeHash)
        && x.version == a.version)
    o.test(a)

    //val eefet = Json.toJson(a).toString
    //val efwef = s"""{"address":"${a.address}","active":false,"name":"${a.name}","balance":"${a.balance.toString}","nextNonce":${a.nextNonce},"codeHash":"${Hex.toHexString(a.codeHash)}","version":${a.version}}"""

    assert(Json.toJson(a).toString.equals(
      s"""{"address":"${a.address}","active":false,"name":"${a.name}","balance":"${a.balance.toString}","nextNonce":${a.nextNonce},"codeHash":"${Hex.toHexString(a.codeHash)}","version":${a.version}}"""))
  }
}
