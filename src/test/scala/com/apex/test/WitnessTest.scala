/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: WitnessTest.scala
 *
 * @author: shan.huang@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.test

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.consensus.{Vote, WitnessInfo, WitnessList}
import com.apex.crypto.{FixedNumber, UInt160}
import org.junit.Test

@Test
class WitnessTest {
  def WitnessInfoCompare(x: WitnessInfo, a: WitnessInfo): Boolean = {
    (x.version.equals(a.version)
      && x.name == a.name
      && x.addr.data.sameElements(a.addr.data)
      && x.url == a.url
      && x.country == a.country
      && x.address == a.address
      && x.longitude == a.longitude
      && x.latitude == a.latitude
      && x.voteCounts.value == a.voteCounts.value)
  }
  @Test
  def testSerialize_WitnessInfo = {
    val a = new WitnessInfo("wefwe", UInt160.Zero, "1 2", "3", "4",
      170, 240, FixedNumber.One, 2)

    val o = new SerializerHelper[WitnessInfo](
      WitnessInfo.deserialize,
      (x, _) => x.version.equals(a.version)
        && x.name == a.name
        && x.addr.data.sameElements(a.addr.data)
        && x.url == a.url
        && x.country == a.country
        && x.address == a.address
        && x.longitude == a.longitude
        && x.latitude == a.latitude
        && x.voteCounts.value == a.voteCounts.value
    )
    o.test(a)
  }
  @Test
  def testSerialize_Vote = {
    val a = new Vote(UInt160.Zero, UInt160.parse("1212121212121212121212121212121212121212").get,
      FixedNumber.One, 2)

    val o = new SerializerHelper[Vote](
      Vote.deserialize,
      (x, _) => x.version.equals(a.version)
        && x.voter.data.sameElements(a.voter.data)
        && x.target.data.sameElements(a.target.data)
        && x.count.value == a.count.value
    )
    o.test(a)
  }
  @Test
  def testSerialize_WitnessList = {
    val a = new WitnessInfo("123", UInt160.Zero, "1 2", "3", "000",
      171, 240, FixedNumber.One, 2)

    val b = new WitnessInfo("456", UInt160.Zero, "8374", "311", "666",
      172, 241, FixedNumber.Zero, 3)

    val c = new WitnessInfo("789", UInt160.Zero, "1 33", "9847", "4",
      173, 242, FixedNumber.One, 4)

    val list1 = new WitnessList(Array(a, b, c), 5, 6, 7)

    val bos = new ByteArrayOutputStream
    val dos = new DataOutputStream(bos)
    list1.serialize(dos)
    val ba = bos.toByteArray
    val bis = new ByteArrayInputStream(ba)
    val is = new DataInputStream(bis)
    val list2 = WitnessList.deserialize(is)

    assert(list2.witnesses.size == 3)
    assert(list2.generateInBlockNum == 5)
    assert(list2.generateInBlockTime == 6)
    assert(list2.version == 7)
    assert(WitnessInfoCompare(list1.witnesses(0), list2.witnesses(0)))
    assert(WitnessInfoCompare(list1.witnesses(1), list2.witnesses(1)))
    assert(WitnessInfoCompare(list1.witnesses(2), list2.witnesses(2)))
  }
}
