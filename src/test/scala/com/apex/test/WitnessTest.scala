/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: WitnessTest.scala
 *
 * @author: shan.huang@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.test

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.consensus._
import com.apex.core.Block.deserialize
import com.apex.crypto.{Crypto, FixedNumber, UInt160, UInt256}
import org.junit.Test

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

@Test
class WitnessTest {
  def WitnessInfoCompare(x: WitnessInfo, a: WitnessInfo): Boolean = {
    (x.version.equals(a.version)
      && x.addr.data.sameElements(a.addr.data)
      && x.isGenesisWitness == a.isGenesisWitness
      && x.name == a.name
      && x.url == a.url
      && x.country == a.country
      && x.address == a.address
      && x.longitude == a.longitude
      && x.latitude == a.latitude
      && x.voteCounts.value == a.voteCounts.value
      )
  }
  @Test
  def testSerialize_WitnessInfo = {
    val a = new WitnessInfo(UInt160.Zero, false, "wefwe", "1 2", "3", "4",
      170, 240, FixedNumber.One, true, 2)

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

    val a = new WitnessVote(UInt160.Zero, scala.collection.mutable.Map[UInt160, FixedNumber](
      UInt160.parse("1212121212121212121212121212121212121212").get -> FixedNumber.One,
      UInt160.parse("9999999999999999999999999999999999999999").get -> FixedNumber.Ten   ),2)

    val o = new SerializerHelper[WitnessVote](
      WitnessVote.deserialize,
      (x, _) => x.version.equals(a.version)
        && x.voter.data.sameElements(a.voter.data)
        && x.targetMap.size == a.targetMap.size
        && x.targetMap.keys.sameElements(a.targetMap.keys)
        && x.targetMap.values.sameElements(a.targetMap.values)
    )
    o.test(a)
  }
  @Test
  def testSerialize_WitnessList = {
    val a = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121211").get, false,
      "123", "1 2", "3", "000",
      171, 240, FixedNumber(2), true, 2)

    val b = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121212").get, false,
      "456", "8374", "311", "666",
      172, 241, FixedNumber(1),  true, 3)

    val c = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121213").get, false,
      "789", "1 33", "9847", "4",
      173, 242, FixedNumber(4),  true, 4)

    val d = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121214").get, false,
      "789", "1 33", "9847", "4",
      173, 242, FixedNumber(3),  true, 4)

    val list1 = WitnessList.create(Array(a, b, c, d), UInt256.Zero, 7)

    assert(!list1.contains(UInt160.Zero))
    assert(list1.contains(UInt160.parse("1212121212121212121212121212121212121212").get))
    assert(!list1.contains(UInt160.parse("1212121212121212121212121212121212121219").get))

    //assert(list1.findLeastVotes() == UInt160.parse("1212121212121212121212121212121212121212").get)

    val leaseVoteRemoved = WitnessList.removeLeastVote(list1.witnesses)

    assert(leaseVoteRemoved.size == 3)
    assert(leaseVoteRemoved.contains(a.addr))
    assert(!leaseVoteRemoved.contains(b.addr))
    assert(leaseVoteRemoved.contains(c.addr))
    assert(leaseVoteRemoved.contains(d.addr))

    val bos = new ByteArrayOutputStream
    val dos = new DataOutputStream(bos)
    list1.serialize(dos)
    val ba = bos.toByteArray
    val bis = new ByteArrayInputStream(ba)
    val is = new DataInputStream(bis)
    val list2 = WitnessList.deserialize(is)

    assert(list2.witnesses.size == 4)
    assert(list2.generateInBlock == UInt256.Zero)
    assert(list2.version == 7)
    assert(WitnessInfoCompare(list1.witnesses(0), list2.witnesses(0)))
    assert(WitnessInfoCompare(list1.witnesses(1), list2.witnesses(1)))
    assert(WitnessInfoCompare(list1.witnesses(2), list2.witnesses(2)))
    assert(WitnessInfoCompare(list1.witnesses(3), list2.witnesses(3)))
  }
  @Test
  def testSerialize_WitnessMap = {
    val a = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121211").get, false,
      "123", "1 2", "3", "000",
      171, 240, FixedNumber(2), true,2)

    val b = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121212").get, false,
      "456", "8374", "311", "666",
      172, 241, FixedNumber(1),  true,3)

    val c = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121213").get, false,
      "789", "1 33", "9847", "4",
      173, 242, FixedNumber(4),  true,4)

    val d = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121214").get, false,
      "789", "1 33", "9847", "4",
      173, 242, FixedNumber(3), true, 4)

    val ddd = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121214").get, false,
      "789", "1 3ddd3", "9847", "4",
      173, 242, FixedNumber(999), true, 456)

    val map1 = new WitnessMap(mutable.Map.empty, 67)
    map1.set(a)
    map1.set(b)
    map1.set(c)
    map1.set(d)
    map1.set(ddd)

    assert(map1.witnesses.size == 4)
    assert(map1.get(UInt160.Zero).isEmpty)
    assert(map1.get(UInt160.parse("1212121212121212121212121212121212121212").get).isDefined)
    assert(map1.get(UInt160.parse("1212121212121212121212121212121212121219").get).isEmpty)
    assert(map1.get(ddd.addr).get.version == 456)


    val bos = new ByteArrayOutputStream
    val dos = new DataOutputStream(bos)
    map1.serialize(dos)
    val ba = bos.toByteArray
    val bis = new ByteArrayInputStream(ba)
    val is = new DataInputStream(bis)
    val map2 = WitnessMap.deserialize(is)

    assert(map2.witnesses.size == 4)
    assert(map2.version == 67)

    assert(WitnessInfoCompare(map1.get(a.addr).get, map2.get(a.addr).get))
    assert(WitnessInfoCompare(map1.get(b.addr).get, map2.get(b.addr).get))
    assert(WitnessInfoCompare(map1.get(c.addr).get, map2.get(c.addr).get))
    assert(WitnessInfoCompare(map1.get(ddd.addr).get, map2.get(ddd.addr).get))

    val wefwe = map2.getAll()
    assert(map2.getAll().size == 4)
  }

  @Test
  def testSortByLocation = {
    val w1 = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121211").get, false,
      "w1", "1 2", "3", "000",
      171, 240, FixedNumber(2),true, 2)

    val w2 = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121212").get, false,
      "w2", "8374", "311", "666",
      172, 241, FixedNumber(1), true, 3)

    val w3 = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121213").get, false,
      "w3", "1 33", "9847", "4",
      2, 242, FixedNumber(4), true, 4)

    val w4 = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121214").get, false,
      "w4", "1 33", "9847", "4",
      999, 242, FixedNumber(3), true, 4)

    val w5 = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121215").get, false,
      "w5", "1 3ddd3", "9847", "4",
      173, 10, FixedNumber(999), true, 456)

    val w6 = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121216").get, false,
      "w6", "1 3ddd3", "9847", "4",
      173, 300, FixedNumber(999), true, 456)

    val w7 = new WitnessInfo(UInt160.parse("2000000000000000000000000000000000000000").get, false,
      "w7", "1 3ddd3", "9847", "4",
      173, 242, FixedNumber(999),  true,456)

    val w8 = new WitnessInfo(UInt160.parse("1000000000000000000000000000000000000000").get, false,
      "w8", "1 3ddd3", "9847", "4",
      173, 242, FixedNumber(999), true, 456)

    val w9 = new WitnessInfo(UInt160.parse("3000000000000000000000000000000000000000").get, false,
      "w9", "1 3ddd3", "9847", "4",
      173, 242, FixedNumber(999), true, 456)

    val witArray = Array(w1, w2, w3, w4, w5, w6, w7, w8, w9)

    val sorted = WitnessList.sortByLocation(witArray)
    assert(sorted(0).name == "w4")  //  999  242
    assert(sorted(1).name == "w6")  //  173     300
    assert(sorted(2).name == "w9")  //  173     242  3000000000000000000000000000000000000000
    assert(sorted(3).name == "w7")  //  173     242  2000000000000000000000000000000000000000
    assert(sorted(4).name == "w8")  //  173     242  1000000000000000000000000000000000000000
    assert(sorted(5).name == "w5")  //  173      10
    assert(sorted(6).name == "w2")  //  172  241
    assert(sorted(7).name == "w1")  //  171  240
    assert(sorted(8).name == "w3")  //    2  242
  }

  @Test
  def testSortByVote = {
    val w1 = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121211").get, false,
      "w1", "1 2", "3", "000",
      171, 240, FixedNumber(400),true, 2)

    val w2 = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121212").get, false,
      "w2", "8374", "311", "666",
      172, 241, FixedNumber(500), true, 3)

    val w3 = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121213").get, false,
      "w3", "1 33", "9847", "4",
      2, 242, FixedNumber(600),  true,4)

    val w4 = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121214").get, false,
      "w4", "1 33", "9847", "4",
      999, 242, FixedNumber(700), true, 4)

    val w5 = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121215").get, false,
      "w5", "1 3ddd3", "9847", "4",
      173, 10, FixedNumber(800),  true,456)

    val w6 = new WitnessInfo(UInt160.parse("1212121212121212121212121212121212121216").get, false,
      "w6", "1 3ddd3", "9847", "4",
      173, 300, FixedNumber(2000),  true, 456)

    val w7 = new WitnessInfo(UInt160.parse("2000000000000000000000000000000000000000").get, false,
      "w7", "1 3ddd3", "9847", "4",
      173, 242, FixedNumber(999),  true,456)

    val w8 = new WitnessInfo(UInt160.parse("1000000000000000000000000000000000000000").get, false,
      "w8", "1 3ddd3", "9847", "4",
      173, 242, FixedNumber(999),  true,456)

    val w9 = new WitnessInfo(UInt160.parse("3000000000000000000000000000000000000000").get, false,
      "w9", "1 3ddd3", "9847", "4",
      173, 242, FixedNumber(999),  true, 456)

    val witArray = Array(w1, w2, w3, w4, w5, w6, w7, w8, w9)

    val sorted = WitnessList.sortByVote(witArray)
    assert(sorted(0).name == "w6")  // 2000
    assert(sorted(1).name == "w9")  //  999
    assert(sorted(2).name == "w7")  //  999
    assert(sorted(3).name == "w8")  //  999
    assert(sorted(4).name == "w5")  //  800
    assert(sorted(5).name == "w4")  //
    assert(sorted(6).name == "w3")  //
    assert(sorted(7).name == "w2")  //
    assert(sorted(8).name == "w1")  //
  }

  @Test
  def testSortByVote2 = {
    val wits = ArrayBuffer.empty[WitnessInfo]
    for (i <- 0 to 99) {
      wits.append(new WitnessInfo(UInt160.fromBytes(Crypto.randomBytes(20))))
    }
    val sorted = WitnessList.sortByVote(wits.toArray)
    sorted.foreach(w => println(w.addr.address))
  }

//  @Test
//  def testRegisterInfo = {
//    val a = new RegisterInfo("abc")
//
//    val bs = new ByteArrayOutputStream()
//    val os = new DataOutputStream(bs)
//    a.serialize(os)
//
//    val bis = new ByteArrayInputStream(bs.toByteArray)
//    val is = new DataInputStream(bis)
//    val newA = RegisterInfo.deserialize(is)
//
//    assert(newA.location == "abc")
//  }

}
