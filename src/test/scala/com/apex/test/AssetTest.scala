///*
// * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
// *
// * FileName: AssetTest.scala
// *
// * @author: ruixiao.xiao@chinapex.com: 18-7-23 下午4:30@version: 1.0
// */
//
//package com.apex.test
//
//import com.apex.core.{AssetType, Asset}
//import com.apex.crypto.Ecdsa.Point
//import com.apex.crypto.{Crypto, Fixed8, UInt160, UInt256}
//import org.junit.Test
//
//@Test
//class AssettViewTest {
//  @Test
//  def testSerialize = {
//    val a = new Asset(
//      AssetType.Token,
//      SerializerHelper.testHash160(),
//      "cpx",
//      Fixed8.fromDecimal(100),
//      Fixed8.fromDecimal(50),
//      2,
//      Fixed8.One,
//      true
//    )
//    val o = new SerializerHelper[Asset](
//      Asset.deserialize,
//      (x, _) => x.version == a.version
//        && x.id == a.id
//        && x.issuer == a.issuer
//        && x.name == a.name
//        && x.amount == a.amount
//        && x.precision == a.precision
//        && x.fee == a.fee
//        && x.available == a.available
//        && x.active == a.active
//    )
//    o.test(a)
//  }
//}
