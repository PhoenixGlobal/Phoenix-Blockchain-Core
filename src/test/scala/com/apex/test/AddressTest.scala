/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: AddressTest.scala
 *
 * @author: shan.huang@chinapex.com: 2018-07-27 下午4:06@version: 1.0
 */

package com.apex.test

import com.apex.crypto.Ecdsa.PrivateKey
import com.apex.crypto.{Base58Check, BinaryData, Crypto, Ecdsa, UInt160}
import org.junit.Test

@Test
class AddressTest {

  @Test
  def testHashToAddress = {

    //20 bytes data
    val address1 = UInt160.parse("0000000000000000000000000000000000000000").get.address

    //20 bytes data
    val address2 = UInt160.parse("654a5851e9372b87810a8e60cdd2e7cfd80b6e31").get.address

    //20 bytes data
    val address3 = UInt160.parse("ffffffffffffffffffffffffffffffffffffffff").get.address

    var privKey = new Ecdsa.PrivateKey(BinaryData("18e14a7b6a307f426a94f8114701e7c8e774e7f9a47e2c2035db29a206321725"))
    var pubKey = privKey.publicKey
    var pubKeyHash = pubKey.pubKeyHash.data  // f54a5851e9372b87810a8e60cdd2e7cfd80b6e31
    val address4 = pubKey.pubKeyHash.address

    assert(address1 == "AP1xWDozWvuVah1W86DKtcWzdw1LLHreMGX")
    assert(address2 == "APBC5XmSaD4vooWo3FNho1wGAUyBQo3WCTQ")
    assert(address3 == "APRJ7CvHoe5xTWSeD7dfD6eGRZWbGomzDi4")

    //assert(address4 == "APQKUqPcJEUwRdwoxpoGQnkrRGstSXkgebk")

  }

  @Test
  def testNeoAddress() = {
    val h1 = UInt160.fromNeoAddress("ARPPoLhqsEuEAEeMUXsULEYm6qHys1G6ce")
    val h2 = UInt160.fromNeoAddress("ARPPoLhqsEuEAEeMUXsULEYm6qHys1G6cc")
    val h3 = UInt160.fromNeoAddress("ARPPoLhqsEuEAEeMUXssULEYm6qHys1G6ce")

    assert(h1.get == UInt160.parse("6970dc57e25c590d6260d4f07da09626d729c0f4").get)
    assert(h1.get != UInt160.parse("6970dc57e25c590d6260d4f07da09626d722c0f4").get)
    assert(h2.isEmpty)
    assert(h3.isEmpty)

    val na1 = UInt160.parse("6970dc57e25c590d6260d4f07da09626d729c0f4").get.neoAddress

    assert(na1 == "ARPPoLhqsEuEAEeMUXsULEYm6qHys1G6ce")
  }

  def neoPrivkeyToAddr(pkey: BinaryData): String = {
    require(pkey.length == 32)
    val privKey = Ecdsa.PrivateKey(pkey)
    val pubKey = privKey.publicKey
    require(pubKey.toBin.length == 33)
    val addrScript = BinaryData("21").toString + pubKey.toBin.toString + BinaryData("ac").toString
    val scriptHash = BinaryData(Crypto.hash160(BinaryData(addrScript)))
    val neoAddr = Base58Check.encode(BinaryData("17"), scriptHash)
    neoAddr
  }

  @Test
  def testNeoPrivKeyAddr() = {
    val p1 = BinaryData("72520405d2ab00326dbcacfddd350b01222a7cc9efc5f304f742077ec9ade463")
    val a1 = neoPrivkeyToAddr(p1)
    assert(a1 == "ARPPoLhqsEuEAEeMUXsULEYm6qHys1G6ce")
    val priv1 = new Ecdsa.PrivateKey(BinaryData("72520405d2ab00326dbcacfddd350b01222a7cc9efc5f304f742077ec9ade463"))
    assert(priv1.publicKey.neoAddress == "ARPPoLhqsEuEAEeMUXsULEYm6qHys1G6ce")

    val p2 = BinaryData("f1b16c28bd5faa804591393c94079097c846b13584515ddd9f0ff8bd83028df4")
    val a2 = neoPrivkeyToAddr(p2)
    assert(a2 == "AKzZ8ca4zvmtm78MuF4mbj3kmnzg59xtem")
    val priv2 = new Ecdsa.PrivateKey(BinaryData("f1b16c28bd5faa804591393c94079097c846b13584515ddd9f0ff8bd83028df4"))
    assert(priv2.publicKey.neoAddress == "AKzZ8ca4zvmtm78MuF4mbj3kmnzg59xtem")

    val wif = PrivateKey(p2).toWIF
    assert(wif == "L5KXjC95dsQjJGig5yHcTJa3xo32m6XxN9Rcksd4yHYzJ6isWWvK")

    val neopubkey = PrivateKey(p2).publicKey.toBin
    assert(neopubkey sameElements BinaryData("0261e4ab90bc5484ef91e0a893902e3947d3fadc84a6b4d1974459390ddc9f14df"))
  }

  @Test
  def testAddressToHash = {

    assert(UInt160.fromAddress("AP1xWDozWvuVah1W86DKtcWzdw1LLHreMGX").isDefined)
    assert(UInt160.fromAddress("APBC5XmSaD4vooWo3FNho1wGAUyBQo3WCTQ").isDefined)
    assert(UInt160.fromAddress("APRJ7CvHoe5xTWSeD7dfD6eGRZWbGomzDi4").isDefined)

    // not 0548 prefix
    assert(UInt160.fromAddress("APRJ7CvHoe5xTWSeD7dfD6eGRZWbGwz1inp").isEmpty)

    val hash = UInt160.fromAddress("APBC5XmSaD4vooWo3FNho1wGAUyBQo3WCTQ").get

    assert(hash.data sameElements BinaryData("654a5851e9372b87810a8e60cdd2e7cfd80b6e31"))

    assert(UInt160.fromAddress("APBC5XmSaD4vooWo3FNho1wGAUyBQo3WCTQ").isDefined)
    assert(UInt160.fromAddress("APBC5XmSaD4vooWo1FNho1wGAUyBQo3WCTQ").isEmpty)
    assert(UInt160.fromAddress("aPBC5XmSaD4vooWo3FNho1wGAUyBQo3WCTQ").isEmpty)
    assert(UInt160.fromAddress("APBC5XmSaD4vooWo3FNho1wGAUy3BQo3WCTQ").isEmpty)
    assert(UInt160.fromAddress("APBC5XmSaD4voWo3FNho1wGAUyBQo3WCTQ").isEmpty)
  }

  @Test
  def testPrivKeyToWIF = {
    val privKey = BinaryData("1e99423a4ed27608a15a2616a2b0e9e52ced330ac530edcc32c8ffc6a526aedd")
    val wif = Ecdsa.PrivateKey.toWIF(privKey)
    assert(wif == "KxFC1jmwwCoACiCAWZ3eXa96mBM6tb3TYzGmf6YwgdGWZgawvrtJ")
  }

  @Test
  def testWIFtoPrivKey = {
    val privKey = Ecdsa.PrivateKey.fromWIF("KxFC1jmwwCoACiCAWZ3eXa96mBM6tb3TYzGmf6YwgdGWZgawvrtJ").get
    assert(privKey.toBin sameElements BinaryData("1e99423a4ed27608a15a2616a2b0e9e52ced330ac530edcc32c8ffc6a526aedd"))
  }

  @Test
  def testKeyGen = {
    println("======")
    for (i <- 1 to 10) {
      println(i)
      val privateKey = new PrivateKey(BinaryData(Crypto.randomBytes(32)))
      assert(privateKey.compressed == true)

      print("priv key raw:           ");  println(privateKey.toString)  // 32
      print("priv key WIF format:    ");  println(privateKey.toWIF)
      print("pub key (compressed):   ");  println(privateKey.publicKey.toString)  // 1 + 32
      print("pub key script:         ");  println(privateKey.publicKey.pubKeyScript)
      print("script hash:            ");  println(privateKey.publicKey.pubKeyHash.toString)
      print("Address:                ");  println(privateKey.publicKey.address)
      print("Neo Address:            ");  println(privateKey.publicKey.neoAddress)

      println("======")
    }
    assert(true)
  }

}
