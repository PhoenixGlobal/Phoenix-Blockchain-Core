/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: CryptoTest.scala
 *
 * @author: shan.huang@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.test

import java.io.{ByteArrayOutputStream, DataOutputStream}

import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey, Scalar}
import com.apex.crypto.{Base58, Base58Check, BinaryData, Crypto, Ecdsa}
import org.junit.Test

@Test
class CryptoTest {   
  @Test
  def testHash256 = {        
     val data = "abc".getBytes("US-ASCII")
     val hash = Crypto.hash256(data)
     assert(hash sameElements BinaryData("4F8B42C22DD3729B519BA6F68D2DA7CC5B2D606D05DAED5AD5128CC03E6C6358"))    
  }
  @Test
  def testHash160 = {        
     val key = BinaryData("0250863ad64a87ae8a2fe83c1af1a8403cb53f53e486d8511dad8a04887e5b2352")
     val hash = Crypto.hash160(key)
     assert(hash sameElements BinaryData("f54a5851e9372b87810a8e60cdd2e7cfd80b6e31"))
  }
  @Test
  def testSha256 = {
     val key = BinaryData("0250863ad64a87ae8a2fe83c1af1a8403cb53f53e486d8511dad8a04887e5b2352")
     val hash = Crypto.sha256(key)
     assert(hash sameElements BinaryData("0b7c28c9b7290c98d7438e70b3d3f7c848fbd7d1dc194ff83f4f7cc9b1378e98"))     
  }
  @Test
  def testSha3 = {
    assert(Crypto.sha3("horse".getBytes) sameElements
      BinaryData("c87f65ff3f271bf5dc8643484f66b200109caffe4bf98c4cb393dc35740b28c0"))

    assert(Crypto.sha3("cow".getBytes) sameElements
      BinaryData("c85ef7d79691fe79573b1a7064c19c1a9819ebdbd1faaab1a8ec92344438aaf4"))
  }
  @Test
  def testSha3_standard = {
    assert(Crypto.sha3_standard("".getBytes) sameElements
      BinaryData("a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"))

    assert(Crypto.sha3_standard("abc".getBytes) sameElements
      BinaryData("3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"))
  }
  @Test
  def testRIPEMD160 = {
     val data = "Rosetta Code".getBytes("US-ASCII")
     val hash = Crypto.RIPEMD160(data)
     assert(hash sameElements BinaryData("b3be159860842cebaa7174c8fff0aa9e50a5199f"))     
  }
  @Test
  def testEcdsaKey = {
     var privKey = new Ecdsa.PrivateKey(BinaryData("18e14a7b6a307f426a94f8114701e7c8e774e7f9a47e2c2035db29a206321725"))
     var pubKey = privKey.publicKey

     //  compressed pub key:  02 50863ad64a87ae8a2fe83c1af1a8403cb53f53e486d8511dad8a04887e5b2352    
     assert(pubKey.toBin.data.toArray sameElements BinaryData("0250863ad64a87ae8a2fe83c1af1a8403cb53f53e486d8511dad8a04887e5b2352"))
     
  }
  @Test
  def testSign = {
     val privKey = BinaryData("f8b8af8ce3c7cca5e300d33939540c10d45ce001b8f252bfbc57ba0342904181")
     val message = "Alan Turing".getBytes("US-ASCII")
     val sig = Crypto.sign(message, privKey)
     assert(sig sameElements BinaryData("304402207063ae83e7f62bbb171798131b4a0564b956930092b33b07b395615d9ec7e15c022058dfcc1e00a35e1572f366ffe34ba0fc47db1e7189759b9fb233c5b05ab388ea"))
     
  }
  @Test
  def testSign2 = {
    val privKey = Ecdsa.PrivateKey(BinaryData("f8b8af8ce3c7cca5e300d33939540c10d45ce001b8f252bfbc57ba0342904181"))
    val pubkey = privKey.publicKey
    val message = "Alan Turing".getBytes("US-ASCII")
    val sig = Crypto.sign(message, privKey)
    assert(sig sameElements BinaryData("304402207063ae83e7f62bbb171798131b4a0564b956930092b33b07b395615d9ec7e15c022058dfcc1e00a35e1572f366ffe34ba0fc47db1e7189759b9fb233c5b05ab388ea"))

  }

  def ethPrivKeyToAddr(pKey: BinaryData): String = {
    require(pKey.length == 32)
    val privKey = new PrivateKey(Scalar(pKey), compressed = false) //Ecdsa.PrivateKey(pKey)
    val pubkey65 = privKey.publicKey
    val pubKey64 = BinaryData(pubkey65.toBin.drop(1))
    require(pubKey64.size == 64)
    val hashKeccak256 = BinaryData(Crypto.sha3(pubKey64))
    val hash20 = BinaryData(hashKeccak256.takeRight(20))
    val ethAddr: String = "0x" + hash20.toString
    ethAddr
  }

  @Test
  def testEthAddrGen() = {
    val a1 = ethPrivKeyToAddr(BinaryData("f8F8a2f43c8376ccb0871305060d7b27b0554d2cc72bccf41b2705608452f315"))
    assert(a1 == "0x001d3f1ef827552ae1114027Bd3ecf1F086ba0f9".toLowerCase)

    val a2 = ethPrivKeyToAddr(BinaryData("b783d2edc76647afa25b2fb380c0cd4953c153bdd44893bdaab6334720893ae9"))
    assert(a2 == "0x6750d5b2c624972FA6d59618574E24942543d8Fe".toLowerCase)

    val a3 = ethPrivKeyToAddr(BinaryData("8b98d41945a8a6f4ea2dd777213fa459124430937881ed1f7a608967455251b5"))
    assert(a3 == "0xb3C4c41B691f182a8C26128A4E880480141e3Ff2".toLowerCase)
  }

  @Test
  def testRecoverPublicKey() = {
    val random = new scala.util.Random()
    val privbytes = new Array[Byte](32)
    val message = new Array[Byte](32)
    for (i <- 0 until 10) {
      random.nextBytes(privbytes)
      random.nextBytes(message)

      val priv = Ecdsa.PrivateKey(privbytes)
      val pub = priv.publicKey
      val (r, s) = Ecdsa.sign(message, priv)
      //      val sigBin = Ecdsa.encodeSignature(r, s)
      //      val (pub11, pub22) = Ecdsa.recoverPublicKey(sigBin, message)
      val (pub1, pub2) = Ecdsa.recoverPublicKey((r, s), message)

      assert(Ecdsa.verifySignature(message, (r, s), pub1))
      assert(Ecdsa.verifySignature(message, (r, s), pub2))
      assert(pub == pub1 || pub == pub2)
    }
  }
  @Test
  def testVerifySignature = {
     val message = "Alan Turing".getBytes("US-ASCII")

     val sig = BinaryData("304402207063ae83e7f62bbb171798131b4a0564b956930092b33b07b395615d9ec7e15c022058dfcc1e00a35e1572f366ffe34ba0fc47db1e7189759b9fb233c5b05ab388ea")

     // 32+1=33 bytes compressed pub key
     val pubKey = PublicKey(BinaryData("0292df7b245b81aa637ab4e867c8d511008f79161a97d64f2ac709600352f7acbc"))

     val (pub1, pub2) = Ecdsa.recoverPublicKey(sig, Crypto.sha256(message))

     assert(Crypto.verifySignature(message, sig, pubKey.toBin))
     assert(Crypto.verifySignature(message, sig, pub1.toBin))
     assert(Crypto.verifySignature(message, sig, pub2.toBin))
     assert(Crypto.verifySignature(message, sig, pubKey.pubKeyHash))
  }
  @Test
  def testBase58 = {     
      //  00  f54a5851e9372b87810a8e60cdd2e7cfd80b6e31  c7f18fe8
      val data = BinaryData("00f54a5851e9372b87810a8e60cdd2e7cfd80b6e31c7f18fe8")
      assert(Base58.encode(data) == "1PMycacnJaSqwwJqjawXBErnLsZ7RkXUAs")      

      val dd = Base58.decode("1PMycacnJaSqwwJqjawXBErnLsZ7RkXUAs").getOrElse(Array[Byte]())      
      assert(dd sameElements BinaryData("00f54a5851e9372b87810a8e60cdd2e7cfd80b6e31c7f18fe8"))     
   }
   @Test
   def testBase58Check = {
      val data = BinaryData("f54a5851e9372b87810a8e60cdd2e7cfd80b6e31")      
      assert(Base58Check.encode(0x00.toByte, data) == "1PMycacnJaSqwwJqjawXBErnLsZ7RkXUAs")      

      //  00  f54a5851e9372b87810a8e60cdd2e7cfd80b6e31     
      val dd = Base58Check.decode("1PMycacnJaSqwwJqjawXBErnLsZ7RkXUAs").getOrElse(Array[Byte]())
      assert(dd sameElements BinaryData("00f54a5851e9372b87810a8e60cdd2e7cfd80b6e31"))
   }
   @Test
   def testAes = {      
      val key = BinaryData("140b41b62a29beb4061bdd66b6747e14")
      val iv  = BinaryData("20814805c1767293bd9f1d9cab2bc3e7")

      val data1 = BinaryData("12345678")
      val encrypted1 = Crypto.AesEncrypt(data1, key, iv)
      assert(encrypted1 sameElements BinaryData("8B536DD84217046497B0EDA6AF72837A"))
      
      val dec = Crypto.AesDecrypt(encrypted1, key, iv)
      assert(dec sameElements BinaryData("12345678"))
   }
   @Test
   def testPointSerialize = {
      
      var privKey = new Ecdsa.PrivateKey(BinaryData("18e14a7b6a307f426a94f8114701e7c8e774e7f9a47e2c2035db29a206321725"))
      var pubKey = privKey.publicKey

      //  compressed pub key:  02 50863ad64a87ae8a2fe83c1af1a8403cb53f53e486d8511dad8a04887e5b2352    
      assert(pubKey.toBin.data.toArray sameElements BinaryData("0250863ad64a87ae8a2fe83c1af1a8403cb53f53e486d8511dad8a04887e5b2352"))
      
      val point : Ecdsa.Point = pubKey.value      

      val bs = new ByteArrayOutputStream
      val os = new DataOutputStream(bs)

      point.serialize(os)

      // 21 02 50863ad64a87ae8a2fe83c1af1a8403cb53f53e486d8511dad8a04887e5b2352 
      // 0x21 = 33 = (32 + 1) bytes
      // 0x02 : compressed type
      assert(bs.toByteArray sameElements BinaryData("210250863ad64a87ae8a2fe83c1af1a8403cb53f53e486d8511dad8a04887e5b2352"))      
   }
}
