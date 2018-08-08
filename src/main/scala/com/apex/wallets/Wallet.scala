/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Wallet.scala
 *
 * @author: shan.huang@chinapex.com: 2018-07-27 下午4:06@version: 1.0
 */

package com.apex.wallets

import com.apex.core.Blockchain
import com.apex.crypto.{Base58Check, BinaryData, Fixed8, UInt160, UInt256}
import com.apex.crypto.Ecdsa.PrivateKey

import scala.collection.mutable.{ArrayBuffer, Set}
//import java.security.SecureRandom

object Wallet {

  var privKeys = Set.empty[PrivateKey]

  def getBalance(address: String, assetId: UInt256): Fixed8 = {
    var sum: Fixed8 = Fixed8.Zero
    val utxo = Blockchain.Current.getUTXOByAddress(toScriptHash(address).get)
    if (utxo != None) {
      for (entry <- utxo.get) {
        val tx = Blockchain.Current.getTransaction(entry._1).get
        if (tx.outputs(entry._2).assetId == assetId) {
          sum += tx.outputs(entry._2).amount
        }
      }
    }
    sum
  }

  def findInputs(assetId: UInt256): Option[Set[(UInt256, Int)]] = {
    val inputs = Set.empty[(UInt256, Int)]
    for (key <- privKeys) {

    }
    None

  }

  def makeTransaction(toAddress: String, assetId: UInt256, amount: Fixed8) = {



  }

  def importPrivKeyFromWIF(wif: String) = {
    val key = getPrivKeyFromWIF(wif)
    if (key != None) {
      privKeys.add(new PrivateKey(BinaryData(key.get), true))
    }
  }
  
  def getPrivKeyFromWIF(wif: String): Option[Array[Byte]] = {
    val decode = Base58Check.decode(wif).getOrElse(Array[Byte]())
    if (decode.length == 34) {
      // 1 bytes prefix + 32 bytes data + 1 byte 0x01 (+ 4 bytes checksum)
      if (decode(33) == 0x01.toByte) {
        Some(decode.slice(1, 33))
      } else {
        None
      }
    } else {
      None
    }
  }
  
  def privKeyToWIF(privateKey: Array[Byte]): String = {
    assert(privateKey.length == 32)
    var data = new Array[Byte](34)
    // 0x80: mainnet
    data(0) = 0x80.toByte
    Array.copy(privateKey, 0, data, 1, 32)
    // 0x01: compressed 
    data(33) = 0x01.toByte
    Base58Check.encode(data)
  }  
 
  def toAddress(scriptHash: Array[Byte]): String = {
    assert(scriptHash.length == 20)
     
    // "0548" is for the "AP" prefix
    Base58Check.encode(BinaryData("0548"), scriptHash)
  }
  
  def toAddress(scriptHash: UInt160): String = {
    toAddress(scriptHash.data)
  }
  
  def toScriptHash(address: String): Option[UInt160] = {
    if (address.startsWith("AP") && address.length == 35) {
      val decode = Base58Check.decode(address).getOrElse(Array[Byte]())
      if (decode.length == 22) {
        // 2 bytes prefix + 20 bytes data (+ 4 bytes checksum)
        Some(UInt160.fromBytes(decode.slice(2, 22)))
      } else {
        None
      }
    } else {
      None
    }
  }

}

//object Wallet {
//  final val Default: Wallet = new Wallet
//}