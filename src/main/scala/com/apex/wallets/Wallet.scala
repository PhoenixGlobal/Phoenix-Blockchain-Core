/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Wallet.scala
 *
 * @author: shan.huang@chinapex.com: 2018-07-27 下午4:06@version: 1.0
 */

package com.apex.wallets

import com.apex.crypto.{BinaryData, Base58Check, UInt160}
//import java.security.SecureRandom

object Wallet {
  
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
     
    // "0FBABD70" is for the "APEX" prefix
    Base58Check.encode(BinaryData("0FBABD70"), scriptHash)      
  }
  
  def toAddress(scriptHash: UInt160): String = {
    toAddress(scriptHash.data)
  }
  
  def toScriptHash(address: String): Option[UInt160] = {    
    if (address.startsWith("APEX")) {
      val decode = Base58Check.decode(address).getOrElse(Array[Byte]())
      if (decode.length == 24) {
        // 4 bytes prefix + 20 bytes data (+ 4 bytes checksum)
        Some(UInt160.fromBytes(decode.slice(4, 24)))
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