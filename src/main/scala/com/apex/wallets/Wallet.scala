/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Wallet.scala
 *
 * @author: shan.huang@chinapex.com: 2018-07-27 下午4:06@version: 1.0
 */

package com.apex.wallets

import com.apex.core._
import com.apex.crypto.{Base58Check, BinaryData, Fixed8, UInt160, UInt256}
import com.apex.crypto.Ecdsa.PrivateKey
import com.apex.core.script.{Script}

import scala.collection.mutable.{ArrayBuffer, Set}
//import java.security.SecureRandom

object Wallet {

  var privKeys = Set.empty[PrivateKey]

  def getBalance(address: String, assetId: UInt256): Fixed8 = {
    var sum: Fixed8 = Fixed8.Zero
    val utxoSet = Blockchain.Current.getUTXOByAddress(toScriptHash(address).get)
    if (utxoSet != None) {
      for (utxo <- utxoSet.get) {
        val tx = Blockchain.Current.getTransaction(utxo._1).get
        if (tx.outputs(utxo._2).assetId == assetId) {
          sum += tx.outputs(utxo._2).amount
        }
      }
    }
    sum
  }

  def findPrivKey(pubKeyHash: UInt160): Option[PrivateKey] = {
    // fixme: use map or db
    for (pKey <- privKeys) {
      if (pubKeyHash.compare(UInt160.fromBytes(pKey.publicKey.hash160)) == 0)
        return Some(pKey)
    }
    None
  }

  def findPrivKey(input: TransactionInput): Option[PrivateKey] = {
    val tx = Blockchain.Current.getTransaction(input.txId).get
    findPrivKey(tx.outputs(input.index).address)
  }

  def findInputs(assetId: UInt256): Set[(UInt256, Int)] = {
    val inputs = Set.empty[(UInt256, Int)]
    for (pKey <- privKeys) {
      val utxoSet = Blockchain.Current.getUTXOByAddress(new UInt160(pKey.publicKey.hash160))
      if (utxoSet != None) {
       for (utxo <- utxoSet.get) {
         val tx = Blockchain.Current.getTransaction(utxo._1).get
         if (tx.outputs(utxo._2).assetId == assetId) {
           inputs.add(utxo)
         }
       }
      }
    }
    inputs
  }

  def makeTransaction(toAddress: String, assetId: UInt256, amount: Fixed8): Option[Transaction] = {
    var curAmount: Fixed8 = Fixed8.Zero
    val inputs: Set[TransactionInput] = Set.empty
    val outputs: Set[TransactionOutput] = Set.empty
    val allInputs = findInputs(assetId)
    if (!allInputs.isEmpty) {
      for (input <- allInputs) {
        if (amount > curAmount) {
          inputs.add(new TransactionInput(input._1, input._2, BinaryData("")))
          curAmount = curAmount + Blockchain.Current.getTransaction(input._1).get.outputs(input._2).amount
        }
      }
    }
    if (curAmount.value >= amount.value) {
      outputs.add(new TransactionOutput(
          toScriptHash(toAddress).get,
          assetId,
          amount,
          Script.write(Script.pay2pkh(toScriptHash(toAddress).get))))
      if (curAmount.value > amount.value) {
        val changeAddress = privKeys.head.publicKey.toAddress
        outputs.add(new TransactionOutput(
            toScriptHash(changeAddress).get,
            assetId,
            curAmount - amount,
            Script.write(Script.pay2pkh(toScriptHash(changeAddress).get))))
      }
      val tx = new TransferTransaction(inputs.toSeq, outputs.toSeq, "123")
      for (index <- 0 to (tx.inputs.length - 1)) {
        tx.signInput(index, 0, 0, findPrivKey(tx.inputs(index)).get)
      }
      Some(tx)
    }
    else
      None
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