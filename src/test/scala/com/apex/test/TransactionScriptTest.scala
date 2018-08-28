///*
// * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
// *
// * FileName: TransactionScriptTest.scala
// *
// * @author: shan.huang@chinapex.com: 2018-08-06 下午4:06@version: 1.0
// */
//
//package com.apex.test
//
//import com.apex.core.{TransactionInput, TransactionOutput, TransferTransaction}
//import com.apex.crypto.{BinaryData, Crypto, Ecdsa, Fixed8, UInt160, UInt256}
//import org.junit.Test
//import com.apex.core.script.{Script}
//
//@Test
//class TransactionScriptTest {
//  @Test
//  def testTransactionScript = {
//    var privKey1 = new Ecdsa.PrivateKey(BinaryData("18e14a7b6a307f426a94f8114701e7c8e774e7f9a47e2c2035db29a206321725"))
//    var pubKey1 = privKey1.publicKey
//    val preOutputPubKeyScript1 = Script.write(Script.pay2pkh(pubKey1))
//
//    var privKeyRecv = new Ecdsa.PrivateKey(BinaryData("12345a7b6a307f426a94f8114701e7c8e774e7f9a47e2c2035db29a206321725"))
//    var pubKeyRecv = privKeyRecv.publicKey
//
//    val input1 = TransactionInput(
//      new UInt256(BinaryData("0000000000000000000000000000000000000000000000000000000000000001")), 0, "1234")
//
//    val output1 = TransactionOutput(
//      new UInt160(pubKeyRecv.hash160),
//      new UInt256(BinaryData("0000000000000000000000000000000000000000000000000000000000000001")),
//      Fixed8.One,
//      Script.write(Script.pay2pkh(pubKeyRecv)))
//
//    val tx = new TransferTransaction(
//      Seq[TransactionInput](input1),
//      Seq[TransactionOutput](output1),
//      "here you go")
//
//    tx.signInput(0, 0, 0, privKey1)
//
//    val runner = new Script.Runner(new Script.Context(tx, 0))
//
//    assert(runner.verifyScripts(tx.inputs(0).signatureScript, preOutputPubKeyScript1))
//  }
//}
