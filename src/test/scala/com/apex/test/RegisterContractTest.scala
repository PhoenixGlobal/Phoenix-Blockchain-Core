/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * @author: fang.wu@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */
package com.apex.test

import com.apex.consensus.{RegisterData, WitnessInfo}
import com.apex.core.{OperationType, Transaction, TransactionType}
import com.apex.crypto.{BinaryData, FixedNumber, UInt160}
import com.apex.test.ResourcePrepare.BlockChainPrepare
import com.apex.vm.DataWord
import org.junit.{AfterClass, Test}

import scala.reflect.io.Directory

class RegisterContractTest extends BlockChainPrepare{
  Directory("RegisterContractTest").deleteRecursively()

  @Test
  def testRegisterSuccess(){
    try {
      val baseDir = "RegisterContractTest/testRegisterSuccess"
      Given.createChain(baseDir){}
        When.produceBlock()
        Then.checkTx()
        And.checkAccount()
        When.makeRegisterTransaction()(checkRegisterSuccess)
    }
    finally {
      chain.close()
    }
  }

  //register address must be same as transaction from address
  @Test
  def testRegisterAccountNotEqualTxFromAccount(){
    try {
      val baseDir = "RegisterContractTest/testRegisterAccountNotEqualTxFromAccount"
      Given.createChain(baseDir){}
        When.produceBlock()
        Then.checkTx()
        And.checkAccount()
        When.makeWrongRegisterTransaction(_acct3.publicKey.pubKeyHash, UInt160.Zero, _acct3.publicKey.pubKeyHash)(
          checkRegisterFailed)
    }
    finally {
      chain.close()
    }
  }

  //register account balance is not enough to register a producer
  @Test
  def testRegisterAccountBalanceNotEnough(){
    try {
      val baseDir = "RegisterContractTest/testRegisterAccountBalanceNotEnough"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      val account = new UInt160(DataWord.of(333).getLast20Bytes)
      When.makeWrongRegisterTransaction(account, account, account)(checkRegisterFailed)
    }
    finally {
      chain.close()
    }
  }

  //register a producer which is already a producer is not allowed
  @Test
  def testRegisterExistInWitnesses(){
    try {
      val baseDir = "RegisterContractTest/testRegisterExistInWitnesses"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeRegisterTransaction(nonce = 1){
        tx => {
          assert(!chain.addTransaction(tx))
          val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
          assert(witness.isDefined)
          assert(witness.get.name == "register node1")
        }
      }
    }
    finally {
      chain.close()
    }
  }

  //cancel a witness success
  @Test
  def testCancelWitnessSuccess(){
    try {
      val baseDir = "RegisterContractTest/testCancelWitnessSuccess"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeRegisterTransaction(OperationType.resisterCancel, 1){
        tx => {
          assert(chain.addTransaction(tx))
          val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
          assert(witness.isEmpty)
          assert(chain.getBalance(_acct3.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(3))
        }
      }
    }
    finally {
      chain.close()
    }
  }

  //register a producer which is already a producer is not allowed
  @Test
  def testCancelWitnessNotExistInWitness(){
    try {
      val baseDir = "RegisterContractTest/testCancelWitnessNotExistInWitness"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeWrongRegisterTransaction(_acct3.publicKey.pubKeyHash, _acct3.publicKey.pubKeyHash,_acct3.publicKey.pubKeyHash,
        OperationType.resisterCancel){
        tx => {
          assert(!chain.addTransaction(tx))
          val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
          assert(witness.isEmpty)
        }
      }
    }
    finally {
      chain.close()
    }
  }

  def makeRegisterTransaction(operationType: OperationType.Value = OperationType.register, nonce: Long = 0,
                              account: UInt160 = _acct3.publicKey.pubKeyHash, name: String = "register node1")(f: Transaction => Unit){
    val txData = RegisterData(account, WitnessInfo(name, account),operationType).toBytes
    val registerContractAddr = new UInt160(DataWord.of("0000000000000000000000000000000000000000000000000000000000000101").getLast20Bytes)
    val tx = new Transaction(TransactionType.Call, account ,registerContractAddr, "", FixedNumber.Zero,
      nonce, txData, FixedNumber(0), 9000000L, BinaryData.empty)
    f(tx)
  }

  def makeWrongRegisterTransaction(txFromAccount: UInt160, registerAccount: UInt160, registerWitnessAddr: UInt160,
                                           operationType: OperationType.Value = OperationType.register, nonce: Long =0)
                                          (f: Transaction => Unit){
    println(txFromAccount.toString)
    val txData = RegisterData(registerAccount, WitnessInfo("register node1", registerWitnessAddr),operationType).toBytes
    val registerContractAddr = new UInt160(DataWord.of("0000000000000000000000000000000000000000000000000000000000000101").getLast20Bytes)
    val tx = new Transaction(TransactionType.Call, txFromAccount ,registerContractAddr, "", FixedNumber.Zero,
      nonce, txData, FixedNumber(0), 9000000L, BinaryData.empty)
    f(tx)
  }



  def checkRegisterSuccess(tx: Transaction): Unit ={
    assert(chain.addTransaction(tx))
    val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
    assert(witness.isDefined)
    assert(witness.get.name == "register node1")
    assert(chain.getBalance(_acct3.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(2))
  }

  def checkRegisterFailed(tx: Transaction): Unit ={
    assert(!chain.addTransaction(tx))
    val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
    assert(witness.isEmpty)
  }

  def checkTx(): Unit ={
    assert(!chain.addTransaction(makeTx(_acct1, _acct3.publicKey.pubKeyHash, FixedNumber.fromDecimal(123), 1)))
    assert(chain.addTransaction(makeTx(_acct1, _acct3.publicKey.pubKeyHash, FixedNumber.fromDecimal(1), 0)))
    assert(!chain.addTransaction(makeTx(_acct1, _acct3.publicKey.pubKeyHash, FixedNumber.fromDecimal(2), 0)))
    assert(chain.addTransaction(makeTx(_acct1, _acct3.publicKey.pubKeyHash, FixedNumber.fromDecimal(2), 1)))
    assert(chain.addTransaction(makeTx(_acct2, _acct4.publicKey.pubKeyHash, FixedNumber.fromDecimal(2), 0)))
    //assert(chain.addTransaction(makeTx(_acct1, _acct3.publicKey.pubKeyHash, FixedNumber.fromDecimal(2), 1)))
  }

  def checkAccount(): Unit ={
    assert(chain.getAccount(_acct3.publicKey.pubKeyHash).isDefined)
    assert(chain.getBalance(_acct3.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(3))
  }



  def When = this
  def Then = this
  def And  =this
  def Given= this
}

object RegisterContractTest {
  @AfterClass
  def cleanUp: Unit = {
    println("clean Directory")
    Directory("RegisterContractTest").deleteRecursively()
  }
}