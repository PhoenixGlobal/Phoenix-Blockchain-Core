/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * @author: fang.wu@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */
package com.apex.test

import java.text.SimpleDateFormat
import java.time.Instant
import java.util.Date

import com.apex.consensus.{OwnerInfo, ProducerUtil, RegisterData, WitnessInfo}
import com.apex.core.{OperationType, Transaction, TransactionType}
import com.apex.crypto.Ecdsa.PrivateKey
import com.apex.crypto.{BinaryData, FixedNumber, UInt160}
import com.apex.test.ResourcePrepare.BlockChainPrepare
import com.apex.vm.DataWord
import com.apex.vm.precompiled._
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

  @Test
  def testRegisterSuccessWithOwner(){
    try {
      val baseDir = "RegisterContractTest/testRegisterSuccessWithOwner"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction(nonce = 2L, owner = _acct1)(checkRegisterSuccessWithOwner)
    }
    finally {
      chain.close()
    }
  }

  def checkRegisterSuccessWithOwner(tx: Transaction): Unit ={
    assert(chain.addTransaction(tx))
    val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
    assert(witness.isDefined)
    assert(witness.get.name == "register node1")
    assert(chain.getBalance(_acct3).get == FixedNumber.fromDecimal(3))
    //    val fixedNumber = FixedNumber(24912)
    //assert(chain.getBalance(_acct3).get == (FixedNumber.fromDecimal(2) - FixedNumber(25108)))  // 24980
    assert(chain.getBalance(_acct1).get == (FixedNumber.fromDecimal(119.12) - FixedNumber(42000)- FixedNumber(26472)))
    println("111111111111"+chain.getBalance(_witness1.pubkeyHash).get.toString)
//    assert(chain.getBalance(_witness1.pubkeyHash).get == (FixedNumber.fromDecimal(233.2) - FixedNumber(26472)))
    assert(chain.getBalance(new UInt160(PrecompiledContracts.registerNodeAddr.getLast20Bytes)).get == FixedNumber.One)

  }

  @Test
  def testRegisterFailIfWitnessUnregisteredInDb(){
    try {
      val baseDir = "RegisterContractTest/testRegisterFailIfWitnessUnregisteredInDb"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction()(checkRegisterSuccess)
      val witness = chain.getWitness(_acct3.publicKey.pubKeyHash).get.copy(register = false)
      chain.setWitness(witness)
      When.makeRegisterTransaction(OperationType.resisterCancel, 1){
        tx => {
          assert(chain.addTransaction(tx))}}
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
      val tx = new Transaction(TransactionType.Transfer, _acct1.publicKey.pubKeyHash, account,
        FixedNumber.fromDecimal(0.1), 2, BinaryData.empty, FixedNumber.MinValue, 21000, BinaryData.empty, executeTime = 0)
      tx.sign(_acct1)
      assert(chain.addTransaction(tx))
      When.makeWrongRegisterTransaction(account, account, account)(
        checkRegisterFailed
      )
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
          assert(chain.addTransaction(tx))
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
      //When.produceBlock()
      var nowTime = Instant.now.toEpochMilli -10000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      sleepTo(blockTime)
      //nowTime = Instant.now.toEpochMilli
      blockTime += _produceInterval
      startProduceBlock(chain, blockTime, Long.MaxValue)
      assert(chain.isProducingBlock())

      //nowTime = Instant.now.toEpochMilli
      assert(nowTime < blockTime - 200)
      sleepTo(blockTime)
      Then.checkTx(blockTime)
      And.checkAccount()
      val acc2 = chain.getBalance(_acct2).get
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeRegisterTransaction(OperationType.resisterCancel, 1){
        tx => {
          assert(chain.addTransaction(tx))
          val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
          assert(witness.isDefined)
          assert(!witness.get.register)

          assert(chain.getScheduleTx().size == 2)
          //assert(chain.getBalance(_acct3).get == FixedNumber.fromDecimal(2) - FixedNumber(50024))
          assert(chain.getBalance(new UInt160(PrecompiledContracts.registerNodeAddr.getLast20Bytes)).get == FixedNumber.One)
        }
      }
      val block1 = chain.produceBlockFinalize()
      assert(block1.isDefined)
      assert(block1.get.transactions.size == 8)
//      assert(chain.getBalance(_acct2).get == (FixedNumber.fromDecimal(230.2) -FixedNumber(88200) - FixedNumber(42000)) )

      assert(!chain.isProducingBlock())
      assert(chain.getHeight() == 1)
      assert(chain.getHeadTime() == blockTime)
      println("chain.getHeadTime()" + blockTime)
      assert(chain.head.id() == block1.get.id())

//      val tx1 = makeTx(_acct3, _acct4, FixedNumber.fromDecimal(1), 2)
//
//      val block2 = makeBlock(chain, block1.get, Seq(tx1))
//
//      // test getTransaction()
//      assert(block2.getTransaction(tx1.id).get.id == tx1.id)
//
//      println("call tryInsertBlock block2")
//      assert(chain.tryInsertBlock(block2, true))
//      println("block2 inserted")
//
//      assert(chain.getBalance(_acct4).get == FixedNumber.fromDecimal(3))
//
//      assert(chain.getBalance(_acct3).get == FixedNumber.fromDecimal(1))

      blockTime += _produceInterval
      startProduceBlock(chain, blockTime, Long.MaxValue)
      assert(chain.isProducingBlock())

      sleepTo(blockTime)
      chain.addTransaction(makeTx(_acct2, _acct4, FixedNumber.fromDecimal(2), 3, executedTime = blockTime + 250))

      val block2 = chain.produceBlockFinalize()
      assert(block2.isDefined)
      assert(block2.get.transactions.size == 2)
      assert(chain.getScheduleTx().size == 3)
      val account2 = chain.getBalance(_acct2).get
     // assert(chain.getBalance(_acct2).get == FixedNumber.fromDecimal(230.2) -FixedNumber(88200) - FixedNumber(42000) -FixedNumber(88200))

      assert(!chain.isProducingBlock())
      assert(chain.getHeight() == 2)
      assert(chain.getHeadTime() == blockTime)
      assert(chain.head.id() == block2.get.id())

      blockTime += _produceInterval
      startProduceBlock(chain, blockTime, Long.MaxValue)
      assert(chain.isProducingBlock())

      sleepTo(blockTime)

      println("chain.blockTime()" + blockTime)

      val block3 = chain.produceBlockFinalize()
      assert(block3.isDefined)
      //assert(block3.get.transactions.size == 4)
      //assert(chain.getScheduleTx().size == 0)

      assert(!chain.isProducingBlock())
      assert(chain.getHeight() == 3)
      assert(chain.getHeadTime() == blockTime)
      assert(chain.head.id() == block3.get.id())

//      assert(chain.getBalance(_acct3).get == FixedNumber.fromDecimal(3) - FixedNumber(49888))
//      assert(chain.getBalance(_acct2).get == FixedNumber.fromDecimal(226.2))
      //assert(chain.getBalance(new UInt160(PrecompiledContracts.registerNodeAddr.getLast20Bytes)).get == FixedNumber.Zero)
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
          assert(chain.addTransaction(tx))
          val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
          assert(witness.isEmpty)
        }
      }
    }
    finally {
      chain.close()
    }
  }

  def makeRegisterTransaction(operationType: OperationType.Value = OperationType.register,
                              nonce: Long = 0,
                              account: PrivateKey = _acct3,
                              name: String = "register node1",
                              owner: PrivateKey = _acct3)(f: Transaction => Unit){
    val txData = RegisterData(account.publicKey.pubKeyHash, WitnessInfo(account.publicKey.pubKeyHash, false, name,  ownerInfo = OwnerInfo(owner.publicKey.pubKeyHash)),operationType).toBytes
//    println(txData)
    val registerContractAddr = new UInt160(DataWord.of("0000000000000000000000000000000000000000000000000000000000000101").getLast20Bytes)
    val tx = new Transaction(TransactionType.Call, owner.publicKey.pubKeyHash ,registerContractAddr, FixedNumber.Zero,
      nonce, txData, FixedNumber.MinValue, 9000000L, BinaryData.empty)
    tx.sign(owner)
    f(tx)
  }

  def makeWrongRegisterTransaction(txFromAccount: UInt160,
                                   registerAccount: UInt160,
                                   registerWitnessAddr: UInt160,
                                   operationType: OperationType.Value = OperationType.register,
                                   nonce: Long =0) (f: Transaction => Unit){
    val txData = RegisterData(registerAccount, WitnessInfo(registerWitnessAddr, false, "register node1", ownerInfo = OwnerInfo(registerWitnessAddr)),operationType).toBytes
    val registerContractAddr = new UInt160(DataWord.of("0000000000000000000000000000000000000000000000000000000000000101").getLast20Bytes)
    val tx = new Transaction(TransactionType.Call, txFromAccount ,registerContractAddr, FixedNumber.Zero,
      nonce, txData, FixedNumber.MinValue, 9000000L, BinaryData.empty)
    f(tx)
  }



  def checkRegisterSuccess(tx: Transaction): Unit ={
    assert(chain.addTransaction(tx))
    val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
    assert(witness.isDefined)
    assert(witness.get.name == "register node1")
//    val fixedNumber = FixedNumber(24912)
    val ewfwef = chain.getBalance(_acct3).get
    //assert(chain.getBalance(_acct3).get == (FixedNumber.fromDecimal(2) - FixedNumber(25108)))  // 24980
    assert(chain.getBalance(_acct3).get > FixedNumber.fromDecimal(1.999))
    assert(FixedNumber.fromDecimal(2) > chain.getBalance(_acct3).get)
    assert(chain.getBalance(new UInt160(PrecompiledContracts.registerNodeAddr.getLast20Bytes)).get == FixedNumber.One)
  }

  def checkRegisterFailed(tx: Transaction): Unit ={
    assert(chain.addTransaction(tx))
    val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
    assert(witness.isEmpty)
  }

  def checkTx(executeTime: Long = 0): Unit ={
    val sheduleTime = if (executeTime ==0) blockTimeForSchedule + 750 else executeTime+ 750
    //assert(!chain.addTransaction(makeTx(_acct1, _acct3, FixedNumber.fromDecimal(123), 1)))
    assert(chain.addTransaction(makeTx(_acct1, _acct3, FixedNumber.fromDecimal(1), 0)))
    assert(!chain.addTransaction(makeTx(_acct1, _acct3, FixedNumber.fromDecimal(2), 0)))
    assert(chain.addTransaction(makeTx(_acct1, _acct3, FixedNumber.fromDecimal(2), 1)))
    assert(chain.addTransaction(makeTx(_acct2, _acct4, FixedNumber.fromDecimal(2), 0)))
    assert(chain.addTransaction(makeTx(_acct2, _acct4, FixedNumber.fromDecimal(2), 1, executedTime = sheduleTime)))
    assert(chain.addTransaction(makeTx(_acct2, _acct4, FixedNumber.fromDecimal(2), 2)))
  }

  def checkAccount(): Unit ={
    assert(chain.getAccount(_acct3).isDefined)
    assert(chain.getBalance(_acct3).get == FixedNumber.fromDecimal(3))
    assert(chain.getBalance(_acct1).get == (FixedNumber.fromDecimal(120.12) - FixedNumber(42000)))
    println(chain.getBalance(_witness1.pubkeyHash).get.toString)
//    assert(chain.getBalance(_witness1.pubkeyHash).get == FixedNumber.fromDecimal(234.2))
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