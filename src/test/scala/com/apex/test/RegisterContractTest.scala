package com.apex.test

import java.time.Instant

import com.apex.consensus.{ProducerUtil, RegisterData, WitnessInfo}
import com.apex.core.{OperationType, Transaction, TransactionType}
import com.apex.crypto.{BinaryData, FixedNumber, UInt160}
import com.apex.test.ResourcePrepare.BlockChainPrepare
import com.apex.vm.DataWord
import org.bouncycastle.util.encoders.Hex
import org.junit.{AfterClass, Test}

import scala.reflect.io.Directory

class RegisterContractTest extends BlockChainPrepare{
  Directory("RegisterContractTest").deleteRecursively()

  @Test
  def testRegisterSuccess(){
    try {
      val baseDir = "RegisterContractTest/testCreateChain"
      Given.createChain(baseDir) {
        When.produceBlock() {}
        Then.checkTx()
        And.checkAccount()
        When.makeRegisterTransaction()(checkRegisterSuccess)
      }
    }
    finally {
      chain.close()
    }
  }

//  @Test
//  def testRegisterFailed(){
//    try {
//      val baseDir = "RegisterContractTest/testCreateChain"
//      Given.createChain(baseDir) {
//        When.produceBlock() {}
//        Then.checkTx()
//        And.checkAccount()
//        When.makeRegisterTransaction()(checkRegisterSuccess)
//      }
//    }
//    finally {
//      chain.close()
//    }
//  }

  private def makeRegisterTransaction()(f: Transaction => Unit){
    val txData = RegisterData(_acct3.publicKey.pubKeyHash, WitnessInfo("register node1", _acct3.publicKey.pubKeyHash),OperationType.register).toBytes
    val registerContractAddr = new UInt160(DataWord.of(9).getLast20Bytes)
    val tx = new Transaction(TransactionType.Call, _acct3.publicKey.pubKeyHash ,registerContractAddr, "", FixedNumber.One,
      0, txData, FixedNumber(0), 9000000L, BinaryData.empty)
    f(tx)
  }

  private def checkRegisterSuccess(tx: Transaction): Unit ={
    assert(chain.addTransaction(tx))
    val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
    assert(witness.isDefined)
    assert(witness.get.name == "register node1")
  }

  private def checkTx(): Unit ={
    assert(!chain.addTransaction(makeTx(_acct1, _acct3.publicKey.pubKeyHash, FixedNumber.fromDecimal(123), 1)))
    assert(chain.addTransaction(makeTx(_acct1, _acct3.publicKey.pubKeyHash, FixedNumber.fromDecimal(1), 0)))
    assert(!chain.addTransaction(makeTx(_acct1, _acct3.publicKey.pubKeyHash, FixedNumber.fromDecimal(2), 0)))
    assert(chain.addTransaction(makeTx(_acct1, _acct3.publicKey.pubKeyHash, FixedNumber.fromDecimal(2), 1)))
  }

  private def checkAccount(): Unit ={
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