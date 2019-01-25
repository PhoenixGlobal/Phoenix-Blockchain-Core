/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * @author: fang.wu@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */
package com.apex.test

import com.apex.consensus.VoteData
import com.apex.core.{OperationType, Transaction, TransactionType}
import com.apex.crypto.{BinaryData, FixedNumber, UInt160}
import com.apex.vm.DataWord
import org.junit.{AfterClass, Test}

import scala.reflect.io.Directory

class VoteContractTest extends RegisterContractTest {
  Directory("VoteContractTest").deleteRecursively()
  Directory("RegisterContractTest").deleteRecursively()

  @Test
  def testVoteSuccess():Unit = {
    try {
      val baseDir = "VoteContractTest/testVoteSuccess"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeVoteTransaction(nonce = 2)(checkVoteSuccess)
    }
    finally {
      chain.close()
    }
  }

  //vote is not allowed when the vote account balance is not enough
  @Test
  def testVoteAccountBalanceNotEnough():Unit = {
    try {
      val baseDir = "VoteContractTest/testVoteAccountBalanceNotEnough"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeVoteTransaction(nonce = 2, counter =FixedNumber(FixedNumber.One.value * 200))(tx => {
        assert(!chain.addTransaction(tx))
      })
    }
    finally {
      chain.close()
    }
  }

  //vote is not allowed when the vote target is not in witness list
  @Test
  def testVoteTargetNotExistInWitnesses():Unit = {
    try {
      val baseDir = "VoteContractTest/testVoteTargetNotExistInWitnesses"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeVoteTransaction(nonce = 2, candidate = _acct4.publicKey.pubKeyHash)(tx => {
        assert(!chain.addTransaction(tx))
      })
    }
    finally {
      chain.close()
    }
  }

  //voter is not allowed to cancel a vote when the voter is not in voter list
  @Test
  def testVoterNotExistInVotes():Unit = {
    try {
      val baseDir = "VoteContractTest/testVoterNotExistInVotes"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeVoteTransaction(operationType = OperationType.resisterCancel, nonce = 1, candidate = _acct3.publicKey.pubKeyHash,
        voter = _acct2.publicKey.pubKeyHash)(tx => {
        assert(!chain.addTransaction(tx))
      })
    }
    finally {
      chain.close()
    }
  }

  //vote is not allowed when a voter change a target from node a to node b
  @Test
  def testVoteTargetChanged():Unit = {
    try {
      val baseDir = "VoteContractTest/testVoteTargetChanged"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeVoteTransaction(nonce = 2)(checkVoteSuccess)
      When.makeRegisterTransaction(account = _acct4.publicKey.pubKeyHash, name = "node 2")(tx => {
        assert(chain.addTransaction(tx))
        val witness = chain.getWitness(_acct4.publicKey.pubKeyHash)
        assert(witness.isDefined)
        assert(witness.get.name == "node 2")
        assert(chain.getBalance(_acct4.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(1))
      })
      When.makeVoteTransaction(nonce = 3, candidate = _acct4.publicKey.pubKeyHash)(tx => {
        assert(!chain.addTransaction(tx))
      })
    }
    finally {
      chain.close()
    }
  }

  //cancel vote a target to witness but request counter bigger than its counter in db is not allowed
  @Test
  def testVoterCancelWitnessLargerThanItsCounter():Unit = {
    try {
      val baseDir = "VoteContractTest/testVoterCancelWitnessLargerThanItsCounter"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeVoteTransaction(nonce = 2, counter = FixedNumber.Ten)(tx => {
        assert(chain.addTransaction(tx))
        assert(chain.getVote(_acct1.publicKey.pubKeyHash).get.count == FixedNumber.Ten)
        assert(chain.getBalance(_acct1.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(110.12))
      })
      When.makeVoteTransaction(OperationType.resisterCancel,nonce = 3, counter = FixedNumber(FixedNumber.One.value * 20))(tx => {
        assert(!chain.addTransaction(tx))
      })
    }
    finally {
      chain.close()
    }
  }

  //vote a target to witness list and cancel it successfully
  @Test
  def testVoteAndCancelSuccess():Unit = {
    try {
      val baseDir = "VoteContractTest/testVoteAndCancelSuccess"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeVoteTransaction(nonce = 2, counter = FixedNumber.Ten)(tx => {
        assert(chain.addTransaction(tx))
        assert(chain.getVote(_acct1.publicKey.pubKeyHash).get.count == FixedNumber.Ten)
        assert(chain.getBalance(_acct1.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(110.12))
      })
      When.makeVoteTransaction(OperationType.resisterCancel,nonce = 3, counter = FixedNumber.One)(tx => {
        assert(chain.addTransaction(tx))
        assert(chain.getVote(_acct1.publicKey.pubKeyHash).get.count == FixedNumber(FixedNumber.One.value * 9))
      })
    }
    finally {
      chain.close()
    }
  }

  private def makeVoteTransaction(operationType: OperationType.Value = OperationType.register, nonce: Long = 0,
                                  counter: FixedNumber = FixedNumber.One, candidate: UInt160 = _acct3.publicKey.pubKeyHash,
                                  voter: UInt160 = _acct1.publicKey.pubKeyHash)(f: Transaction => Unit){
    val txData = VoteData(candidate, counter,operationType).toBytes
    val registerContractAddr = new UInt160(DataWord.of("0000000000000000000000000000000000000000000000000000000000000102").getLast20Bytes)
    val tx = new Transaction(TransactionType.Call, voter ,registerContractAddr, "", FixedNumber.Zero,
      nonce, txData, FixedNumber(0), 9000000L, BinaryData.empty)
    f(tx)
  }

  def checkVoteSuccess(tx: Transaction): Unit ={
    assert(chain.addTransaction(tx))
    val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
    assert(witness.isDefined)
    assert(witness.get.name == "register node1")
    assert(chain.getBalance(_acct3.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(2))
    val account = chain.getBalance(_acct1.publicKey.pubKeyHash).get
    assert(account == FixedNumber.fromDecimal(119.12))
    assert(chain.getVote(_acct1.publicKey.pubKeyHash).get.count == FixedNumber.One)
  }


  override def When = this
  override def Then = this
  override def And  =this
  override def Given= this


}

object VoteContractTest {
  @AfterClass
  def cleanUp: Unit = {
    println("clean Directory")
    Directory("VoteContractTest").deleteRecursively()
    Directory("RegisterContractTest").deleteRecursively()
  }
}
