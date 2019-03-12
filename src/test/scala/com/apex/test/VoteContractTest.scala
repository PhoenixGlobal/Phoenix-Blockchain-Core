/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * @author: fang.wu@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */
package com.apex.test

import java.time.Instant

import com.apex.consensus.{ProducerUtil, VoteData}
import com.apex.core.{OperationType, Transaction, TransactionType}
import com.apex.crypto.{BinaryData, FixedNumber, UInt160}
import com.apex.vm.{DataWord, PrecompiledContracts}
import org.bouncycastle.math.ec.FixedPointUtil
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
  def testVoteRequestValid():Unit = {
    try {
      val baseDir = "VoteContractTest/testVoteRequestValid"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeVoteTransaction(nonce = 2, counter =FixedNumber.Zero)(tx => {
        assert(chain.addTransaction(tx))
      })
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
        assert(chain.addTransaction(tx))
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
        assert(chain.addTransaction(tx))
      })
    }
    finally {
      chain.close()
    }
  }

  //vote is not allowed when the vote target is not in witness list
  @Test
  def testCancelWitnessExistInVoterTargetMap():Unit = {
    try {
      val baseDir = "VoteContractTest/testCancelWitnessExistInVoterTargetMap"
      Given.createChain(baseDir){}
      When.produceBlock()
      Then.checkTx()
      And.checkAccount()
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeVoteTransaction(nonce = 2)(checkVoteSuccess)
      When.makeVoteTransaction(operationType = OperationType.resisterCancel, nonce = 3, candidate = _acct4.publicKey.pubKeyHash)(tx => {
        assert(chain.addTransaction(tx))
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
      When.makeVoteTransaction(operationType = OperationType.resisterCancel, nonce = 3, candidate = _acct3.publicKey.pubKeyHash,
        voter = _acct2.publicKey.pubKeyHash)(tx => {
        assert(chain.addTransaction(tx))
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
        assert(chain.getVote(_acct1.publicKey.pubKeyHash).get.targetMap(_acct3.publicKey.pubKeyHash) == FixedNumber.Ten)
        assert(chain.getBalance(_acct1.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(BigDecimal("110.11999999999993514")))
        assert(chain.getBalance(_acct1.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(110.12) - FixedNumber(42000)- FixedNumber(22860))
      })
      When.makeVoteTransaction(OperationType.resisterCancel,nonce = 3, counter = FixedNumber(FixedNumber.One.value * 20))(tx => {
        assert(chain.addTransaction(tx))
      })
    }
    finally {
      chain.close()
    }
  }

  //vote two target respectively success
  @Test
  def testVoteTwoTargetSuccess():Unit = {
    try {
      val baseDir = "VoteContractTest/testVoteTwoTargetSuccess"
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
        assert(chain.getBalance(_acct4.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(BigDecimal("2.999999999999975504")))
        val balance = chain.getBalance(_acct4.publicKey.pubKeyHash).get
        assert(chain.getBalance(_acct4.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(3) - FixedNumber(24496))
      })
      When.makeVoteTransaction(nonce = 3, candidate = _acct4.publicKey.pubKeyHash)(tx => {
        assert(chain.addTransaction(tx))
        assert(chain.getVote(_acct1.publicKey.pubKeyHash).get
          .targetMap.sameElements(scala.collection.mutable.Map(_acct3.publicKey.pubKeyHash -> FixedNumber.One,
          _acct4.publicKey.pubKeyHash -> FixedNumber.One)))
        assert(chain.getBalance(new UInt160(PrecompiledContracts.voteAddr.getLast20Bytes)).get == FixedNumber(FixedNumber.One.value * 2))
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
      var nowTime = Instant.now.toEpochMilli - 10000
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
      When.makeRegisterTransaction()(checkRegisterSuccess)
      When.makeVoteTransaction(nonce = 2, counter = FixedNumber.Ten)(tx => {
        assert(chain.addTransaction(tx))
        println(chain.getBalance(_acct1.publicKey.pubKeyHash).get)
        assert(chain.getVote(_acct1.publicKey.pubKeyHash).get.targetMap(_acct3.publicKey.pubKeyHash) == FixedNumber.Ten)
        assert(chain.getBalance(_acct1.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(BigDecimal("110.119999999999935140")))
        assert(chain.getBalance(_acct1.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(110.12) - FixedNumber(42000) - FixedNumber(22860))
        assert(chain.getBalance(new UInt160(PrecompiledContracts.voteAddr.getLast20Bytes)).get == FixedNumber.Ten)
      })
      When.makeVoteTransaction(OperationType.resisterCancel,nonce = 3, counter = FixedNumber.One)(tx => {
        assert(chain.addTransaction(tx))
        assert(chain.getVote(_acct1.publicKey.pubKeyHash).get.targetMap(_acct3.publicKey.pubKeyHash) == FixedNumber(FixedNumber.One.value * 9))
        assert(chain.getBalance(new UInt160(PrecompiledContracts.voteAddr.getLast20Bytes)).get == FixedNumber(FixedNumber.One.value * 10))
        assert(chain.getBalance(_acct1.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(BigDecimal("110.119999999999912220")))
        assert(chain.getBalance(_acct1.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(110.12)- FixedNumber(42000) - FixedNumber(22860) - FixedNumber(22920))
      })

      val block1 = chain.produceBlockFinalize()
      assert(block1.isDefined)
      assert(block1.get.transactions.size == 9)

      assert(!chain.isProducingBlock())
      assert(chain.getHeight() == 1)
      assert(chain.getHeadTime() == blockTime)
      assert(chain.head.id() == block1.get.id())

      blockTime += _produceInterval
      startProduceBlock(chain, blockTime, Long.MaxValue)
      assert(chain.isProducingBlock())

      sleepTo(blockTime)

      val block2 = chain.produceBlockFinalize()
      assert(block2.isDefined)
      assert(block2.get.transactions.size == 1)

      assert(!chain.isProducingBlock())
      assert(chain.getHeight() == 2)
      assert(chain.getHeadTime() == blockTime)
      assert(chain.head.id() == block2.get.id())

      blockTime += _produceInterval
      startProduceBlock(chain, blockTime, Long.MaxValue)
      assert(chain.isProducingBlock())

      sleepTo(blockTime)

      val block3 = chain.produceBlockFinalize()
      assert(block3.isDefined)
      assert(block3.get.transactions.size == 3)

      assert(!chain.isProducingBlock())
      assert(chain.getHeight() == 3)
      assert(chain.getHeadTime() == blockTime)
      assert(chain.head.id() == block3.get.id())

      assert(chain.getBalance(new UInt160(PrecompiledContracts.voteAddr.getLast20Bytes)).get == FixedNumber(FixedNumber.One.value * 9))
      assert(chain.getBalance(_acct1.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(BigDecimal("111.119999999999912220")))
      assert(chain.getBalance(_acct1.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(111.12) - FixedNumber(42000) - FixedNumber(22860) - FixedNumber(22920))
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
    val tx = new Transaction(TransactionType.Call, voter, registerContractAddr, FixedNumber.Zero,
      nonce, txData, FixedNumber(1), 9000000L, BinaryData.empty)
    f(tx)
  }

  def checkVoteSuccess(tx: Transaction): Unit ={
    assert(chain.addTransaction(tx))
    val witness = chain.getWitness(_acct3.publicKey.pubKeyHash)
    assert(witness.isDefined)
    assert(witness.get.name == "register node1")
    assert(chain.getBalance(_acct3.publicKey.pubKeyHash).get == FixedNumber.fromDecimal(BigDecimal("1.999999999999975088")))
    val account = chain.getBalance(_acct1.publicKey.pubKeyHash).get
    assert(account == FixedNumber.fromDecimal(BigDecimal("119.119999999999935144")))
    assert(chain.getBalance(_acct3.publicKey.pubKeyHash).get == (FixedNumber.fromDecimal(2) - FixedNumber(24912)))

    assert(account == (FixedNumber.fromDecimal(119.12) - FixedNumber(22856) - FixedNumber(42000)))
    assert(chain.getVote(_acct1.publicKey.pubKeyHash).get.targetMap.get(_acct3.publicKey.pubKeyHash).get == FixedNumber.One)
    assert(chain.getBalance(new UInt160(PrecompiledContracts.voteAddr.getLast20Bytes)).get == FixedNumber.One)
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
