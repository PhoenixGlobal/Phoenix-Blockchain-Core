package com.apex.test

import java.time.Instant

import com.apex.consensus.{ProducerUtil, RegisterData, WitnessVoteData, WitnessInfo}
import com.apex.core._
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, FixedNumber, MerkleTree, UInt160, UInt256}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.settings.{ConsensusSettings, RuntimeParas, _}
import com.apex.solidity.Abi
import com.apex.vm.DataWord
import org.junit.{AfterClass, Test}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.io.Directory

@Test
class BlockchainVoteTest2 {

  Directory("BlockchainVoteTest2").deleteRecursively()

  val _produceInterval = 500

  val _minerAward: Double = 12.3

  val _acct1 = Ecdsa.PrivateKey.fromWIF("L2FZjjTbqnkUJEVLHZdLR5SVB4UED6zqK5pCg6PSVLUJjZjxpusY").get  // !!!
  val _acct2 = Ecdsa.PrivateKey.fromWIF("L32JpLopG2hWjEMSCkAjS1nUnPixVrDTPqFAGYbddQrtUjRfkjEP").get
  val _acct3 = Ecdsa.PrivateKey.fromWIF("KyUTLv2BeP9SJD6Sa8aHBVmuRkgw9eThjNGJDE4PySEgf2TvCQCn").get
  val _acct4 = Ecdsa.PrivateKey.fromWIF("L33Uh9L35pSoEqBPP43U6rQcD2xMpJ7F4b3QMjUMAL6HZhxUqEGq").get
  val _acct5 = Ecdsa.PrivateKey.fromWIF("KxisR46MUfkekvgfuuydTD91avsjxhoqs5S6Ech2uiG21RDUEbna").get

  val w1 = new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))
  val w2 = new PrivateKey(BinaryData("cc7b7fa6e706944fa2d75652065f95ef2f364316e172601320655aac0e648165"))
  val w3 = new PrivateKey(BinaryData("db71fe7c0ac4ca3e8cef95bf55cf535eaa8fe0c80d18e0cb19af8d7071b8a184"))
  val w4 = new PrivateKey(BinaryData("9456beec947b368eda4be03f6c306703d9b2eda49f661285944b4e1f07ae18f3"))

  val _witness1 = InitWitness("init1", w1.publicKey.address)
  val _witness2 = InitWitness("init2", w2.publicKey.address)
  val _witness3 = InitWitness("init3", w3.publicKey.address)
  val _witness4 = InitWitness("init4", w4.publicKey.address)

//  val _miners = MinerSettings(Array(
//    new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471")),
//    new PrivateKey(BinaryData("cc7b7fa6e706944fa2d75652065f95ef2f364316e172601320655aac0e648165")),
//    new PrivateKey(BinaryData("db71fe7c0ac4ca3e8cef95bf55cf535eaa8fe0c80d18e0cb19af8d7071b8a184")),
//    new PrivateKey(BinaryData("9456beec947b368eda4be03f6c306703d9b2eda49f661285944b4e1f07ae18f3")),
//    _acct1))

  val _miners3 = MinerSettings(Array(w1, w2, w3))

  val _consensusSettings  = ConsensusSettings(_produceInterval, 500, 1, 4, 2000, Array(_witness1, _witness2, _witness3, _witness4))
  //val _consensusSettings2 = ConsensusSettings(_produceInterval, 500, 3, 4, 63000, Array(_witness1, _witness2, _witness3, _witness4))

  val _runtimeParas = RuntimeParas(100)

  private val minerCoinFrom = UInt160.Zero

  private def makeTx(from: PrivateKey,
                     to: PrivateKey,
                     amount: FixedNumber,
                     nonce: Long,
                     gasLimit: Long = 21000,
                     gasPrice: FixedNumber = FixedNumber.MinValue,
                     txType: TransactionType.Value = TransactionType.Transfer): Transaction = {

    val tx = new Transaction(txType, from.publicKey.pubKeyHash, to.publicKey.pubKeyHash,
      amount, nonce, BinaryData.empty, gasPrice, gasLimit, BinaryData.empty)
    tx.sign(from)
    tx
  }

//  private def makeBlock(chain: Blockchain,
//                        preBlock: Block,
//                        txs: Seq[Transaction],
//                        award: Double = _minerAward): Block = {
//    val blockTime = preBlock.header.timeStamp + _consensusSettings.produceInterval
//    val miner = chain.getWitness(blockTime)
//
//    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
//      miner, FixedNumber.fromDecimal(award), preBlock.height + 1,
//      BinaryData(Crypto.randomBytes(8)), // add random bytes to distinct different blocks with same block index during debug in some cases
//      FixedNumber.Zero, 0, BinaryData.empty)
//
//    val allTxs = ArrayBuffer.empty[Transaction]
//
//    allTxs.append(minerTx)
//    txs.foreach(allTxs.append(_))
//
//    val header: BlockHeader = BlockHeader.build(preBlock.header.index + 1,
//      blockTime, MerkleTree.root(allTxs.map(_.id)),
//      preBlock.id(), _miners.findPrivKey(miner).get)
//
//    Block.build(header, allTxs)
//  }

  private def makeBlock(chain: Blockchain,
                        preBlock: Block,
                        txs: Seq[Transaction],
                        award: Double = _minerAward): Block = {

    var produced = false
    var nextBlockTime: Long = preBlock.header.timeStamp + _produceInterval

    while (!produced) {
      val witness = chain.getWitness(nextBlockTime)
      if (_miners3.findPrivKey(witness).isDefined) {
        chain.startProduceBlock(_miners3.findPrivKey(witness).get, nextBlockTime, Long.MaxValue)
        txs.foreach(tx => {chain.addTransaction(tx)})
        chain.produceBlockFinalize()
        produced = true
      }
      else
        nextBlockTime += _produceInterval
    }
    chain.head
  }

//  private def insertBlocks(chain: Blockchain,
//                           preBlock: Block,
//                           blockCount: Int): Block = {
//
//  }

//  private def makeBlockByTime(chain: Blockchain, preBlock: Block,
//                              //txs: Seq[Transaction],
//                              blockTime: Long): Block = {
//    //val blockTime = preBlock.header.timeStamp + _consensusSettings.produceInterval
//    val miner = chain.getWitness(blockTime)
//
//    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
//      miner, FixedNumber.fromDecimal(_minerAward), preBlock.height + 1,
//      BinaryData(Crypto.randomBytes(8)), // add random bytes to distinct different blocks with same block index during debug in some cases
//      FixedNumber.Zero, 0, BinaryData.empty)
//
//    val allTxs = ArrayBuffer.empty[Transaction]
//
//    allTxs.append(minerTx)
//    //txs.foreach(allTxs.append(_))
//
//    val header: BlockHeader = BlockHeader.build(preBlock.header.index + 1,
//      blockTime, MerkleTree.root(allTxs.map(_.id)),
//      preBlock.id(), _miners.findPrivKey(miner).get)
//
//    Block.build(header, allTxs)
//  }

//  private def startProduceBlock(chain: Blockchain, blockTime: Long, stopProcessTxTime: Long) = {
//
//    val witness = chain.getWitness(blockTime)
//    chain.startProduceBlock(_miners.findPrivKey(witness).get, blockTime, stopProcessTxTime)
//  }

  private def createChain(path: String,
                          consensusSettings: ConsensusSettings = _consensusSettings): Blockchain = {
    val baseDir = s"BlockchainVoteTest2/$path"
    val chainSetting = ChainSettings(
      BlockBaseSettings(s"$baseDir/block", false, 0, DBType.LevelDB),
      DataBaseSettings(s"$baseDir/data", false, 0, DBType.LevelDB),
      ForkBaseSettings(s"$baseDir/fork", false, 0, DBType.LevelDB),
      _minerAward,
      GenesisSettings(Instant.EPOCH,
        "7a93d447bffe6d89e690f529a3a0bdff8ff6169172458e04849ef1d4eafd7f86",
        Array(CoinAirdrop(_acct1.publicKey.address, 200),
          CoinAirdrop(_acct2.publicKey.address, 300))
      )
    )

    new Blockchain(chainSetting, consensusSettings, _runtimeParas, Notification())
  }

  //  private def sleepTo(time: Long) = {
  //    val nowTime = Instant.now.toEpochMilli
  //    if (time > nowTime)
  //      Thread.sleep(time - nowTime)
  //  }

  private def createRegisterTransaction(//operationType: OperationType.Value = OperationType.register,
                              nonce: Long = 0,
                              from: PrivateKey,
                              name: String = "register node1",
                              isGenesisWitness: Boolean = false): Transaction = {
    val txData = RegisterData(
                    from.publicKey.pubKeyHash,
                    WitnessInfo(from.publicKey.pubKeyHash, isGenesisWitness, name),
                    OperationType.register).toBytes
    val registerContractAddr = new UInt160(DataWord.of("0000000000000000000000000000000000000000000000000000000000000101").getLast20Bytes)
    val tx = new Transaction(TransactionType.Call, from.publicKey.pubKeyHash, registerContractAddr,
      FixedNumber.Zero, nonce, txData, FixedNumber(1), 9000000L, BinaryData.empty)
    tx.sign(from)
    tx
  }

  private def createVoteTransaction(voter: PrivateKey,
                                  candidate: PrivateKey,
                                  operationType: OperationType.Value = OperationType.register,
                                  counter: FixedNumber = FixedNumber.One,
                                  nonce: Long = 0): Transaction = {
    val txData = WitnessVoteData(candidate.publicKey.pubKeyHash, counter, operationType).toBytes
    val registerContractAddr = new UInt160(DataWord.of("0000000000000000000000000000000000000000000000000000000000000102").getLast20Bytes)
    val tx = new Transaction(TransactionType.Call, voter.publicKey.pubKeyHash, registerContractAddr, FixedNumber.Zero,
      nonce, txData, FixedNumber(1), 9000000L, BinaryData.empty)
    tx.sign(voter)
    tx
  }

  @Test
  def testBasicRegVote(): Unit = {
    val chain = createChain("testBasicRegVote")
    try {

      assert(chain.getHeight() == 0)

      //makeBlock(chain, chain.head, Seq.empty)

      val block1 = makeBlock(chain, chain.head, Seq.empty)
      assert(chain.head.id() == block1.id())

      assert(chain.getProducers("all").witnesses.size == 4)
      assert(!chain.getProducers("all").contains(_acct2.publicKey.pubKeyHash))

      val block2 = makeBlock(chain, block1, Seq(createRegisterTransaction(0, _acct2)))

      assert(chain.getHeight() == 2)

      println("block2 inserted")

      assert(chain.getProducers("all").witnesses.size == 5)

      assert(chain.getProducers("all").contains(_acct2.publicKey.pubKeyHash))

      val block3 = makeBlock(chain, block2, Seq.empty)

      println("block3 inserted")

      val block4 = makeBlock(chain, block3, Seq.empty)

      assert(chain.getHeight() == 4)

      println("block4 inserted")
      assert(chain.getProducers("pending").contains(_acct2.publicKey.pubKeyHash))

      val block5 = makeBlock(chain, block4, Seq.empty)
      assert(chain.getHeight() == 5)
      println("block5 inserted")

      assert(chain.getProducers("pending").contains(_acct2.publicKey.pubKeyHash))

      val block6 = makeBlock(chain, block5, Seq.empty)

      assert(block6.height() == 6)
      assert(chain.getHeight() == 6)
      println("block6 inserted")

      val block7 = makeBlock(chain, block6, Seq.empty)
      assert(block7.height() == 7)
      assert(chain.getHeight() == 7)
      println("block7 inserted")

      println("_acct2 is " + _acct2.publicKey.pubKeyHash.address)
      assert(chain.getProducers("active").contains(_acct2.publicKey.pubKeyHash))

      val block8 = makeBlock(chain, block7, Seq.empty)
      assert(block8.height() == 8)
      assert(chain.getHeight() == 8)
      println("block8 inserted")

    }
    finally {
      chain.close()
    }
  }


}

object BlockchainVoteTest2 {
  @AfterClass
  def cleanUp: Unit = {
    println("clean Directory")
    Directory("BlockchainVoteTest2").deleteRecursively()
  }
}
