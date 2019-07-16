package com.apex.test

import java.time.Instant

import com.apex.consensus.{ProducerUtil, RegisterData, WitnessInfo, WitnessVoteData}
import com.apex.core._
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, FixedNumber, MerkleTree, UInt160, UInt256}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.proposal.{ProposalData, ProposalType, ProposalVoteData}
import com.apex.settings.{ConsensusSettings, RuntimeParas, _}
import com.apex.solidity.Abi
import com.apex.vm.DataWord
import org.junit.{AfterClass, Test}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.io.Directory


@Test
class ProposalVoteTest {

  Directory("ProposalVoteTest").deleteRecursively()

  val _produceInterval = 1000

  val _minerAward: Double = 12.3

  val priv1 = new PrivateKey(BinaryData("108fd85be78e0dcc96b93c9c53c30281115ee453e9c472654fad985dcd7b91db"))
  val priv2 = new PrivateKey(BinaryData("ad28867b93d8be8558d61ee58e971117142aefd28ebb54bf4f20792bfb7fab25"))
  val priv3 = new PrivateKey(BinaryData("43427dd5def74e97ab0409eaec3d64aff0557707a6edc1004d1c6e08ea705b45"))
  val priv4 = new PrivateKey(BinaryData("adc2a556a1a1726ecce71eb38e306914af4d82f547a545eed677ba555409932f"))

  val _witness1 = InitWitness("init1", priv1.publicKey.address)
  val _witness2 = InitWitness("init2", priv2.publicKey.address)
  val _witness3 = InitWitness("init3", priv3.publicKey.address)
  val _witness4 = InitWitness("init4", priv4.publicKey.address)

  val _acct1 = Ecdsa.PrivateKey.fromWIF("KwmuSp41VWBtGSWaQQ82ZRRSFzkJVTAyuDLQ9NzP9CPqLWirh4UQ").get
  val _acct2 = Ecdsa.PrivateKey.fromWIF("L32JpLopG2hWjEMSCkAjS1nUnPixVrDTPqFAGYbddQrtUjRfkjEP").get
  val _acct3 = Ecdsa.PrivateKey.fromWIF("KyUTLv2BeP9SJD6Sa8aHBVmuRkgw9eThjNGJDE4PySEgf2TvCQCn").get
  val _acct4 = Ecdsa.PrivateKey.fromWIF("L33Uh9L35pSoEqBPP43U6rQcD2xMpJ7F4b3QMjUMAL6HZhxUqEGq").get

  val _miners = MinerSettings(Array(priv1, priv2, priv3, priv4, _acct1))

  val _consensusSettings  = ConsensusSettings(_produceInterval, 500, 1, 4, 4000, Array(_witness1, _witness2, _witness3, _witness4))
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

  private def makeBlock(chain: Blockchain,
                        preBlock: Block,
                        txs: Seq[Transaction],
                        award: Double = _minerAward): Block = {
    val blockTime = preBlock.timeStamp + _consensusSettings.produceInterval
    val miner = chain.getWitness(blockTime)

    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
      miner, FixedNumber.fromDecimal(award), preBlock.height + 1,
      BinaryData(Crypto.randomBytes(8)), // add random bytes to distinct different blocks with same block index during debug in some cases
      FixedNumber.MinValue, 0, BinaryData.empty)

    val allTxs = ArrayBuffer.empty[Transaction]

    allTxs.append(minerTx)
    txs.foreach(allTxs.append(_))

    val header: BlockHeader = BlockHeader.build(preBlock.header.index + 1,
      blockTime, MerkleTree.root(allTxs.map(_.id)),
      preBlock.id(), _miners.findPrivKey(miner).get)

    Block.build(header, allTxs)
  }

  private def makeBlockByTime(chain: Blockchain, preBlock: Block,
                              //txs: Seq[Transaction],
                              blockTime: Long): Block = {
    //val blockTime = preBlock.timeStamp + _consensusSettings.produceInterval
    val miner = chain.getWitness(blockTime)

    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
      miner, FixedNumber.fromDecimal(_minerAward), preBlock.height + 1,
      BinaryData(Crypto.randomBytes(8)), // add random bytes to distinct different blocks with same block index during debug in some cases
      FixedNumber.MinValue, 0, BinaryData.empty)

    val allTxs = ArrayBuffer.empty[Transaction]

    allTxs.append(minerTx)
    //txs.foreach(allTxs.append(_))

    val header: BlockHeader = BlockHeader.build(preBlock.header.index + 1,
      blockTime, MerkleTree.root(allTxs.map(_.id)),
      preBlock.id(), _miners.findPrivKey(miner).get)

    Block.build(header, allTxs)
  }

  private def startProduceBlock(chain: Blockchain, blockTime: Long, stopProcessTxTime: Long) = {

    val witness = chain.getWitness(blockTime)
    chain.startProduceBlock(_miners.findPrivKey(witness).get, blockTime, stopProcessTxTime)
  }

  private def createChain(path: String,
                          consensusSettings: ConsensusSettings = _consensusSettings): Blockchain = {
    val baseDir = s"ProposalVoteTest/$path"
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

  private def createProposalTransaction(//operationType: OperationType.Value = OperationType.register,
                              nonce: Long = 0,
                              from: PrivateKey,
                              proposalType: ProposalType.Value,
                              data: BinaryData): Transaction = {
    val txData = ProposalData(proposalType, 0, data).toBytes
    val contractAddr = new UInt160(DataWord.of("0000000000000000000000000000000000000000000000000000000000000103").getLast20Bytes)
    val tx = new Transaction(TransactionType.Call, from.publicKey.pubKeyHash, contractAddr,
      FixedNumber.Zero, nonce, txData, FixedNumber(1), 9000000L, BinaryData.empty)
    tx.sign(from)
    tx
  }

  private def createVoteTransaction(voter: PrivateKey,
                                    proposalID: UInt256,
                                    agree: Boolean,
//                                  operationType: OperationType.Value = OperationType.register,
//                                  counter: FixedNumber = FixedNumber.One,
                                  nonce: Long = 0): Transaction = {
    val txData = ProposalVoteData(proposalID, agree).toBytes
    val contractAddr = new UInt160(DataWord.of("0000000000000000000000000000000000000000000000000000000000000104").getLast20Bytes)
    val tx = new Transaction(TransactionType.Call, voter.publicKey.pubKeyHash, contractAddr, FixedNumber.Zero,
      nonce, txData, FixedNumber(1), 9000000L, BinaryData.empty)
    tx.sign(voter)
    tx
  }

  @Test
  def testBasicVote(): Unit = {
    val chain = createChain("testBasicVote")
    try {

      assert(chain.getHeight() == 0)
      assert(_produceInterval == 1000)

      val oneWeek = 7 * 24 * 3600 * 1000
      var nowTime = Instant.now.toEpochMilli - oneWeek - oneWeek - oneWeek
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      //sleepTo(blockTime)
      blockTime += _produceInterval
      startProduceBlock(chain, blockTime, Long.MaxValue)

      assert(chain.isProducingBlock())

      val block1 = chain.produceBlockFinalize().get
      assert(chain.head.id() == block1.id())

      val block2 = makeBlock(chain, block1, Seq.empty)
      assert(chain.tryInsertBlock(block2))

      println("block2 inserted")

      val block3 = makeBlock(chain, block2, Seq.empty)
      assert(chain.tryInsertBlock(block3))
//
//      val newAcct1Witness = chain.getWitness(_acct1.publicKey.pubKeyHash).get.copy(register = false)
//      chain.setWitness(newAcct1Witness)
//      chain.updateWitnessList(block1)

      println("block3 inserted")

      val block4 = makeBlock(chain, block3, Seq.empty)
      assert(chain.tryInsertBlock(block4))

      println("block4 inserted")


      val block5 = makeBlock(chain, block4, Seq.empty)
      assert(chain.tryInsertBlock(block5))
      println("block5 inserted")



      val block6 = makeBlock(chain, block5, Seq.empty)
      assert(chain.tryInsertBlock(block6))

      val block7 = makeBlock(chain, block6, Seq.empty)
      assert(chain.tryInsertBlock(block7))
      println("block7 inserted")

      val block8 = makeBlock(chain, block7, Seq.empty)
      assert(chain.tryInsertBlock(block8))

      println("block8 inserted")


      val block9 = makeBlock(chain, block8, Seq.empty)
      assert(chain.tryInsertBlock(block9))

      println("block9 inserted")

      val block10 = makeBlockByTime(chain, block9, block9.timeStamp() + oneWeek + 2000)
      assert(chain.tryInsertBlock(block10))

      println("block10 inserted")

      val proposalTx = createProposalTransaction(0, _acct1, ProposalType.BlockAward, FixedNumber.One.toBytes)

      val block11 = makeBlock(chain, block10, Seq(proposalTx))
      assert(chain.tryInsertBlock(block11))

      println("block11 inserted")

      val block12 = makeBlock(chain, block11, Seq(
        createVoteTransaction(_acct2, proposalTx.id, true, 0),
        createVoteTransaction(_acct3, proposalTx.id, true, 0),
        createVoteTransaction(_acct4, proposalTx.id, true, 0),
      ))
      assert(chain.tryInsertBlock(block12))

      println("block12 inserted")

      val block13 = makeBlockByTime(chain, block12, block12.timeStamp() + oneWeek + 2000)
      assert(chain.tryInsertBlock(block13))

      println("block13 inserted")

      val block14 = makeBlock(chain, block13, Seq.empty)
      assert(!chain.tryInsertBlock(block14))  // miner award not valid

      val block14ok = makeBlock(chain, block13, Seq.empty, 1)
      assert(chain.tryInsertBlock(block14ok))  // miner award valid

    }
    finally {
      chain.close()
    }
  }


  //  def tryInsertBlock(chain: Blockchain, block: Block): Boolean = {
  //    sleepTo(block.timeStamp)
  //    chain.tryInsertBlock(block, true)
  //  }

}

object ProposalVoteTest {
  @AfterClass
  def cleanUp: Unit = {
    println("clean Directory")
    Directory("ProposalVoteTest").deleteRecursively()
  }
}
