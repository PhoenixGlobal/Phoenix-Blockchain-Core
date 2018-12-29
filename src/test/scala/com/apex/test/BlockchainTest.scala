package com.apex.test

import java.time.Instant

import com.apex.consensus.ProducerUtil
import com.apex.core._
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, FixedNumber, MerkleTree, UInt160, UInt256}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.settings._
import com.apex.solidity.Abi
import com.apex.vm.DataWord
import org.junit.{AfterClass, Test}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.io.Directory

//======
//1
//priv key raw:           108fd85be78e0dcc96b93c9c53c30281115ee453e9c472654fad985dcd7b91db
//priv key WIF format:    KwmuSp41VWBtGSWaQQ82ZRRSFzkJVTAyuDLQ9NzP9CPqLWirh4UQ
//pub key (compressed):   024f3076ac9b2b8ef8bf1c303c3ac3057472c9fa391df62c543789f34017168c62
//pub key hash160:        c98bae4cc033c7e7bce053ebaa926bd61c120454
//Address:                APLLBNZjMq4tFANSNHsEoXwGJ7NVfi4BxUa
//======
//2
//priv key raw:           ad28867b93d8be8558d61ee58e971117142aefd28ebb54bf4f20792bfb7fab25
//priv key WIF format:    L32JpLopG2hWjEMSCkAjS1nUnPixVrDTPqFAGYbddQrtUjRfkjEP
//pub key (compressed):   02add2d02786ba350148aee109e67495d6c4c2dccd9b5aaa57ad866d7b6105ac8f
//pub key hash160:        2ee607c3ed304353dd8a2d16636b46bd91d11152
//Address:                AP6EUsRV9DCvbywpDFLXS3JhjRryFPe9Qo8
//======
//3
//priv key raw:           43427dd5def74e97ab0409eaec3d64aff0557707a6edc1004d1c6e08ea705b45
//priv key WIF format:    KyUTLv2BeP9SJD6Sa8aHBVmuRkgw9eThjNGJDE4PySEgf2TvCQCn
//pub key (compressed):   03d7a23792639a9824d1013151711b521653f0c72563d69481bd914db75b03309d
//pub key hash160:        00d08ce14267fbce154d2983b8723dd5ad2addd5
//Address:                AP22p497htDs1SKXHjwyCgrYuFtTgcP5F5w
//======
//4
//priv key raw:           adc2a556a1a1726ecce71eb38e306914af4d82f547a545eed677ba555409932f
//priv key WIF format:    L33Uh9L35pSoEqBPP43U6rQcD2xMpJ7F4b3QMjUMAL6HZhxUqEGq
//pub key (compressed):   03f5bb4aa2a0773d658e9df51865ffbfd41f98891bd1994337afae4dc27c1d1927
//pub key hash160:        da55e4f6f216187259e7bc56cbb1587edc5156b4
//Address:                APMrxRgE3ZBqw9nC249175AvjUcmYMCq7XX
//======
//5
//priv key raw:           2cd68534541d6babbc8edb7a15036f8cee5f3506d667aecc022ffa1ea1b1b269
//priv key WIF format:    KxisR46MUfkekvgfuuydTD91avsjxhoqs5S6Ech2uiG21RDUEbna
//pub key (compressed):   03300f032f1d130956f0ff994cbd52cab3fc4167c0f6e96f1b5714da10ed51c1bd
//pub key hash160:        63d15a28a09748540eca07aeabf3b831b3056ebe
//Address:                APB4Hur58nvWWHuSE5qNQFWYPjZaKZetFAe
//======
//6
//priv key raw:           0c165d5428409eef6e025d6fe827b3b0f595f5a1cc4051c9ff43a7f1e20fed56
//priv key WIF format:    KwdCy3jFbU5MK4ZMbgKLTDkenhQgCU44hevVVXAv7ZUZRYJypqCB
//pub key (compressed):   034a79dcbd7b6da30739516f00e0be2e8c511a9ee446cc7362b8a441504ec0e7bf
//pub key hash160:        3a01649c803b08d4a3522c3ca49dedf687d576f9
//Address:                AP7FD5j2aPj83dmb3xSJZVVJ1JwNLry74xa
//======
//7
//priv key raw:           29e286f51e12df09f46b451d606650a0cce4b0d6d42a5d2e5450ab7c8b58a2c3
//priv key WIF format:    Kxd8UDvptjG4AcT7k2ULe8tVkmfK5naXKc3gzrvRtg6msGLA3xLY
//pub key (compressed):   03a549ab71a681d09bd090ed67bad589e55b582a2bbbdf3fb7d68ad3a71bfee211
//pub key hash160:        3252379053bf9f8827896cf0a5ff2cbd6fdca06f
//Address:                AP6YaVweecsk2sLRpSdUEwiQ2yE9WvitBtr
//======

@Test
class BlockchainTest {

  Directory("BlockchainTest").deleteRecursively()

  val _produceInterval = 2000

  val _minerAward: Double = 12.3

  val _witness1 = Witness("init1",
    PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8"),
    Some(new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))))
  val _witness2 = Witness("init2",
    PublicKey("03c3333373adc0636b1d67d4bca3d8b34a53d663698119369981e67866250d3a74"),
    Some(new PrivateKey(BinaryData("cc7b7fa6e706944fa2d75652065f95ef2f364316e172601320655aac0e648165"))))
  val _witness3 = Witness("init3",
    PublicKey("020550de6ce7ed53ff018cccf1095893edba43f798252d6983e0fd2ca5af3ee0da"),
    Some(new PrivateKey(BinaryData("db71fe7c0ac4ca3e8cef95bf55cf535eaa8fe0c80d18e0cb19af8d7071b8a184"))))
  val _witness4 = Witness("init4",  // APPnx5YahVg1dTgeWkp1fE33ftvAaGbeQaR  L2C4Za8VSx2iBgszQarHx4YzqHvfumkHjbi6bNqvqst6mc8QcuZ7
    PublicKey("0246f896de22582786884d7d7ae27ef00cc8fed167bcdb8c305fbbc3dd9cca696c"),
    Some(new PrivateKey(BinaryData("9456beec947b368eda4be03f6c306703d9b2eda49f661285944b4e1f07ae18f3"))))

  val _consensusSettings = ConsensusSettings(_produceInterval, 500, 1, Array(_witness1, _witness2, _witness3, _witness4))

  val _acct1 = Ecdsa.PrivateKey.fromWIF("KwmuSp41VWBtGSWaQQ82ZRRSFzkJVTAyuDLQ9NzP9CPqLWirh4UQ").get
  val _acct2 = Ecdsa.PrivateKey.fromWIF("L32JpLopG2hWjEMSCkAjS1nUnPixVrDTPqFAGYbddQrtUjRfkjEP").get
  val _acct3 = Ecdsa.PrivateKey.fromWIF("KyUTLv2BeP9SJD6Sa8aHBVmuRkgw9eThjNGJDE4PySEgf2TvCQCn").get
  val _acct4 = Ecdsa.PrivateKey.fromWIF("L33Uh9L35pSoEqBPP43U6rQcD2xMpJ7F4b3QMjUMAL6HZhxUqEGq").get

  private val minerCoinFrom = PublicKey(BinaryData("02866facba8742cd702b302021a9588e78b3cd96599a3b1c85688d6dc0a72585e6"))

  private def makeTx(from: PrivateKey,
                     to: UInt160,
                     amount: FixedNumber,
                     nonce: Long,
                     txType: TransactionType.Value = TransactionType.Transfer) = {

    val tx = new Transaction(txType, from.publicKey, to, "",
      amount, UInt256.Zero, nonce, BinaryData.empty, FixedNumber.Zero, 0, BinaryData.empty)
    tx.sign(from)
    tx
  }

  private def makeBlock(preBlock: Block,
                        txs: Seq[Transaction],
                        award: Double = _minerAward): Block = {
    val blockTime = preBlock.header.timeStamp + _consensusSettings.produceInterval
    val miner = ProducerUtil.getWitness(blockTime, _consensusSettings)

    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
      miner.pubkey.pubKeyHash, "", FixedNumber.fromDecimal(award), UInt256.Zero,
      preBlock.height + 1,
      BinaryData(Crypto.randomBytes(8)), // add random bytes to distinct different blocks with same block index during debug in some cases
      FixedNumber.Zero, 0, BinaryData.empty)

    val allTxs = ArrayBuffer.empty[Transaction]

    allTxs.append(minerTx)
    txs.foreach(allTxs.append(_))

    val header: BlockHeader = BlockHeader.build(preBlock.header.index + 1,
      blockTime, MerkleTree.root(allTxs.map(_.id)),
      preBlock.id(), miner.pubkey, miner.privkey.get)

    Block.build(header, allTxs)
  }

  private def makeBlockByTime(preBlock: Block,
                        //txs: Seq[Transaction],
                         blockTime: Long): Block = {
    //val blockTime = preBlock.header.timeStamp + _consensusSettings.produceInterval
    val miner = ProducerUtil.getWitness(blockTime, _consensusSettings)

    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
      miner.pubkey.pubKeyHash, "", FixedNumber.fromDecimal(_minerAward), UInt256.Zero,
      preBlock.height + 1,
      BinaryData(Crypto.randomBytes(8)), // add random bytes to distinct different blocks with same block index during debug in some cases
      FixedNumber.Zero, 0, BinaryData.empty)

    val allTxs = ArrayBuffer.empty[Transaction]

    allTxs.append(minerTx)
    //txs.foreach(allTxs.append(_))

    val header: BlockHeader = BlockHeader.build(preBlock.header.index + 1,
      blockTime, MerkleTree.root(allTxs.map(_.id)),
      preBlock.id(), miner.pubkey, miner.privkey.get)

    Block.build(header, allTxs)
  }

  private def createChain(path: String): LevelDBBlockchain = {
    val baseDir = s"BlockchainTest/$path"
    val chainSetting = ChainSettings(
      BlockBaseSettings(s"$baseDir/block", false, 0),
      DataBaseSettings(s"$baseDir/data", false, 0),
      ForkBaseSettings(s"$baseDir/fork", false, 0),
      _minerAward,
      GenesisSettings(Instant.EPOCH,
        "03b4534b44d1da47e4b4a504a210401a583f860468dec766f507251a057594e682",
        "7a93d447bffe6d89e690f529a3a0bdff8ff6169172458e04849ef1d4eafd7f86",
        Array(CoinAirdrop(_acct1.publicKey.address, 123.12),
          CoinAirdrop(_acct2.publicKey.address, 234.2))
      )
    )

    Blockchain.populate(chainSetting, _consensusSettings, Notification())
  }

  @Test
  def testCreateChain(): Unit = {
    val chain = createChain("testCreateChain")
    try {
      //      val header = BlockHeader.build(
      //        1, 0, UInt256.Zero,
      //        UInt256.Zero, PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8"),
      //        new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471")))
      //      val block = Block.build(header, Seq.empty)
      //      assert(chain.tryInsertBlock(block, true) == false)

      assert(chain.getHeight() == 0)

      val balance1 = chain.getBalance(_acct1.publicKey.pubKeyHash)
      assert(balance1.get.get(UInt256.Zero).get == FixedNumber.fromDecimal(123.12))

      val balance2 = chain.getBalance(_acct2.publicKey.pubKeyHash)
      assert(balance2.get.get(UInt256.Zero).get == FixedNumber.fromDecimal(234.2))

      var blockTime = chain.getHeadTime() + _consensusSettings.produceInterval
      chain.startProduceBlock(ProducerUtil.getWitness(blockTime, _consensusSettings), blockTime)

      assert(chain.isProducingBlock())

      // not enough coin
      assert(!chain.addTransaction(makeTx(_acct1, UInt160.Zero, FixedNumber.fromDecimal(123.13), 0)))
      // not enough coin
      assert(!chain.addTransaction(makeTx(_acct3, UInt160.Zero, FixedNumber.fromDecimal(1), 0)))
      // wrong nonce
      assert(!chain.addTransaction(makeTx(_acct1, UInt160.Zero, FixedNumber.fromDecimal(123), 1)))
      assert(chain.addTransaction(makeTx(_acct1, UInt160.Zero, FixedNumber.fromDecimal(1), 0)))
      assert(!chain.addTransaction(makeTx(_acct1, UInt160.Zero, FixedNumber.fromDecimal(2), 0)))
      assert(chain.addTransaction(makeTx(_acct1, UInt160.Zero, FixedNumber.fromDecimal(2), 1)))
      assert(chain.addTransaction(makeTx(_acct1, _acct3.publicKey.pubKeyHash, FixedNumber.fromDecimal(100), 2)))
      assert(!chain.addTransaction(makeTx(_acct1, UInt160.Zero, FixedNumber.fromDecimal(20.121), 3)))
      assert(chain.addTransaction(makeTx(_acct1, UInt160.Zero, FixedNumber.fromDecimal(20.02), 3)))

      assert(!chain.addTransaction(makeTx(_acct3, UInt160.Zero, FixedNumber.fromDecimal(100.1), 0)))
      assert(chain.addTransaction(makeTx(_acct3, UInt160.Zero, FixedNumber.fromDecimal(80), 0)))

      val block1 = chain.produceBlockFinalize()
      assert(block1.isDefined)
      assert(block1.get.transactions.size == 6)
      assert(!chain.isProducingBlock())
      assert(chain.getHeight() == 1)
      assert(chain.getHeadTime() == blockTime)
      assert(chain.getBalance(_acct3.publicKey.pubKeyHash).get.get(UInt256.Zero).get == FixedNumber.fromDecimal(20))
      assert(chain.getBalance(_acct1.publicKey.pubKeyHash).get.get(UInt256.Zero).get == FixedNumber.fromDecimal(0.1))

      val block2 = makeBlock(block1.get, Seq(makeTx(_acct3, _acct4.publicKey.pubKeyHash, FixedNumber.fromDecimal(11), 1)))
      assert(chain.tryInsertBlock(block2, true))

      assert(chain.getBalance(_acct4.publicKey.pubKeyHash).get.get(UInt256.Zero).get == FixedNumber.fromDecimal(11))

      assert(chain.getBalance(_acct3.publicKey.pubKeyHash).get.get(UInt256.Zero).get == FixedNumber.fromDecimal(9))

      assert(!chain.tryInsertBlock(makeBlock(block2, Seq.empty[Transaction], _minerAward + 0.1), true))

      val block22 = makeBlock(block1.get, Seq.empty[Transaction])
      assert(chain.tryInsertBlock(block22, true))

      assert(chain.head.id() == block2.id())
      assert(chain.getLatestHeader().id() == block2.id())

      val block33 = makeBlock(block22, Seq.empty[Transaction])
      assert(chain.tryInsertBlock(block33, true))

      assert(chain.head.id() == block33.id())
      assert(chain.getLatestHeader().id() == block33.id())

      assert(chain.getBalance(_acct3.publicKey.pubKeyHash).get.get(UInt256.Zero).get == FixedNumber.fromDecimal(20))
      assert(chain.getBalance(_acct4.publicKey.pubKeyHash).isEmpty)

    }
    finally {
      chain.close()
    }
  }


  @Test
  def testChainFork1(): Unit = {
    val chain = createChain("testChainFork1")
    try {
      //      val header = BlockHeader.build(
      //        1, 0, UInt256.Zero,
      //        UInt256.Zero, PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8"),
      //        new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471")))
      //      val block = Block.build(header, Seq.empty)
      //      assert(chain.tryInsertBlock(block, true) == false)

      assert(chain.getHeight() == 0)

      var blockTime = chain.getHeadTime() + _consensusSettings.produceInterval
      chain.startProduceBlock(ProducerUtil.getWitness(blockTime, _consensusSettings), blockTime)

      assert(chain.isProducingBlock())


      val block1 = chain.produceBlockFinalize()
      assert(block1.isDefined)

      val time1 = block1.get.header.timeStamp     // A
      val time2 = time1 + _consensusSettings.produceInterval  // B
      val time3 = time2 + _consensusSettings.produceInterval  // C
      val time4 = time3 + _consensusSettings.produceInterval  // D

      val time5 = time4 + _consensusSettings.produceInterval   // a
      val time6 = time5 + _consensusSettings.produceInterval   // b
      val time7 = time6 + _consensusSettings.produceInterval   // c
      val time8 = time7 + _consensusSettings.produceInterval   // d

      val time9 = time8 + _consensusSettings.produceInterval   // a
      val time10 = time9 + _consensusSettings.produceInterval  // b
      val time11 = time10 + _consensusSettings.produceInterval  // c
      val time12 = time11 + _consensusSettings.produceInterval

      val time13 = time12 + _consensusSettings.produceInterval
      val time14 = time13 + _consensusSettings.produceInterval
      val time15 = time14 + _consensusSettings.produceInterval
      val time16 = time15 + _consensusSettings.produceInterval


      assert(!chain.isProducingBlock())

      val block2 = makeBlockByTime(block1.get, time2)
      assert(chain.tryInsertBlock(block2, true))   // b

      val block3 = makeBlockByTime(block2, time3)
      assert(chain.tryInsertBlock(block3, true))   // c

      val block4 = makeBlockByTime(block3, time5)
      assert(chain.tryInsertBlock(block4, true))   // a

      assert(chain.getConfirmedHeader().get.id() == block1.get.id())

      val block5 = makeBlockByTime(block4, time6)
      assert(chain.tryInsertBlock(block5, true))   // b

      assert(chain.getConfirmedHeader().get.id() == block2.id())
      assert(chain.getHeight() == 5)
      assert(chain.getLatestHeader().id() == block5.id())

      chain.startProduceBlock(ProducerUtil.getWitness(time9, _consensusSettings), time9)  // a
      assert(chain.isProducingBlock())
      val block999 = chain.produceBlockFinalize()
      assert(block999.isDefined)

      assert(chain.getHeight() == 6)
      assert(chain.getLatestHeader().id() == block999.get.id())

      val block6 = makeBlockByTime(block5, time7)                  // c
      assert(chain.tryInsertBlock(block6, true))

      assert(chain.getHeight() == 6)
      assert(chain.getLatestHeader().id() == block6.id())

      val block7 = makeBlockByTime(block6, time10)                  // b
      assert(chain.tryInsertBlock(block7, true))

      assert(chain.getLatestHeader().id() == block7.id())
      assert(chain.getConfirmedHeader().get.id() == block3.id())

      val block8 = makeBlockByTime(block7, time11)                  // c
      assert(chain.tryInsertBlock(block8, true))

      assert(chain.getHeight() == 8)
      assert(chain.getConfirmedHeader().get.id() == block3.id())

      val block9 = makeBlockByTime(block8, time13)                  // a
      assert(chain.tryInsertBlock(block9, true))

      val block10 = makeBlockByTime(block9, time14)                  // b
      assert(chain.tryInsertBlock(block10, true))

      assert(chain.getConfirmedHeader().get.id() == block7.id())

      val block11 = makeBlockByTime(block10, time15)                  // c
      assert(chain.tryInsertBlock(block11, true))

      assert(chain.getConfirmedHeader().get.id() == block8.id())

      assert(chain.getHeight() == 11)

    }
    finally {
      chain.close()
    }
  }


  @Test
  def testChainFork2(): Unit = {
    val chain = createChain("testChainFork2")
    try {
      //      val header = BlockHeader.build(
      //        1, 0, UInt256.Zero,
      //        UInt256.Zero, PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8"),
      //        new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471")))
      //      val block = Block.build(header, Seq.empty)
      //      assert(chain.tryInsertBlock(block, true) == false)

      assert(chain.getHeight() == 0)

      var blockTime = chain.getHeadTime() + _consensusSettings.produceInterval
      chain.startProduceBlock(ProducerUtil.getWitness(blockTime, _consensusSettings), blockTime)

      assert(chain.isProducingBlock())


      val block1 = chain.produceBlockFinalize()
      assert(block1.isDefined)

      val time1 = block1.get.header.timeStamp     // A
      val time2 = time1 + _consensusSettings.produceInterval  // B
      val time3 = time2 + _consensusSettings.produceInterval  // C
      val time4 = time3 + _consensusSettings.produceInterval  // D

      val time5 = time4 + _consensusSettings.produceInterval   // a
      val time6 = time5 + _consensusSettings.produceInterval   // b
      val time7 = time6 + _consensusSettings.produceInterval   // c
      val time8 = time7 + _consensusSettings.produceInterval   // d

      val time9 = time8 + _consensusSettings.produceInterval   // a
      val time10 = time9 + _consensusSettings.produceInterval  // b
      val time11 = time10 + _consensusSettings.produceInterval  // c
      val time12 = time11 + _consensusSettings.produceInterval

      val time13 = time12 + _consensusSettings.produceInterval
      val time14 = time13 + _consensusSettings.produceInterval
      val time15 = time14 + _consensusSettings.produceInterval
      val time16 = time15 + _consensusSettings.produceInterval


      assert(!chain.isProducingBlock())

      val block2 = makeBlockByTime(block1.get, time2)
      assert(chain.tryInsertBlock(block2, true))   // b

      val block3 = makeBlockByTime(block2, time3)
      assert(chain.tryInsertBlock(block3, true))   // c

      val block9999 = makeBlockByTime(block3, time5)
      assert(chain.tryInsertBlock(block9999, true))   // a

      assert(chain.getConfirmedHeader().get.id() == block1.get.id())
      assert(chain.getHeight() == 4)
      assert(chain.getLatestHeader().id() == block9999.id())

      val block4 = makeBlockByTime(block3, time4)
      assert(chain.tryInsertBlock(block4, true))   // d

      assert(chain.getConfirmedHeader().get.id() == block1.get.id())
      assert(chain.getHeight() == 4)
      assert(chain.getLatestHeader().id() == block9999.id())

      val block5 = makeBlockByTime(block4, time7)
      assert(chain.tryInsertBlock(block5, true))   // c

      assert(chain.getConfirmedHeader().get.id() == block1.get.id())
      assert(chain.getHeight() == 5)
      assert(chain.getLatestHeader().id() == block5.id())


      val block6 = makeBlockByTime(block5, time8)
      assert(chain.tryInsertBlock(block6, true))   // d

      assert(chain.getConfirmedHeader().get.id() == block1.get.id())
      assert(chain.getHeight() == 6)
      assert(chain.getLatestHeader().id() == block6.id())

      val block7 = makeBlockByTime(block6, time9)
      assert(chain.tryInsertBlock(block7, true))   // a

      assert(chain.getConfirmedHeader().get.id() == block4.id())
      assert(chain.getHeight() == 7)
      assert(chain.getLatestHeader().id() == block7.id())
    }
    finally {
      chain.close()
    }
  }


}

object BlockchainTest {
  @AfterClass
  def cleanUp: Unit = {
    println("clean Directory")
    Directory("BlockchainTest").deleteRecursively()
  }
}
