package com.apex.test

import java.time.Instant

import com.apex.core._
import com.apex.crypto.{BinaryData, Ecdsa, Fixed8, UInt160, UInt256}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.settings._
import org.junit.{AfterClass, Test}

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


@Test
class BlockchainTest {

  val _produceInterval = 2000

  val _witness1 = Witness("init1",
    PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8"),
    Some(new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))))
  val _witness2 = Witness("init2",
    PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8"),
    Some(new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))))

  val _consensusSettings = ConsensusSettings(_produceInterval, 500, 1, Array(_witness1, _witness2))

  val _acct1 = Ecdsa.PrivateKey.fromWIF("KwmuSp41VWBtGSWaQQ82ZRRSFzkJVTAyuDLQ9NzP9CPqLWirh4UQ").get
  val _acct2 = Ecdsa.PrivateKey.fromWIF("L32JpLopG2hWjEMSCkAjS1nUnPixVrDTPqFAGYbddQrtUjRfkjEP").get

  private def makeTx(from: PrivateKey,
                     to: UInt160,
                     nonce: Long,
                     amount: Fixed8) = {

  }

  private def createChain(path: String): LevelDBBlockchain = {
    val baseDir = s"BlockchainTest/$path"
    val chainSetting = ChainSettings(
      BlockBaseSettings(s"$baseDir/block", false, 0),
      DataBaseSettings(s"$baseDir/data", false, 0),
      ForkBaseSettings(s"$baseDir/fork", false, 0),
      10,
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
      assert(balance1.get.get(UInt256.Zero).get == Fixed8.fromDecimal(123.12).value)

      val balance2 = chain.getBalance(_acct2.publicKey.pubKeyHash)
      assert(balance2.get.get(UInt256.Zero).get == Fixed8.fromDecimal(234.2).value)


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
