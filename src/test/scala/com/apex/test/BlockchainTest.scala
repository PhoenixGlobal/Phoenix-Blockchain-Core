package com.apex.test

import com.apex.core.{Block, BlockHeader, Blockchain, LevelDBBlockchain}
import com.apex.crypto.{BinaryData, UInt256}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.settings._
import org.junit.{AfterClass, Test}

import scala.reflect.io.Directory

@Test
class BlockchainTest {

  private def createChain(path: String): LevelDBBlockchain = {
    val baseDir = s"BlockchainTest/$path"
    val chainSetting = ChainSettings(
      BlockBaseSettings(s"$baseDir/block", false, 0),
      DataBaseSettings(s"$baseDir/data", false, 0),
      ForkBaseSettings(s"$baseDir/fork", false, 0),
      "03b4534b44d1da47e4b4a504a210401a583f860468dec766f507251a057594e682",
      10,
      GenesisSettings(0,
        "03b4534b44d1da47e4b4a504a210401a583f860468dec766f507251a057594e682",
        "7a93d447bffe6d89e690f529a3a0bdff8ff6169172458e04849ef1d4eafd7f86",
        "AP3YkgmCrctzg2iw2FzjhsnUqPyWDogud6x"
      )
    )
    val consensusSettings = ConsensusSettings(2000, 500, 1,
      Array(
        Witness("init1", PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8"),
          Some(new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471")))
        ),
        Witness("init2", PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8"),
          Some(new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471")))
        )
      )
    )
    Blockchain.populate(chainSetting, consensusSettings)
  }

  @Test
  def testTryInsertBlock1(): Unit = {
    val chain = createChain("TryInsertBlock1")
    try {
      val header = BlockHeader.build(
        1, 0, UInt256.Zero,
        UInt256.Zero, PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8"),
        new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471")))
      val block = Block.build(header, Seq.empty)
      assert(chain.tryInsertBlock(block, true) == false)
    }
    finally {
      chain.close()
    }
  }
  
}

object BlockchainTest {
  @AfterClass
  def cleanUp: Unit = {
    Directory("BlockchainTest").deleteRecursively()
  }
}
