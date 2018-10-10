/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: StateStoreTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-10-10 下午4:05@version: 1.0
 *
 */

package com.apex.test

import com.apex.core.HeadBlockStore
import com.apex.crypto.BinaryData
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import org.junit.{AfterClass, Test}

@Test
class StateStoreTest {
  private val PubA = PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8")
  private val PriA = new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))

  @Test
  def testCommitRollBack: Unit = {
    val genesis = BlockBuilder.genesisBlock()
    val blk1 = BlockBuilder.newBlock(PubA, PriA, genesis)

    {
      val db = DbManager.open(StateStoreTest.testClass, "testCommitRollBack")
      val headStore = new HeadBlockStore(db)
      db.newSession()
      assert(headStore.set(genesis.header))
      assert(headStore.get.exists(_.id.equals(genesis.id)))
      db.rollBack()
      assert(headStore.get.isEmpty)
      DbManager.close(StateStoreTest.testClass, "testCommitRollBack")
    }
    {
      val db = DbManager.open(StateStoreTest.testClass, "testCommitRollBack")
      val headStore = new HeadBlockStore(db)
      assert(headStore.get.isEmpty)
      db.newSession()
      headStore.set(genesis.header)
      db.newSession()
      headStore.set(blk1.header)
      assert(headStore.get.exists(_.id.equals(blk1.id)))
      DbManager.close(StateStoreTest.testClass, "testCommitRollBack")
    }
    {
      val db = DbManager.open(StateStoreTest.testClass, "testCommitRollBack")
      val headStore = new HeadBlockStore(db)
      assert(headStore.get.exists(_.id.equals(blk1.id)))
      db.rollBack()
      assert(headStore.get.exists(_.id.equals(genesis.id)))
      db.commit()
      DbManager.close(StateStoreTest.testClass, "testCommitRollBack")
    }
    {
      val db = DbManager.open(StateStoreTest.testClass, "testCommitRollBack")
      val headStore = new HeadBlockStore(db)
      assert(headStore.get.exists(_.id.equals(genesis.id)))
      db.rollBack()
      assert(headStore.get.exists(_.id.equals(genesis.id)))
      db.newSession()
      headStore.set(blk1.header)
      db.commit()
      DbManager.close(StateStoreTest.testClass, "testCommitRollBack")
    }
    {
      val db = DbManager.open(StateStoreTest.testClass, "testCommitRollBack")
      val headStore = new HeadBlockStore(db)
      assert(headStore.get.exists(_.id.equals(blk1.id)))
      db.rollBack()
      assert(headStore.get.exists(_.id.equals(blk1.id)))
    }
  }
}

object StateStoreTest {
  private final val testClass = "StateStoreTest"

  @AfterClass
  def clearUp: Unit = {
    DbManager.clearUp(testClass)
  }
}
