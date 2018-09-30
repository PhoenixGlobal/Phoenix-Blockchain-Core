/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: ForkBaseTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-9-29 下午4:41@version: 1.0
 *
 */

package com.apex.test

import java.time.Instant

import com.apex.core.{Block, BlockHeader, ForkBase, ForkItem}
import com.apex.crypto.{BinaryData, UInt256}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.settings.{ForkBaseSettings, Witness}
import com.apex.test.LevelDBStorageTest.{dbs, deleteDir, dirs}
import org.junit.{AfterClass, Test}

import scala.collection.mutable.ListBuffer
import scala.reflect.io.Directory

@Test
class ForkBaseTest {
  private val PubA = PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8")
  private val PriA = new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))
  private val PubB = PublicKey("0238eb90b322fac718ce10b21d451d00b7003a2a1de2a1d584a158d7b7ffee297b")
  private val PriB = new PrivateKey(BinaryData("485cfb9f743d9997e316f5dca216b1c6adf12aa301c1d520e020269debbebbf0"))
  private val witnesses = Array(Witness("A", PubA, Some(PriA)), Witness("B", PubB, Some(PriB)))
  private val genesis = ForkBaseTest.genesisBlock

  @Test
  def testHead(): Unit = {
    val blk1a = ForkBaseTest.newBlock(PubA, PriA, genesis)
    val blk2a = ForkBaseTest.newBlock(PubA, PriA, blk1a)
    val blk3a = ForkBaseTest.newBlock(PubA, PriA, blk2a)
    val blk4a = ForkBaseTest.newBlock(PubA, PriA, blk3a)
    val blk3b = ForkBaseTest.newBlock(PubB, PriB, blk2a)
    val blk4b = ForkBaseTest.newBlock(PubB, PriB, blk3b)
    val forkBase = ForkBaseTest.open("forkBase_head", witnesses)
    assert(forkBase.head.isEmpty)
    forkBase.add(genesis)
    assert(forkBase.head.exists(_.block.equals(genesis)))
    forkBase.add(blk1a)
    assert(forkBase.head.exists(_.block.equals(blk1a)))
    forkBase.add(blk2a)
    assert(forkBase.head.exists(_.block.equals(blk2a)))
    forkBase.add(blk3a)
    assert(forkBase.head.exists(_.block.equals(blk3a)))
    forkBase.add(blk4a)
    assert(forkBase.head.exists(_.block.equals(blk4a)))
    forkBase.add(blk3b)
    assert(forkBase.head.exists(_.block.equals(blk3b)))
    forkBase.add(blk4b)
    assert(forkBase.head.exists(_.block.equals(blk4b)))
  }

  @Test
  def testGet(): Unit = {
    val forkBase = ForkBaseTest.open("forkBase_get", witnesses)

    def assertBlock(block: Block,
                    beforeId: Boolean = false,
                    beforeHeight: Boolean = false,
                    afterId: Boolean = true,
                    afterHeight: Boolean = true): Unit = {
      assert(forkBase.get(block.id).isDefined == beforeId)
      assert(forkBase.get(block.height).isDefined == beforeHeight)
      forkBase.add(block)
      assert(forkBase.get(block.id).isDefined == afterId)
      assert(forkBase.get(block.height).isDefined == afterHeight)
    }

    assertBlock(genesis)

    val blk1a = ForkBaseTest.newBlock(PubA, PriA, genesis)
    assertBlock(blk1a)

    val blk2a = ForkBaseTest.newBlock(PubA, PriA, blk1a)
    assertBlock(blk2a)

    val blk3a = ForkBaseTest.newBlock(PubA, PriA, blk2a)
    assertBlock(blk3a)

    val blk3b = ForkBaseTest.newBlock(PubB, PriB, blk2a)
    assertBlock(blk3b, false, true, true, true)
  }

  @Test
  def testGetNext(): Unit = {
    val forkBase = ForkBaseTest.open("forkBase_next", witnesses)
    assert(forkBase.getNext(genesis.id).isEmpty)
    forkBase.add(genesis)
    assert(forkBase.getNext(genesis.id).isEmpty)
    val blk1a = ForkBaseTest.newBlock(PubA, PriA, genesis)
    val blk1b = ForkBaseTest.newBlock(PubB, PriB, genesis)
    val blk2b = ForkBaseTest.newBlock(PubB, PriB, blk1b)
    forkBase.add(blk1a)
    assert(forkBase.getNext(genesis.id).forall(_.equals(blk1a.id)))
    forkBase.add(blk1b)
    assert(forkBase.getNext(genesis.id).forall(_.equals(blk1a.id)))
    forkBase.add(blk2b)
    assert(forkBase.getNext(genesis.id).forall(_.equals(blk1b.id)))
  }

  @Test
  def testAdd(): Unit = {
    val forkBase = ForkBaseTest.open("forkBase_add", witnesses)
    assert(forkBase.add(genesis))
    assert(!forkBase.add(genesis))
    val blk1a = ForkBaseTest.newBlock(PubA, PriA, genesis)
    assert(forkBase.add(blk1a))
    val blk2a = ForkBaseTest.newBlock(PubA, PriA, blk1a)
    val blk3a = ForkBaseTest.newBlock(PubA, PriA, blk2a)
    assert(!forkBase.add(blk3a))
  }

  @Test
  def testFork(): Unit = {
    val forkBase = ForkBaseTest.open("forkBase_fork", witnesses)
    val blk1a = ForkBaseTest.newBlock(PubA, PriA, genesis)
    val blk2a = ForkBaseTest.newBlock(PubA, PriA, blk1a)
    val blk3a = ForkBaseTest.newBlock(PubA, PriA, blk2a)
    val blk4a = ForkBaseTest.newBlock(PubA, PriA, blk3a)
    forkBase.add(genesis)
    forkBase.add(blk1a)
    assert(forkBase.head.exists(_.block.id.equals(blk1a.id)))
    forkBase.add(blk2a)
    assert(forkBase.head.exists(_.block.id.equals(blk2a.id)))
    forkBase.add(blk3a)
    assert(forkBase.head.exists(_.block.id.equals(blk3a.id)))
    val blk3b = ForkBaseTest.newBlock(PubB, PriB, blk2a)
    val blk4b = ForkBaseTest.newBlock(PubB, PriB, blk3b)
    forkBase.add(blk3b)
    assert(forkBase.head.exists(_.block.id.equals(blk3b.id)))
    assert(forkBase.get(blk3a.id).exists(_.master == false))
    assert(forkBase.get(blk3b.id).exists(_.master == true))
    forkBase.add(blk4a)
    assert(forkBase.head.exists(_.block.id.equals(blk3b.id)))
    forkBase.add(blk4b)
    assert(forkBase.head.exists(_.block.id.equals(blk4b.id)))
  }

}

object ForkBaseTest {
  private final val dirs = ListBuffer.empty[String]
  private final val dbs = ListBuffer.empty[ForkBase]

  @AfterClass
  def cleanUp: Unit = {
    dbs.foreach(_.close())
    dirs.foreach(deleteDir)
  }

  def open(dir: String, witnesses: Array[Witness]): ForkBase = {
    def forkStr(title: String, fork: Seq[ForkItem]): String = {
      s"  $title: ${fork.map(blk => s"${blk.block.height}(${blk.block.id.toString.substring(0, 6)})").mkString(" <- ")}"
    }

    val settings = new ForkBaseSettings(dir, false, 0)
    val forkBase = new ForkBase(settings, witnesses,
      blk => println(s"confirm block ${blk.height}"),
      (from, to) => println(s"switch\n${forkStr("from", from)}\n${forkStr("to", to)}"))
    dbs.append(forkBase)
    dirs.append(dir)
    forkBase
  }

  def newBlock(pub: PublicKey, pri: PrivateKey, prevBlock: Block) = {
    val root = SerializerTest.testHash256("test")
    val timeStamp = Instant.now.toEpochMilli
    val header = BlockHeader.build(
      prevBlock.height + 1, timeStamp,
      root, prevBlock.id, pub, pri)
    Block.build(header, Seq.empty)
  }

  private def genesisBlock() = {
    val pub = PublicKey("03b4534b44d1da47e4b4a504a210401a583f860468dec766f507251a057594e682")
    val pri = new PrivateKey(BinaryData("7a93d447bffe6d89e690f529a3a0bdff8ff6169172458e04849ef1d4eafd7f86"))

    val genesisHeader = BlockHeader.build(
      0, Instant.now.toEpochMilli,
      UInt256.Zero, UInt256.Zero, pub, pri)
    Block.build(genesisHeader, Seq.empty)
  }

  private def deleteDir(dir: String): Unit = {
    try {
      Directory(dir).deleteRecursively()
    } catch {
      case e: Throwable => println(e.getMessage)
    }
  }
}