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

import com.apex.consensus.{WitnessInfo, WitnessList}
import com.apex.core._
import com.apex.crypto.{BinaryData, UInt256}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.settings.{DBType, ForkBaseSettings, Witness}
import org.junit.{AfterClass, Test}

import scala.collection.mutable.{ListBuffer, Seq}
import scala.reflect.io.Directory

@Test
class ForkBaseTest {
  private val PubA = PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8").pubKeyHash
  private val PriA = new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))
  private val PubB = PublicKey("0238eb90b322fac718ce10b21d451d00b7003a2a1de2a1d584a158d7b7ffee297b").pubKeyHash
  private val PriB = new PrivateKey(BinaryData("485cfb9f743d9997e316f5dca216b1c6adf12aa301c1d520e020269debbebbf0"))
  private val PubC = PublicKey("0234b9b7d2909231d143a6693082665837965438fc273fbc4c507996e41394c8c1").pubKeyHash
  private val PriC = new PrivateKey(BinaryData("5dfee6af4775e9635c67e1cea1ed617efb6d22ca85abfa97951771d47934aaa0"))
  //private val witnesses = Array(Witness("A", PubA), Witness("B", PubB))
  private val witnessesAB = WitnessList.create(Array(WitnessInfo(PubA), WitnessInfo(PubB)), UInt256.Zero)
  private val witnessesABC = WitnessList.create(Array(WitnessInfo(PubA), WitnessInfo(PubB), WitnessInfo(PubC)), UInt256.Zero)
  private val genesis = BlockBuilder.genesisBlock

  @Test
  def testHead(): Unit = {
    val blk1a = ForkBaseTest.newBlock(PriA, genesis)
    val blk2a = ForkBaseTest.newBlock(PriA, blk1a)
    val blk3a = ForkBaseTest.newBlock(PriA, blk2a)
    val blk4a = ForkBaseTest.newBlock(PriA, blk3a)
    val blk3b = ForkBaseTest.newBlock(PriB, blk2a)
    val blk4b = ForkBaseTest.newBlock(PriB, blk3b)
    val forkBase = ForkBaseTest.open("forkBase_head")
    assert(forkBase.head.isEmpty)
    forkBase.add(genesis, witnessesAB)
    assert(forkBase.head.exists(_.block.equals(genesis)))
    forkBase.add(blk1a, witnessesAB)
    assert(forkBase.head.exists(_.block.equals(blk1a)))
    forkBase.add(blk2a, witnessesAB)
    assert(forkBase.head.exists(_.block.equals(blk2a)))
    forkBase.add(blk3a, witnessesAB)
    assert(forkBase.head.exists(_.block.equals(blk3a)))
    forkBase.add(blk4a, witnessesAB)
    assert(forkBase.head.exists(_.block.equals(blk4a)))
    forkBase.add(blk3b, witnessesAB)
    assert(forkBase.head.exists(_.block.equals(blk3b)))
    forkBase.add(blk4b, witnessesAB)
    assert(forkBase.head.exists(_.block.equals(blk4b)))
  }

  @Test
  def testGet(): Unit = {
    val forkBase = ForkBaseTest.open("forkBase_get")

    def assertBlock(block: Block,
                    beforeId: Boolean = false,
                    beforeHeight: Boolean = false,
                    afterId: Boolean = true,
                    afterHeight: Boolean = true): Unit = {
      assert(forkBase.get(block.id).isDefined == beforeId)
      assert(forkBase.get(block.height).isDefined == beforeHeight)
      forkBase.add(block, witnessesAB)
      assert(forkBase.get(block.id).isDefined == afterId)
      assert(forkBase.get(block.height).isDefined == afterHeight)
    }

    assertBlock(genesis)

    val blk1a = ForkBaseTest.newBlock(PriA, genesis)
    assertBlock(blk1a)

    val blk2a = ForkBaseTest.newBlock(PriA, blk1a)
    assertBlock(blk2a)

    val blk3a = ForkBaseTest.newBlock(PriA, blk2a)
    assertBlock(blk3a)

    val blk3b = ForkBaseTest.newBlock(PriB, blk2a)
    assertBlock(blk3b, false, true, true, true)
  }

  @Test
  def testGetNext(): Unit = {
    val forkBase = ForkBaseTest.open("forkBase_next")
    assert(forkBase.getNext(genesis.id).isEmpty)
    forkBase.add(genesis, witnessesAB)
    assert(forkBase.getNext(genesis.id).isEmpty)
    val blk1a = ForkBaseTest.newBlock(PriA, genesis)
    val blk1b = ForkBaseTest.newBlock(PriB, genesis)
    val blk2b = ForkBaseTest.newBlock(PriB, blk1b)
    forkBase.add(blk1a, witnessesAB)
    assert(forkBase.getNext(genesis.id).forall(_.equals(blk1a.id)))
    forkBase.add(blk1b, witnessesAB)
    assert(forkBase.getNext(genesis.id).forall(_.equals(blk1a.id)))
    forkBase.add(blk2b, witnessesAB)
    assert(forkBase.getNext(genesis.id).forall(_.equals(blk1b.id)))
  }

  @Test
  def testAdd(): Unit = {
    val forkBase = ForkBaseTest.open("forkBase_add")
    assert(forkBase.add(genesis, witnessesAB))
    assert(!forkBase.add(genesis, witnessesAB))
    val blk1a = ForkBaseTest.newBlock(PriA, genesis)
    assert(forkBase.add(blk1a, witnessesAB))
    val blk2a = ForkBaseTest.newBlock(PriA, blk1a)
    val blk3a = ForkBaseTest.newBlock(PriA, blk2a)
    assert(!forkBase.add(blk3a, witnessesAB))
  }

  @Test
  def testSwitch(): Unit = {
    val forkBase = ForkBaseTest.open("forkBase_switch")
    val blk1a = ForkBaseTest.newBlock(PriA, genesis)
    val blk2a = ForkBaseTest.newBlock(PriA, blk1a)
    val blk3a = ForkBaseTest.newBlock(PriA, blk2a)
    val blk4a = ForkBaseTest.newBlock(PriA, blk3a)
    val blk5a = ForkBaseTest.newBlock(PriA, blk4a)
    val blk3b = ForkBaseTest.newBlock(PriB, blk2a)
    val blk4b = ForkBaseTest.newBlock(PriB, blk3b)
    val blk4c = ForkBaseTest.newBlock(PriC, blk3b)
    forkBase.add(genesis, witnessesABC)
    forkBase.add(blk1a, witnessesABC)
    forkBase.add(blk2a, witnessesABC)
    forkBase.add(blk3b, witnessesABC)
    forkBase.add(blk4b, witnessesABC)
    assert(forkBase.get(blk1a.id).exists(_.master))
    assert(forkBase.get(blk2a.id).exists(_.master))
    assert(forkBase.get(blk3b.id).exists(_.master))
    assert(forkBase.get(blk4b.id).exists(_.master))
    assert(forkBase.head.exists(_.id.equals(blk4b.id)))
    forkBase.add(blk3a, witnessesABC)
    forkBase.add(blk4a, witnessesABC)
    assert(forkBase.get(blk1a.id).exists(_.master))
    assert(forkBase.get(blk2a.id).exists(_.master))
    assert(forkBase.get(blk3a.id).exists(!_.master))
    assert(forkBase.get(blk4a.id).exists(!_.master))
    assert(forkBase.head.exists(_.id.equals(blk4b.id)))
    forkBase.add(blk5a, witnessesABC)
    assert(forkBase.get(blk3b.id).exists(!_.master))
    assert(forkBase.get(blk4b.id).exists(!_.master))
    assert(forkBase.get(blk3a.id).exists(_.master))
    assert(forkBase.get(blk4a.id).exists(_.master))
    assert(forkBase.get(blk5a.id).exists(_.master))
    assert(forkBase.head.exists(_.id.equals(blk5a.id)))
    forkBase.add(blk4c, witnessesABC)
    assert(forkBase.get(blk4b.id).exists(!_.master))
    assert(forkBase.get(blk3a.id).exists(!_.master))
    assert(forkBase.get(blk4a.id).exists(!_.master))
    assert(forkBase.get(blk5a.id).exists(!_.master))
    assert(forkBase.get(blk3b.id).exists(_.master))
    assert(forkBase.get(blk4c.id).exists(_.master))
    assert(forkBase.head.exists(_.id.equals(blk4c.id)))
  }

  @Test
  def testSwitchFailed(): Unit = {
    def  applyBlock(blk: Block) = {
      !blk.header.producer.equals(PubC)
    }

    def onSwitch(a: Seq[ForkItem], b: Seq[ForkItem], c: SwitchState, d: Boolean) = {
      var i = 0
      for (blk <- b.map(_.block) if applyBlock(blk)) {
        i += 1
      }
      if (i < b.size) {
        SwitchResult(false, b(i))
      } else {
        SwitchResult(true)
      }
    }

    val forkBase = ForkBaseTest.open("forkBase_switchFailed", onSwitch)
    val blk1a = ForkBaseTest.newBlock(PriA, genesis)
    val blk2a = ForkBaseTest.newBlock(PriA, blk1a)
    val blk3a = ForkBaseTest.newBlock(PriA, blk2a)
    val blk4a = ForkBaseTest.newBlock(PriA, blk3a)
    val blk5a = ForkBaseTest.newBlock(PriA, blk4a)
    val blk3b = ForkBaseTest.newBlock(PriB, blk2a)
    val blk4b = ForkBaseTest.newBlock(PriB, blk3b)
    val blk4c = ForkBaseTest.newBlock(PriC, blk3b)

    forkBase.add(genesis, witnessesABC)
    forkBase.add(blk1a, witnessesABC)
    forkBase.add(blk2a, witnessesABC)
    forkBase.add(blk3b, witnessesABC)
    forkBase.add(blk4b, witnessesABC)
    assert(forkBase.get(blk1a.id).exists(_.master))
    assert(forkBase.get(blk2a.id).exists(_.master))
    assert(forkBase.get(blk3b.id).exists(_.master))
    assert(forkBase.get(blk4b.id).exists(_.master))
    assert(forkBase.head.exists(_.id.equals(blk4b.id)))
    forkBase.add(blk3a, witnessesABC)
    forkBase.add(blk4a, witnessesABC)
    assert(forkBase.get(blk1a.id).exists(_.master))
    assert(forkBase.get(blk2a.id).exists(_.master))
    assert(forkBase.get(blk3a.id).exists(!_.master))
    assert(forkBase.get(blk4a.id).exists(!_.master))
    assert(forkBase.head.exists(_.id.equals(blk4b.id)))
    forkBase.add(blk5a, witnessesABC)
    assert(forkBase.get(blk3b.id).exists(!_.master))
    assert(forkBase.get(blk4b.id).exists(!_.master))
    assert(forkBase.get(blk3a.id).exists(_.master))
    assert(forkBase.get(blk4a.id).exists(_.master))
    assert(forkBase.get(blk5a.id).exists(_.master))
    assert(forkBase.head.exists(_.id.equals(blk5a.id)))
    forkBase.add(blk4c, witnessesABC)
    assert(forkBase.get(blk1a.id).exists(_.master))
    assert(forkBase.get(blk2a.id).exists(_.master))
    assert(forkBase.get(blk3a.id).exists(_.master))
    assert(forkBase.get(blk4a.id).exists(_.master))
    assert(forkBase.get(blk5a.id).exists(_.master))
    assert(forkBase.get(blk3b.id).exists(!_.master))
    assert(forkBase.get(blk4b.id).exists(!_.master))
    assert(forkBase.get(blk4c.id).isEmpty)
    assert(forkBase.head.exists(_.id.equals(blk5a.id)))
  }

  @Test
  def testRemoveFork(): Unit = {
    val forkBase = ForkBaseTest.open("forkBase_removeFork")
    val blk1a = ForkBaseTest.newBlock(PriA, genesis)
    val blk2a = ForkBaseTest.newBlock(PriA, blk1a)
    val blk3a = ForkBaseTest.newBlock(PriA, blk2a)
    val blk4a = ForkBaseTest.newBlock(PriA, blk3a)
    val blk5a = ForkBaseTest.newBlock(PriA, blk4a)
    val blk3b = ForkBaseTest.newBlock(PriB, blk2a)
    val blk4b = ForkBaseTest.newBlock(PriB, blk3b)
    val blk3c = ForkBaseTest.newBlock(PriC, blk2a)
    forkBase.add(genesis, witnessesABC)
    forkBase.add(blk1a, witnessesABC)
    forkBase.add(blk2a, witnessesABC)
    forkBase.add(blk3b, witnessesABC)
    forkBase.add(blk4b, witnessesABC)
    forkBase.add(blk3a, witnessesABC)
    forkBase.add(blk4a, witnessesABC)
    forkBase.add(blk5a, witnessesABC)
    assert(forkBase.removeFork(blk4a.id))
    assert(forkBase.get(blk1a.id).isDefined)
    assert(forkBase.get(blk2a.id).isDefined)
    assert(forkBase.get(blk3a.id).isDefined)
    assert(forkBase.get(blk3b.id).isDefined)
    assert(forkBase.get(blk4b.id).isDefined)
    assert(forkBase.get(blk4a.id).isEmpty)
    assert(forkBase.get(blk5a.id).isEmpty)
    assert(forkBase.removeFork(blk2a.id))
    assert(forkBase.get(blk1a.id).isDefined)
    assert(forkBase.get(blk2a.id).isEmpty)
    assert(forkBase.get(blk3a.id).isEmpty)
    assert(forkBase.get(blk3b.id).isEmpty)
    assert(forkBase.get(blk4b.id).isEmpty)
    assert(!forkBase.removeFork(blk3c.id))
  }

  @Test
  def testFork(): Unit = {
    val forkBase = ForkBaseTest.open("forkBase_fork")
    val blk1a = ForkBaseTest.newBlock(PriA, genesis)
    val blk2a = ForkBaseTest.newBlock(PriA, blk1a)
    val blk3a = ForkBaseTest.newBlock(PriA, blk2a)
    val blk4a = ForkBaseTest.newBlock(PriA, blk3a)
    forkBase.add(genesis, witnessesAB)
    forkBase.add(blk1a, witnessesAB)
    assert(forkBase.head.exists(_.block.id.equals(blk1a.id)))
    forkBase.add(blk2a, witnessesAB)
    assert(forkBase.head.exists(_.block.id.equals(blk2a.id)))
    forkBase.add(blk3a, witnessesAB)
    assert(forkBase.head.exists(_.block.id.equals(blk3a.id)))
    val blk3b = ForkBaseTest.newBlock(PriB, blk2a)
    val blk4b = ForkBaseTest.newBlock(PriB, blk3b)
    forkBase.add(blk3b, witnessesAB)
    assert(forkBase.head.exists(_.block.id.equals(blk3b.id)))
    assert(forkBase.get(blk3a.id).exists(_.master == false))
    assert(forkBase.get(blk3b.id).exists(_.master == true))
    forkBase.add(blk4a, witnessesAB)
    assert(forkBase.head.exists(_.block.id.equals(blk3b.id)))
    forkBase.add(blk4b, witnessesAB)
    assert(forkBase.head.exists(_.block.id.equals(blk4b.id)))
  }
}

object ForkBaseTest {
  type SwitchCallback = (Seq[ForkItem], Seq[ForkItem], SwitchState, Boolean) => SwitchResult

  private final val dirs = ListBuffer.empty[String]
  private final val dbs = ListBuffer.empty[ForkBase]

  @AfterClass
  def cleanUp: Unit = {
    dbs.foreach(_.close())
    dirs.foreach(deleteDir)
  }

  def open(dir: String, onSwitch: SwitchCallback = null): ForkBase = {
    def forkStr(title: String, fork: Seq[ForkItem]): String = {
      s"  $title: ${fork.map(blk => s"${blk.block.height}(${blk.block.shortId()})").mkString(" <- ")}"
    }

    var switchCallback: SwitchCallback = null
    if (onSwitch == null) {
      switchCallback = (from, to, _, _) => {
        println(s"switch\n${forkStr("from", from)}\n${forkStr("to", to)}")
        SwitchResult(true)
      }
    } else {
      switchCallback = (from, to, state, _) => {
        println(s"switch\n${forkStr("from", from)}\n${forkStr("to", to)}")
        onSwitch(from, to, state, false)
      }
    }

    val settings = new ForkBaseSettings(dir, false, 0, DBType.LevelDB)
    val forkBase = new ForkBase(settings, //witnesses,
      blk => println(s"confirm block ${blk.height}"),
      switchCallback)
    dbs.append(forkBase)
    dirs.append(dir)
    forkBase
  }

  def newBlock(pri: PrivateKey, prevBlock: Block) = {
    BlockBuilder.newBlock(pri, prevBlock)
  }

  private def deleteDir(dir: String): Unit = {
    try {
      Directory(dir).deleteRecursively()
    } catch {
      case e: Throwable => println(e.getMessage)
    }
  }
}