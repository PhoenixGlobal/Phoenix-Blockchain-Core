/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: TestHelper.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-10-10 下午4:47@version: 1.0
 *
 */

package com.apex.test


import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.time.Instant

import com.apex.core.{Block, BlockHeader}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.crypto.{BinaryData, Crypto, UInt160, UInt256}
import com.apex.storage.LevelDbStorage

import scala.collection.mutable.Map
import scala.reflect.io.Directory

class DbManager(testClass: String) {
  private final val dbs = Map.empty[String, LevelDbStorage]

  def openDB(dir: String): LevelDbStorage = {
    if (!dbs.contains(dir)) {
      val db = LevelDbStorage.open(s"$testClass/$dir")
      dbs.put(dir, db)
    }
    dbs(dir)
  }

  def cleanUp: Unit = {
    dbs.values.foreach(_.close())
    deleteDir(testClass)
  }

  private def deleteDir(dir: String): Unit = {
    try {
      Directory(dir).deleteRecursively()
    } catch {
      case e: Throwable => println(e.getMessage)
    }
  }
}

object DbManager {
  private final val dbManagers = Map.empty[String, DbManager]

  def open(testClass: String, dir: String): LevelDbStorage = {
    if (!dbManagers.contains(testClass)) {
      val dbMgr = new DbManager(testClass)
      dbManagers.put(testClass, dbMgr)
    }
    dbManagers(testClass).openDB(dir)
  }

  def close(testClass: String, dir: String) = {
    dbManagers.get(testClass).foreach(dbMgr => {
      if (dbMgr.dbs.contains(dir)) {
        dbMgr.dbs(dir).close()
        dbMgr.dbs.remove(dir)
      }
    })
  }

  def clearUp(testClass: String) = {
    dbManagers.get(testClass).foreach(_.cleanUp)
    dbManagers.remove(testClass)
  }
}

object BlockBuilder {
  def newBlock(pri: PrivateKey, prevBlock: Block) = {
    val root = SerializerHelper.testHash256("test")
    val timeStamp = Instant.now.toEpochMilli
    val header = BlockHeader.build(
      prevBlock.height + 1, timeStamp,
      root, prevBlock.id, pri)
    Block.build(header, Seq.empty)
  }

  def genesisBlock() = {
    val pub = PublicKey("03b4534b44d1da47e4b4a504a210401a583f860468dec766f507251a057594e682")
    val pri = new PrivateKey(BinaryData("7a93d447bffe6d89e690f529a3a0bdff8ff6169172458e04849ef1d4eafd7f86"))

    val genesisHeader = BlockHeader.build(
      0, Instant.now.toEpochMilli,
      UInt256.Zero, UInt256.Zero, pri)
    Block.build(genesisHeader, Seq.empty)
  }
}

class SerializerHelper[T <: com.apex.common.Serializable](deserializer: DataInputStream => T, eqComparer: (T, T) => Boolean = (x: T, y: T) => x.equals(y)) {
  def test(value: T) = {
    SerializerHelper.test(value, deserializer, eqComparer)
  }
}

object SerializerHelper {
  def testHash256(str: String = "test") = UInt256.fromBytes(Crypto.hash256(str.getBytes("UTF-8")))

  def testHash160(str: String = "test") = UInt160.fromBytes(Crypto.hash160(str.getBytes("UTF-8")))

  def test[T <: com.apex.common.Serializable](value: T, deserializer: DataInputStream => T, eqComparer: (T, T) => Boolean) = {
    val bos = new ByteArrayOutputStream
    val os = new DataOutputStream(bos)
    os.write(value)
    val ba = bos.toByteArray
    val bis = new ByteArrayInputStream(ba)
    val is = new DataInputStream(bis)
    import com.apex.common.Serializable._
    assert(eqComparer(is.readObj(deserializer), value))
  }
}