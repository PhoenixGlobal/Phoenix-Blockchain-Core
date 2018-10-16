/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: DataBase.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-9-27 下午12:02@version: 1.0
 *
 */

package com.apex.core

import com.apex.common.ApexLogging
import com.apex.crypto.{Fixed8, UInt160, UInt256}
import com.apex.settings.DataBaseSettings
import com.apex.storage.LevelDbStorage

class DataBase(settings: DataBaseSettings) extends ApexLogging {
  private val db = LevelDbStorage.open(settings.dir)

  //  private val headerStore = new HeaderStore(db, settings.cacheSize)
  //  private val heightStore = new HeightStore(db, settings.cacheSize)
  //  private val txStore = new TransactionStore(db, settings.cacheSize)
  private val accountStore = new AccountStore(db, settings.cacheSize)
  //  private val addressStore = new AddressStore(db)
  //  private val blkTxMappingStore = new BlkTxMappingStore(db, settings.cacheSize)
  //  private val headBlkStore = new HeadBlockStore(db)

  private val nameToAccountStore = new NameToAccountStore(db, settings.cacheSize)

  def nameExists(name: String): Boolean = {
    nameToAccountStore.contains(name)
  }

  def registerExists(register: UInt160): Boolean = {
    accountStore.contains(register)
  }

  def getAccount(address: UInt160): Option[Account] = {
    accountStore.get(address)
  }

  def setAccount(from: (UInt160, Account),
                 to: (UInt160, Account)) = {
    try {
      db.batchWrite(batch => {
        accountStore.set(from._1, from._2, batch)
        accountStore.set(to._1, to._2, batch)
      })
      true
    }
    catch {
      case e: Throwable => {
        log.error("setAccount failed", e)
        false
      }
    }
  }

  def getBalance(address: UInt160): Option[scala.collection.immutable.Map[UInt256, Fixed8]] = {
    accountStore.get(address).map(_.balances)
  }

  def startSession(): Unit = {
    db.newSession()
  }

  def rollBack(): Unit = {
    db.rollBack()
  }

  def commit(revision: Int): Unit = {
    db.commit(revision)
  }

  def commit(): Unit = {
    db.commit()
  }

  def close(): Unit = {
    db.close()
  }

  def revision(): Int = {
    db.revision()
  }
}
