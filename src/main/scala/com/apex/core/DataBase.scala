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

import com.apex.crypto.{Fixed8, UInt160, UInt256}
import com.apex.settings.DataBaseSettings
import com.apex.storage.LevelDbStorage

class DataBase(settings: DataBaseSettings) {
  private val db = LevelDbStorage.open(settings.dir)

  //  private val headerStore = new HeaderStore(db, settings.cacheSize)
  //  private val heightStore = new HeightStore(db, settings.cacheSize)
  //  private val txStore = new TransactionStore(db, settings.cacheSize)
  private val accountStore = new AccountStore(db, settings.cacheSize)
  //  private val addressStore = new AddressStore(db)
  //  private val blkTxMappingStore = new BlkTxMappingStore(db, settings.cacheSize)
  //  private val headBlkStore = new HeadBlockStore(db)
  //private val utxoStore = new UTXOStore(db, 10)
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

  def getBalance(address: UInt160): Option[Map[UInt256, Fixed8]] = {
    accountStore.get(address).map(_.balances)
  }

  def onApplyTransaction(transaction: Transaction): Unit = {
    //TODO update balance etc
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
}
