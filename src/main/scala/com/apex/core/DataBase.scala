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

import java.math.BigInteger

import com.apex.common.ApexLogging
import com.apex.crypto.{FixedNumber, UInt160, UInt256}
import com.apex.settings.DataBaseSettings
import com.apex.storage.LevelDbStorage

class DataBase(settings: DataBaseSettings) extends ApexLogging {
  private val db = LevelDbStorage.open(settings.dir)

  private val accountStore = new AccountStore(db, settings.cacheSize)
  private val receiptStore = new ReceiptStore(db, settings.cacheSize)
  private val contractStore = new ContractStore(db, settings.cacheSize)
  private val contractStateStore = new ContractStateStore(db, settings.cacheSize)
  private val nameToAccountStore = new NameToAccountStore(db, settings.cacheSize)

  def nameExists(name: String): Boolean = {
    nameToAccountStore.contains(name)
  }

  def registerExists(register: UInt160): Boolean = {
    accountStore.contains(register)
  }

  def increaseNonce(address: UInt160) = {
    // TODO
  }

  def getNonce(address: UInt160): Long = {
    val account = getAccount(address)
    if (account.isDefined)
      account.get.nextNonce
    else
      0
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

  def transfer(from: UInt160, to: UInt160, value: FixedNumber) = {
    //TODO
  }

  def transfer(from: UInt160, to: UInt160, value: BigInteger) = {
    //TODO
  }

  def addBalance(address: UInt160, value: FixedNumber) = {
    //TODO
  }

  def addBalance(address: UInt160, value: BigInteger) = {
    //TODO
  }

  def getBalance(address: UInt160): Option[scala.collection.immutable.Map[UInt256, FixedNumber]] = {
    accountStore.get(address).map(_.balances)
  }

  def getCode(address: UInt160): Array[Byte] = {
    contractStore.get(address).map(_.code).getOrElse(Array.empty)
  }

  def saveCode(address: UInt160, code: Array[Byte]) = {
    contractStore.set(address, Contract(address, code))
  }

  def getContractState(address: UInt160, key: Array[Byte]): Array[Byte] = {
    contractStateStore.get(address.data ++ key).getOrElse(Array.empty)
  }

  def saveContractState(address: UInt160, key: Array[Byte], value: Array[Byte]): Unit = {
    contractStateStore.set(address.data ++ key, value)
  }

  def getReceipt(txid: UInt256): Option[TransactionReceipt] = {
    receiptStore.get(txid)
  }

  def setReceipt(txid: UInt256, receipt: TransactionReceipt) = {
    receiptStore.set(txid, receipt)
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
