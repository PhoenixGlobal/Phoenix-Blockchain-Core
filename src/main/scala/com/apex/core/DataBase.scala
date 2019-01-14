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
import com.apex.crypto.{FixedNumber, UInt160, UInt256}
import com.apex.settings.DataBaseSettings
import com.apex.storage.Storage

class DataBase(settings: DataBaseSettings) extends ApexLogging {
  private val db = Storage.open(settings.dbType, settings.dir)

  private val accountStore = new AccountStore(db, settings.cacheSize)
  private val receiptStore = new ReceiptStore(db, settings.cacheSize)
  private val contractStore = new ContractStore(db, settings.cacheSize)
  private val contractStateStore = new ContractStateStore(db, settings.cacheSize)
  private val nameToAccountStore = new NameToAccountStore(db, settings.cacheSize)

  def nameExists(name: String): Boolean = {
    nameToAccountStore.contains(name)
  }

  def accountExists(register: UInt160): Boolean = {
    accountStore.contains(register)
  }

  // increase nonce by one
  def increaseNonce(address: UInt160) = {
    val account = accountStore.get(address).getOrElse(Account.newAccount(address))
    accountStore.set(address, account.increaseNonce)
  }

  // get the expected next nonce
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

  // set two accounts in batch
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

  // create empty account
  def createAccount(address: UInt160) = {
    accountStore.set(address, Account.newAccount(address))
  }

  // transfer values
  def transfer(from: UInt160, to: UInt160, value: FixedNumber): Unit = {
    val fromAcct = getAccount(from).getOrElse(Account.newAccount(from))
    val toAcct = getAccount(to).getOrElse(Account.newAccount(to))

    setAccount(
      (from, fromAcct.addBalance(-value)),
      (to, toAcct.addBalance(value))
    )
  }

  // transfer values
  def transfer(from: UInt160, to: UInt160, value: BigInt): Unit = {
    transfer(from, to, FixedNumber(value))
  }

  // add balance for single account
  def addBalance(address: UInt160, value: FixedNumber): FixedNumber = {
    val account = getAccount(address)
      .getOrElse(Account.newAccount(address))
      .addBalance(value)
    accountStore.set(address, account)
    account.balance
  }

  // add balance for single account
  def addBalance(address: UInt160, value: BigInt): BigInt = {
    addBalance(address, FixedNumber(value)).value
  }

  // get balance for specified account
  def getBalance(address: UInt160): Option[FixedNumber] = {
    accountStore.get(address).map(_.balance)
  }

  // get code hash
  def getCodeHash(address:UInt160): Array[Byte] = {
    accountStore.get(address).map(_.codeHash).getOrElse(Array.empty)
  }

  // get code
  def getCode(address: UInt160): Array[Byte] = {
    contractStore.get(address).map(_.code).getOrElse(Array.empty)
  }

  // save code
  def saveCode(address: UInt160, code: Array[Byte]) = {
    contractStore.set(address, Contract(address, code))
  }

  // get contract state of key
  def getContractState(address: UInt160, key: Array[Byte]): Array[Byte] = {
    contractStateStore.get(address.data ++ key).getOrElse(Array.empty)
  }

  // save contract state key-value pairs
  def saveContractState(address: UInt160, key: Array[Byte], value: Array[Byte]): Unit = {
    contractStateStore.set(address.data ++ key, value)
  }

  // get tx receipt
  def getReceipt(txid: UInt256): Option[TransactionReceipt] = {
    receiptStore.get(txid)
  }

  // set tx receipt
  def setReceipt(txid: UInt256, receipt: TransactionReceipt) = {
    receiptStore.set(txid, receipt)
  }

  def startTracking(): DataBase = {
    this
  }

  // start new session
  def startSession(): Unit = {
    db.newSession()
  }

  // undo all operations in the latest session
  def rollBack(): Unit = {
    db.rollBack()
  }

  // commit all operations in sessions whose revision is equal to or larger than the specified revision
  def commit(revision: Int): Unit = {
    db.commit(revision)
  }

  // commit all operations in the latest session
  def commit(): Unit = {
    db.commit()
  }

  def close(): Unit = {
    db.close()
  }

  // return latest revision
  def revision(): Int = {
    db.revision()
  }
}
