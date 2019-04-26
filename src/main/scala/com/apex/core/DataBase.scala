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

import java.time.Instant

import com.apex.common.ApexLogging
import com.apex.consensus._
import com.apex.crypto.{BinaryData, FixedNumber, UInt160, UInt256}
import com.apex.proposal.{Proposal, ProposalVote, ProposalVoteList}
import com.apex.settings.{ConsensusSettings, DataBaseSettings}
import com.apex.storage.Storage
import com.apex.vm.DataWord
import com.apex.vm.precompiled.PrecompiledContracts

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class DataBase(settings: DataBaseSettings, consensusSettings: ConsensusSettings,
               db: Storage.lowLevelRaw, tracking: Tracking) extends ApexLogging {
  private val accountStore = new AccountStore(tracking, settings.cacheSize)
  private val contractStore = new ContractStore(tracking, settings.cacheSize)
  private val contractStateStore = new ContractStateStore(tracking, settings.cacheSize)
  private val scheduleTxStore = new ScheduleTxStore(tracking, settings.cacheSize)

  private val witnessVoteStore = new WitnessVoteStore(tracking, settings.cacheSize)
  private val witnessInfoStore = new WitnessInfoStore(tracking, settings.cacheSize)

  private val previousWitnessStore = new PreviousWitnessStore(tracking)
  private val currentWitnessStore = new CurrentWitnessStore(tracking)
  private val pendingWitnessStore = new PendingWitnessStore(tracking)

  private val proposalStore = new ProposalStore(tracking, settings.cacheSize)
  private val proposalVoteListStore = new ProposalVoteListStore(tracking)

  private val witnessBlockCountLastWeekStore = new WitnessBlockCountLastWeekStore(tracking)
  private val witnessBlockCountThisWeekStore = new WitnessBlockCountThisWeekStore(tracking)

  def this(settings: DataBaseSettings, consensusSettings: ConsensusSettings, db: Storage.lowLevelRaw) = {
    this(settings, consensusSettings, db, Tracking.root(db))
  }

  def this(settings: DataBaseSettings, consensusSettings: ConsensusSettings) = {
    this(settings, consensusSettings, Storage.openTemp(settings.dbType, settings.dir))
  }

  //  def nameExists(name: String): Boolean = {
  //    nameToAccountStore.contains(name)
  //  }

  def getAllScheduleTx() = {
    scheduleTxStore.getLists(Array(StoreType.Data.id.toByte, DataType.scheduleTransaction.id.toByte))
  }

  def setScheduleTx(id: UInt256, tx: Transaction) ={
    scheduleTxStore.set(id, tx)
  }

  def getScheduleTx(id: UInt256) : Option[Transaction] = {
    scheduleTxStore.get(id)
  }

  def deleteScheduleTx(id: UInt256): Unit ={
    scheduleTxStore.delete(id)
  }

  def accountExists(address: UInt160): Boolean = {
    accountStore.contains(address)
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

  // create empty account
  def createAccount(address: UInt160) = {
    accountStore.set(address, Account.newAccount(address))
  }

  // transfer values
  def transfer(from: UInt160, to: UInt160, value: FixedNumber): Unit = {
    addBalance(from,-value)
    addBalance(to,value)
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
  def getCodeHash(address: UInt160): Array[Byte] = {
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

  def getAllWitness(): ArrayBuffer[WitnessInfo] = {
    witnessInfoStore.getLists(Array(StoreType.Data.id.toByte, DataType.WitnessInfo.id.toByte))
  }

  def getWitness(address: UInt160): Option[WitnessInfo] = {
    witnessInfoStore.get(address)
  }

  def setWitness(witness: WitnessInfo) = {
    witnessInfoStore.set(witness.addr, witness)
  }

  def deleteWitness(address: UInt160): Unit = {
    witnessInfoStore.delete(address)
  }

  def getWitnessVote(address: UInt160): Option[WitnessVote] = {
    witnessVoteStore.get(address)
  }

  def createWitnessVote(address: UInt160, vote: WitnessVote): Unit = {
    witnessVoteStore.set(address, vote)
  }

  // Previous active producer
  def getPreviousWitnessList(): Option[WitnessList] = previousWitnessStore.get()
  def setPreviousWitnessList(wl: WitnessList): Unit = previousWitnessStore.set(wl)

  // current active producer
  def getCurrentWitnessList(): Option[WitnessList] = currentWitnessStore.get()
  def setCurrentWitnessList(wl: WitnessList): Unit = currentWitnessStore.set(wl)

  // next active producer
  def getPendingWitnessList(): Option[WitnessList] = pendingWitnessStore.get()
  def setPendingWitnessList(wl: WitnessList): Unit = pendingWitnessStore.set(wl)

  def setProposal(p: Proposal): Unit = proposalStore.set(p.proposalID, p)
  def getProposal(proposalID: UInt256): Option[Proposal] = proposalStore.get(proposalID)
  def deleteProposal(proposalID: UInt256) = proposalStore.delete(proposalID)

  def getAllProposal(): ArrayBuffer[Proposal] = {
    proposalStore.getLists(Array(StoreType.Data.id.toByte, DataType.Proposal.id.toByte))
  }

  def addProposalVote(vote: ProposalVote) = {
    proposalVoteListStore.set(getProposalVoteList().add(vote))
  }
  def getProposalVoteList(): ProposalVoteList = proposalVoteListStore.get.getOrElse(new ProposalVoteList(Array.empty))
  def deleteProposalVote(proposalID: UInt256) = {
    val votes = getProposalVoteList()
    proposalVoteListStore.set(votes.remove(proposalID))
  }

  def getBlockCountLastWeek(): WitnessBlockCount = {
    witnessBlockCountLastWeekStore.get.getOrElse(new WitnessBlockCount(mutable.Map.empty))
  }

  def getLastWeekValidVoters(): Array[UInt160] = {
    witnessBlockCountLastWeekStore.get.map(_.getTop(consensusSettings.witnessNum)).getOrElse(Array.empty)
  }

  def getBlockCountThisWeek(): WitnessBlockCount = {
    witnessBlockCountThisWeekStore.get.getOrElse(new WitnessBlockCount(mutable.Map.empty))
  }

  def setWitnessBlockCountNewWeek() = {
    witnessBlockCountLastWeekStore.set(witnessBlockCountThisWeekStore.get().getOrElse(new WitnessBlockCount(mutable.Map.empty)))
    witnessBlockCountThisWeekStore.set(new WitnessBlockCount(mutable.Map.empty))
  }

  def witnessBlockCountAdd(producer: UInt160) = {
    val thisWeek = getBlockCountThisWeek()
    thisWeek.increase(producer)
    witnessBlockCountThisWeekStore.set(thisWeek)
  }

  def startTracking(): DataBase = {
    new DataBase(settings, consensusSettings, db, tracking.newTracking)
  }

  // start new session
  def startSession(): Unit = {
    tracking.newSession()
  }

  // undo all operations in the latest session
  def rollBack(): Unit = {
    tracking.rollBack()
  }

  // commit all operations in sessions whose revision is equal to or larger than the specified revision
  def commit(revision: Long): Unit = {
    tracking.commit(revision)
  }

  // apply changes
  def commit(): Unit = {
    tracking.commit()
  }

  // return latest revision
  def revision(): Long = {
    tracking.revision()
  }

  def close(): Unit = {
    db.close()
  }
}
