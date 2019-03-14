package com.apex.core

import java.io.{ByteArrayInputStream, DataInputStream}

import com.apex.consensus.RegisterData
import com.apex.crypto.{BinaryData, FixedNumber, UInt160, UInt256}
import com.apex.settings.ContractSettings
import com.apex.vm.hook.VMHook
import com.apex.vm._
import com.apex.vm.hook.VMHook
import com.apex.vm.program.Program
import com.apex.vm.program.ProgramResult
import com.apex.vm.program.invoke.{ProgramInvoke, ProgramInvokeImpl}
//import com.apex.vm.program.invoke.ProgramInvokeFactory
import org.slf4j.Logger
import org.slf4j.LoggerFactory
//import java.math.BigInteger
import org.apache.commons.lang3.ArrayUtils.getLength
import org.apache.commons.lang3.ArrayUtils.isEmpty

object TransactionExecutor {
  private val logger = LoggerFactory.getLogger("execute")
  private val stateLogger = LoggerFactory.getLogger("state")
}

class TransactionExecutor(val tx: Transaction,
                          val coinbase: UInt160,
                          val track: DataBase,
                          val stopTime: Long,
                          val blockTime: Long,
                          val blockIndex: Long,
                          val chain: Blockchain,
                          val isScheduleTx: Boolean = false) {

  private val vmSettings = ContractSettings(0, false, Int.MaxValue)

  private val cacheTrack = track.startTracking
  private var readyToExecute = false
  private var execError: String = ""
  private var receipt: TransactionReceipt = null
  private var result = new ProgramResult
  private var vm: VM = null
  private var program: Program = null
  private[core] var precompiledContract: PrecompiledContract = null
  private[core] var m_endGas: BigInt = tx.gasLimit
  private[core] var basicTxCost: Long = 0

  private def execError(err: String): Unit = {
    TransactionExecutor.logger.warn(err)
    execError = err
  }

  def isReadyToExecuted = readyToExecute

  /**
    * Do all the basic validation, if the executor
    * will be ready to run the transaction at the end
    * set readyToExecute = true
    */
  def init(): Unit = {
    basicTxCost = tx.transactionCost()
    if (tx.gasLimit < basicTxCost) {
      execError(s"Not enough gas for transaction execution: Require: ${basicTxCost} Got: ${tx.gasLimit}")
      //execError(String.format("Not enough gas for transaction execution: Require: %s Got: %s", basicTxCost, txGasLimit))
      return
    }
    if (!isScheduleTx) {
      val reqNonce = track.getNonce(tx.sender())
      val txNonce = tx.nonce
      if (reqNonce != txNonce) {
        execError(s"Invalid nonce: required: $reqNonce , tx.nonce: $txNonce")
        return
      }
    }
    val txGasCost = tx.gasPrice * tx.gasLimit
    val totalCost = tx.amount + txGasCost
    val senderBalance = track.getBalance(tx.sender()).getOrElse(FixedNumber.Zero)
    if (senderBalance.value < totalCost.value) {
      execError(s"Not enough cash: Require: ${totalCost}, Sender cash ${senderBalance}")
      return
    }
    readyToExecute = true
    if (!isScheduleTx)
      track.increaseNonce(tx.sender())
  }

  def execute(): Unit = {
    if (!readyToExecute) return

    val txGasCost = tx.gasPrice * tx.gasLimit
    track.addBalance(tx.sender(), -txGasCost)
    if (TransactionExecutor.logger.isInfoEnabled)
      TransactionExecutor.logger.info("Paying: txGasCost: [{}], gasPrice: [{}], gasLimit: [{}]", txGasCost, tx.gasPrice, tx.gasLimit)

    if (tx.isContractCreation)
      create()
    else
      call()
  }

  private def call(): Unit = {
    if (!readyToExecute) return
    val targetAddress = tx.toPubKeyHash
    precompiledContract = PrecompiledContracts.getContractForAddress(DataWord.of(targetAddress.data), vmSettings, cacheTrack, tx, blockTime)
    if (precompiledContract != null) {
      val requiredGas = precompiledContract.getGasForData(tx.data)
      val spendingGas = BigInt(requiredGas) + basicTxCost
      if (m_endGas < spendingGas) { // no refund
        // no endowment
        execError(s"Out of Gas calling precompiled contract ${targetAddress.address} required: $spendingGas  left: $m_endGas")
        //execError("Out of Gas calling precompiled contract 0x" + toHexString(targetAddress) + ", required: " + spendingGas + ", left: " + m_endGas)
        m_endGas = 0
        return
      }
      else {
        m_endGas = m_endGas - spendingGas
        // FIXME: save return for vm trace
        val out = precompiledContract.execute(tx.data)
        if (!out._1) {
          val reason = new String(out._2)
          execError(s"Error executing precompiled contract ${targetAddress.address}, causes: ${reason.toString}")
          //execError("Error executing precompiled contract 0x" + toHexString(targetAddress))
          m_endGas = 0
          return
        }
      }
    }
    else {
      val code = track.getCode(targetAddress)
      if (isEmpty(code)) {
        m_endGas = m_endGas - basicTxCost
        result.spendGas(basicTxCost)
      }
      else {
        val programInvoke = createInvoker(tx.data)
        //val programInvoke = programInvokeFactory.createProgramInvoke(tx, currentBlock, cacheTrack, track, blockStore)
        this.vm = new VM(vmSettings, VMHook.EMPTY)
        this.program = new Program(vmSettings, code, programInvoke, stopTime) //.withCommonConfig(commonConfig)
      }
    }
    val endowment = tx.amount
    //transfer(cacheTrack, tx.sender(), targetAddress, endowment)
    cacheTrack.transfer(tx.sender(), targetAddress, endowment)
    //touchedAccounts.add(targetAddress)
  }

  private def createInvoker(data: Array[Byte]): ProgramInvoke = {
    new ProgramInvokeImpl(DataWord.of(if (tx.isContractCreation()) tx.getContractAddress().get.data else tx.toPubKeyHash.data), DataWord.of(tx.sender().data), DataWord.of(tx.sender().data), DataWord.ZERO, DataWord.of(tx.gasPrice.value), DataWord.of(tx.gasLimit), DataWord.ZERO, data, DataWord.ZERO, DataWord.of(coinbase), DataWord.of(blockTime), DataWord.of(blockIndex), DataWord.of(tx.gasLimit), cacheTrack, track, chain)
  }

  private def create(): Unit = {
    val newContractAddress = tx.getContractAddress

    //In case of hashing collisions (for TCK tests only), check for any balance before createAccount()
    val oldBalance = track.getBalance(newContractAddress.get)
    //cacheTrack.createAccount(tx.getContractAddress)
    if (oldBalance.isDefined)
      cacheTrack.addBalance(newContractAddress.get, oldBalance.get)
    cacheTrack.increaseNonce(newContractAddress.get)
    if (tx.data.data.length == 0) {
      m_endGas = m_endGas - basicTxCost
      result.spendGas(basicTxCost)
    }
    else {
      val programInvoke = createInvoker(Array.empty)
      this.vm = new VM(vmSettings, VMHook.EMPTY)
      this.program = new Program(vmSettings, tx.data, programInvoke, stopTime) //.withCommonConfig(commonConfig)
    }
    val endowment = tx.amount
    cacheTrack.transfer(tx.sender(), newContractAddress.get, endowment)
  }

  def go(): Unit = {
    val EMPTY_BYTE_ARRAY = new Array[Byte](0)
    if (!readyToExecute) return
    try {
      if (vm != null) { // Charge basic cost of the transaction
        program.spendGas(tx.transactionCost(), "TRANSACTION COST")
        //if (config.playVM)
        vm.play(program)
        result = program.getResult
        m_endGas = tx.gasLimit - program.getResult.getGasUsed
        if (tx.isContractCreation && !result.isRevert) {
          val returnDataGasValue = getLength(program.getResult.getHReturn) * GasCost.CREATE_DATA
          if (m_endGas < BigInt(returnDataGasValue)) { // Not enough gas to return contract code
            result.setHReturn(EMPTY_BYTE_ARRAY)
          }
          else { // Contract successfully created
            m_endGas = m_endGas - returnDataGasValue
            cacheTrack.saveCode(tx.getContractAddress.get, result.getHReturn)
          }
        }
        if (result.getException != null || result.isRevert) {
          //result.getDeleteAccounts.clear()
          result.getLogInfoList.clear()
          result.resetFutureRefund()
          rollback()
          if (result.getException != null) throw result.getException
          else execError("REVERT opcode executed")
        }
        else {
          //touchedAccounts.addAll(result.getTouchedAccounts)
          cacheTrack.commit()
        }
      }
      else cacheTrack.commit()
    }
    catch {
      case e: Throwable =>
        // TODO: catch whatever they will throw on you !!!
        //            https://github.com/ethereum/cpp-ethereum/blob/develop/libethereum/Executive.cpp#L241
        rollback()
        m_endGas = 0
        execError(e.getMessage)
    }
  }

  private def rollback(): Unit = {
    cacheTrack.rollBack()
    // remove touched account
    //touchedAccounts.remove(if (tx.isContractCreation) tx.getContractAddress else tx.getReceiveAddress)
  }

  def finalization(): TransactionExecutionSummary = {
    if (!readyToExecute) return null
    val summaryBuilder = new TransactionExecutionSummary.Builder(tx)
    summaryBuilder.gasLeftover(m_endGas)
    summaryBuilder.logs(result.getLogInfoList)
    summaryBuilder.result(result.getHReturn)
    if (result != null) { // Accumulate refunds for suicides
      result.addFutureRefund(result.getDeleteAccounts.size * GasCost.SUICIDE_REFUND)
      //result.addFutureRefund(result.getDeleteAccounts.size * config.getBlockchainConfig.getConfigForBlock(currentBlock.getNumber).getGasCost.getSUICIDE_REFUND)
      val gasRefund = BigInt(0).max(result.getFutureRefund).min(getGasUsed.longValue() / 2)

      val addr = if (tx.isContractCreation) tx.getContractAddress else tx.toPubKeyHash
      m_endGas = m_endGas + gasRefund

      summaryBuilder.gasUsed(result.getGasUsed)
      summaryBuilder.gasRefund(gasRefund)
      summaryBuilder.deletedAccounts(result.getDeleteAccounts)
      //summaryBuilder.internalTransactions(result.getInternalTransactions)

      if (result.getException != null)
        summaryBuilder.markAsFailed
    }
    val summary = summaryBuilder.build
    // Refund for gas leftover
    track.addBalance(tx.sender(), summary.getLeftover + summary.getRefund)
    //TransactionExecutor.logger.info("Pay total refund to sender: [{}], refund val: [{}]", tx.sender().address, summary.getRefund)
    // Transfer fees to miner
    track.addBalance(coinbase, summary.getFee)
    //touchedAccounts.add(coinbase)
    TransactionExecutor.logger.info("Pay fees to miner: [{}], feesEarned: [{}]", coinbase.address, summary.getFee.longValue())
    summary
  }

  def getReceipt: TransactionReceipt = {
    if (receipt == null) {
      receipt = TransactionReceipt(tx.id(),
        tx.txType,
        tx.from,
        tx.toPubKeyHash,
        blockIndex,
        getGasUsed,
        //gasUsedInTheBlock + getGasUsed,
        getResult.getHReturn,
        0,
        execError)
    }
    receipt
  }

  //def getVMLogs: util.List[LogInfo] = logs

  def getResult: ProgramResult = result

  def getGasUsed: BigInt = tx.gasLimit - m_endGas

  private def extractData(data: Array[Byte]): Transaction = {
    import com.apex.common.Serializable._
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    val version = is.readInt
    val txType = TransactionType(is.readByte)
    val from = UInt160.deserialize(is)
    val toPubKeyHash = UInt160.deserialize(is)
    val amount = FixedNumber.deserialize(is)
    val nonce = is.readLong
    val metaData = is.readByteArray()
    val gasPrice = FixedNumber.deserialize(is)
    val gasLimit = BigInt(is.readByteArray())
    val executeTime = is.readLong()
    new Transaction(txType, from, toPubKeyHash, amount, nonce, metaData, gasPrice, gasLimit, null, version, executeTime)
  }
}


