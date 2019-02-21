package com.apex.core

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
                          val timeStamp: Long,
                          val blockIndex: Long,
                          val chain: Blockchain) {
  //this.m_endGas = toBI(tx.getGasLimit)
  //this.vmHook = if (isNull(vmHook)) VMHook.EMPTY else vmHook
  //withCommonConfig(CommonConfig.getDefault)

  private val vmSettings = ContractSettings(0, false, Int.MaxValue)
  //private[core] var config = null

  //private[core] var commonConfig: CommonConfig = null
  //private[core] var blockchainConfig: BlockchainConfig = null

  private var cacheTrack = track.startTracking
  private var readyToExecute = false
  private var execError: String = ""
  private var receipt: TransactionReceipt = null
  private var result = new ProgramResult
  private var vm: VM = null
  private var program: Program = null
  private[core] var precompiledContract: PrecompiledContract = null
  private[core] var m_endGas: BigInt = tx.gasLimit
  private[core] var basicTxCost: Long = 0
  //private[core] var logs = null
  //private val touchedAccounts = new ByteArraySet
  private[core] var localCall = false
  //final private var vmHook = null

  private def execError(err: String): Unit = {
    TransactionExecutor.logger.warn(err)
    execError = err
  }

  /**
    * Do all the basic validation, if the executor
    * will be ready to run the transaction at the end
    * set readyToExecute = true
    */
  def init(): Unit = {
    basicTxCost = tx.transactionCost()
    if (localCall) {
      readyToExecute = true
      return
    }
    val txGasLimit = tx.gasLimit
    if (txGasLimit < basicTxCost) {
      execError(s"Not enough gas for transaction execution: Require: ${basicTxCost} Got: ${txGasLimit}")
      //execError(String.format("Not enough gas for transaction execution: Require: %s Got: %s", basicTxCost, txGasLimit))
      return
    }
    val reqNonce = track.getNonce(tx.sender())
    val txNonce = tx.nonce
    if (reqNonce != txNonce) {
      execError(s"Invalid nonce: required: $reqNonce , tx.nonce: $txNonce")
      return
    }
    val txGasCost = tx.gasPrice * txGasLimit
    val totalCost = tx.amount + txGasCost
    val senderBalance = track.getBalance(tx.sender()).getOrElse(FixedNumber.Zero)
    if (senderBalance.value < totalCost.value) {
      execError(s"Not enough cash: Require: ${totalCost}, Sender cash ${senderBalance}")
      return
    }
    readyToExecute = true
  }

  def execute(): Unit = {
    if (!readyToExecute) return
    if (!localCall) {
      track.increaseNonce(tx.sender())
      val txGasLimit = tx.gasLimit
      val txGasCost = tx.gasPrice * txGasLimit
      track.addBalance(tx.sender(), -txGasCost)
      if (TransactionExecutor.logger.isInfoEnabled)
        TransactionExecutor.logger.info("Paying: txGasCost: [{}], gasPrice: [{}], gasLimit: [{}]", txGasCost, tx.gasPrice, txGasLimit)
    }
    if (tx.isContractCreation)
      create()
    else
      call()
  }

  private def call(): Unit = {
    if (!readyToExecute) return
    val targetAddress = tx.toPubKeyHash
    precompiledContract = PrecompiledContracts.getContractForAddress(DataWord.of(targetAddress.data), vmSettings, cacheTrack, tx, timeStamp)
    if (precompiledContract != null) {
      val requiredGas = precompiledContract.getGasForData(tx.data)
      val spendingGas = BigInt(requiredGas) + basicTxCost
      if (!localCall && m_endGas < spendingGas) { // no refund
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
    new ProgramInvokeImpl(
      DataWord.of(if (tx.isContractCreation()) tx.getContractAddress().get.data else tx.toPubKeyHash.data),
      DataWord.of(tx.sender().data),
      DataWord.of(tx.sender().data),
      DataWord.ZERO,            // balance
      DataWord.of(tx.gasPrice.value),
      DataWord.of(tx.gasLimit),
      DataWord.ZERO,            // callValue
      data,                     // msgData
      DataWord.ZERO,            // lastHash
      DataWord.of(coinbase),    // coinbase
      DataWord.of(timeStamp),   // timestamp
      DataWord.of(blockIndex),  // number
      cacheTrack,               // dataBase
      track,                    // origDataBase
      null,    
      chain                     // chain
    )
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
    //transfer(cacheTrack, tx.sender(), newContractAddress, endowment)
    cacheTrack.transfer(tx.sender(), newContractAddress.get, endowment)
    //touchedAccounts.add(newContractAddress)
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
            //     if (!blockchainConfig.getConstants.createEmptyContractOnOOG) {
            //       program.setRuntimeFailure(Program.Exception.notEnoughSpendingGas("No gas to return just created contract", returnDataGasValue, program))
            //       result = program.getResult
            //     }
            result.setHReturn(EMPTY_BYTE_ARRAY)
          }
          //            else if (getLength(result.getHReturn) > blockchainConfig.getConstants.getMAX_CONTRACT_SZIE) { // Contract size too large
          //              program.setRuntimeFailure(Program.Exception.notEnoughSpendingGas("Contract size too large: " + getLength(result.getHReturn), returnDataGasValue, program))
          //              result = program.getResult
          //              result.setHReturn(EMPTY_BYTE_ARRAY)
          //            }
          else { // Contract successfully created
            m_endGas = m_endGas - returnDataGasValue
            cacheTrack.saveCode(tx.getContractAddress.get, result.getHReturn)
          }
        }
        //    val err = config.getBlockchainConfig.getConfigForBlock(currentBlock.getNumber).validateTransactionChanges(blockStore, currentBlock, tx, null)
        //    if (err != null)
        //       program.setRuntimeFailure(new RuntimeException("Transaction changes validation failed: " + err))
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

  def setLocalCall(localCall: Boolean): TransactionExecutor = {
    this.localCall = localCall
    this
  }

  def getReceipt: TransactionReceipt = {
    if (receipt == null) {
      receipt = TransactionReceipt(tx.id(),
        tx.txType,
        tx.from,
        tx.toPubKeyHash,
        getGasUsed,
        //gasUsedInTheBlock + getGasUsed,
        getResult.getHReturn,
        0,
        execError)
      //      val totalGasUsed = gasUsedInTheBlock + getGasUsed
      //      receipt.setCumulativeGas(totalGasUsed)
      //      receipt.setTransaction(tx)
      //      receipt.setLogInfoList(getVMLogs)
      //      receipt.setGasUsed(getGasUsed)
      //      receipt.setExecutionResult(getResult.getHReturn)
      //receipt.error = execError     //setError(execError)
      //      //   receipt.setPostTxState(track.getRoot()); // TODO later when RepositoryTrack.getRoot() is implemented
    }
    receipt
  }

  //def getVMLogs: util.List[LogInfo] = logs

  def getResult: ProgramResult = result

  def getGasUsed: BigInt = tx.gasLimit - m_endGas
}


