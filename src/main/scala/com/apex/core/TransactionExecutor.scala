package com.apex.core

import com.apex.crypto.UInt256
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
import java.math.BigInteger
import org.apache.commons.lang3.ArrayUtils.getLength
import org.apache.commons.lang3.ArrayUtils.isEmpty
//import com.apex.utils.ByteUtil.toBI

object TransactionExecutor {
  private val logger = LoggerFactory.getLogger("execute")
  private val stateLogger = LoggerFactory.getLogger("state")
}

class TransactionExecutor(var tx: Transaction,
                          //var coinbase: Array[Byte],
                          var track: DataBase,
                          //var blockStore: BlockStore,
                          //var programInvokeFactory: ProgramInvokeFactory,
                          var currentBlock: Block,
                          //val listener: EthereumListener,
                          val gasUsedInTheBlock: BigInt = 0
                          //val vmHook: VMHook
                         ) {
  //this.m_endGas = toBI(tx.getGasLimit)
  //this.vmHook = if (isNull(vmHook)) VMHook.EMPTY else vmHook
  //withCommonConfig(CommonConfig.getDefault)

  private val vmSettings = ContractSettings(0, false, false, false, false, true, false, false, false, false, false, false)
  //private[core] var config = null

  //private[core] var commonConfig: CommonConfig = null
  //private[core] var blockchainConfig: BlockchainConfig = null

  private var cacheTrack = track //track.startTracking
  private var readyToExecute = false
  private var execError: String = null
  private var receipt: TransactionReceipt = null
  private var result = new ProgramResult
  private var vm: VM = null
  private var program: Program = null
  private[core] var precompiledContract: PrecompiledContract = null
  private[core] var m_endGas = tx.gasLimit
  private[core] var basicTxCost: Long = 0
  //private[core] var logs = null
  //private val touchedAccounts = new ByteArraySet
  private[core] var localCall = false
  //final private var vmHook = null

  //  def this(tx: Transaction, coinbase: Array[Byte], track: DataBase, blockStore: BlockStore, currentBlock: Block) {
  //    this(tx, coinbase, track, blockStore, currentBlock, 0, VMHook.EMPTY)
  //  }
  //
  //  def this(tx: Transaction, coinbase: Array[Byte], track: DataBase,
  //           blockStore: BlockStore,
  //           currentBlock: Block,
  //           //listener: EthereumListener,
  //           gasUsedInTheBlock: Long) {
  //    this(tx, coinbase, track, blockStore, currentBlock,
  //      //listener,
  //      gasUsedInTheBlock, VMHook.EMPTY)
  //  }

//  def withCommonConfig(commonConfig: CommonConfig): TransactionExecutor = {
//    this.commonConfig = commonConfig
//    this.config = commonConfig.systemProperties
//    this.blockchainConfig = config.getBlockchainConfig.getConfigForBlock(currentBlock.getNumber)
//    this
//  }

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
    basicTxCost = tx.transactionCost(/*config.getBlockchainConfig,*/ currentBlock)
    if (localCall) {
      readyToExecute = true
      return
    }
    val txGasLimit = tx.gasLimit  //new BigInteger(1, tx.gasLimit)
    val curBlockGasLimit = currentBlock.header.gasLimit
    val cumulativeGasReached = (txGasLimit + gasUsedInTheBlock > curBlockGasLimit)
    if (cumulativeGasReached) {
      execError("Too much gas used in this block")
      //execError(String.format("Too much gas used in this block: Require: %s Got: %s", new BigInteger(1, currentBlock.header.gasLimit).longValue - toBI(tx.gasLimit).longValue, toBI(tx.gasLimit).longValue))
      //return
    }
    if (txGasLimit.compareTo(BigInteger.valueOf(basicTxCost)) < 0) {
      execError("Not enough gas for transaction execution")
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
    val senderBalance = track.getBalance(tx.sender())
    //    if (!isCovers(senderBalance, totalCost)) {
    //      execError(String.format("Not enough cash: Require: %s, Sender cash: %s", totalCost, senderBalance))
    //      return
    //    }
    //    if (!blockchainConfig.acceptTransactionSignature(tx)) {
    //      execError("Transaction signature not accepted ")
    //      return
    //    }
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
    precompiledContract = PrecompiledContracts.getContractForAddress(DataWord.of(targetAddress.data), vmSettings)
    if (precompiledContract != null) {
      val requiredGas = precompiledContract.getGasForData(tx.data)
      val spendingGas = BigInteger.valueOf(requiredGas).add(BigInteger.valueOf(basicTxCost))
      if (!localCall && m_endGas.compareTo(spendingGas) < 0) { // no refund
        // no endowment
        execError("Out of Gas calling precompiled contract")
        //execError("Out of Gas calling precompiled contract 0x" + toHexString(targetAddress) + ", required: " + spendingGas + ", left: " + m_endGas)
        m_endGas = BigInteger.ZERO
        return
      }
      else {
        m_endGas = m_endGas -spendingGas
        // FIXME: save return for vm trace
        val out = precompiledContract.execute(tx.data)
        if (!out._1) {
          execError("Error executing precompiled contract")
          //execError("Error executing precompiled contract 0x" + toHexString(targetAddress))
          m_endGas = BigInteger.ZERO
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
        this.program = new Program(vmSettings,  //track.getCodeHash(targetAddress),
          code, programInvoke/*, tx, config, vmHook*/) //.withCommonConfig(commonConfig)
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
      DataWord.ZERO,
      DataWord.of(tx.gasPrice.value),
      DataWord.of(Int.MaxValue),
      DataWord.ZERO,
      data,
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.ZERO,
      track,
      null,
      null)
  }

  private def create(): Unit = {
    val newContractAddress = tx.getContractAddress

      //    val existingAddr = cacheTrack.getAccountState(newContractAddress)
      //    if (existingAddr != null && existingAddr.isContractExist(blockchainConfig)) {
      //      //execError("Trying to create a contract with existing contract address: 0x" + toHexString(newContractAddress))
      //      execError("Trying to create a contract with existing contract address")
      //      m_endGas = BigInteger.ZERO
      //      return
      //    }

    //In case of hashing collisions (for TCK tests only), check for any balance before createAccount()
    val oldBalance = track.getBalance(newContractAddress.get)
    //cacheTrack.createAccount(tx.getContractAddress)
    if (oldBalance.isDefined)
      cacheTrack.addBalance(newContractAddress.get, oldBalance.get)
    if (vmSettings.eip161) cacheTrack.increaseNonce(newContractAddress.get)
    if (tx.data.data.length == 0) {
      m_endGas = m_endGas - basicTxCost
      result.spendGas(basicTxCost)
    }
    else {
      val programInvoke = createInvoker(Array.empty)
      //val programInvoke = programInvokeFactory.createProgramInvoke(tx, currentBlock, cacheTrack, track, blockStore)
      this.vm = new VM(vmSettings, VMHook.EMPTY)
      this.program = new Program(vmSettings, tx.data, programInvoke /*, tx, config, vmHook */) //.withCommonConfig(commonConfig)
      // reset storage if the contract with the same address already exists
      // TCK test case only - normally this is near-impossible situation in the real network
      // TODO make via Trie.clear() without keyset
      //            ContractDetails contractDetails = program.getStorage().getContractDetails(newContractAddress);
      //            for (DataWord key : contractDetails.getStorageKeys()) {
      //                program.storageSave(key, DataWord.ZERO);
      //            }
    }
    val endowment = tx.amount
    //transfer(cacheTrack, tx.sender(), newContractAddress, endowment)
    cacheTrack.transfer(tx.sender(), newContractAddress.get, endowment)
    //touchedAccounts.add(newContractAddress)
  }

  def go(): Unit = {
    val EMPTY_BYTE_ARRAY = new Array[Byte](0)
    if (!readyToExecute) return
    try
        if (vm != null) { // Charge basic cost of the transaction
          program.spendGas(tx.transactionCost(/*config.getBlockchainConfig, */ currentBlock), "TRANSACTION COST")
          //if (config.playVM)
            vm.play(program)
          result = program.getResult
          m_endGas = tx.gasLimit - program.getResult.getGasUsed
          if (tx.isContractCreation && !result.isRevert) {
            val returnDataGasValue = getLength(program.getResult.getHReturn) * GasCost.CREATE_DATA
            if (m_endGas.compareTo(BigInteger.valueOf(returnDataGasValue)) < 0) { // Not enough gas to return contract code
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
    catch {
      case e: Throwable =>
        // TODO: catch whatever they will throw on you !!!
        //            https://github.com/ethereum/cpp-ethereum/blob/develop/libethereum/Executive.cpp#L241
        rollback()
        m_endGas = BigInteger.ZERO
        execError(e.getMessage)
    }
  }

  private def rollback(): Unit = {
    //cacheTrack.rollback()
    // remove touched account
    //touchedAccounts.remove(if (tx.isContractCreation) tx.getContractAddress else tx.getReceiveAddress)
  }

//  def finalization: TransactionExecutionSummary = {
//    if (!readyToExecute) return null
//    val summaryBuilder = TransactionExecutionSummary.builderFor(tx).gasLeftover(m_endGas).logs(result.getLogInfoList).result(result.getHReturn)
//    if (result != null) { // Accumulate refunds for suicides
//      result.addFutureRefund(result.getDeleteAccounts.size * config.getBlockchainConfig.getConfigForBlock(currentBlock.getNumber).getGasCost.getSUICIDE_REFUND)
//      val gasRefund = Math.min(Math.max(0, result.getFutureRefund), getGasUsed / 2)
//      val addr = if (tx.isContractCreation) tx.getContractAddress else tx.toPubKeyHash
//      m_endGas = m_endGas.add(BigInteger.valueOf(gasRefund))
//      summaryBuilder.gasUsed(toBI(result.getGasUsed)).gasRefund(toBI(gasRefund)).deletedAccounts(result.getDeleteAccounts).internalTransactions(result.getInternalTransactions)
//      val contractDetails = track.getContractDetails(addr)
//      if (contractDetails != null) {
//        // TODO
//        //                summaryBuilder.storageDiff(track.getContractDetails(addr).getStorage());
//        //
//        //                if (program != null) {
//        //                    summaryBuilder.touchedStorage(contractDetails.getStorage(), program.getStorageDiff());
//        //                }
//      }
//      if (result.getException != null) summaryBuilder.markAsFailed
//    }
//    val summary = summaryBuilder.build
//    // Refund for gas leftover
//    track.addBalance(tx.sender(), summary.getLeftover.add(summary.getRefund))
//    //TransactionExecutor.logger.info("Pay total refund to sender: [{}], refund val: [{}]", toHexString(tx.getSender), summary.getRefund)
//    // Transfer fees to miner
//    track.addBalance(coinbase, summary.getFee)
//    //touchedAccounts.add(coinbase)
//    //TransactionExecutor.logger.info("Pay fees to miner: [{}], feesEarned: [{}]", toHexString(coinbase), summary.getFee)
//    if (result != null) {
//      logs = result.getLogInfoList
//      // Traverse list of suicides
//      import scala.collection.JavaConversions._
//      for (address <- result.getDeleteAccounts) {
//        track.delete(address.getLast20Bytes)
//      }
//    }
//    //    if (blockchainConfig.eip161) {
//    //      import scala.collection.JavaConversions._
//    //      for (acctAddr <- touchedAccounts) {
//    //        val state = track.getAccountState(acctAddr)
//    //        if (state != null && state.isEmpty) track.delete(acctAddr)
//    //      }
//    //    }
//    //listener.onTransactionExecuted(summary)
//    if (config.vmTrace && program != null && result != null) {
//      var trace = program.getTrace.result(result.getHReturn).error(result.getException).toString
//      if (config.vmTraceCompressed) trace = zipAndEncode(trace)
//      val txHash = toHexString(tx.getHash)
//      saveProgramTraceFile(config, txHash, trace)
//      //listener.onVMTraceCreated(txHash, trace)
//    }
//    summary
//  }

  def setLocalCall(localCall: Boolean): TransactionExecutor = {
    this.localCall = localCall
    this
  }

  def getReceipt: TransactionReceipt = {
    if (receipt == null) {
      receipt = TransactionReceipt(tx.id(),
        tx.txType,
        tx.fromPubKeyHash(),
        tx.toPubKeyHash,
        getGasUsed,
        gasUsedInTheBlock + getGasUsed,
        getResult.getHReturn,
        0)
        //      val totalGasUsed = gasUsedInTheBlock + getGasUsed
        //      receipt.setCumulativeGas(totalGasUsed)
        //      receipt.setTransaction(tx)
        //      receipt.setLogInfoList(getVMLogs)
        //      receipt.setGasUsed(getGasUsed)
        //      receipt.setExecutionResult(getResult.getHReturn)
        //      receipt.setError(execError)
        //      //   receipt.setPostTxState(track.getRoot()); // TODO later when RepositoryTrack.getRoot() is implemented
    }
    receipt
  }

  //def getVMLogs: util.List[LogInfo] = logs

  def getResult: ProgramResult = result

  def getGasUsed: BigInt = tx.gasLimit - m_endGas
}


