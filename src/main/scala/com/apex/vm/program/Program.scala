/*
 * Copyright (c) [2016] [ <ether.camp> ]
 * This file is part of the ethereumJ library.
 *
 * The ethereumJ library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The ethereumJ library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with the ethereumJ library. If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Program.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-28 下午1:43@version: 1.0
 *
 */

package com.apex.vm.program

import java.time.Instant
import java.util

import com.apex.common.ApexLogging
import com.apex.core.{Account, DataBase}
import com.apex.crypto.{Crypto, FixedNumber, UInt160}
import com.apex.settings.ContractSettings
import com.apex.vm.DataWord
import com.apex.vm.exceptions._
import com.apex.vm.hook.VMHook
import com.apex.vm.program.invoke.{ProgramInvoke, ProgramInvokeImpl}
import com.apex.vm.program.listener.{CompositeProgramListener, ProgramListenerAware, ProgramStorageChangeListener}
import com.apex.vm.program.trace.{ProgramTrace, ProgramTraceListener}
import org.apex.vm.{OpCache, OpCode}

class Program(settings: ContractSettings,
              ops: Array[Byte],
              invoke: ProgramInvoke,
              stopTime: Long,
              vmHook: VMHook = VMHook.EMPTY) extends ApexLogging {
  private final val MAX_STACKSIZE = 1024
  /**
    * This attribute defines the number of recursive calls allowed in the EVM
    * Note: For the JVM to reach this level without a StackOverflow exception,
    * ethereumj may need to be started with a JVM argument to increase
    * the stack size. For example: -Xss10m
    */
  private final val MAX_DEPTH = 1024

  private var listener: ProgramOutListener = _
  private var traceListener: ProgramTraceListener = new ProgramTraceListener(settings.vmTrace)
  private val storageDiffListener = new ProgramStorageChangeListener
  private val programListener = new CompositeProgramListener

  private val result = new ProgramResult
  private var trace = new ProgramTrace

  private val memory = setupProgramListener(new Memory)
  private val stack: Stack = setupProgramListener(new Stack)

  private var programPrecompile: ProgramPrecompile = _

  private var returnDataBuffer: Array[Byte] = _
  private var previouslyExecutedOp: Byte = _
  private var stopped: Boolean = _
  private var lastOp: Byte = _
  private var pc: Int = _

  import com.apex.vm._

  def getCurrentOp(): Byte = if (ops.isEmpty) 0 else ops(pc)

  def setLastOp(op: Byte): Unit = lastOp = op

  /**
    * Should be set only after the OP is fully executed.
    */
  def setPreviouslyExecutedOp(op: Byte): Unit = previouslyExecutedOp = op

  /**
    * Returns the last fully executed OP.
    */
  def getPreviouslyExecutedOp: Byte = this.previouslyExecutedOp

  def stackPush(data: Array[Byte]): Unit = {
    stackPush(DataWord.of(data))
  }

  def stackPushZero(): Unit = {
    stackPush(DataWord.ZERO)
  }

  def stackPushOne(): Unit = {
    val stackWord = DataWord.ONE
    stackPush(stackWord)
  }

  def stackPush(stackWord: DataWord): Unit = {
    verifyStackOverflow(0, 1) //Sanity Check
    stack.push(stackWord)
  }

  def getMemSize: Int = memory.size

  def memorySave(addrB: DataWord, value: DataWord): Unit = {
    memory.write(addrB.intValue, value.getData, value.getData.length, false)
  }

  def memorySaveLimited(addr: Int, data: Array[Byte], dataSize: Int): Unit = {
    memory.write(addr, data, dataSize, true)
  }

  def memorySave(addr: Int, value: Array[Byte]): Unit = {
    memory.write(addr, value, value.length, false)
  }

  def memoryExpand(outDataOffs: DataWord, outDataSize: DataWord): Unit = {
    if (!outDataSize.isZero) memory.extend(outDataOffs.intValue, outDataSize.intValue)
  }

  /**
    * Allocates a piece of memory and stores value at given offset address
    *
    * @param addr      is the offset address
    * @param allocSize size of memory needed to write
    * @param value     the data to write to memory
    */
  def memorySave(addr: Int, allocSize: Int, value: Array[Byte]): Unit = {
    memory.extendAndWrite(addr, allocSize, value)
  }

  def memoryLoad(addr: DataWord): DataWord = memory.readWord(addr.intValue)

  def memoryLoad(address: Int): DataWord = memory.readWord(address)

  def memoryChunk(offset: Int, size: Int): Array[Byte] = memory.read(offset, size)

  def getStack: Stack = stack

  def getGasLong: Long = (invoke.getGasLimitLong - result.getGasUsed).longValue()

  def getGas: DataWord = DataWord.of(invoke.getGasLimitLong - result.getGasUsed)

  def getPC: Int = pc

  def setPC(pc: DataWord): Unit = setPC(pc.intValue)

  def setPC(pc: Int): Unit = {
    this.pc = pc
    if (this.pc >= ops.length) {
      stop()
    }
  }

  def isStopped: Boolean = stopped

  def stop(): Unit = stopped = true

  def setHReturn(buff: Array[Byte]): Unit = result.setHReturn(buff)

  def step(): Unit = setPC(pc + 1)

  def sweep(n: Int): Array[Byte] = {
    if (pc + n > ops.length) stop()
    val data = util.Arrays.copyOfRange(ops, pc, pc + n)
    pc += n
    if (pc >= ops.length) stop()
    data
  }

  def getPrevHash: DataWord = invoke.getPrevHash

  def getCoinbase: DataWord = invoke.getCoinbase

  def getTimestamp: DataWord = invoke.getTimestamp

  def getNumber: DataWord = invoke.getNumber

  //def getDifficulty: DataWord = invoke.getDifficulty

  def getGasLimit: DataWord = invoke.getGaslimit

  def isStaticCall: Boolean = invoke.isStaticCall

  def getResult: ProgramResult = result

  def setRuntimeFailure(e: RuntimeException): Unit = {
    getResult.setException(e)
  }

  def getCallDeep: Int = invoke.getCallDeep

  def stackPop: DataWord = stack.pop

  def verifyStackSize(stackSize: Int) = {
    if (stack.size < stackSize) {
      throw StackTooSmallException(stackSize, stack.size)
    }
  }

  def verifyStackOverflow(argsReqs: Int, returnReqs: Int): Unit = {
    if ((stack.size - argsReqs + returnReqs) > MAX_STACKSIZE) {
      throw StackTooLargeException(MAX_STACKSIZE)
    }
  }

  def getCode: Array[Byte] = ops

  def getCodeAt(address: DataWord): Array[Byte] = {
    invoke.getDataBase.getCode(address.toUInt160)
  }

  def getCodeHashAt(address: DataWord): Array[Byte] = {
    invoke.getDataBase.getCodeHash(address.toUInt160)
  }

  def getOwnerAddress: DataWord = invoke.getOwnerAddress

  def getBlockHash(index: Long): DataWord = {
    if (index < getNumber.longValue)
      DataWord.of(invoke.getChain.getBlock(index).get.id())
    else
      DataWord.ZERO
  }

  def getBalance(address: DataWord): DataWord = {
    val balance = getStorage.getBalance(address.toUInt160).map(_.value).getOrElse(BigInt(0))
    DataWord.of(balance)
  }

  def getOriginAddress: DataWord = invoke.getOriginAddress

  def getCallerAddress: DataWord = invoke.getCallerAddress

  def getGasPrice: DataWord = invoke.getMinGasPrice

  def getCallValue: DataWord = invoke.getCallValue

  def getDataSize: DataWord = invoke.getDataSize

  def getDataValue(index: DataWord): DataWord = invoke.getDataValue(index)

  def getDataCopy(offset: DataWord, length: DataWord): Array[Byte] = invoke.getDataCopy(offset, length)

  def getReturnDataBufferSize: DataWord = DataWord.of(getReturnDataBufferSizeI)

  def getStorage: DataBase = invoke.getDataBase

  /**
    * Create contract for OpCode#CREATE
    *
    * @param value    Endowment
    * @param memStart Code memory offset
    * @param memSize  Code memory size
    */
  def createContract(value: DataWord, memStart: DataWord, memSize: DataWord): Unit = {
    returnDataBuffer = null // reset return buffer right before the call

    val senderAddress = getOwnerAddress.toUInt160
    val endowment = value.value
    if (verifyCall(senderAddress, endowment)) {
      val nonce = getStorage.getNonce(senderAddress).toBytes
      val contractAddress = Crypto.calcNewAddr(senderAddress, nonce)
      val programCode = memoryChunk(memStart.intValue, memSize.intValue)
      createContractImpl(value, programCode, contractAddress)
    }
  }

  /**
    * Create contract for OpCode#CREATE2
    *
    * @param value    Endowment
    * @param memStart Code memory offset
    * @param memSize  Code memory size
    * @param salt     Salt, used in contract address calculation
    */
  def createContract2(value: DataWord, memStart: DataWord, memSize: DataWord, salt: DataWord): Unit = {
    returnDataBuffer = null // reset return buffer right before the call

    val senderAddress = getOwnerAddress.toUInt160
    val endowment = value.value
    if (verifyCall(senderAddress, endowment)) {
      val programCode = memoryChunk(memStart.intValue, memSize.intValue)
      val contractAddress = Crypto.calcSaltAddr(senderAddress, programCode, salt.getData)
      createContractImpl(value, programCode, contractAddress)
    }
  }

  /**
    * Verifies CREATE attempt
    */
  private def verifyCall(senderAddress: UInt160, endowment: BigInt): Boolean = {
    if (getCallDeep == MAX_DEPTH) {
      stackPushZero()
      false
    } else if (getStorage.getBalance(senderAddress).forall(_.value < endowment)) {
      stackPushZero()
      false
    } else {
      true
    }
  }

  /**
    * All stages required to create contract on provided address after initial check
    *
    * @param value       Endowment
    * @param programCode Contract code
    * @param newAddress  Contract address
    */
  private def createContractImpl(value: DataWord, programCode: Array[Byte], newAddress: UInt160): Unit = { // [1] LOG, SPEND GAS
    import com.apex.vm._
    val senderAddress = getOwnerAddress.toUInt160
    if (log.isInfoEnabled) log.info(s"creating a new contract inside contract run: [${senderAddress.toString}]")

    //  actual gas subtract
    spendGas(getGas.longValue, "internal call")

    //  [2] CREATE THE CONTRACT ADDRESS
    val contractAlreadyExists = getStorage.accountExists(newAddress)

    if (byTestingSuite) { // This keeps track of the contracts created for a test
      getResult.addCallCreate(programCode, Array.empty, getGas.getNoLeadZeroesData, value.getNoLeadZeroesData)
    }

    val track = getStorage.startTracking
    val oldBalance = track.getBalance(newAddress)
    track.createAccount(newAddress)
    track.increaseNonce(newAddress)
    oldBalance.foreach(track.addBalance(newAddress, _))

    // [4] TRANSFER THE BALANCE
    val endowment = value.value
    var newBalance = FixedNumber.Zero.value
    if (!byTestingSuite) {
      track.addBalance(senderAddress, -endowment)
      newBalance = track.addBalance(newAddress, endowment)
    }

    // [5] COOK THE INVOKE AND EXECUTE
    val nonce = getStorage.getNonce(senderAddress).toBytes
    // create internal transaction
    var result: ProgramResult = ProgramResult.createEmpty
    val programInvoke = new ProgramInvokeImpl(
      DataWord.of(newAddress), getOriginAddress, getOwnerAddress,
      DataWord.of(newBalance), getGas, getGasPrice, value, null,
      getPrevHash, getCoinbase, getTimestamp, getNumber,
      track, invoke.getOrigDataBase, invoke.getBlockStore, invoke.getChain,
      getCallDeep + 1, false, byTestingSuite)

    if (contractAlreadyExists) {
      result.setException(new BytecodeExecutionException(s"Trying to create a contract with existing contract address: ${newAddress.toString}"))
    } else {
      val program = new Program(settings, programCode, programInvoke, stopTime)
      result = VM.play(settings, vmHook, program)
    }

    // 4. CREATE THE CONTRACT OUT OF RETURN
    val code = result.getHReturn
    val storageCost = code.length * GasCost.CREATE_DATA
    val afterSpend = programInvoke.getGaslimit.longValue - storageCost - result.getGasUsed
    if (afterSpend < 0) {
      result.setException(Program.notEnoughSpendingGas("No gas to return just created contract", storageCost, programInvoke, result))
    } else if (code.length > settings.maxContractSize) {
      result.setException(Program.notEnoughSpendingGas("Contract size too large: ", storageCost, programInvoke, result))
    } else if (!result.isRevert) {
      result.spendGas(storageCost)
      track.saveCode(newAddress, code)
    }

    getResult.merge(result)
    if (result.getException != null || result.isRevert) {
      log.debug(s"contract run halted by Exception: contract: [${newAddress.toString}], exception: [${result.getException}]")

//      result.rejectInternalTransactions()
//      track.rollback()
      stackPushZero()
      if (result.getException == null) {
        returnDataBuffer = result.getHReturn
      }
    } else {
      if (!byTestingSuite) {
        track.commit()
      }

      // IN SUCCESS PUSH THE ADDRESS INTO THE STACK
      stackPush(DataWord.of(newAddress))
    }

    if (result.getException == null) {
      // 5. REFUND THE REMAIN GAS
      val refund = getGas.longValue - result.getGasUsed.toLong
      if (refund > 0) {
        refundGas(refund, "remain gas from the internal call")
        if (log.isInfoEnabled) {
          log.info(s"The remaining gas is refunded, account: [${getOwnerAddress.toUInt160.toString}], gas: [${refund}] ")
        }
      }
      //    touchedAccounts.add(newAddress)
    }
  }

  private def getReturnDataBufferSizeI = {
    if (returnDataBuffer == null) 0 else returnDataBuffer.length
  }

  def getReturnDataBufferData(off: DataWord, size: DataWord): Array[Byte] = {
    if (off.intValueSafe.toLong + size.intValueSafe > getReturnDataBufferSizeI) {
      null
    } else {
      if (returnDataBuffer == null) {
        new Array[Byte](0)
      } else {
        util.Arrays.copyOfRange(returnDataBuffer, off.intValueSafe, off.intValueSafe + size.intValueSafe)
      }
    }
  }

  def storageLoad(key: DataWord): DataWord = {
    val address = getOwnerAddress.toUInt160
    val value = invoke.getDataBase.getContractState(address, key.data)
    DataWord.of(value)
  }

  def storageSave(word1: DataWord, word2: DataWord): Unit = {
    storageSave(word1.getData, word2.getData)
  }

  def storageSave(key: Array[Byte], value: Array[Byte]): Unit = {
    val address = getOwnerAddress.toUInt160
    invoke.getDataBase.saveContractState(address, key, value)
  }

  /**
    * @return current Storage data for key
    */
  def getCurrentValue(key: DataWord): DataWord = {
    val address = getOwnerAddress.toUInt160
    val value = invoke.getDataBase.getContractState(address, key.data)
    DataWord.of(value)
  }

  /*
     * Original storage value at the beginning of current frame execution
     * For more info check EIP-1283 https://eips.ethereum.org/EIPS/eip-1283
     * @return Storage data at the beginning of Program execution
     */
  def getOriginalValue(key: DataWord): DataWord = {
    val address = getOwnerAddress.toUInt160
    val value = invoke.getOrigDataBase.getContractState(address, key.data)
    DataWord.of(value)
  }

  def fullTrace(): Unit = {
    //    if (log.isTraceEnabled || listener != null) {
    //      val stackData = new StringBuilder
    //      for (i <- 0 to stack.size - 1) {
    //        stackData.append(" ").append(stack.get(i))
    //        if (i < stack.size - 1) stackData.append("\n")
    //      }
    //      if (stackData.length > 0) stackData.insert(0, "\n")
    //    }
  }

  def saveOpTrace(): Unit = {
    if (pc < ops.length) {
      trace.addOp(ops(pc), pc, getCallDeep, getGas.value, traceListener.resetActions)
    }
  }

  def getTrace: ProgramTrace = trace

  def spendGas(gasValue: Long, cause: String): Unit = {
    if (log.isDebugEnabled) {
      log.debug(s"[${invoke.hashCode}] Spent for cause: [$cause], gas: [$gasValue]")
    }
    if (getGasLong < gasValue) {
      throw Program.notEnoughSpendingGas(cause, gasValue, invoke, result)
    }
    result.spendGas(gasValue)
  }

  def checkStopTime(): Unit = {
    if (Instant.now.toEpochMilli > stopTime) {
      log.error("Error: vm execution timeout")
      throw OutOfBlockTimeException(s"Error: vm execution timeout $stopTime")
    }
  }

  def spendAllGas(): Unit = {
    spendGas(getGas.longValue, "Spending all remaining")
  }

  def refundGas(gasValue: Long, cause: String): Unit = {
    log.info(s"[${invoke.hashCode}] Refund for cause: [${cause}], gas: [${gasValue}]")
    result.refundGas(gasValue)
  }

  def futureRefundGas(gasValue: Long): Unit = {
    log.info(s"Future refund added: [$gasValue]")
    result.addFutureRefund(gasValue)
  }

  def resetFutureRefund(): Unit = {
    result.resetFutureRefund()
  }

  def addListener(outListener: ProgramOutListener): Unit = {
    listener = outListener
  }

  def verifyJumpDest(nextPC: DataWord): Int = {
    if (nextPC.bytesOccupied > 4) {
      throw Program.badJumpDestination(-1)
    }

    val ret = nextPC.intValue
    if (!getProgramPrecompile.hasJumpDest(ret)) {
      throw Program.badJumpDestination(ret)
    }
    ret
  }

  def getProgramPrecompile(): ProgramPrecompile = {
    if (programPrecompile == null) {
      programPrecompile = ProgramPrecompile.compile(ops)
    }
    programPrecompile
  }

  def callToPrecompiledAddress(msg: MessageCall, contract: PrecompiledContract): Unit = {
    returnDataBuffer = null // reset return buffer right before the call

    if (getCallDeep == MAX_DEPTH) {
      stackPushZero()
      refundGas(msg.gas.longValue, " call deep limit reach")
    } else {
      val track = getStorage.startTracking

      val senderAddress = getOwnerAddress.toUInt160
      val codeAddress = msg.codeAddress.toUInt160
      val stateLess = OpCache.fromCode(msg.opCode.value).callIsStateless
      val contextAddress = if (stateLess) senderAddress else codeAddress
      if (track.getBalance(senderAddress).forall(_.value < msg.endowment.value)) {
        stackPushZero()
        refundGas(msg.gas.longValue, "refund gas from message call")
      } else {
        val data = memoryChunk(msg.inDataOffs.intValue, msg.inDataSize.intValue)
        // Charge for endowment - is not reversible by rollback
        track.transfer(senderAddress, contextAddress, msg.endowment.value)

        if (byTestingSuite) { // This keeps track of the calls created for a test
          getResult.addCallCreate(data, msg.codeAddress.getLast20Bytes, msg.gas.getNoLeadZeroesData, msg.endowment.getNoLeadZeroesData)
          stackPushOne()
        } else {
          val requiredGas = contract.getGasForData(data)
          if (requiredGas > msg.gas.longValue) {
            refundGas(0, "call pre-compiled") //matches cpp logic
            stackPushZero()
            //track.rollback()
          } else {
            if (log.isDebugEnabled) {
              log.debug(s"Call ${contract.getClass.getSimpleName}(data = ${data.toHex})")
            }

            val (succeed, result) = contract.execute(data)
            if (succeed) { // success
              refundGas(msg.gas.longValue - requiredGas, "call pre-compiled")
              stackPushOne()
              returnDataBuffer = result
              track.commit()
            }
            else { // spend all gas on failure, push zero and revert state changes
              refundGas(0, "call pre-compiled")
              stackPushZero()
              //track.rollback()
            }
          }
        }
      }
    }
  }

  def callToAddress(msg: MessageCall): Unit = {
    returnDataBuffer = null // reset return buffer right before the call

    if (getCallDeep == MAX_DEPTH) {
      stackPushZero()
      refundGas(msg.gas.longValue, " call deep limit reach")
    } else {
      val data = memoryChunk(msg.inDataOffs.intValue, msg.inDataSize.intValue)

      // FETCH THE SAVED STORAGE
      val codeAddress = msg.codeAddress.toUInt160
      val senderAddress = getOwnerAddress.toUInt160
      val stateLess = OpCache.fromCode(msg.opCode.value).callIsStateless
      val contextAddress = if (stateLess) senderAddress else codeAddress

      if (log.isInfoEnabled) {
        log.info(s"${msg.opCode.name} for existing contract: address: [${contextAddress.toString}], outDataOffs: [${msg.outDataOffs.longValue}], outDataSize: [${msg.outDataSize.longValue}]  ")
      }

      val track = getStorage.startTracking
      if (track.getBalance(senderAddress).forall(_.value < msg.endowment.value)) {
        stackPushZero()
        refundGas(msg.gas.longValue, "refund gas from message call")
      } else {
        var contextBalance = BigInt(0)
        if (byTestingSuite) { // This keeps track of the calls created for a test
          getResult.addCallCreate(data, contextAddress.data, msg.gas.getNoLeadZeroesData, msg.endowment.getNoLeadZeroesData)
        } else {
          track.addBalance(senderAddress, -msg.endowment.value)
          contextBalance = track.addBalance(contextAddress, msg.endowment.value)
        }

        //TODO
        // CREATE CALL INTERNAL TRANSACTION

        val programCode = getStorage.getCode(codeAddress)
        if (!programCode.isEmpty) {
          val op = OpCache.fromCode(msg.opCode.value)
          val isDelegate = op.callIsDelegate
          val callerAddress = if (isDelegate) getCallerAddress else getOwnerAddress
          val callValue = if (isDelegate) getCallValue else msg.endowment
          val programInvoke = new ProgramInvokeImpl(
            DataWord.of(contextAddress), getOriginAddress, callerAddress,
            DataWord.of(contextBalance), msg.gas, getGasPrice, callValue,
            data, getPrevHash, getCoinbase, getTimestamp, getNumber,
            track, invoke.getOrigDataBase, invoke.getBlockStore, invoke.getChain,
            getCallDeep + 1, op.callIsStatic || isStaticCall, byTestingSuite)
          val program = new Program(settings, programCode, programInvoke, stopTime)
          val result = VM.play(settings, vmHook, program)
          getTrace.merge(program.getTrace)
          getResult.merge(result)

          if (result.getException != null || result.isRevert) {
            log.debug(s"contract run halted by Exception: contract: [${contextAddress.toString}], exception: [${result.getException}]")
            //TODO reject internal transaction

            //track.rollback()
            stackPushZero()
          } else {
            // 4. THE FLAG OF SUCCESS IS ONE PUSHED INTO THE STACK
            track.commit()
            stackPushOne()
          }

          if (result.getException == null) {
            if (byTestingSuite) {
              log.info("Testing run, skipping storage diff listener")
            } else {
              //            if (Arrays.equals(transaction.getReceiveAddress, internalTx.getReceiveAddress)) {
              //              storageDiffListener.merge(program.getStorageDiff)
              //            }
            }

            // 3. APPLY RESULTS: result.getHReturn() into out_memory allocated
            val buffer = result.getHReturn
            val offset = msg.outDataOffs.intValue
            val size = msg.outDataSize.intValue
            memorySaveLimited(offset, buffer, size)
            returnDataBuffer = buffer

            // 5. REFUND THE REMAIN GAS
            val refund = msg.gas.value - result.getGasUsed
            if (refund.signum > 0) {
              refundGas(refund.longValue, "remaining gas from the internal call")
              if (log.isInfoEnabled) {
                log.info(s"The remaining gas refunded, account: [${senderAddress.toString}], gas: [${refund}] ")
              }
            }
          }
        } else {
          // 4. THE FLAG OF SUCCESS IS ONE PUSHED INTO THE STACK
          track.commit()
          stackPushOne()
          // 5. REFUND THE REMAIN GAS
          refundGas(msg.gas.longValue, "remaining gas from the internal call")
        }
      }
    }
  }

  def suicide(obtainerAddress: DataWord): Unit = {
    val owner = getOwnerAddress.toUInt160
    val obtainer = obtainerAddress.toUInt160
    val balance = getStorage.getBalance(owner).map(_.value).getOrElse(BigInt(0))
    if (log.isInfoEnabled) {
      log.info(s"Transfer to: [${obtainer.toString}] heritage: [${balance}]")
    }
    if (owner.equals(obtainer)) {
      getStorage.addBalance(owner, -balance)
    } else {
      getStorage.transfer(owner, obtainer, balance)
    }
    getResult.addDeleteAccount(owner)
  }

  def byTestingSuite: Boolean = invoke.byTestingSuite

  private def setupProgramListener[T <: ProgramListenerAware](programListenerAware: T) = {
    if (programListener.isEmpty) {
      programListener.addListener(traceListener)
      programListener.addListener(storageDiffListener)
    }
    programListenerAware.setProgramListener(programListener)
    programListenerAware
  }
}

object Program {

  def notEnoughOpGas(op: OpCode.Value, opGas: DataWord, programGas: DataWord): OutOfGasException = {
    notEnoughOpGas(op, opGas.longValue, programGas.longValue)
  }

  def notEnoughOpGas(op: OpCode.Value, opGas: BigInt, programGas: BigInt): OutOfGasException = {
    notEnoughOpGas(op, opGas.toLong, programGas.toLong)
  }

  def notEnoughOpGas(op: OpCode.Value, opGas: Long, programGas: Long) = {
    OutOfGasException(s"Not enough gas for '$op' operation executing: opGas[$opGas], programGas[$programGas]")
  }


  def notEnoughSpendingGas(cause: String, gasValue: Long, invoke: ProgramInvoke, result: ProgramResult) = {
    OutOfGasException(s"Not enough gas for '$cause' cause spending: invokeGas[${invoke.getGaslimit.longValue}], gas[$gasValue], usedGas[${result.getGasUsed}]")
  }

  def gasOverflow(actualGas: BigInt, gasLimit: BigInt) = {
    OutOfGasException(s"Gas value overflow: actualGas[$actualGas], gasLimit[$gasLimit]")
  }

  def returnDataCopyIllegalBoundsException(off: DataWord, size: DataWord, returnDataSize: Long) = {
    ReturnDataCopyIllegalBoundsException(s"Illegal RETURNDATACOPY arguments: offset ($off) + size (${size.intValue}) > RETURNDATASIZE ($returnDataSize)")
  }

  def staticCallModificationException() = {
    StaticCallModificationException("Attempt to call a state modifying opcode inside STATICCALL")
  }

  def badJumpDestination(pc: Int) = {
    BadJumpDestinationException(pc)
  }

  def invalidOpCode(code: OpCode.Value) = {
    IllegalOperationException(code.value)
  }
}

trait ProgramOutListener {
  def output(out: String): Unit
}