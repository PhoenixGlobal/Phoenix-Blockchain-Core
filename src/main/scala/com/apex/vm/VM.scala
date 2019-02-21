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
 * @author: ruixiao.xiao@chinapex.com: 18-11-27 下午5:17@version: 1.0
 *
 */

package com.apex.vm

import com.apex.common.ApexLogging
import com.apex.crypto.{Crypto, UInt160}
import com.apex.exceptions.InvalidOperationException
import com.apex.settings.ContractSettings
import com.apex.vm.hook.VMHook
import com.apex.vm.program.{Program, ProgramResult}
import com.apex.vm.program.trace.LogInfo
import org.apex.vm._

import scala.collection.mutable.ListBuffer

class VM(settings: ContractSettings, hook: VMHook) extends com.apex.common.ApexLogging {
  private val MAX_MEM_SIZE = BigInt(Integer.MAX_VALUE)

  private val opValidators = OpCode.emptyValidators

  private val hooks = Array(VMHook.EMPTY, hook).filterNot(_.isEmpty)
  private val hasHooks = hooks.length > 0

  private val dumpBlock = settings.dumpBlock
  private val vmTrace = settings.vmTrace
  private var vmCounter = 0

  import VM._

  def play(program: Program): Unit = {
    if (!program.byTestingSuite) {
      try {
        if (hasHooks) hooks.foreach(_.startPlay(program))
        import java.util.Date
        val s = new Date().getTime()
        while (!program.isStopped) {
          step(program)
          /*overwritestep(program)*/
        }
        println("end:" + (new Date().getTime() - s))
      } catch {
        case e: RuntimeException => program.setRuntimeFailure(e)
        case e: StackOverflowError => {
          log.error(s"\n !!! StackOverflowError: update your java run command with -Xss2M (-Xss8M for tests) !!! ", e)
          System.exit(-1)
        }
      } finally {
        if (hasHooks) {
          hooks.foreach(_.stopPlay(program))
        }
      }
    }
  }

  def step(program: Program): Unit = {
    if (vmTrace) {
      program.saveOpTrace()
    }

    try {
      val op = OpCache.fromCode(program.getCurrentOp)
      validateOp(op, program)
      program.setLastOp(op.code.value)
      program.verifyStackSize(op.require)
      program.verifyStackOverflow(op.require, op.ret)
      val oldMemSize = program.getMemSize
      val stack = program.getStack

      var hint = ""
      // parameters for logging
      val callGas = 0L
      val memWords = 0L

      var gasCost = op.tier.toLong
      val gasBefore = program.getGasLong
      //      val stepBefore = program.getPC
      var adjustedCallGas: DataWord = null

      // Calculate fees and spend gas
      op.code match {
        case OpCode.STOP => gasCost = GasCost.STOP
        case OpCode.SUICIDE => {
          gasCost = GasCost.SUICIDE
          val suicideAddress = stack.get(stack.size - 1).toUInt160
          if (!program.getStorage.accountExists(suicideAddress)) {
            gasCost += GasCost.NEW_ACCT_SUICIDE
          }
        }
        case OpCode.SSTORE => {
          var currentValue = program.getCurrentValue(stack.peek)
          if (currentValue == null) currentValue = DataWord.ZERO
          val newValue = stack.get(stack.size - 2)
          if (newValue == currentValue) {
            gasCost = GasCost.REUSE_SSTORE
          } else {
            var origValue = program.getOriginalValue(stack.peek)
            if (origValue == null) origValue = DataWord.ZERO
            if (currentValue == origValue) {
              if (origValue.isZero) {
                gasCost = GasCost.SET_SSTORE
              } else {
                gasCost = GasCost.CLEAR_SSTORE
                if (newValue.isZero) {
                  program.futureRefundGas(GasCost.REFUND_SSTORE)
                }
              }
            } else {
              gasCost = GasCost.REUSE_SSTORE
              if (!origValue.isZero) {
                if (currentValue.isZero) {
                  program.futureRefundGas(-GasCost.REFUND_SSTORE)
                } else if (newValue.isZero) {
                  program.futureRefundGas(GasCost.REFUND_SSTORE)
                }
              }
              if (origValue == newValue) {
                if (origValue.isZero) {
                  program.futureRefundGas(GasCost.SET_SSTORE - GasCost.REUSE_SSTORE)
                } else {
                  program.futureRefundGas(GasCost.CLEAR_SSTORE - GasCost.REUSE_SSTORE)
                }
              }
            }
          }
        }
        case OpCode.SLOAD => gasCost = GasCost.SLOAD
        case OpCode.BALANCE => gasCost = GasCost.BALANCE
        case OpCode.MSTORE => gasCost += calcMemGas(oldMemSize, memNeeded(stack.peek, DataWord.of(32)), 0)
        case OpCode.MSTORE8 => gasCost += calcMemGas(oldMemSize, memNeeded(stack.peek, DataWord.ONE), 0)
        case OpCode.MLOAD => gasCost += calcMemGas(oldMemSize, memNeeded(stack.peek, DataWord.of(32)), 0)
        case OpCode.RETURN | OpCode.REVERT => {
          gasCost += GasCost.STOP + calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 2)), 0)
        }
        case OpCode.SHA3 => {
          gasCost = GasCost.SHA3 + calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 2)), 0)
          val size = stack.get(stack.size - 2)
          val chunkUsed = VM.getSizeInWords(size.longValueSafe)
          gasCost += chunkUsed * GasCost.SHA3_WORD
        }
        case OpCode.CALLDATACOPY | OpCode.RETURNDATACOPY => {
          gasCost += calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 3)), stack.get(stack.size - 3).longValueSafe)
        }
        case OpCode.CODECOPY => {
          gasCost += calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 3)), stack.get(stack.size - 3).longValueSafe)
        }
        case OpCode.EXTCODESIZE => gasCost = GasCost.EXT_CODE_SIZE
        case OpCode.EXTCODECOPY => {
          gasCost = GasCost.EXT_CODE_COPY + calcMemGas(oldMemSize, memNeeded(stack.get(stack.size - 2), stack.get(stack.size - 4)), stack.get(stack.size - 4).longValueSafe)
        }
        case OpCode.EXTCODEHASH => gasCost = GasCost.EXT_CODE_HASH
        case OpCode.CALL | OpCode.CALLCODE | OpCode.DELEGATECALL | OpCode.STATICCALL => {
          gasCost = GasCost.CALL
          val callGasWord = stack.get(stack.size - 1)
          val callAddressWord = stack.get(stack.size - 2)
          val value = if (op.callHasValue) {
            stack.get(stack.size - 3)
          } else {
            DataWord.ZERO
          }

          //check to see if account does not exist and is not a precompiled contract
          if (op.code == OpCode.CALL) {
            if (isDeadAccount(program, callAddressWord.toUInt160) && !value.isZero) {
              gasCost += GasCost.NEW_ACCT_CALL
            }
          }

          //TODO #POC9 Make sure this is converted to BigInt (256num support)
          if (!value.isZero) {
            gasCost += GasCost.VT_CALL
          }

          val opOff = if (op.callHasValue) 4 else 3
          val in = memNeeded(stack.get(stack.size - opOff), stack.get(stack.size - opOff - 1))
          // in offset+size
          val out = memNeeded(stack.get(stack.size - opOff - 2), stack.get(stack.size - opOff - 3)) // out offset+size
          gasCost += calcMemGas(oldMemSize, in.max(out), 0)

          if (gasCost > program.getGas.longValueSafe) {
            throw Program.notEnoughOpGas(op.code, callGasWord, program.getGas)
          }

          val gasLeft = program.getGas
          val subResult = gasLeft.sub(DataWord.of(gasCost))
          //adjustedCallGas = blockchainConfig.getCallGas(op, callGasWord, subResult)
          adjustedCallGas = getCallGas(op.code, callGasWord, subResult)
          gasCost += adjustedCallGas.longValueSafe
        }
        case OpCode.CREATE => gasCost = GasCost.CREATE + calcMemGas(oldMemSize, memNeeded(stack.get(stack.size - 2), stack.get(stack.size - 3)), 0)
        case OpCode.CREATE2 => {
          val codeSize = stack.get(stack.size - 3)
          gasCost = GasCost.CREATE +
            calcMemGas(oldMemSize, memNeeded(stack.get(stack.size - 2), codeSize), 0) +
            VM.getSizeInWords(codeSize.longValueSafe) * GasCost.SHA3_WORD
        }
        case OpCode.LOG0 | OpCode.LOG1 | OpCode.LOG2 | OpCode.LOG3 | OpCode.LOG4 => {
          val dataSize = stack.get(stack.size - 2).value
          val dataCost = dataSize * BigInt(GasCost.LOG_DATA_GAS)
          if (program.getGas.value < dataCost) {
            throw Program.notEnoughOpGas(op.code, dataCost, program.getGas.value)
          }
          val nTopics = op.code.value - OpCode.LOG0.value
          gasCost = GasCost.LOG_GAS + GasCost.LOG_TOPIC_GAS * nTopics +
            GasCost.LOG_DATA_GAS * stack.get(stack.size - 2).longValue +
            calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 2)), 0)
        }
        case OpCode.EXP => {
          val exp = stack.get(stack.size - 2)
          val bytesOccupied = exp.bytesOccupied
          gasCost = GasCost.EXP_GAS + GasCost.EXP_BYTE_GAS * bytesOccupied
        }
        case _ => {}
      }

      //DEBUG System.out.println(" OP IS " + op.name() + " GASCOST IS " + gasCost + " NUM IS " + op.asInt());
      program.spendGas(gasCost, op.code.name)

      program.checkStopTime()

      // Log debugging line for VM
      if (program.getNumber.intValue == dumpBlock) {
        dumpLine(op.code, gasBefore, gasCost + callGas, memWords, program)
      }

      if (hasHooks) {
        hooks.foreach(_.step(program, op.code))
      }

      def binaryOp1(op: String, action: (DataWord, DataWord) => DataWord) = {
        val word1 = program.stackPop
        val word2 = program.stackPop

        if (log.isInfoEnabled) {
          hint = s"${word1.value} $op ${word2.value}"
        }

        val result = action(word1, word2)
        program.stackPush(result)
        program.step()
      }

      def binaryOp2(action: (DataWord, DataWord) => DataWord) = {
        val word1 = program.stackPop
        val word2 = program.stackPop

        val result = action(word1, word2)
        if (log.isInfoEnabled) {
          hint = s"${result.value}"
        }
        program.stackPush(result)
        program.step()
      }

      def ternaryOp(action: (DataWord, DataWord, DataWord) => DataWord) = {
        val word1 = program.stackPop
        val word2 = program.stackPop
        val word3 = program.stackPop
        val result = action(word1, word2, word3)
        program.stackPush(result)
        program.step()
      }

      // Execute operation
      op.code match {
        /**
          * Stop and Arithmetic Operations
          */
        case OpCode.STOP => {
          program.setHReturn(Array.empty)
          program.stop()
        }
        case OpCode.ADD => {
          binaryOp1("+", (word1, word2) => word1.add(word2))
        }
        case OpCode.MUL => {
          binaryOp1("*", (word1, word2) => word1.mul(word2))
        }
        case OpCode.SUB => {
          binaryOp1("-", (word1, word2) => word1.sub(word2))
        }
        case OpCode.DIV => {
          binaryOp1("/", (word1, word2) => word1.div(word2))
        }
        case OpCode.SDIV => {
          binaryOp1("/", (word1, word2) => word1.sDiv(word2))
        }
        case OpCode.MOD => {
          binaryOp1("%", (word1, word2) => word1.mod(word2))
        }
        case OpCode.EXP => {
          binaryOp1("**", (word1, word2) => word1.exp(word2))
        }
        case OpCode.SIGNEXTEND => {
          val word1 = program.stackPop
          val k = word1.value

          if (k < _32_) {
            val word2 = program.stackPop
            if (log.isInfoEnabled) hint = s"$word1  ${word2.value}"

            val extendResult = word2.signExtend(k.byteValue)
            program.stackPush(extendResult)
          }
          program.step()
        }
        case OpCode.NOT => {
          val word1 = program.stackPop
          val bnotWord = word1.bnot

          if (log.isInfoEnabled) hint = s"${bnotWord.value}"

          program.stackPush(bnotWord)
          program.step()
        }
        case OpCode.LT => {
          // TODO: can be improved by not using BigInt
          binaryOp1("<", (word1, word2) => {
            if (word1.value < word2.value) DataWord.ONE else DataWord.ZERO
          })
        }
        case OpCode.SLT => {
          // TODO: can be improved by not using BigInt
          binaryOp1("<", (word1, word2) => {
            if (word1.sValue < word2.sValue) DataWord.ONE else DataWord.ZERO
          })
        }
        case OpCode.SGT => {
          // TODO: can be improved by not using BigInt
          binaryOp1(">", (word1, word2) => {
            if (word1.sValue > word2.sValue) DataWord.ONE else DataWord.ZERO
          })
        }
        case OpCode.GT => {
          // TODO: can be improved by not using BigInt
          binaryOp1(">", (word1, word2) => {
            if (word1.value > word2.value) DataWord.ONE else DataWord.ZERO
          })
        }
        case OpCode.EQ => {
          binaryOp1("==", (word1, word2) => {
            if (word1.xor(word2).isZero) DataWord.ONE else DataWord.ZERO
          })
        }
        case OpCode.ISZERO => {
          val word1 = program.stackPop
          if (word1.isZero) program.stackPush(DataWord.ONE)
          else program.stackPush(DataWord.ZERO)

          if (log.isInfoEnabled) hint = s"${word1.value}"

          program.step()
        }

        /**
          * Bitwise Logic Operations
          */
        case OpCode.AND => {
          binaryOp1("&&", (word1, word2) => word1.and(word2))
        }
        case OpCode.OR => {
          binaryOp1("||", (word1, word2) => word1.or(word2))
        }
        case OpCode.XOR => {
          binaryOp1("^", (word1, word2) => word1.xor(word2))
        }
        case OpCode.BYTE => {
          val word1 = program.stackPop
          val word2 = program.stackPop
          val result = if (word1.value < _32_) DataWord.of(word2.getData(word1.intValue)) else DataWord.ZERO

          if (log.isInfoEnabled) hint = s"${word1.value}"

          program.stackPush(result)
          program.step()
        }
        case OpCode.SHL => {
          binaryOp2((word1, word2) => word1.shiftLeft(word2))
        }
        case OpCode.SHR => {
          binaryOp2((word1, word2) => word1.shiftRight(word2))
        }
        case OpCode.SAR => {
          binaryOp2((word1, word2) => word1.shiftRightSigned(word2))
        }
        case OpCode.ADDMOD => {
          ternaryOp((word1, word2, word3) => word1.addMod(word2, word3))
        }
        case OpCode.MULMOD => {
          ternaryOp((word1, word2, word3) => word1.mulMod(word2, word3))
        }
        case OpCode.SHA3 => {
          binaryOp2((memOffsetData, lengthData) => {
            val buffer = program.memoryChunk(memOffsetData.intValueSafe, lengthData.intValueSafe)

            val encoded = Crypto.sha3(buffer)
            DataWord.of(encoded)
          })
        }

        /**
          * Environmental Information
          */
        case OpCode.ADDRESS => {
          val address = program.getOwnerAddress
          if (log.isInfoEnabled) {
            hint = s"address: ${address.getLast20Bytes.toHex}"
          }
          program.stackPush(address)
          program.step()
        }
        case OpCode.BALANCE => {
          val address = program.stackPop
          val balance = program.getBalance(address)
          if (log.isInfoEnabled) {
            hint = s"address: ${address.getLast20Bytes.toHex} balance: ${balance.toString}"
          }
          program.stackPush(balance)
          program.step()
        }
        case OpCode.ORIGIN => {
          val address = program.getOriginAddress
          if (log.isInfoEnabled) {
            hint = s"address: ${address.getLast20Bytes.toHex}"
          }
          program.stackPush(address)
          program.step()
        }
        case OpCode.CALLER => {
          val address = program.getCallerAddress
          if (log.isInfoEnabled) {
            hint = s"address: ${address.getLast20Bytes.toHex}"
          }
          program.stackPush(address)
          program.step()
        }
        case OpCode.CALLVALUE => {
          val value = program.getCallValue
          if (log.isInfoEnabled) {
            hint = s"value: ${value}"
          }
          program.stackPush(value)
          program.step()
        }
        case OpCode.CALLDATALOAD => {
          val dataOffs = program.stackPop
          val value = program.getDataValue(dataOffs)
          if (log.isInfoEnabled) {
            hint = s"data: ${value}"
          }
          program.stackPush(value)
          program.step()
        }
        case OpCode.CALLDATASIZE => {
          val size = program.getDataSize
          if (log.isInfoEnabled) {
            hint = s"size: ${size}"
          }
          program.stackPush(size)
          program.step()
        }
        case OpCode.CALLDATACOPY => {
          val memOffsetData = program.stackPop
          val dataOffsetData = program.stackPop
          val lengthData = program.stackPop
          val msgData = program.getDataCopy(dataOffsetData, lengthData)
          if (log.isInfoEnabled) {
            hint = s"data: ${msgData.toHex}"
          }
          program.memorySave(memOffsetData.intValueSafe, lengthData.intValueSafe, msgData)
          program.step()
        }
        case OpCode.RETURNDATASIZE => {
          val size = program.getReturnDataBufferSize
          if (log.isInfoEnabled) {
            hint = s"size: ${size}"
          }
          program.stackPush(size)
          program.step()
        }
        case OpCode.RETURNDATACOPY => {
          val memOffsetData = program.stackPop
          val dataOffsetData = program.stackPop
          val lengthData = program.stackPop
          val msgData = program.getReturnDataBufferData(dataOffsetData, lengthData)
          if (msgData == null) {
            throw Program.returnDataCopyIllegalBoundsException(dataOffsetData, lengthData, program.getReturnDataBufferSize.longValueSafe)
          }
          if (log.isInfoEnabled) {
            hint = s"data: ${msgData.toHex}"
          }
          program.memorySave(memOffsetData.intValueSafe, lengthData.intValueSafe, msgData)
          program.step()
        }
        case OpCode.CODESIZE | OpCode.EXTCODESIZE => {
          var length = 0
          if (op.code == OpCode.CODESIZE) {
            length = program.getCode.length
          } else {
            val address = program.stackPop
            length = program.getCodeAt(address).length
          }
          val codeLength = DataWord.of(length)

          if (log.isInfoEnabled) hint = s"size: $length"

          program.stackPush(codeLength)
          program.step()
        }
        case OpCode.CODECOPY | OpCode.EXTCODECOPY => {
          var fullCode = Array.emptyByteArray
          if (op.code == OpCode.CODECOPY) {
            fullCode = program.getCode
          }

          if (op.code == OpCode.EXTCODECOPY) {
            val address = program.stackPop
            fullCode = program.getCodeAt(address)
          }

          val memOffset = program.stackPop.intValueSafe
          val codeOffset = program.stackPop.intValueSafe
          val lengthData = program.stackPop.intValueSafe
          val sizeToBeCopied = if (codeOffset.toLong + lengthData > fullCode.length) {
            if (fullCode.length < codeOffset) 0 else fullCode.length - codeOffset
          } else {
            lengthData
          }

          val codeCopy = new Array[Byte](lengthData)

          if (codeOffset < fullCode.length) {
            System.arraycopy(fullCode, codeOffset, codeCopy, 0, sizeToBeCopied)
          }

          if (log.isInfoEnabled) hint = s"code: ${codeCopy.toHex}"

          program.memorySave(memOffset, lengthData, codeCopy)
          program.step()
        }
        case OpCode.EXTCODEHASH => {
          val address = program.stackPop
          val codeHash = program.getCodeHashAt(address)
          program.stackPush(codeHash)
          program.step()
        }
        case OpCode.GASPRICE => {
          val gasPrice = program.getGasPrice

          if (log.isInfoEnabled) hint = s"price: ${gasPrice.toString}"

          program.stackPush(gasPrice)
          program.step()
        }

        /**
          * Block Information
          */
        case OpCode.BLOCKHASH => {
          val blockIndex = program.stackPop.intValueSafe
          val blockHash = program.getBlockHash(blockIndex)
          if (log.isInfoEnabled) hint = s"blockHash: $blockHash"
          program.stackPush(blockHash)
          program.step()
        }
        case OpCode.COINBASE => {
          val coinbase = program.getCoinbase
          if (log.isInfoEnabled) {
            hint = s"coinbase: ${coinbase.getLast20Bytes.toHex}"
          }
          program.stackPush(coinbase)
          program.step()
        }
        case OpCode.TIMESTAMP => {
          val timestamp = program.getTimestamp
          if (log.isInfoEnabled) hint = s"timestamp: ${timestamp.value}"
          program.stackPush(timestamp)
          program.step()
        }
        case OpCode.NUMBER => {
          val number = program.getNumber
          if (log.isInfoEnabled) hint = s"number: ${number.value}"
          program.stackPush(number)
          program.step()
        }
        case OpCode.DIFFICULTY => {
          val difficulty = DataWord.ZERO  //program.getDifficulty
          if (log.isInfoEnabled) hint = s"difficulty: $difficulty"
          program.stackPush(difficulty)
          program.step()
        }
        case OpCode.GASLIMIT => {
          val gaslimit = program.getGasLimit
          if (log.isInfoEnabled) hint = s"gaslimit: $gaslimit"
          program.stackPush(gaslimit)
          program.step()
        }
        case OpCode.POP => {
          program.stackPop
          program.step()
        }
        case OpCode.DUP1 | OpCode.DUP2 | OpCode.DUP3 | OpCode.DUP4
             | OpCode.DUP5 | OpCode.DUP6 | OpCode.DUP7 | OpCode.DUP8
             | OpCode.DUP9 | OpCode.DUP10 | OpCode.DUP11 | OpCode.DUP12
             | OpCode.DUP13 | OpCode.DUP14 | OpCode.DUP15 | OpCode.DUP16 => {
          val n = op.code.value - OpCode.DUP1.value + 1
          val word_1 = stack.get(stack.size - n)
          program.stackPush(word_1)
          program.step()
        }
        case OpCode.SWAP1 | OpCode.SWAP2 | OpCode.SWAP3 | OpCode.SWAP4
             | OpCode.SWAP5 | OpCode.SWAP6 | OpCode.SWAP7 | OpCode.SWAP8
             | OpCode.SWAP9 | OpCode.SWAP10 | OpCode.SWAP11 | OpCode.SWAP12
             | OpCode.SWAP13 | OpCode.SWAP14 | OpCode.SWAP15 | OpCode.SWAP16 => {
          val n = op.code.value - OpCode.SWAP1.value + 2
          stack.swap(stack.size - 1, stack.size - n)
          program.step()
        }
        case OpCode.LOG1 | OpCode.LOG2 | OpCode.LOG3 | OpCode.LOG4 => {
          if (program.isStaticCall) {
            throw Program.staticCallModificationException
          }
          val address = program.getOwnerAddress

          val memStart = stack.pop
          val memOffset = stack.pop

          val nTopics = op.code.value - OpCode.LOG0.value

          val topics = ListBuffer.empty[DataWord]
          for (_ <- 1 to nTopics) {
            topics.append(stack.pop)
          }

          val data = program.memoryChunk(memStart.intValueSafe, memOffset.intValueSafe)

          val logInfo = LogInfo(address.getLast20Bytes, topics, data)

          if (log.isInfoEnabled) hint = logInfo.toString

          program.getResult.addLogInfo(logInfo)
          program.step()
        }
        case OpCode.MLOAD => {
          val address = program.stackPop
          val data = program.memoryLoad(address)

          if (log.isInfoEnabled) hint = s"data: $data"

          program.stackPush(data)
          program.step()
        }
        case OpCode.MSTORE => {
          val addr = program.stackPop
          val value = program.stackPop

          if (log.isInfoEnabled) hint = s"addr: $addr value: $value"

          program.memorySave(addr, value)
          program.step()
        }
        case OpCode.MSTORE8 => {
          val addr = program.stackPop
          val value = program.stackPop
          val byteVal = Array(value.getData(31))
          program.memorySave(addr.intValueSafe, byteVal)
          program.step()
        }
        case OpCode.SLOAD => {
          val key = program.stackPop
          var value = program.storageLoad(key)

          if (log.isInfoEnabled) hint = s"key: $key value: $value"

          if (value == null) value = key.and(DataWord.ZERO)

          program.stackPush(value)
          program.step()
        }
        case OpCode.SSTORE => {
          if (program.isStaticCall) {
            throw Program.staticCallModificationException
          }

          val addr = program.stackPop
          val value = program.stackPop

          if (log.isInfoEnabled) hint = s"[${program.getOwnerAddress.toPrefixString}] key: $addr value: $value"

          program.storageSave(addr, value)
          program.step()
        }
        case OpCode.JUMP => {
          val pos = program.stackPop
          val nextPC = program.verifyJumpDest(pos)

          if (log.isInfoEnabled) hint = s"~> $nextPC"

          program.setPC(nextPC)
        }
        case OpCode.JUMPI => {
          val pos = program.stackPop
          val cond = program.stackPop

          if (!cond.isZero) {
            val nextPC = program.verifyJumpDest(pos)
            if (log.isInfoEnabled) hint = s"~> $nextPC"
            program.setPC(nextPC)
          }
          else program.step()
        }
        case OpCode.PC => {
          val pc = program.getPC
          val pcWord = DataWord.of(pc)

          if (log.isInfoEnabled) hint = pcWord.toString

          program.stackPush(pcWord)
          program.step()
        }
        case OpCode.MSIZE => {
          val memSize = program.getMemSize
          val wordMemSize = DataWord.of(memSize)

          if (log.isInfoEnabled) hint = s"$memSize"

          program.stackPush(wordMemSize)
          program.step()
        }
        case OpCode.GAS => {
          val gas = program.getGas

          if (log.isInfoEnabled) hint = s"$gas"

          program.stackPush(gas)
          program.step()
        }
        case OpCode.PUSH1 | OpCode.PUSH2 | OpCode.PUSH3 | OpCode.PUSH4
             | OpCode.PUSH5 | OpCode.PUSH6 | OpCode.PUSH7 | OpCode.PUSH8
             | OpCode.PUSH9 | OpCode.PUSH10 | OpCode.PUSH11 | OpCode.PUSH12
             | OpCode.PUSH13 | OpCode.PUSH14 | OpCode.PUSH15 | OpCode.PUSH16
             | OpCode.PUSH17 | OpCode.PUSH18 | OpCode.PUSH19 | OpCode.PUSH20
             | OpCode.PUSH21 | OpCode.PUSH22 | OpCode.PUSH23 | OpCode.PUSH24
             | OpCode.PUSH25 | OpCode.PUSH26 | OpCode.PUSH27 | OpCode.PUSH28
             | OpCode.PUSH29 | OpCode.PUSH30 | OpCode.PUSH31 | OpCode.PUSH32 => {
          program.step()
          val nPush = op.code.value - OpCode.PUSH1.value + 1

          val data = program.sweep(nPush)

          if (log.isInfoEnabled) hint = s"${data.toHex}"

          program.stackPush(data)
        }
        case OpCode.JUMPDEST => {
          program.step()
        }
        case OpCode.CREATE => {
          if (program.isStaticCall) throw Program.staticCallModificationException

          val value = program.stackPop
          val inOffset = program.stackPop
          val inSize = program.stackPop

          if (log.isInfoEnabled) {
            log.info(s"[${program.getPC.formatted("%5s")}]    Op: [${op.code.name.formatted("%-12s")}]  Gas: [${program.getGas.value}] Deep: [${program.getCallDeep}]  Hint: [${hint}]")
          }

          program.createContract(value, inOffset, inSize)

          program.step()
        }
        case OpCode.CREATE2 => {
          if (program.isStaticCall) throw Program.staticCallModificationException

          val value = program.stackPop
          val inOffset = program.stackPop
          val inSize = program.stackPop
          val salt = program.stackPop

          if (log.isInfoEnabled) {
            log.info(s"[${program.getPC.formatted("%5s")}]    Op: [${op.code.name.formatted("%-12s")}]  Gas: [${program.getGas.value}] Deep: [${program.getCallDeep}]  Hint: [${hint}]")
          }

          program.createContract2(value, inOffset, inSize, salt)

          program.step()
        }
        case OpCode.CALL | OpCode.CALLCODE | OpCode.DELEGATECALL | OpCode.STATICCALL => {
          program.stackPop // use adjustedCallGas instead of requested

          val codeAddress = program.stackPop
          val value = if (op.callHasValue) program.stackPop else DataWord.ZERO

          if (program.isStaticCall && (op.code == OpCode.CALL) && !value.isZero) {
            throw Program.staticCallModificationException
          }

          if (!value.isZero) {
            adjustedCallGas = adjustedCallGas.add(DataWord.of(GasCost.STIPEND_CALL))
          }

          val inDataOffs = program.stackPop
          val inDataSize = program.stackPop
          val outDataOffs = program.stackPop
          val outDataSize = program.stackPop

          if (log.isInfoEnabled) {
            hint = s"addr: ${codeAddress.getLast20Bytes.toHex} gas: ${adjustedCallGas.shortHex} inOff: ${inDataOffs.shortHex} inSize: ${inDataSize.shortHex}"
            log.info(s"[${program.getPC.formatted("%5s")}]    Op: [${op.code.name.formatted("%-12s")}]  Gas: [${program.getGas.value}] Deep: [${program.getCallDeep}]  Hint: [${hint}]")
          }

          program.memoryExpand(outDataOffs, outDataSize)

          val msg = MessageCall(op.code, adjustedCallGas, codeAddress, value, inDataOffs, inDataSize, outDataOffs, outDataSize)
          val contract = PrecompiledContracts.getContractForAddress(codeAddress, settings)

          if (!op.callIsStateless) {
            program.getResult.addTouchAccount(codeAddress.toUInt160)
          }

          if (contract != null) {
            program.callToPrecompiledAddress(msg, contract)
          } else {
            program.callToAddress(msg)
          }

          program.step()
        }
        case OpCode.RETURN | OpCode.REVERT => {
          val offset = program.stackPop
          val size = program.stackPop

          val hReturn = program.memoryChunk(offset.intValueSafe, size.intValueSafe)
          program.setHReturn(hReturn)

          if (log.isInfoEnabled) {
            hint = s"data: ${hReturn.toHex} offset: ${offset.value} size: ${size.value}"
          }

          program.step()
          program.stop()

          if (op.code == OpCode.REVERT) {
            program.getResult.setRevert()
          }
        }
        case OpCode.SUICIDE => {
          if (program.isStaticCall) throw Program.staticCallModificationException

          val address = program.stackPop
          program.suicide(address)
          program.getResult.addTouchAccount(address.toUInt160)

          if (log.isInfoEnabled) {
            hint = s"address: ${program.getOwnerAddress.getLast20Bytes.toHex}"
          }

          program.stop()
        }
        case _ => {}
      }

      program.setPreviouslyExecutedOp(op.code.value)

      if (log.isInfoEnabled && !op.isCall) {
        log.info(s"[${program.getPC.formatted("%5s")}]    Op: [${op.code.name.formatted("%-12s")}]  Gas: [${program.getGas.value}] Deep: [${program.getCallDeep}]  Hint: [${hint}]")
      }

      vmCounter += 1
    } catch {
      case e: RuntimeException => {
        log.warn(s"VM halted: [$e]")
        program.spendAllGas()
        program.resetFutureRefund()
        program.stop()
        throw e
      }
    } finally {
      program.fullTrace()
    }
  }

  def overwritestep(program: Program): Unit = {
    if (vmTrace) {
      program.saveOpTrace()
    }

    try {
      val op = OpCache.fromCode(program.getCurrentOp)
      validateOp(op, program)
      program.setLastOp(op.code.value)
      program.verifyStackSize(op.require)
      program.verifyStackOverflow(op.require, op.ret)
      val oldMemSize = program.getMemSize
      val stack = program.getStack

      // parameters for logging
      val callGas = 0L
      val memWords = 0L

      var gasCost = op.tier.toLong
      val gasBefore = program.getGasLong
      //      val stepBefore = program.getPC
      var adjustedCallGas: DataWord = null

      // Calculate fees and spend gas
      op.code match {
        case OpCode.STOP => gasCost = GasCost.STOP
        case OpCode.SUICIDE => {
          gasCost = GasCost.SUICIDE
          val suicideAddress = stack.get(stack.size - 1).toUInt160
          if (!program.getStorage.accountExists(suicideAddress)) {
            gasCost += GasCost.NEW_ACCT_SUICIDE
          }
        }
        case OpCode.SSTORE => {
          var currentValue = program.getCurrentValue(stack.peek)
          if (currentValue == null) currentValue = DataWord.ZERO
          val newValue = stack.get(stack.size - 2)
          if (newValue == currentValue) {
            gasCost = GasCost.REUSE_SSTORE
          } else {
            var origValue = program.getOriginalValue(stack.peek)
            if (origValue == null) origValue = DataWord.ZERO
            if (currentValue == origValue) {
              if (origValue.isZero) {
                gasCost = GasCost.SET_SSTORE
              } else {
                gasCost = GasCost.CLEAR_SSTORE
                if (newValue.isZero) {
                  program.futureRefundGas(GasCost.REFUND_SSTORE)
                }
              }
            } else {
              gasCost = GasCost.REUSE_SSTORE
              if (!origValue.isZero) {
                if (currentValue.isZero) {
                  program.futureRefundGas(-GasCost.REFUND_SSTORE)
                } else if (newValue.isZero) {
                  program.futureRefundGas(GasCost.REFUND_SSTORE)
                }
              }
              if (origValue == newValue) {
                if (origValue.isZero) {
                  program.futureRefundGas(GasCost.SET_SSTORE - GasCost.REUSE_SSTORE)
                } else {
                  program.futureRefundGas(GasCost.CLEAR_SSTORE - GasCost.REUSE_SSTORE)
                }
              }
            }
          }
        }
        case OpCode.SLOAD => gasCost = GasCost.SLOAD
        case OpCode.BALANCE => gasCost = GasCost.BALANCE
        case OpCode.MSTORE => gasCost += calcMemGas(oldMemSize, memNeeded(stack.peek, DataWord.of(32)), 0)
        case OpCode.MSTORE8 => gasCost += calcMemGas(oldMemSize, memNeeded(stack.peek, DataWord.ONE), 0)
        case OpCode.MLOAD => gasCost += calcMemGas(oldMemSize, memNeeded(stack.peek, DataWord.of(32)), 0)
        case OpCode.RETURN | OpCode.REVERT => {
          gasCost += GasCost.STOP + calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 2)), 0)
        }
        case OpCode.SHA3 => {
          gasCost = GasCost.SHA3 + calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 2)), 0)
          val size = stack.get(stack.size - 2)
          val chunkUsed = VM.getSizeInWords(size.longValueSafe)
          gasCost += chunkUsed * GasCost.SHA3_WORD
        }
        case OpCode.CALLDATACOPY | OpCode.RETURNDATACOPY => {
          gasCost += calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 3)), stack.get(stack.size - 3).longValueSafe)
        }
        case OpCode.CODECOPY => {
          gasCost += calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 3)), stack.get(stack.size - 3).longValueSafe)
        }
        case OpCode.EXTCODESIZE => gasCost = GasCost.EXT_CODE_SIZE
        case OpCode.EXTCODECOPY => {
          gasCost = GasCost.EXT_CODE_COPY + calcMemGas(oldMemSize, memNeeded(stack.get(stack.size - 2), stack.get(stack.size - 4)), stack.get(stack.size - 4).longValueSafe)
        }
        case OpCode.EXTCODEHASH => gasCost = GasCost.EXT_CODE_HASH
        case OpCode.CALL | OpCode.CALLCODE | OpCode.DELEGATECALL | OpCode.STATICCALL => {
          gasCost = GasCost.CALL
          val callGasWord = stack.get(stack.size - 1)
          val callAddressWord = stack.get(stack.size - 2)
          val value = if (op.callHasValue) {
            stack.get(stack.size - 3)
          } else {
            DataWord.ZERO
          }

          //check to see if account does not exist and is not a precompiled contract
          if (op.code == OpCode.CALL) {
            if (isDeadAccount(program, callAddressWord.toUInt160) && !value.isZero) {
              gasCost += GasCost.NEW_ACCT_CALL
            }
          }

          //TODO #POC9 Make sure this is converted to BigInt (256num support)
          if (!value.isZero) {
            gasCost += GasCost.VT_CALL
          }

          val opOff = if (op.callHasValue) 4 else 3
          val in = memNeeded(stack.get(stack.size - opOff), stack.get(stack.size - opOff - 1))
          // in offset+size
          val out = memNeeded(stack.get(stack.size - opOff - 2), stack.get(stack.size - opOff - 3)) // out offset+size
          gasCost += calcMemGas(oldMemSize, in.max(out), 0)

          if (gasCost > program.getGas.longValueSafe) {
            throw Program.notEnoughOpGas(op.code, callGasWord, program.getGas)
          }

          val gasLeft = program.getGas
          val subResult = gasLeft.sub(DataWord.of(gasCost))
          //adjustedCallGas = blockchainConfig.getCallGas(op, callGasWord, subResult)
          adjustedCallGas = getCallGas(op.code, callGasWord, subResult)
          gasCost += adjustedCallGas.longValueSafe
        }
        case OpCode.CREATE => gasCost = GasCost.CREATE + calcMemGas(oldMemSize, memNeeded(stack.get(stack.size - 2), stack.get(stack.size - 3)), 0)
        case OpCode.CREATE2 => {
          val codeSize = stack.get(stack.size - 3)
          gasCost = GasCost.CREATE +
            calcMemGas(oldMemSize, memNeeded(stack.get(stack.size - 2), codeSize), 0) +
            VM.getSizeInWords(codeSize.longValueSafe) * GasCost.SHA3_WORD
        }
        case OpCode.LOG0 | OpCode.LOG1 | OpCode.LOG2 | OpCode.LOG3 | OpCode.LOG4 => {
          val dataSize = stack.get(stack.size - 2).value
          val dataCost = dataSize * BigInt(GasCost.LOG_DATA_GAS)
          if (program.getGas.value < dataCost) {
            throw Program.notEnoughOpGas(op.code, dataCost, program.getGas.value)
          }
          val nTopics = op.code.value - OpCode.LOG0.value
          gasCost = GasCost.LOG_GAS + GasCost.LOG_TOPIC_GAS * nTopics +
            GasCost.LOG_DATA_GAS * stack.get(stack.size - 2).longValue +
            calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 2)), 0)
        }
        case OpCode.EXP => {
          val exp = stack.get(stack.size - 2)
          val bytesOccupied = exp.bytesOccupied
          gasCost = GasCost.EXP_GAS + GasCost.EXP_BYTE_GAS * bytesOccupied
        }
        case _ => {}
      }

      //DEBUG System.out.println(" OP IS " + op.name() + " GASCOST IS " + gasCost + " NUM IS " + op.asInt());
      program.spendGas(gasCost, op.code.name)
      program.checkStopTime()

      // Log debugging line for VM
      if (program.getNumber.intValue == dumpBlock) {
        dumpLine(op.code, gasBefore, gasCost + callGas, memWords, program)
      }

      if (hasHooks) {
        hooks.foreach(_.step(program, op.code))
      }

      val codearrParam = new CodearrParam(program, op, adjustedCallGas, settings)
      val res = codearr(op.code.id)(codearrParam)

      program.setPreviouslyExecutedOp(op.code.value)

      if (log.isInfoEnabled && !op.isCall) {
        log.info(s"[${program.getPC.formatted("%5s")}]    Op: [${op.code.name.formatted("%-12s")}]  Gas: [${program.getGas.value}] Deep: [${program.getCallDeep}]  Hint: [${res.toString}]")
      }

      vmCounter += 1
    } catch {
      case e: RuntimeException => {
        log.warn(s"VM halted: [$e]")
        program.spendAllGas()
        program.resetFutureRefund()
        program.stop()
        throw e
      }
    } finally {
      program.fullTrace()
    }
  }

  private def calcMemGas(oldMemSize: Long, newMemSize: BigInt, copySize: Long) = {
    // Avoid overflows
    if (newMemSize > MAX_MEM_SIZE) {
      throw Program.gasOverflow(newMemSize, MAX_MEM_SIZE)
    }

    var gasCost = 0L
    // memory gas calc
    val memoryUsage = (newMemSize.longValue + 31) / 32 * 32
    if (memoryUsage > oldMemSize) {
      val memWords = memoryUsage / 32
      val memWordsOld = oldMemSize / 32
      //TODO #POC9 c_quadCoeffDiv = 512, this should be a constant, not magic number
      val memGas = (GasCost.MEMORY * memWords + memWords * memWords / 512) - (GasCost.MEMORY * memWordsOld + memWordsOld * memWordsOld / 512)
      gasCost += memGas
    }
    if (copySize > 0) {
      val copyGas = GasCost.COPY_GAS * ((copySize + 31) / 32)
      gasCost += copyGas
    }
    gasCost
  }

  /**
    * Utility to calculate new total memory size needed for an operation.
    * <br/> Basically just offset + size, unless size is 0, in which case the result is also 0.
    *
    * @param offset starting position of the memory
    * @param size   number of bytes needed
    * @return offset + size, unless size is 0. In that case memNeeded is also 0.
    */
  private def memNeeded(offset: DataWord, size: DataWord) = {
    if (size.isZero) {
      BigInt(0)
    } else {
      offset.value + size.value
    }
  }

  private def getCallGas(op: OpCode.Value, requestedGas: DataWord, availableGas: DataWord): DataWord = {
    if (requestedGas.value > availableGas.value) {
      throw Program.notEnoughOpGas(op, requestedGas, availableGas)
    } else {
      requestedGas
    }
  }

  private def validateOp(op: OpObject, program: Program): Unit = {
    if (opValidators.contains(op.code) && !opValidators(op.code)) {
      throw InvalidOperationException(s"invalid operation ${op.code}")
    }
  }

  // TODO
  private def isDeadAccount(program: Program, address: UInt160): Boolean = {
    program.getStorage.getAccount(address).forall(_.isEmpty)
  }

  private def dumpLine(op: OpCode.Value, gasBefore: Long, gasCost: Long, memWords: Long, program: Program): Unit = {

  }
}

object VM extends ApexLogging {

  private val _32_ = BigInt(32)
  val codearr = new Array[CodearrParam => Any](256)
  private val loadCodearr = codeArr()

  def codeArr() {

    var hint = ""

    def binaryOp1(op: String, action: (DataWord, DataWord) => DataWord, program: Program) = {
      val word1 = program.stackPop
      val word2 = program.stackPop

      if (log.isInfoEnabled) {
        hint = s"${word1.value} $op ${word2.value}"
      }
      val result = action(word1, word2)
      program.stackPush(result)
      program.step()
      hint
    }

    def binaryOp2(action: (DataWord, DataWord) => DataWord, program: Program) = {
      val word1 = program.stackPop
      val word2 = program.stackPop

      val result = action(word1, word2)
      if (log.isInfoEnabled) {
        hint = s"${result.value}"
      }
      program.stackPush(result)
      program.step()
      hint
    }

    def ternaryOp(action: (DataWord, DataWord, DataWord) => DataWord, program: Program) = {
      val word1 = program.stackPop
      val word2 = program.stackPop
      val word3 = program.stackPop
      val result = action(word1, word2, word3)
      program.stackPush(result)
      program.step()
    }

    codearr(OpCode.STOP.id) = codearrParam => {
      val program = codearrParam.program
      program.setHReturn(Array.empty)
      program.stop()
    }
    codearr(OpCode.ADD.id) = codearrParam => {
      binaryOp1("+", (word1, word2) => word1.add(word2), codearrParam.program)
    }
    codearr(OpCode.MUL.id) = codearrParam => {
      binaryOp1("*", (word1, word2) => word1.mul(word2), codearrParam.program)
    }
    codearr(OpCode.SUB.id) = codearrParam => {
      binaryOp1("-", (word1, word2) => word1.sub(word2), codearrParam.program)
    }
    codearr(OpCode.DIV.id) = codearrParam => {
      binaryOp1("/", (word1, word2) => word1.div(word2), codearrParam.program)
    }
    codearr(OpCode.SDIV.id) = codearrParam => {
      binaryOp1("/", (word1, word2) => word1.sDiv(word2), codearrParam.program)

    }
    codearr(OpCode.MOD.id) = codearrParam => {
      binaryOp1("%", (word1, word2) => word1.mod(word2), codearrParam.program)

    }
    codearr(OpCode.ADDMOD.id) = codearrParam => {
      ternaryOp((word1, word2, word3) => word1.addMod(word2, word3), codearrParam.program)
    }
    codearr(OpCode.MULMOD.id) = codearrParam => {
      ternaryOp((word1, word2, word3) => word1.mulMod(word2, word3), codearrParam.program)

    }
    codearr(OpCode.EXP.id) = codearrParam => {
      binaryOp1("**", (word1, word2) => word1.exp(word2), codearrParam.program)

    }
    codearr(OpCode.SIGNEXTEND.id) = codearrParam => {
      val program = codearrParam.program
      val word1 = program.stackPop
      val k = word1.value
      if (k < _32_) {
        val word2 = program.stackPop
        if (log.isInfoEnabled) hint = "$word1  ${word2.value}"
        val extendResult = word2.signExtend(k.byteValue)
        program.stackPush(extendResult)
      }
      program.step()
      hint
    }
    codearr(OpCode.LT.id) = codearrParam => {
      // TODO: can be improved by not using BigInt
      binaryOp1("<", (word1, word2) => {
        if (word1.value < word2.value) DataWord.ONE else DataWord.ZERO
      }, codearrParam.program)
    }
    codearr(OpCode.GT.id) = codearrParam => {
      // TODO: can be improved by not using BigInt
      binaryOp1(">", (word1, word2) => {
        if (word1.value > word2.value) DataWord.ONE else DataWord.ZERO
      }, codearrParam.program)
    }
    codearr(OpCode.SLT.id) = codearrParam => {
      // TODO: can be improved by not using BigInt
      binaryOp1("<", (word1, word2) => {
        if (word1.sValue < word2.sValue) DataWord.ONE else DataWord.ZERO
      }, codearrParam.program)
    }
    codearr(OpCode.SGT.id) = codearrParam => {
      // TODO: can be improved by not using BigInt
      binaryOp1(">", (word1, word2) => {
        if (word1.sValue > word2.sValue) DataWord.ONE else DataWord.ZERO
      }, codearrParam.program)
    }
    codearr(OpCode.EQ.id) = codearrParam => {
      binaryOp1("==", (word1, word2) => {
        if (word1.xor(word2).isZero) DataWord.ONE else DataWord.ZERO
      }, codearrParam.program)
    }
    codearr(OpCode.ISZERO.id) = codearrParam => {
      val program = codearrParam.program
      val word1 = program.stackPop
      if (word1.isZero) program.stackPush(DataWord.ONE)
      else program.stackPush(DataWord.ZERO)
      if (log.isInfoEnabled) hint = "${word1.value}"
      program.step()
      hint
    }
    codearr(OpCode.AND.id) = codearrParam => {
      binaryOp1("&&", (word1, word2) => word1.and(word2), codearrParam.program)
    }
    codearr(OpCode.OR.id) = codearrParam => {
      binaryOp1("||", (word1, word2) => word1.or(word2), codearrParam.program)
    }
    codearr(OpCode.XOR.id) = codearrParam => {
      binaryOp1("^", (word1, word2) => word1.xor(word2), codearrParam.program)
    }
    codearr(OpCode.NOT.id) = codearrParam => {
      val program = codearrParam.program
      val word1 = program.stackPop
      val bnotWord = word1.bnot
      if (log.isInfoEnabled) hint = "${bnotWord.value}"
      program.stackPush(bnotWord)
      program.step()
      hint
    }
    codearr(OpCode.BYTE.id) = codearrParam => {
      val program = codearrParam.program
      val word1 = program.stackPop
      val word2 = program.stackPop
      val result = if (word1.value < _32_) DataWord.of(word2.getData(word1.intValue)) else DataWord.ZERO
      if (log.isInfoEnabled) hint = "${word1.value}"
      program.stackPush(result)
      program.step()
      hint
    }
    codearr(OpCode.SHL.id) = codearrParam => {
      binaryOp2((word1, word2) => word1.shiftLeft(word2), codearrParam.program)
    }
    codearr(OpCode.SHR.id) = codearrParam => {
      binaryOp2((word1, word2) => word1.shiftRight(word2), codearrParam.program)
    }
    codearr(OpCode.SAR.id) = codearrParam => {
      binaryOp2((word1, word2) => word1.shiftRightSigned(word2), codearrParam.program)
    }
    codearr(OpCode.SHA3.id) = codearrParam => {
      val program = codearrParam.program
      binaryOp2((memOffsetData, lengthData) => {
        val buffer = program.memoryChunk(memOffsetData.intValueSafe, lengthData.intValueSafe)
        val encoded = Crypto.sha3(buffer)
        DataWord.of(encoded)
      }, codearrParam.program)
    }
    codearr(OpCode.ADDRESS.id) = codearrParam => {
      val program = codearrParam.program
      val address = program.getOwnerAddress
      if (log.isInfoEnabled) {
        hint = "address: ${address.getLast20Bytes.toHex}"
      }
      program.stackPush(address)
      program.step()
      hint
    }
    codearr(OpCode.BALANCE.id) = codearrParam => {
      val program = codearrParam.program
      val address = program.stackPop
      val balance = program.getBalance(address)
      if (log.isInfoEnabled) {
        hint = "address: ${address.getLast20Bytes.toHex} balance: ${balance.toString}"
      }
      program.stackPush(balance)
      program.step()
      hint
    }
    codearr(OpCode.ORIGIN.id) = codearrParam => {
      val program = codearrParam.program
      val address = program.getOriginAddress
      if (log.isInfoEnabled) {
        hint = s"address: ${address.getLast20Bytes.toHex}"
      }
      program.stackPush(address)
      program.step()
      hint
    }
    codearr(OpCode.CALLER.id) = codearrParam => {
      val program = codearrParam.program
      val address = program.getCallerAddress
      if (log.isInfoEnabled) {
        hint = s"address: ${address.getLast20Bytes.toHex}"
      }
      program.stackPush(address)
      program.step()
      hint
    }
    codearr(OpCode.CALLVALUE.id) = codearrParam => {
      val program = codearrParam.program
      val value = program.getCallValue
      if (log.isInfoEnabled) {
        hint = "value: ${value}"
      }
      program.stackPush(value)
      program.step()
      hint
    }
    codearr(OpCode.CALLDATALOAD.id) = codearrParam => {
      val program = codearrParam.program
      val dataOffs = program.stackPop
      val value = program.getDataValue(dataOffs)
      if (log.isInfoEnabled) {
        hint = "data: ${value}"
      }
      program.stackPush(value)
      program.step()
      hint
    }
    codearr(OpCode.CALLDATASIZE.id) = codearrParam => {
      val program = codearrParam.program
      val size = program.getDataSize
      if (log.isInfoEnabled) {
        hint = "size: ${size}"
      }
      program.stackPush(size)
      program.step()
      hint
    }
    codearr(OpCode.CALLDATACOPY.id) = codearrParam => {
      val program = codearrParam.program
      val memOffsetData = program.stackPop
      val dataOffsetData = program.stackPop
      val lengthData = program.stackPop
      val msgData = program.getDataCopy(dataOffsetData, lengthData)
      if (log.isInfoEnabled) {
        hint = "data: ${msgData.toHex}"
      }
      program.memorySave(memOffsetData.intValueSafe, lengthData.intValueSafe, msgData)
      program.step()
      hint
    }
    codearr(OpCode.CODESIZE.id) = codearrParam => {
      val program = codearrParam.program
      val op = codearrParam.op
      var length = 0
      if (op.code == OpCode.CODESIZE) {
        length = program.getCode.length
      } else {
        val address = program.stackPop
        length = program.getCodeAt(address).length
      }
      val codeLength = DataWord.of(length)
      if (log.isInfoEnabled) hint = "size: $length"
      program.stackPush(codeLength)
      program.step()
      hint
    }
    codearr(OpCode.CODECOPY.id) = codearrParam => {
      val program = codearrParam.program
      val op = codearrParam.op
      var fullCode = Array.emptyByteArray
      if (op.code == OpCode.CODECOPY) {
        fullCode = program.getCode
      }
      if (op.code == OpCode.EXTCODECOPY) {
        val address = program.stackPop
        fullCode = program.getCodeAt(address)
      }
      val memOffset = program.stackPop.intValueSafe
      val codeOffset = program.stackPop.intValueSafe
      val lengthData = program.stackPop.intValueSafe
      val sizeToBeCopied = if (codeOffset.toLong + lengthData > fullCode.length) {
        if (fullCode.length < codeOffset) 0 else fullCode.length - codeOffset
      } else {
        lengthData
      }
      val codeCopy = new Array[Byte](lengthData)
      if (codeOffset < fullCode.length) {
        System.arraycopy(fullCode, codeOffset, codeCopy, 0, sizeToBeCopied)
      }
      if (log.isInfoEnabled) hint = "code: ${codeCopy.toHex}"
      program.memorySave(memOffset, lengthData, codeCopy)
      program.step()
      hint
    }
    codearr(OpCode.GASPRICE.id) = codearrParam => {
      val program = codearrParam.program
      val gasPrice = program.getGasPrice

      if (log.isInfoEnabled) hint = s"price: ${gasPrice.toString}"

      program.stackPush(gasPrice)
      program.step()
      hint
    }
    codearr(OpCode.EXTCODESIZE.id) = codearrParam => {
      val program = codearrParam.program
      val op = codearrParam.op
      var length = 0
      if (op.code == OpCode.CODESIZE) {
        length = program.getCode.length
      } else {
        val address = program.stackPop
        length = program.getCodeAt(address).length
      }
      val codeLength = DataWord.of(length)

      if (log.isInfoEnabled) hint = s"size: $length"

      program.stackPush(codeLength)
      program.step()
      hint
    }
    codearr(OpCode.EXTCODECOPY.id) = codearrParam => {
      val program = codearrParam.program
      val op = codearrParam.op
      var fullCode = Array.emptyByteArray
      if (op.code == OpCode.CODECOPY) {
        fullCode = program.getCode
      }

      if (op.code == OpCode.EXTCODECOPY) {
        val address = program.stackPop
        fullCode = program.getCodeAt(address)
      }

      val memOffset = program.stackPop.intValueSafe
      val codeOffset = program.stackPop.intValueSafe
      val lengthData = program.stackPop.intValueSafe
      val sizeToBeCopied = if (codeOffset.toLong + lengthData > fullCode.length) {
        if (fullCode.length < codeOffset) 0 else fullCode.length - codeOffset
      } else {
        lengthData
      }

      val codeCopy = new Array[Byte](lengthData)

      if (codeOffset < fullCode.length) {
        System.arraycopy(fullCode, codeOffset, codeCopy, 0, sizeToBeCopied)
      }

      if (log.isInfoEnabled) hint = s"code: ${codeCopy.toHex}"

      program.memorySave(memOffset, lengthData, codeCopy)
      program.step()
      hint
    }
    codearr(OpCode.RETURNDATASIZE.id) = codearrParam => {
      val program = codearrParam.program
      val size = program.getReturnDataBufferSize
      if (log.isInfoEnabled) {
        hint = s"size: ${size}"
      }
      program.stackPush(size)
      program.step()
      hint
    }
    codearr(OpCode.RETURNDATACOPY.id) = codearrParam => {
      val program = codearrParam.program
      val memOffsetData = program.stackPop
      val dataOffsetData = program.stackPop
      val lengthData = program.stackPop
      val msgData = program.getReturnDataBufferData(dataOffsetData, lengthData)
      if (msgData == null) {
        throw Program.returnDataCopyIllegalBoundsException(dataOffsetData, lengthData, program.getReturnDataBufferSize.longValueSafe)
      }
      if (log.isInfoEnabled) {
        hint = s"data: ${msgData.toHex}"
      }
      program.memorySave(memOffsetData.intValueSafe, lengthData.intValueSafe, msgData)
      program.step()
      hint
    }
    codearr(OpCode.EXTCODEHASH.id) = codearrParam => {
      val program = codearrParam.program
      val address = program.stackPop
      val codeHash = program.getCodeHashAt(address)
      program.stackPush(codeHash)
      program.step()
    }
    codearr(OpCode.BLOCKHASH.id) = codearrParam => {
      val program = codearrParam.program
      val blockIndex = program.stackPop.intValueSafe
      val blockHash = program.getBlockHash(blockIndex)
      if (log.isInfoEnabled) hint = s"blockHash: $blockHash"
      program.stackPush(blockHash)
      program.step()
      hint
    }
    codearr(OpCode.COINBASE.id) = codearrParam => {
      val program = codearrParam.program
      val coinbase = program.getCoinbase
      if (log.isInfoEnabled) {
        hint = s"coinbase: ${coinbase.getLast20Bytes.toHex}"
      }
      program.stackPush(coinbase)
      program.step()
      hint
    }
    codearr(OpCode.TIMESTAMP.id) = codearrParam => {
      val program = codearrParam.program
      val timestamp = program.getTimestamp
      if (log.isInfoEnabled) hint = s"timestamp: ${timestamp.value}"
      program.stackPush(timestamp)
      program.step()
      hint
    }
    codearr(OpCode.NUMBER.id) = codearrParam => {
      val program = codearrParam.program
      val number = program.getNumber
      if (log.isInfoEnabled) hint = s"number: ${number.value}"
      program.stackPush(number)
      program.step()
      hint
    }
    codearr(OpCode.DIFFICULTY.id) = codearrParam => {
      val program = codearrParam.program
      val difficulty = DataWord.ZERO //program.getDifficulty
      if (log.isInfoEnabled) hint = s"difficulty: $difficulty"
      program.stackPush(difficulty)
      program.step()
      hint
    }
    codearr(OpCode.GASLIMIT.id) = codearrParam => {
      val program = codearrParam.program
      val gaslimit = program.getGasLimit
      if (log.isInfoEnabled) hint = s"gaslimit: $gaslimit"
      program.stackPush(gaslimit)
      program.step()
      hint
    }
    codearr(OpCode.POP.id) = codearrParam => {
      val program = codearrParam.program
      program.stackPop
      program.step()
    }
    codearr(OpCode.MLOAD.id) = codearrParam => {
      val program = codearrParam.program
      val address = program.stackPop
      val codeHash = program.getCodeHashAt(address)
      program.stackPush(codeHash)
      program.step()
    }
    codearr(OpCode.MSTORE.id) = codearrParam => {
      val program = codearrParam.program
      val addr = program.stackPop
      val value = program.stackPop

      if (log.isInfoEnabled) hint = s"addr: $addr value: $value"

      program.memorySave(addr, value)
      program.step()
      hint
    }
    codearr(OpCode.MSTORE8.id) = codearrParam => {
      val program = codearrParam.program
      val addr = program.stackPop
      val value = program.stackPop
      val byteVal = Array(value.getData(31))
      program.memorySave(addr.intValueSafe, byteVal)
      program.step()
    }
    codearr(OpCode.SLOAD.id) = codearrParam => {
      val program = codearrParam.program
      val key = program.stackPop
      var value = program.storageLoad(key)

      if (log.isInfoEnabled) hint = s"key: $key value: $value"

      if (value == null) value = key.and(DataWord.ZERO)

      program.stackPush(value)
      program.step()
      hint
    }
    codearr(OpCode.SSTORE.id) = codearrParam => {
      val program = codearrParam.program
      if (program.isStaticCall) {
        throw Program.staticCallModificationException
      }

      val addr = program.stackPop
      val value = program.stackPop

      if (log.isInfoEnabled) hint = s"[${program.getOwnerAddress.toPrefixString}] key: $addr value: $value"

      program.storageSave(addr, value)
      program.step()
      hint
    }
    codearr(OpCode.JUMP.id) = codearrParam => {
      val program = codearrParam.program
      val pos = program.stackPop
      val nextPC = program.verifyJumpDest(pos)

      if (log.isInfoEnabled) hint = s"~> $nextPC"

      program.setPC(nextPC)
      hint
    }
    codearr(OpCode.JUMPI.id) = codearrParam => {
      val program = codearrParam.program
      val pos = program.stackPop
      val cond = program.stackPop

      if (!cond.isZero) {
        val nextPC = program.verifyJumpDest(pos)
        if (log.isInfoEnabled) hint = s"~> $nextPC"
        program.setPC(nextPC)
      }
      else program.step()
      hint
    }
    codearr(OpCode.PC.id) = codearrParam => {
      val program = codearrParam.program
      val pc = program.getPC
      val pcWord = DataWord.of(pc)

      if (log.isInfoEnabled) hint = pcWord.toString

      program.stackPush(pcWord)
      program.step()
      hint
    }
    codearr(OpCode.MSIZE.id) = codearrParam => {
      val program = codearrParam.program
      val memSize = program.getMemSize
      val wordMemSize = DataWord.of(memSize)

      if (log.isInfoEnabled) hint = s"$memSize"

      program.stackPush(wordMemSize)
      program.step()
      hint
    }
    codearr(OpCode.GAS.id) = codearrParam => {
      val program = codearrParam.program
      val gas = program.getGas

      if (log.isInfoEnabled) hint = s"$gas"

      program.stackPush(gas)
      program.step()
      hint
    }
    codearr(OpCode.JUMPDEST.id) = codearrParam => {
      val program = codearrParam.program
      program.step()
    }
    val pushFun: CodearrParam => Unit = codearrParam => {
      val program = codearrParam.program
      val op = codearrParam.op
      program.step()
      val nPush = op.code.value - OpCode.PUSH1.value + 1

      val data = program.sweep(nPush)

      if (log.isInfoEnabled) hint = s"${data.toHex}"

      program.stackPush(data)
      hint
    }
    codearr(OpCode.PUSH1.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH2.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH3.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH4.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH5.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH6.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH7.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH8.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH9.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH10.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH11.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH12.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH13.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH14.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH15.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH16.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH17.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH18.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH19.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH20.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH21.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH22.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH23.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH24.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH25.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH26.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH27.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH28.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH29.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH30.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH31.id) = codearrParam => {
      pushFun(codearrParam)
    }
    codearr(OpCode.PUSH32.id) = codearrParam => {
      pushFun(codearrParam)
    }
    val dupFun: CodearrParam => Unit = codearrParam => {
      val op = codearrParam.op
      val program = codearrParam.program
      val stack = program.getStack
      val n = op.code.value - OpCode.DUP1.value + 1
      val word_1 = stack.get(stack.size - n)
      program.stackPush(word_1)
      program.step()
    }
    codearr(OpCode.DUP1.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP2.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP3.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP4.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP5.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP6.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP7.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP8.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP9.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP10.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP11.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP12.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP13.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP14.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP15.id) = codearrParam => {
      dupFun(codearrParam)
    }
    codearr(OpCode.DUP16.id) = codearrParam => {
      dupFun(codearrParam)
    }

    val swapFun: CodearrParam => Unit = codearrParam => {
      val op = codearrParam.op
      val program = codearrParam.program
      val stack = program.getStack
      val n = op.code.value - OpCode.SWAP1.value + 2
      stack.swap(stack.size - 1, stack.size - n)
      program.step()
    }
    codearr(OpCode.SWAP1.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP2.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP3.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP4.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP5.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP6.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP7.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP8.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP9.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP10.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP11.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP12.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP13.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP14.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP15.id) = codearrParam => {
      swapFun(codearrParam)
    }
    codearr(OpCode.SWAP16.id) = codearrParam => {
      swapFun(codearrParam)
    }
    val logFun: CodearrParam => Unit = codearrParam => {
      val program = codearrParam.program
      val stack = program.getStack
      val op = codearrParam.op
      if (program.isStaticCall) {
        throw Program.staticCallModificationException
      }
      val address = program.getOwnerAddress

      val memStart = stack.pop
      val memOffset = stack.pop

      val nTopics = op.code.value - OpCode.LOG0.value

      val topics = ListBuffer.empty[DataWord]
      for (_ <- 1 to nTopics) {
        topics.append(stack.pop)
      }

      val data = program.memoryChunk(memStart.intValueSafe, memOffset.intValueSafe)

      val logInfo = LogInfo(address.getLast20Bytes, topics, data)

      if (log.isInfoEnabled) hint = logInfo.toString

      program.getResult.addLogInfo(logInfo)
      program.step()
      hint
    }
    codearr(OpCode.LOG1.id) = codearrParam => {
      logFun(codearrParam)
    }
    codearr(OpCode.LOG2.id) = codearrParam => {
      logFun(codearrParam)
    }
    codearr(OpCode.LOG3.id) = codearrParam => {
      logFun(codearrParam)
    }
    codearr(OpCode.LOG4.id) = codearrParam => {
      logFun(codearrParam)
    }
    codearr(OpCode.CREATE.id) = codearrParam => {
      val program = codearrParam.program
      val op = codearrParam.op
      if (program.isStaticCall) throw Program.staticCallModificationException

      val value = program.stackPop
      val inOffset = program.stackPop
      val inSize = program.stackPop

      if (log.isInfoEnabled) {
        log.info(s"[${program.getPC.formatted("%5s")}]    Op: [${op.code.name.formatted("%-12s")}]  Gas: [${program.getGas.value}] Deep: [${program.getCallDeep}]  Hint: [${hint}]")
      }

      program.createContract(value, inOffset, inSize)

      program.step()
    }

    val callFun: CodearrParam => Unit = codearrParam => {
      val program = codearrParam.program
      val op = codearrParam.op
      var adjustedCallGas = codearrParam.adjustedCallGas
      val settings = codearrParam.settings
      program.stackPop // use adjustedCallGas instead of requested
      val codeAddress = program.stackPop
      val value = if (op.callHasValue) program.stackPop else DataWord.ZERO

      if (program.isStaticCall && (op.code == OpCode.CALL) && !value.isZero) {
        throw Program.staticCallModificationException
      }

      if (!value.isZero) {
        adjustedCallGas = adjustedCallGas.add(DataWord.of(GasCost.STIPEND_CALL))
      }

      val inDataOffs = program.stackPop
      val inDataSize = program.stackPop
      val outDataOffs = program.stackPop
      val outDataSize = program.stackPop

      if (log.isInfoEnabled) {
        hint = s"addr: ${codeAddress.getLast20Bytes.toHex} gas: ${adjustedCallGas.shortHex} inOff: ${inDataOffs.shortHex} inSize: ${inDataSize.shortHex}"
        log.info(s"[${program.getPC.formatted("%5s")}]    Op: [${op.code.name.formatted("%-12s")}]  Gas: [${program.getGas.value}] Deep: [${program.getCallDeep}]  Hint: [${hint}]")
      }

      program.memoryExpand(outDataOffs, outDataSize)

      val msg = MessageCall(op.code, adjustedCallGas, codeAddress, value, inDataOffs, inDataSize, outDataOffs, outDataSize)
      val contract = PrecompiledContracts.getContractForAddress(codeAddress, settings)

      if (!op.callIsStateless) {
        program.getResult.addTouchAccount(codeAddress.toUInt160)
      }

      if (contract != null) {
        program.callToPrecompiledAddress(msg, contract)
      } else {
        program.callToAddress(msg)
      }

      program.step()
      hint
    }
    codearr(OpCode.CALL.id) = codearrParam => {
      callFun(codearrParam)
    }
    codearr(OpCode.CALLCODE.id) = codearrParam => {
      callFun(codearrParam)
    }
    val reFun: CodearrParam => Unit = codearrParam => {
      val program = codearrParam.program
      val op = codearrParam.op
      val offset = program.stackPop
      val size = program.stackPop

      val hReturn = program.memoryChunk(offset.intValueSafe, size.intValueSafe)
      program.setHReturn(hReturn)

      if (log.isInfoEnabled) {
        hint = s"data: ${hReturn.toHex} offset: ${offset.value} size: ${size.value}"
      }

      program.step()
      program.stop()

      if (op.code == OpCode.REVERT) {
        program.getResult.setRevert()
      }
      hint
    }
    codearr(OpCode.RETURN.id) = codearrParam => {
      reFun(codearrParam)
    }
    codearr(OpCode.DELEGATECALL.id) = codearrParam => {
      callFun(codearrParam)
    }
    codearr(OpCode.CREATE2.id) = codearrParam => {
      val program = codearrParam.program
      val op = codearrParam.op
      if (program.isStaticCall) throw Program.staticCallModificationException

      val value = program.stackPop
      val inOffset = program.stackPop
      val inSize = program.stackPop
      val salt = program.stackPop

      if (log.isInfoEnabled) {
        log.info(s"[${program.getPC.formatted("%5s")}]    Op: [${op.code.name.formatted("%-12s")}]  Gas: [${program.getGas.value}] Deep: [${program.getCallDeep}]  Hint: [${hint}]")
      }

      program.createContract2(value, inOffset, inSize, salt)

      program.step()
    }
    codearr(OpCode.STATICCALL.id) = codearrParam => {
      callFun(codearrParam)
    }
    codearr(OpCode.REVERT.id) = codearrParam => {
      reFun(codearrParam)
    }
    codearr(OpCode.SUICIDE.id) = codearrParam => {
      val program = codearrParam.program
      if (program.isStaticCall) throw Program.staticCallModificationException

      val address = program.stackPop
      program.suicide(address)
      program.getResult.addTouchAccount(address.toUInt160)

      if (log.isInfoEnabled) {
        hint = s"address: ${program.getOwnerAddress.getLast20Bytes.toHex}"
      }

      program.stop()
      hint
    }
  }

  /**
    * Returns number of VM words required to hold data of size
    */
  def getSizeInWords(size: Long): Long = {
    if (size == 0) 0 else (size - 1) / 32 + 1
  }

  def play(settings: ContractSettings, vmHook: VMHook, program: Program): ProgramResult = {
    new VM(settings, vmHook).play(program)
    program.getResult
  }
}

case class CodearrParam(program: Program, op: OpObject, adjustedCallGas: DataWord,
                        settings: ContractSettings)