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
import org.slf4j.Logger

import scala.collection.mutable.ListBuffer

class VM(settings: ContractSettings, hook: VMHook) extends com.apex.common.ApexLogging {
  private val hooks = Array(VMHook.EMPTY, hook).filterNot(_.isEmpty)

  private val opValidators = OpCode.emptyValidators

  private val hasHooks = hooks.length > 0

  private val dumpBlock = settings.dumpBlock
  private val vmTrace = settings.vmTrace
  private var vmCounter = 0

  import VM._

  def play(program: Program): Unit = {
    if (!program.byTestingSuite) {
      try {
        if (hasHooks) hooks.foreach(_.startPlay(program))
        while (!program.isStopped) {
          step(program)
        }
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
      val ctx = new ExecuteContext(op, program, settings, log)

      val runtime = instructionTable(op.code.id)
      // Calculate fees and spend gas
      runtime.spendGas(ctx)

      runtime.checkStopTime(ctx)
      // Log debugging line for VM
      runtime.dumpLine(ctx, dumpBlock)

      if (hasHooks) {
        hooks.foreach(_.step(program, op.code))
      }

      val res = runtime.execute(ctx)

      if (log.isInfoEnabled && !op.isCall) {
        ctx.logHint
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
}

object VM {
  private val MAX_MEM_SIZE = BigInt(Integer.MAX_VALUE)
  private val opValidators = OpCode.emptyValidators
  private val _32_ = BigInt(32)

  private val instructionTable = new Array[Runtime](256)

  instructionTable(OpCode.STOP.id) = Runtime(ctx => {
    val program = ctx.program
    program.setHReturn(Array.empty)
    program.stop()
  })(Some(GasCost.STOP))

  instructionTable(OpCode.ADD.id) = Runtime(ctx => {
    binaryOp1("+", (word1, word2) => word1.add(word2), ctx)
  })

  instructionTable(OpCode.MUL.id) = Runtime(ctx => {
    binaryOp1("*", (word1, word2) => word1.mul(word2), ctx)
  })

  instructionTable(OpCode.SUB.id) = Runtime(ctx => {
    binaryOp1("-", (word1, word2) => word1.sub(word2), ctx)
  })

  instructionTable(OpCode.DIV.id) = Runtime(ctx => {
    binaryOp1("/", (word1, word2) => word1.div(word2), ctx)
  })

  instructionTable(OpCode.SDIV.id) = Runtime(ctx => {
    binaryOp1("/", (word1, word2) => word1.sDiv(word2), ctx)
  })

  instructionTable(OpCode.MOD.id) = Runtime(ctx => {
    binaryOp1("%", (word1, word2) => word1.mod(word2), ctx)
  })

  instructionTable(OpCode.ADDMOD.id) = Runtime(ctx => {
    ternaryOp((word1, word2, word3) => word1.addMod(word2, word3), ctx)
  })

  instructionTable(OpCode.MULMOD.id) = Runtime(ctx => {
    ternaryOp((word1, word2, word3) => word1.mulMod(word2, word3), ctx)
  })

  instructionTable(OpCode.EXP.id) = Runtime(ctx => {
    binaryOp1("**", (word1, word2) => word1.exp(word2), ctx)
  }, expGasCost)

  instructionTable(OpCode.SIGNEXTEND.id) = Runtime(ctx => {
    val program = ctx.program
    val word1 = program.stackPop
    val k = word1.value
    if (k < _32_) {
      val word2 = program.stackPop
      ctx.setHint(s"$word1  ${word2.value}")
      val extendResult = word2.signExtend(k.byteValue)
      program.stackPush(extendResult)
    }
    program.step()
  })

  instructionTable(OpCode.LT.id) = Runtime(ctx => {
    // TODO: can be improved by not using BigInt
    binaryOp1("<", (word1, word2) => {
      if (word1.value < word2.value) DataWord.ONE else DataWord.ZERO
    }, ctx)
  })

  instructionTable(OpCode.GT.id) = Runtime(ctx => {
    // TODO: can be improved by not using BigInt
    binaryOp1(">", (word1, word2) => {
      if (word1.value > word2.value) DataWord.ONE else DataWord.ZERO
    }, ctx)
  })

  instructionTable(OpCode.SLT.id) = Runtime(ctx => {
    // TODO: can be improved by not using BigInt
    binaryOp1("<", (word1, word2) => {
      if (word1.sValue < word2.sValue) DataWord.ONE else DataWord.ZERO
    }, ctx)
  })

  instructionTable(OpCode.SGT.id) = Runtime(ctx => {
    // TODO: can be improved by not using BigInt
    binaryOp1(">", (word1, word2) => {
      if (word1.sValue > word2.sValue) DataWord.ONE else DataWord.ZERO
    }, ctx)
  })

  instructionTable(OpCode.EQ.id) = Runtime(ctx => {
    binaryOp1("==", (word1, word2) => {
      if (word1.xor(word2).isZero) DataWord.ONE else DataWord.ZERO
    }, ctx)
  })

  instructionTable(OpCode.ISZERO.id) = Runtime(ctx => {
    val program = ctx.program
    val word1 = program.stackPop
    if (word1.isZero) program.stackPush(DataWord.ONE)
    else program.stackPush(DataWord.ZERO)
    ctx.setHint(s"${word1.value}")
    program.step()
  })

  instructionTable(OpCode.AND.id) = Runtime(ctx => {
    binaryOp1("&&", (word1, word2) => word1.and(word2), ctx)
  })

  instructionTable(OpCode.OR.id) = Runtime(ctx => {
    binaryOp1("||", (word1, word2) => word1.or(word2), ctx)
  })

  instructionTable(OpCode.XOR.id) = Runtime(ctx => {
    binaryOp1("^", (word1, word2) => word1.xor(word2), ctx)
  })

  instructionTable(OpCode.NOT.id) = Runtime(ctx => {
    val program = ctx.program
    val word1 = program.stackPop
    val bnotWord = word1.bnot
    ctx.setHint(s"${bnotWord.value}")
    program.stackPush(bnotWord)
    program.step()
  })

  instructionTable(OpCode.BYTE.id) = Runtime(ctx => {
    val program = ctx.program
    val word1 = program.stackPop
    val word2 = program.stackPop
    val result = if (word1.value < _32_) DataWord.of(word2.getData(word1.intValue)) else DataWord.ZERO
    ctx.setHint(s"${word1.value}")
    program.stackPush(result)
    program.step()
  })

  instructionTable(OpCode.SHL.id) = Runtime(ctx => {
    binaryOp2((word1, word2) => word1.shiftLeft(word2), ctx)
  })

  instructionTable(OpCode.SHR.id) = Runtime(ctx => {
    binaryOp2((word1, word2) => word1.shiftRight(word2), ctx)
  })

  instructionTable(OpCode.SAR.id) = Runtime(ctx => {
    binaryOp2((word1, word2) => word1.shiftRightSigned(word2), ctx)
  })

  instructionTable(OpCode.SHA3.id) = Runtime(ctx => {
    val program = ctx.program
    binaryOp2((memOffsetData, lengthData) => {
      val buffer = program.memoryChunk(memOffsetData.intValueSafe, lengthData.intValueSafe)
      val encoded = Crypto.sha3(buffer)
      DataWord.of(encoded)
    }, ctx)
  }, sha3GasCost)

  instructionTable(OpCode.ADDRESS.id) = Runtime(ctx => {
    val program = ctx.program
    val address = program.getOwnerAddress
    ctx.setHint(s"address: ${address.getLast20Bytes.toHex}")
    program.stackPush(address)
    program.step()
  })

  instructionTable(OpCode.BALANCE.id) = Runtime(ctx => {
    val program = ctx.program
    val address = program.stackPop
    val balance = program.getBalance(address)
    ctx.setHint(s"address: ${address.getLast20Bytes.toHex} balance: ${balance.toString}")
    program.stackPush(balance)
    program.step()
  })(Some(GasCost.BALANCE))

  instructionTable(OpCode.ORIGIN.id) = Runtime(ctx => {
    val program = ctx.program
    val address = program.getOriginAddress
    ctx.setHint(s"address: ${address.getLast20Bytes.toHex}")
    program.stackPush(address)
    program.step()
  })

  instructionTable(OpCode.CALLER.id) = Runtime(ctx => {
    val program = ctx.program
    val address = program.getCallerAddress
    ctx.setHint(s"address: ${address.getLast20Bytes.toHex}")
    program.stackPush(address)
    program.step()
  })

  instructionTable(OpCode.CALLVALUE.id) = Runtime(ctx => {
    val program = ctx.program
    val value = program.getCallValue
    ctx.setHint(s"value: ${value}")
    program.stackPush(value)
    program.step()
  })

  instructionTable(OpCode.CALLDATALOAD.id) = Runtime(ctx => {
    val program = ctx.program
    val dataOffs = program.stackPop
    val value = program.getDataValue(dataOffs)
    ctx.setHint(s"data: ${value}")
    program.stackPush(value)
    program.step()
  })

  instructionTable(OpCode.CALLDATASIZE.id) = Runtime(ctx => {
    val program = ctx.program
    val size = program.getDataSize
    ctx.setHint(s"size: ${size}")
    program.stackPush(size)
    program.step()
  })

  instructionTable(OpCode.CALLDATACOPY.id) = Runtime(ctx => {
    val program = ctx.program
    val memOffsetData = program.stackPop
    val dataOffsetData = program.stackPop
    val lengthData = program.stackPop
    val msgData = program.getDataCopy(dataOffsetData, lengthData)
    ctx.setHint(s"data: ${msgData.toHex}")
    program.memorySave(memOffsetData.intValueSafe, lengthData.intValueSafe, msgData)
    program.step()
  }, dataCopyGasCost)

  instructionTable(OpCode.CODESIZE.id) = Runtime(ctx => {
    val program = ctx.program
    val op = ctx.op
    var length = 0
    if (op.code == OpCode.CODESIZE) {
      length = program.getCode.length
    } else {
      val address = program.stackPop
      length = program.getCodeAt(address).length
    }
    val codeLength = DataWord.of(length)
    ctx.setHint(s"size: $length")
    program.stackPush(codeLength)
    program.step()
  })

  instructionTable(OpCode.CODECOPY.id) = Runtime(ctx => {
    val program = ctx.program
    val op = ctx.op
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
    ctx.setHint(s"code: ${codeCopy.toHex}")
    program.memorySave(memOffset, lengthData, codeCopy)
    program.step()
  },codeCopyGasCost)

  instructionTable(OpCode.GASPRICE.id) = Runtime(ctx => {
    val program = ctx.program
    val gasPrice = program.getGasPrice
    ctx.setHint(s"price: ${gasPrice.toString}")
    program.stackPush(gasPrice)
    program.step()
  })

  instructionTable(OpCode.EXTCODESIZE.id) = Runtime(ctx => {
    val program = ctx.program
    val op = ctx.op
    var length = 0
    if (op.code == OpCode.CODESIZE) {
      length = program.getCode.length
    } else {
      val address = program.stackPop
      length = program.getCodeAt(address).length
    }
    val codeLength = DataWord.of(length)
    ctx.setHint(s"size: $length")
    program.stackPush(codeLength)
    program.step()
  })(Some(GasCost.EXT_CODE_SIZE))

  instructionTable(OpCode.EXTCODECOPY.id) = Runtime(ctx => {
    val program = ctx.program
    val op = ctx.op
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

    ctx.setHint(s"code: ${codeCopy.toHex}")
    program.memorySave(memOffset, lengthData, codeCopy)
    program.step()
  },extCodeCopyGasCost)

  instructionTable(OpCode.RETURNDATASIZE.id) = Runtime(ctx => {
    val program = ctx.program
    val size = program.getReturnDataBufferSize
    ctx.setHint(s"size: ${size}")
    program.stackPush(size)
    program.step()
  })

  instructionTable(OpCode.RETURNDATACOPY.id) = Runtime(ctx => {
    val program = ctx.program
    val memOffsetData = program.stackPop
    val dataOffsetData = program.stackPop
    val lengthData = program.stackPop
    val msgData = program.getReturnDataBufferData(dataOffsetData, lengthData)
    if (msgData == null) {
      throw Program.returnDataCopyIllegalBoundsException(dataOffsetData, lengthData, program.getReturnDataBufferSize.longValueSafe)
    }

    ctx.setHint(s"data: ${msgData.toHex}")
    program.memorySave(memOffsetData.intValueSafe, lengthData.intValueSafe, msgData)
    program.step()
  }, dataCopyGasCost)

  instructionTable(OpCode.EXTCODEHASH.id) = Runtime(ctx => {
    val program = ctx.program
    val address = program.stackPop
    val codeHash = program.getCodeHashAt(address)
    program.stackPush(codeHash)
    program.step()
  })(Some(GasCost.EXT_CODE_HASH))

  instructionTable(OpCode.BLOCKHASH.id) = Runtime(ctx => {
    val program = ctx.program
    val blockIndex = program.stackPop.intValueSafe
    val blockHash = program.getBlockHash(blockIndex)
    ctx.setHint(s"blockHash: $blockHash")
    program.stackPush(blockHash)
    program.step()
  })

  instructionTable(OpCode.COINBASE.id) = Runtime(ctx => {
    val program = ctx.program
    val coinbase = program.getCoinbase
    ctx.setHint(s"coinbase: ${coinbase.getLast20Bytes.toHex}")
    program.stackPush(coinbase)
    program.step()
  })

  instructionTable(OpCode.TIMESTAMP.id) = Runtime(ctx => {
    val program = ctx.program
    val timestamp = program.getTimestamp
    ctx.setHint(s"timestamp: ${timestamp.value}")
    program.stackPush(timestamp)
    program.step()
  })

  instructionTable(OpCode.NUMBER.id) = Runtime(ctx => {
    val program = ctx.program
    val number = program.getNumber
    ctx.setHint(s"number: ${number.value}")
    program.stackPush(number)
    program.step()
  })

  instructionTable(OpCode.DIFFICULTY.id) = Runtime(ctx => {
    val program = ctx.program
    val difficulty = DataWord.ZERO //program.getDifficulty
    ctx.setHint(s"difficulty: $difficulty")
    program.stackPush(difficulty)
    program.step()
  })

  instructionTable(OpCode.GASLIMIT.id) = Runtime(ctx => {
    val program = ctx.program
    val gaslimit = program.getGasLimit
    ctx.setHint(s"gaslimit: $gaslimit")
    program.stackPush(gaslimit)
    program.step()
  })

  instructionTable(OpCode.POP.id) = Runtime(ctx => {
    val program = ctx.program
    program.stackPop
    program.step()
  })

  instructionTable(OpCode.MLOAD.id) = Runtime(ctx => {
    val program = ctx.program
    val address = program.stackPop
    val data = program.memoryLoad(address)
    ctx.setHint(s"data: $data")
    program.stackPush(data)
    program.step()
  }, mloadGasCost)

  instructionTable(OpCode.MSTORE.id) = Runtime(ctx => {
    val program = ctx.program
    val addr = program.stackPop
    val value = program.stackPop
    ctx.setHint(s"addr: $addr value: $value")
    program.memorySave(addr, value)
    program.step()
  }, mstoreGasCost)

  instructionTable(OpCode.MSTORE8.id) = Runtime(ctx => {
    val program = ctx.program
    val addr = program.stackPop
    val value = program.stackPop
    val byteVal = Array(value.getData(31))
    program.memorySave(addr.intValueSafe, byteVal)
    program.step()
  }, mstore8GasCost)

  instructionTable(OpCode.SLOAD.id) = Runtime(ctx => {
    val program = ctx.program
    val key = program.stackPop
    var value = program.storageLoad(key)
    ctx.setHint(s"key: $key value: $value")
    if (value == null) value = key.and(DataWord.ZERO)
    program.stackPush(value)
    program.step()
  })(Some(GasCost.SLOAD))

  instructionTable(OpCode.SSTORE.id) = Runtime(ctx => {
    val program = ctx.program
    if (program.isStaticCall) {
      throw Program.staticCallModificationException
    }

    val addr = program.stackPop
    val value = program.stackPop
    ctx.setHint(s"[${program.getOwnerAddress.toPrefixString}] key: $addr value: $value")
    program.storageSave(addr, value)
    program.step()
  }, sstoreGasCost)

  instructionTable(OpCode.JUMP.id) = Runtime(ctx => {
    val program = ctx.program
    val pos = program.stackPop
    val nextPC = program.verifyJumpDest(pos)
    ctx.setHint(s"~> $nextPC")
    program.setPC(nextPC)
  })

  instructionTable(OpCode.JUMPI.id) = Runtime(ctx => {
    val program = ctx.program
    val pos = program.stackPop
    val cond = program.stackPop

    if (!cond.isZero) {
      val nextPC = program.verifyJumpDest(pos)
      ctx.setHint(s"~> $nextPC")
      program.setPC(nextPC)
    } else {
      program.step()
    }
  })

  instructionTable(OpCode.PC.id) = Runtime(ctx => {
    val program = ctx.program
    val pc = program.getPC
    val pcWord = DataWord.of(pc)

    ctx.setHint(pcWord.toString)

    program.stackPush(pcWord)
    program.step()
  })

  instructionTable(OpCode.MSIZE.id) = Runtime(ctx => {
    val program = ctx.program
    val memSize = program.getMemSize
    val wordMemSize = DataWord.of(memSize)

    ctx.setHint(s"$memSize")

    program.stackPush(wordMemSize)
    program.step()
  })

  instructionTable(OpCode.GAS.id) = Runtime(ctx => {
    val program = ctx.program
    val gas = program.getGas

    ctx.setHint(s"$gas")

    program.stackPush(gas)
    program.step()
  })

  instructionTable(OpCode.JUMPDEST.id) = Runtime(ctx => {
    val program = ctx.program
    program.step()
  })

  for (i <- OpCode.PUSH1.id to OpCode.PUSH32.id) {
    instructionTable(i) = Runtime(ctx => {
      val program = ctx.program
      val op = ctx.op
      program.step()
      val nPush = op.code.value - OpCode.PUSH1.value + 1

      val data = program.sweep(nPush)

      ctx.setHint(s"${data.toHex}")

      program.stackPush(data)
    })
  }

  for (i <- OpCode.DUP1.id to OpCode.DUP16.id) {
    instructionTable(i) = Runtime(ctx => {
      val op = ctx.op
      val program = ctx.program
      val stack = program.getStack
      val n = op.code.value - OpCode.DUP1.value + 1
      val word_1 = stack.get(stack.size - n)
      program.stackPush(word_1)
      program.step()
    })
  }

  for (i <- OpCode.SWAP1.id to OpCode.SWAP16.id) {
    instructionTable(i) = Runtime(ctx => {
      val op = ctx.op
      val program = ctx.program
      val stack = program.getStack
      val n = op.code.value - OpCode.SWAP1.value + 2
      stack.swap(stack.size - 1, stack.size - n)
      program.step()
    })
  }

  for (i <- OpCode.LOG0.id to OpCode.LOG4.id) {
    instructionTable(i) = Runtime(ctx => {
      val op = ctx.op
      if(op.code.id != OpCode.LOG0.id){
        val program = ctx.program
        val stack = program.getStack
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

        ctx.setHint(logInfo.toString)

        program.getResult.addLogInfo(logInfo)
        program.step()
      }
    }, logGasCost)
  }

  instructionTable(OpCode.CREATE.id) = Runtime(ctx => {
    val program = ctx.program
    val op = ctx.op
    if (program.isStaticCall) throw Program.staticCallModificationException

    val value = program.stackPop
    val inOffset = program.stackPop
    val inSize = program.stackPop

    ctx.logHint

    program.createContract(value, inOffset, inSize)

    program.step()
  }, createGasCost)

  instructionTable(OpCode.CREATE2.id) = Runtime(ctx => {
    val program = ctx.program
    val op = ctx.op
    if (program.isStaticCall) throw Program.staticCallModificationException

    val value = program.stackPop
    val inOffset = program.stackPop
    val inSize = program.stackPop
    val salt = program.stackPop

    ctx.logHint

    program.createContract2(value, inOffset, inSize, salt)

    program.step()
  }, create2GasCost)

  instructionTable(OpCode.CALL.id) = Runtime(ctx => {
    val program = ctx.program
    val op = ctx.op
    program.stackPop // use adjustedCallGas instead of requested
    val codeAddress = program.stackPop
    val value = if (op.callHasValue) program.stackPop else DataWord.ZERO

    if (program.isStaticCall && (op.code == OpCode.CALL) && !value.isZero) {
      throw Program.staticCallModificationException
    }

    if (!value.isZero) {
      ctx.adjustedCallGas = ctx.adjustedCallGas.add(DataWord.of(GasCost.STIPEND_CALL))
    }

    val inDataOffs = program.stackPop
    val inDataSize = program.stackPop
    val outDataOffs = program.stackPop
    val outDataSize = program.stackPop

    ctx.setHint(s"addr: ${codeAddress.getLast20Bytes.toHex} gas: ${ctx.adjustedCallGas.shortHex} inOff: ${inDataOffs.shortHex} inSize: ${inDataSize.shortHex}")
    ctx.logHint

    program.memoryExpand(outDataOffs, outDataSize)

    val msg = MessageCall(op.code, ctx.adjustedCallGas, codeAddress, value, inDataOffs, inDataSize, outDataOffs, outDataSize)
    val contract = PrecompiledContracts.getContractForAddress(codeAddress, ctx.settings)

    if (!op.callIsStateless) {
      program.getResult.addTouchAccount(codeAddress.toUInt160)
    }

    if (contract != null) {
      program.callToPrecompiledAddress(msg, contract)
    } else {
      program.callToAddress(msg)
    }

    program.step()
  }, callGasCost)

  instructionTable(OpCode.CALLCODE.id) = instructionTable(OpCode.CALL.id)

  instructionTable(OpCode.DELEGATECALL.id) = instructionTable(OpCode.CALL.id)

  instructionTable(OpCode.STATICCALL.id) = instructionTable(OpCode.CALL.id)

  instructionTable(OpCode.RETURN.id) = Runtime(ctx => {
    val program = ctx.program
    val op = ctx.op
    val offset = program.stackPop
    val size = program.stackPop

    val hReturn = program.memoryChunk(offset.intValueSafe, size.intValueSafe)
    program.setHReturn(hReturn)

    ctx.setHint(s"data: ${hReturn.toHex} offset: ${offset.value} size: ${size.value}")

    program.step()
    program.stop()

    if (op.code == OpCode.REVERT) {
      program.getResult.setRevert()
    }
  }, stopGasCost)

  instructionTable(OpCode.REVERT.id) = instructionTable(OpCode.RETURN.id)

  instructionTable(OpCode.SUICIDE.id) = Runtime(ctx => {
    val program = ctx.program
    if (program.isStaticCall) throw Program.staticCallModificationException

    val address = program.stackPop
    program.suicide(address)
    program.getResult.addTouchAccount(address.toUInt160)

    ctx.setHint(s"address: ${program.getOwnerAddress.getLast20Bytes.toHex}")

    program.stop()
  }, suicideGasCost)


  private def callGasCost(ctx: ExecuteContext): Unit = {
    val op = ctx.op
    val stack = ctx.stack
    val program = ctx.program

    var gasCost: Long = GasCost.CALL
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
    gasCost += calcMemGas(ctx.oldMemSize, in.max(out), 0)

    if (gasCost > program.getGas.longValueSafe) {
      throw Program.notEnoughOpGas(op.code, callGasWord, program.getGas)
    }

    val gasLeft = program.getGas
    val subResult = gasLeft.sub(DataWord.of(gasCost))
    val adjustedCallGas = getCallGas(op.code, callGasWord, subResult)
    ctx.spendGas(gasCost, adjustedCallGas, true)
  }

  private def suicideGasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val program = ctx.program
    val suicideAddress = stack.get(stack.size - 1).toUInt160
    var gasCost = GasCost.SUICIDE
    if (!program.getStorage.accountExists(suicideAddress)) {
      gasCost += GasCost.NEW_ACCT_SUICIDE
    }
    ctx.spendGas(gasCost)
  }

  private def sstoreGasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val program = ctx.program
    var currentValue = program.getCurrentValue(stack.peek)
    if (currentValue == null) currentValue = DataWord.ZERO
    val newValue = stack.get(stack.size - 2)
    var gasCost = 0
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
    ctx.spendGas(gasCost)
  }

  private def mstoreGasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val oldMemSize = ctx.oldMemSize
    val gasCost = calcMemGas(oldMemSize, memNeeded(stack.peek, DataWord.of(32)), 0)
    ctx.spendGas(gasCost, false)
  }

  private def mstore8GasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val oldMemSize = ctx.oldMemSize
    val gasCost = calcMemGas(oldMemSize, memNeeded(stack.peek, DataWord.ONE), 0)
    ctx.spendGas(gasCost, false)
  }

  private def mloadGasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val oldMemSize = ctx.oldMemSize
    val gasCost = calcMemGas(oldMemSize, memNeeded(stack.peek, DataWord.of(32)), 0)
    ctx.spendGas(gasCost, false)
  }

  private def stopGasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val oldMemSize = ctx.oldMemSize
    val gasCost = GasCost.STOP + calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 2)), 0)
    ctx.spendGas(gasCost)
  }

  private def sha3GasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val oldMemSize = ctx.oldMemSize
    var gasCost = GasCost.SHA3 + calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 2)), 0)
    val size = stack.get(stack.size - 2)
    val chunkUsed = VM.getSizeInWords(size.longValueSafe)
    gasCost += chunkUsed * GasCost.SHA3_WORD
    ctx.spendGas(gasCost)
  }

  private def dataCopyGasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val oldMemSize = ctx.oldMemSize
    val gasCost = calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 3)), stack.get(stack.size - 3).longValueSafe)
    ctx.spendGas(gasCost, false)
  }

  private def codeCopyGasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val oldMemSize = ctx.oldMemSize
    val gasCost = calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 3)), stack.get(stack.size - 3).longValueSafe)
    ctx.spendGas(gasCost, false)
  }

  private def extCodeCopyGasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val oldMemSize = ctx.oldMemSize
    val gasCost = GasCost.EXT_CODE_COPY + calcMemGas(oldMemSize, memNeeded(stack.get(stack.size - 2), stack.get(stack.size - 4)), stack.get(stack.size - 4).longValueSafe)
    ctx.spendGas(gasCost)
  }

  private def createGasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val oldMemSize = ctx.oldMemSize
    val gasCost = GasCost.CREATE + calcMemGas(oldMemSize, memNeeded(stack.get(stack.size - 2), stack.get(stack.size - 3)), 0)
    ctx.spendGas(gasCost)
  }

  private def create2GasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val oldMemSize = ctx.oldMemSize
    val codeSize = stack.get(stack.size - 3)
    val gasCost = GasCost.CREATE +
      calcMemGas(oldMemSize, memNeeded(stack.get(stack.size - 2), codeSize), 0) +
      getSizeInWords(codeSize.longValueSafe) * GasCost.SHA3_WORD
    ctx.spendGas(gasCost)
  }

  private def logGasCost(ctx: ExecuteContext): Unit = {
    val op = ctx.op
    val stack = ctx.stack
    val program = ctx.program
    val oldMemSize = ctx.oldMemSize
    val dataSize = stack.get(stack.size - 2).value
    val dataCost = dataSize * BigInt(GasCost.LOG_DATA_GAS)
    if (program.getGas.value < dataCost) {
      throw Program.notEnoughOpGas(op.code, dataCost, program.getGas.value)
    }
    val nTopics = op.code.value - OpCode.LOG0.value
    val gasCost = GasCost.LOG_GAS + GasCost.LOG_TOPIC_GAS * nTopics +
      GasCost.LOG_DATA_GAS * stack.get(stack.size - 2).longValue +
      calcMemGas(oldMemSize, memNeeded(stack.peek, stack.get(stack.size - 2)), 0)
    ctx.spendGas(gasCost)
  }

  private def expGasCost(ctx: ExecuteContext): Unit = {
    val stack = ctx.stack
    val exp = stack.get(stack.size - 2)
    val bytesOccupied = exp.bytesOccupied
    val gasCost = GasCost.EXP_GAS + GasCost.EXP_BYTE_GAS * bytesOccupied
    ctx.spendGas(gasCost)
  }

  private def binaryOp1(op: String, action: (DataWord, DataWord) => DataWord, ctx: ExecuteContext) = {
    val word1 = ctx.program.stackPop
    val word2 = ctx.program.stackPop

    val result = action(word1, word2)
    ctx.setHint(s"${word1.value} $op ${word2.value}")
    ctx.program.stackPush(result)
    ctx.program.step()
  }

  private def binaryOp2(action: (DataWord, DataWord) => DataWord, ctx: ExecuteContext) = {
    val word1 = ctx.program.stackPop
    val word2 = ctx.program.stackPop

    val result = action(word1, word2)
    ctx.setHint(s"${result.value}")
    ctx.program.stackPush(result)
    ctx.program.step()
  }

  private def ternaryOp(action: (DataWord, DataWord, DataWord) => DataWord, ctx: ExecuteContext) = {
    val word1 = ctx.program.stackPop
    val word2 = ctx.program.stackPop
    val word3 = ctx.program.stackPop
    val result = action(word1, word2, word3)
    ctx.program.stackPush(result)
    ctx.program.step()
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

class Runtime(executor: ExecuteContext => Unit,
              calculator: ExecuteContext => Unit) {
  def execute(ctx: ExecuteContext) = {
    executor(ctx)
  }

  def spendGas(ctx: ExecuteContext) = {
    calculator(ctx)
  }

  def checkStopTime(ctx: ExecuteContext) = {
    ctx.checkTime()
  }

  def dumpLine(ctx: ExecuteContext, dumpBlock: Long) = {
    ctx.dumpLine(dumpBlock)
  }
}

object Runtime {
  def apply(executor: ExecuteContext => Unit)(implicit gasCostOpt: Option[Int] = None): Runtime = {
    apply(executor, gasCostOpt match {
      case Some(gasCost) => ctx => ctx.spendGas(gasCost)
      case None => ctx => ctx.spendGas(0, false)
    })
  }

  def apply(executor: ExecuteContext => Unit, calculator: ExecuteContext => Unit): Runtime = {
    new Runtime(ctx => executor.andThen(_ => ctx.setPreviousOp)(ctx), calculator)
  }
}

class ExecuteContext(val op: OpObject, val program: Program, val settings: ContractSettings, val log: Logger) {
  program.setLastOp(op.code.value)
  program.verifyStackSize(op.require)
  program.verifyStackOverflow(op.require, op.ret)

  val gasBefore = program.getGasLong
  val oldMemSize = program.getMemSize
  val stack = program.getStack

  val memWords = 0L
  val callGas = 0L

  var gasCost = op.tier.toLong
  var adjustedCallGas: DataWord = null
  var hint: String = ""

  def spendGas(gas: Long, reset: Boolean = true): Unit = {
    if (reset) gasCost = gas else gasCost += gas
    program.spendGas(gasCost, op.code.name)
  }

  def spendGas(gas: Long, adjustedGas: DataWord, reset: Boolean): Unit = {
    spendGas(gas + adjustedGas.longValueSafe, reset)
    adjustedCallGas = adjustedGas
  }

  def checkTime() = {
    program.checkStopTime()
  }

  def setPreviousOp() = {
    program.setPreviouslyExecutedOp(op.code.value)
  }

  def setHint(message: String) = {
    if (log.isInfoEnabled) {
      hint = message
    }
  }

  def logHint() = {
    if (log.isInfoEnabled) {
      log.info(s"[${program.getPC.formatted("%5s")}]    Op: [${op.code.name.formatted("%-12s")}]  Gas: [${program.getGas.value}] Deep: [${program.getCallDeep}]  Hint: [${hint}]")
    }
  }

  def dumpLine(dumpBlock: Long) = {
    if (program.getNumber.intValue == dumpBlock) {

    }
  }
}