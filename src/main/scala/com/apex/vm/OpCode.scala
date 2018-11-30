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

package org.apex.vm

import com.apex.vm.exceptions.IllegalOperationException

object Tier extends Enumeration {
  val ZeroTier = Value(0)
  val BaseTier = Value(2)
  val VeryLowTier = Value(3)
  val LowTier = Value(5)
  val MidTier = Value(8)
  val HighTier = Value(10)
  val ExtTier = Value(20)
  val SpecialTier = Value(1) //TODO #POC9 is this correct?? "multiparam" from cpp
//  val InvalidTier = Value(0)

  implicit class Extension(tier: Tier.Value) {
    def toLong = {
      tier.id.toLong
    }
  }
}

object CallFlags extends Enumeration {
  val Call = Value
  val Stateless = Value
  val HasValue = Value
  val Static = Value
  val Delegate = Value
}

case class OpObject(code: OpCode.Value, require: Int, ret: Int, tier: Tier.Value, callFlags: CallFlags.Value*) {
  /**
    * Indicates that opcode is a call
    */
  def isCall: Boolean = callFlags.contains(CallFlags.Call)

  /**
    * Indicates that the opcode has value parameter (3rd on stack)
    */
  def callHasValue: Boolean = {
    checkCall()
    callFlags.contains(CallFlags.HasValue)
  }

  /**
    * Indicates that the code is executed in the context of the caller
    */
  def callIsStateless: Boolean = {
    checkCall()
    callFlags.contains(CallFlags.Stateless)
  }

  private def checkCall(): Unit = {
    if (!isCall) throw new RuntimeException("Opcode is not a call: " + this)
  }
}

object OpCache {
  private val ops = collection.mutable.Map.empty[Byte, OpObject]

  def fromCode(code: Byte): OpObject = {
    if (!ops.contains(code)) {
      throw IllegalOperationException(code)
    } else {
      ops(code)
    }
  }

  def add(code: OpCode.Value, require: Int, ret: Int, tier: Tier.Value, callFlags: CallFlags.Value*): Unit = {
    ops.put(code.value, OpObject(code, require, ret, tier, callFlags: _*))
  }
}

/**
  * Instruction set for the Ethereum Virtual Machine
  * See Yellow Paper: http://www.gavwood.com/Paper.pdf
  * - Appendix G. Virtual Machine Specification
  */
object OpCode extends Enumeration {
  // TODO #POC9 Need to make tiers more accurate
  /**
    * Halts execution (0x00)
    */
  val STOP = getValue(0x00, 0, 0, Tier.ZeroTier)

  /*  Arithmetic Operations   */

  /**
    * (0x01) Addition operation
    */
  val ADD = getValue(0x01, 2, 1, Tier.VeryLowTier)
  /**
    * (0x02) Multiplication operation
    */
  val MUL = getValue(0x02, 2, 1, Tier.LowTier)
  /**
    * (0x03) Subtraction operations
    */
  val SUB = getValue(0x03, 2, 1, Tier.VeryLowTier)
  /**
    * (0x04) Integer division operation
    */
  val DIV = getValue(0x04, 2, 1, Tier.LowTier)
  /**
    * (0x05) Signed integer division operation
    */
  val SDIV = getValue(0x05, 2, 1, Tier.LowTier)
  /**
    * (0x06) Modulo remainder operation
    */
  val MOD = getValue(0x06, 2, 1, Tier.LowTier)
  /**
    * (0x07) Signed modulo remainder operation
    */
  val SMOD = getValue(0x07, 2, 1, Tier.LowTier)
  /**
    * (0x08) Addition combined with modulo
    * remainder operation
    */
  val ADDMOD = getValue(0x08, 3, 1, Tier.MidTier)
  /**
    * (0x09) Multiplication combined with modulo
    * remainder operation
    */
  val MULMOD = getValue(0x09, 3, 1, Tier.MidTier)
  /**
    * (0x0a) Exponential operation
    */
  val EXP = getValue(0x0a, 2, 1, Tier.SpecialTier)
  /**
    * (0x0b) Extend length of signed integer
    */
  val SIGNEXTEND = getValue(0x0b, 2, 1, Tier.LowTier)

  /*  Bitwise Logic & Comparison Operations   */

  /**
    * (0x10) Less-than comparison
    */
  val LT = getValue(0X10, 2, 1, Tier.VeryLowTier)
  /**
    * (0x11) Greater-than comparison
    */
  val GT = getValue(0X11, 2, 1, Tier.VeryLowTier)
  /**
    * (0x12) Signed less-than comparison
    */
  val SLT = getValue(0X12, 2, 1, Tier.VeryLowTier)
  /**
    * (0x13) Signed greater-than comparison
    */
  val SGT = getValue(0X13, 2, 1, Tier.VeryLowTier)
  /**
    * (0x14) Equality comparison
    */
  val EQ = getValue(0X14, 2, 1, Tier.VeryLowTier)
  /**
    * (0x15) Negation operation
    */
  val ISZERO = getValue(0x15, 1, 1, Tier.VeryLowTier)
  /**
    * (0x16) Bitwise AND operation
    */
  val AND = getValue(0x16, 2, 1, Tier.VeryLowTier)
  /**
    * (0x17) Bitwise OR operation
    */
  val OR = getValue(0x17, 2, 1, Tier.VeryLowTier)
  /**
    * (0x18) Bitwise XOR operation
    */
  val XOR = getValue(0x18, 2, 1, Tier.VeryLowTier)
  /**
    * (0x19) Bitwise NOT operationr
    */
  val NOT = getValue(0x19, 1, 1, Tier.VeryLowTier)
  /**
    * (0x1a) Retrieve single byte from word
    */
  val BYTE = getValue(0x1a, 2, 1, Tier.VeryLowTier)
  /**
    * (0x1b) Shift left
    */
  val SHL = getValue(0x1b, 2, 1, Tier.VeryLowTier)
  /**
    * (0x1c) Logical shift right
    */
  val SHR = getValue(0x1c, 2, 1, Tier.VeryLowTier)
  /**
    * (0x1d) Arithmetic shift right
    */
  val SAR = getValue(0x1d, 2, 1, Tier.VeryLowTier)

  /*  Cryptographic Operations    */

  /**
    * (0x20) Compute SHA3-256 hash
    */
  val SHA3 = getValue(0x20, 2, 1, Tier.SpecialTier)

  /*  Environmental Information   */

  /**
    * (0x30)  Get address of currently
    * executing account
    */
  val ADDRESS = getValue(0x30, 0, 1, Tier.BaseTier)
  /**
    * (0x31) Get balance of the given account
    */
  val BALANCE = getValue(0x31, 1, 1, Tier.ExtTier)
  /**
    * (0x32) Get execution origination address
    */
  val ORIGIN = getValue(0x32, 0, 1, Tier.BaseTier)
  /**
    * (0x33) Get caller address
    */
  val CALLER = getValue(0x33, 0, 1, Tier.BaseTier)
  /**
    * (0x34) Get deposited value by the
    * instruction/transaction responsible
    * for this execution
    */
  val CALLVALUE = getValue(0x34, 0, 1, Tier.BaseTier)
  /**
    * (0x35) Get input data of current
    * environment
    */
  val CALLDATALOAD = getValue(0x35, 1, 1, Tier.VeryLowTier)
  /**
    * (0x36) Get size of input data in current
    * environment
    */
  val CALLDATASIZE = getValue(0x36, 0, 1, Tier.BaseTier)
  /**
    * (0x37) Copy input data in current
    * environment to memory
    */
  val CALLDATACOPY = getValue(0x37, 3, 0, Tier.VeryLowTier)
  /**
    * (0x38) Get size of code running in
    * current environment
    */
  val CODESIZE = getValue(0x38, 0, 1, Tier.BaseTier)
  /**
    * (0x39) Copy code running in current
    * environment to memory
    */
  val CODECOPY = getValue(0x39, 3, 0, Tier.VeryLowTier) // [len code_start mem_start CODECOPY]

  val RETURNDATASIZE = getValue(0x3d, 0, 1, Tier.BaseTier)

  val RETURNDATACOPY = getValue(0x3e, 3, 0, Tier.VeryLowTier)
  /**
    * (0x3a) Get price of gas in current
    * environment
    */
  val GASPRICE = getValue(0x3a, 0, 1, Tier.BaseTier)
  /**
    * (0x3b) Get size of code running in
    * current environment with given offset
    */
  val EXTCODESIZE = getValue(0x3b, 1, 1, Tier.ExtTier)
  /**
    * (0x3c) Copy code running in current
    * environment to memory with given offset
    */
  val EXTCODECOPY = getValue(0x3c, 4, 0, Tier.ExtTier)
  /**
    * (0x3f) Returns the keccak256 hash of
    * a contract’s code
    */
  val EXTCODEHASH = getValue(0x3f, 1,1 , Tier.ExtTier)

  /*  Block Information   */

  /**
    * (0x40) Get hash of most recent
    * complete block
    */
  val BLOCKHASH = getValue(0x40, 1, 1, Tier.ExtTier)
  /**
    * (0x41) Get the block’s coinbase address
    */
  val COINBASE = getValue(0x41, 0, 1, Tier.BaseTier)
  /**
    * (x042) Get the block’s timestamp
    */
  val TIMESTAMP = getValue(0x42, 0, 1, Tier.BaseTier)
  /**
    * (0x43) Get the block’s number
    */
  val NUMBER = getValue(0x43, 0, 1, Tier.BaseTier)
  /**
    * (0x44) Get the block’s difficulty
    */
  val DIFFICULTY = getValue(0x44, 0, 1, Tier.BaseTier)
  /**
    * (0x45) Get the block’s gas limit
    */
  val GASLIMIT = getValue(0x45, 0, 1, Tier.BaseTier)

  /*  Memory, Storage and Flow Operations */

  /**
    * (0x50) Remove item from stack
    */
  val POP = getValue(0x50, 1, 0, Tier.BaseTier)
  /**
    * (0x51) Load word from memory
    */
  val MLOAD = getValue(0x51, 1, 1, Tier.VeryLowTier)
  /**
    * (0x52) Save word to memory
    */
  val MSTORE = getValue(0x52, 2, 0, Tier.VeryLowTier)
  /**
    * (0x53) Save byte to memory
    */
  val MSTORE8 = getValue(0x53, 2, 0, Tier.VeryLowTier)
  /**
    * (0x54) Load word from storage
    */
  val SLOAD = getValue(0x54, 1, 1, Tier.SpecialTier)
  /**
    * (0x55) Save word to storage
    */
  val SSTORE = getValue(0x55, 2, 0, Tier.SpecialTier)
  /**
    * (0x56) Alter the program counter
    */
  val JUMP = getValue(0x56, 1, 0, Tier.MidTier)
  /**
    * (0x57) Conditionally alter the program
    * counter
    */
  val JUMPI = getValue(0x57, 2, 0, Tier.HighTier)
  /**
    * (0x58) Get the program counter
    */
  val PC = getValue(0x58, 0, 1, Tier.BaseTier)
  /**
    * (0x59) Get the size of active memory
    */
  val MSIZE = getValue(0x59, 0, 1, Tier.BaseTier)
  /**
    * (0x5a) Get the amount of available gas
    */
  val GAS = getValue(0x5a, 0, 1, Tier.BaseTier)
  /**
    * (0x5b)
    */
  val JUMPDEST = getValue(0x5b, 0, 0, Tier.SpecialTier)

  /*  Push Operations */

  /**
    * (0x60) Place 1-byte item on stack
    */
  val PUSH1 = getValue(0x60, 0, 1, Tier.VeryLowTier)
  /**
    * (0x61) Place 2-byte item on stack
    */
  val PUSH2 = getValue(0x61, 0, 1, Tier.VeryLowTier)
  /**
    * (0x62) Place 3-byte item on stack
    */
  val PUSH3 = getValue(0x62, 0, 1, Tier.VeryLowTier)
  /**
    * (0x63) Place 4-byte item on stack
    */
  val PUSH4 = getValue(0x63, 0, 1, Tier.VeryLowTier)
  /**
    * (0x64) Place 5-byte item on stack
    */
  val PUSH5 = getValue(0x64, 0, 1, Tier.VeryLowTier)
  /**
    * (0x65) Place 6-byte item on stack
    */
  val PUSH6 = getValue(0x65, 0, 1, Tier.VeryLowTier)
  /**
    * (0x66) Place 7-byte item on stack
    */
  val PUSH7 = getValue(0x66, 0, 1, Tier.VeryLowTier)
  /**
    * (0x67) Place 8-byte item on stack
    */
  val PUSH8 = getValue(0x67, 0, 1, Tier.VeryLowTier)
  /**
    * (0x68) Place 9-byte item on stack
    */
  val PUSH9 = getValue(0x68, 0, 1, Tier.VeryLowTier)
  /**
    * (0x69) Place 10-byte item on stack
    */
  val PUSH10 = getValue(0x69, 0, 1, Tier.VeryLowTier)
  /**
    * (0x6a) Place 11-byte item on stack
    */
  val PUSH11 = getValue(0x6a, 0, 1, Tier.VeryLowTier)
  /**
    * (0x6b) Place 12-byte item on stack
    */
  val PUSH12 = getValue(0x6b, 0, 1, Tier.VeryLowTier)
  /**
    * (0x6c) Place 13-byte item on stack
    */
  val PUSH13 = getValue(0x6c, 0, 1, Tier.VeryLowTier)
  /**
    * (0x6d) Place 14-byte item on stack
    */
  val PUSH14 = getValue(0x6d, 0, 1, Tier.VeryLowTier)
  /**
    * (0x6e) Place 15-byte item on stack
    */
  val PUSH15 = getValue(0x6e, 0, 1, Tier.VeryLowTier)
  /**
    * (0x6f) Place 16-byte item on stack
    */
  val PUSH16 = getValue(0x6f, 0, 1, Tier.VeryLowTier)
  /**
    * (0x70) Place 17-byte item on stack
    */
  val PUSH17 = getValue(0x70, 0, 1, Tier.VeryLowTier)
  /**
    * (0x71) Place 18-byte item on stack
    */
  val PUSH18 = getValue(0x71, 0, 1, Tier.VeryLowTier)
  /**
    * (0x72) Place 19-byte item on stack
    */
  val PUSH19 = getValue(0x72, 0, 1, Tier.VeryLowTier)
  /**
    * (0x73) Place 20-byte item on stack
    */
  val PUSH20 = getValue(0x73, 0, 1, Tier.VeryLowTier)
  /**
    * (0x74) Place 21-byte item on stack
    */
  val PUSH21 = getValue(0x74, 0, 1, Tier.VeryLowTier)
  /**
    * (0x75) Place 22-byte item on stack
    */
  val PUSH22 = getValue(0x75, 0, 1, Tier.VeryLowTier)
  /**
    * (0x76) Place 23-byte item on stack
    */
  val PUSH23 = getValue(0x76, 0, 1, Tier.VeryLowTier)
  /**
    * (0x77) Place 24-byte item on stack
    */
  val PUSH24 = getValue(0x77, 0, 1, Tier.VeryLowTier)
  /**
    * (0x78) Place 25-byte item on stack
    */
  val PUSH25 = getValue(0x78, 0, 1, Tier.VeryLowTier)
  /**
    * (0x79) Place 26-byte item on stack
    */
  val PUSH26 = getValue(0x79, 0, 1, Tier.VeryLowTier)
  /**
    * (0x7a) Place 27-byte item on stack
    */
  val PUSH27 = getValue(0x7a, 0, 1, Tier.VeryLowTier)
  /**
    * (0x7b) Place 28-byte item on stack
    */
  val PUSH28 = getValue(0x7b, 0, 1, Tier.VeryLowTier)
  /**
    * (0x7c) Place 29-byte item on stack
    */
  val PUSH29 = getValue(0x7c, 0, 1, Tier.VeryLowTier)
  /**
    * (0x7d) Place 30-byte item on stack
    */
  val PUSH30 = getValue(0x7d, 0, 1, Tier.VeryLowTier)
  /**
    * (0x7e) Place 31-byte item on stack
    */
  val PUSH31 = getValue(0x7e, 0, 1, Tier.VeryLowTier)
  /**
    * (0x7f) Place 32-byte (full word)
    * item on stack
    */
  val PUSH32 = getValue(0x7f, 0, 1, Tier.VeryLowTier)

  /*  Duplicate Nth item from the stack   */

  /**
    * (0x80) Duplicate 1st item on stack
    */
  val DUP1 = getValue(0x80, 1, 2, Tier.VeryLowTier)
  /**
    * (0x81) Duplicate 2nd item on stack
    */
  val DUP2 = getValue(0x81, 2, 3, Tier.VeryLowTier)
  /**
    * (0x82) Duplicate 3rd item on stack
    */
  val DUP3 = getValue(0x82, 3, 4, Tier.VeryLowTier)
  /**
    * (0x83) Duplicate 4th item on stack
    */
  val DUP4 = getValue(0x83, 4, 5, Tier.VeryLowTier)
  /**
    * (0x84) Duplicate 5th item on stack
    */
  val DUP5 = getValue(0x84, 5, 6, Tier.VeryLowTier)
  /**
    * (0x85) Duplicate 6th item on stack
    */
  val DUP6 = getValue(0x85, 6, 7, Tier.VeryLowTier)
  /**
    * (0x86) Duplicate 7th item on stack
    */
  val DUP7 = getValue(0x86, 7, 8, Tier.VeryLowTier)
  /**
    * (0x87) Duplicate 8th item on stack
    */
  val DUP8 = getValue(0x87, 8, 9, Tier.VeryLowTier)
  /**
    * (0x88) Duplicate 9th item on stack
    */
  val DUP9 = getValue(0x88, 9, 10, Tier.VeryLowTier)
  /**
    * (0x89) Duplicate 10th item on stack
    */
  val DUP10 = getValue(0x89, 10, 11, Tier.VeryLowTier)
  /**
    * (0x8a) Duplicate 11th item on stack
    */
  val DUP11 = getValue(0x8a, 11, 12, Tier.VeryLowTier)
  /**
    * (0x8b) Duplicate 12th item on stack
    */
  val DUP12 = getValue(0x8b, 12, 13, Tier.VeryLowTier)
  /**
    * (0x8c) Duplicate 13th item on stack
    */
  val DUP13 = getValue(0x8c, 13, 14, Tier.VeryLowTier)
  /**
    * (0x8d) Duplicate 14th item on stack
    */
  val DUP14 = getValue(0x8d, 14, 15, Tier.VeryLowTier)
  /**
    * (0x8e) Duplicate 15th item on stack
    */
  val DUP15 = getValue(0x8e, 15, 16, Tier.VeryLowTier)
  /**
    * (0x8f) Duplicate 16th item on stack
    */
  val DUP16 = getValue(0x8f, 16, 17, Tier.VeryLowTier)

  /*  Swap the Nth item from the stack with the top   */

  /**
    * (0x90) Exchange 2nd item from stack with the top
    */
  val SWAP1 = getValue(0x90, 2, 2, Tier.VeryLowTier)
  /**
    * (0x91) Exchange 3rd item from stack with the top
    */
  val SWAP2 = getValue(0x91, 3, 3, Tier.VeryLowTier)
  /**
    * (0x92) Exchange 4th item from stack with the top
    */
  val SWAP3 = getValue(0x92, 4, 4, Tier.VeryLowTier)
  /**
    * (0x93) Exchange 5th item from stack with the top
    */
  val SWAP4 = getValue(0x93, 5, 5, Tier.VeryLowTier)
  /**
    * (0x94) Exchange 6th item from stack with the top
    */
  val SWAP5 = getValue(0x94, 6, 6, Tier.VeryLowTier)
  /**
    * (0x95) Exchange 7th item from stack with the top
    */
  val SWAP6 = getValue(0x95, 7, 7, Tier.VeryLowTier)
  /**
    * (0x96) Exchange 8th item from stack with the top
    */
  val SWAP7 = getValue(0x96, 8, 8, Tier.VeryLowTier)
  /**
    * (0x97) Exchange 9th item from stack with the top
    */
  val SWAP8 = getValue(0x97, 9, 9, Tier.VeryLowTier)
  /**
    * (0x98) Exchange 10th item from stack with the top
    */
  val SWAP9 = getValue(0x98, 10, 10, Tier.VeryLowTier)
  /**
    * (0x99) Exchange 11th item from stack with the top
    */
  val SWAP10 = getValue(0x99, 11, 11,Tier.VeryLowTier)
  /**
    * (0x9a) Exchange 12th item from stack with the top
    */
  val SWAP11 = getValue(0x9a, 12, 12, Tier.VeryLowTier)
  /**
    * (0x9b) Exchange 13th item from stack with the top
    */
  val SWAP12 = getValue(0x9b, 13, 13, Tier.VeryLowTier)
  /**
    * (0x9c) Exchange 14th item from stack with the top
    */
  val SWAP13 = getValue(0x9c, 14, 14, Tier.VeryLowTier)
  /**
    * (0x9d) Exchange 15th item from stack with the top
    */
  val SWAP14 = getValue(0x9d, 15, 15, Tier.VeryLowTier)
  /**
    * (0x9e) Exchange 16th item from stack with the top
    */
  val SWAP15 = getValue(0x9e, 16, 16, Tier.VeryLowTier)
  /**
    * (0x9f) Exchange 17th item from stack with the top
    */
  val SWAP16 = getValue(0x9f, 17, 17, Tier.VeryLowTier)

  /**
    * (0xa[n]) log some data for some addres with 0..n tags [addr [tag0..tagn] data]
    */
  val LOG0 = getValue(0xa0, 2, 0, Tier.SpecialTier)
  val LOG1 = getValue(0xa1, 3, 0, Tier.SpecialTier)
  val LOG2 = getValue(0xa2, 4, 0, Tier.SpecialTier)
  val LOG3 = getValue(0xa3, 5, 0, Tier.SpecialTier)
  val LOG4 = getValue(0xa4, 6, 0, Tier.SpecialTier)

  /*  System operations   */

  /**
    * (0xf0) Create a new account with associated code
    */
  val CREATE = getValue(0xf0, 3, 1, Tier.SpecialTier)   //       [in_size] [in_offs] [gas_val] CREATE
  /**
    * (cxf1) Message-call into an account
    */
  val CALL = getValue(0xf1, 7, 1, Tier.SpecialTier, CallFlags.Call, CallFlags.HasValue)
  //       [out_data_size] [out_data_start] [in_data_size] [in_data_startCallFlags.] [value] [to_addr]
  // [gas] CALL
  /**
    * (0xf2) Calls self, but grabbing the code from the
    * TO argument instead of from one's own address
    */
  val CALLCODE = getValue(0xf2, 7, 1, Tier.SpecialTier, CallFlags.Call, CallFlags.HasValue, CallFlags.Stateless)
  /**
    * (0xf3) Halt execution returning output data
    */
  val RETURN = getValue(0xf3, 2, 0, Tier.ZeroTier)

  /**
    * (0xf4)  similar in idea to CALLCODE, except that it propagates the sender and value
    *  from the parent scope to the child scope, ie. the call created has the same sender
    *  and value as the original call.
    *  also the Value parameter is omitted for this opCode
    */
  val DELEGATECALL = getValue(0xf4, 6, 1, Tier.SpecialTier, CallFlags.Call, CallFlags.Stateless, CallFlags.Delegate)

  /**
    * (0xf5) Skinny CREATE2, same as CREATE but with deterministic address
    */
  val CREATE2 = getValue(0xf5, 4, 1, Tier.SpecialTier)

  /**
    *  opcode that can be used to call another contract (or itself) while disallowing any
    *  modifications to the state during the call (and its subcalls, if present).
    *  Any opcode that attempts to perform such a modification (see below for details)
    *  will result in an exception instead of performing the modification.
    */
  val STATICCALL = getValue(0xfa, 6, 1, Tier.SpecialTier, CallFlags.Call, CallFlags.Static)

  /**
    * (0xfd) The `REVERT` instruction will stop execution, roll back all state changes done so far
    * and provide a pointer to a memory section, which can be interpreted as an error code or message.
    * While doing so, it will not consume all the remaining gas.
    */
  val REVERT = getValue(0xfd, 2, 0, Tier.ZeroTier)
  /**
    * (0xff) Halt execution and register account for
    * later deletion
    */
  val SUICIDE = getValue(0xff, 1, 0, Tier.ZeroTier)

  private def getValue(code: Int, require: Int, ret: Int, tier: Tier.Value, callFlags: CallFlags.Value*) = {
    val value = Value(code)
    OpCache.add(value, require, ret, tier, callFlags: _*)
    value
  }

  implicit class Extension(code: OpCode.Value) {
    def value: Byte = code.id.toByte

    def name: String = code.name
  }
}
