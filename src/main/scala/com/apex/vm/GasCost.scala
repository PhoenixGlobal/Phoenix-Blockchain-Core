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
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: GasCost.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-28 下午3:59@version: 1.0
 *
 */

package com.apex.vm

/**
  * The fundamental network cost unit. Paid for exclusively by Ether, which is converted
  * freely to and from Gas as required. Gas does not exist outside of the internal Ethereum
  * computation engine; its price is set by the Transaction and miners are free to
  * ignore Transactions whose Gas price is too low.
  */
object GasCost {
  /* backwards compatibility, remove eventually */
  final val STEP = 1
  final val SSTORE = 300


  final val ZEROSTEP = 0
  final val QUICKSTEP = 2
  final val FASTESTSTEP = 3
  final val FASTSTEP = 5
  final val MIDSTEP = 8
  final val SLOWSTEP = 10
  final val EXTSTEP = 20

  final val GENESISGASLIMIT = 1000000
  final val MINGASLIMIT = 125000

  final val BALANCE = 20
  final val SHA3 = 30
  final val SHA3_WORD = 6
  final val SLOAD = 50
  final val STOP = 0
  final val SUICIDE = 0
  final val CLEAR_SSTORE = 5000
  final val SET_SSTORE = 20000
  final val RESET_SSTORE = 5000
  final val REFUND_SSTORE = 15000
  final val REUSE_SSTORE = 200
  final val CREATE = 32000

  final val JUMPDEST = 1
  final val CREATE_DATA_BYTE = 5
  final val CALL = 40
  final val STIPEND_CALL = 2300
  final val VT_CALL = 9000 //value transfer call

  final val NEW_ACCT_CALL = 25000 //new account call

  final val MEMORY = 3
  final val SUICIDE_REFUND = 24000
  final val QUAD_COEFF_DIV = 512
  final val CREATE_DATA = 200
  final val TX_NO_ZERO_DATA = 68
  final val TX_ZERO_DATA = 4
  final val TRANSACTION = 21000
  final val TRANSACTION_CREATE_CONTRACT = 53000
  final val LOG_GAS = 375
  final val LOG_DATA_GAS = 8
  final val LOG_TOPIC_GAS = 375
  final val COPY_GAS = 3
  final val EXP_GAS = 10
  final val EXP_BYTE_GAS = 10
  final val IDENTITY = 15
  final val IDENTITY_WORD = 3
  final val RIPEMD160 = 600
  final val RIPEMD160_WORD = 120
  final val SHA256 = 60
  final val SHA256_WORD = 12
  final val EC_RECOVER = 3000
  final val EXT_CODE_SIZE = 20
  final val EXT_CODE_COPY = 20
  final val EXT_CODE_HASH = 400
  final val NEW_ACCT_SUICIDE = 0
}
