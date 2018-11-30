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
 * FileName: Constants.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-27 下午3:39@version: 1.0
 *
 */

package com.apex.vm

object Constants {
  final val MAXIMUM_EXTRA_DATA_SIZE = 32
  final val MIN_GAS_LIMIT = 125000
  final val GAS_LIMIT_BOUND_DIVISOR = 1024
  final val MINIMUM_DIFFICULTY = BigInt(131072)
  final val DIFFICULTY_BOUND_DIVISOR = BigInt(2048)
  final val EXP_DIFFICULTY_PERIOD = 100000

  final val UNCLE_GENERATION_LIMIT = 7
  final val UNCLE_LIST_LIMIT = 2

  final val BEST_NUMBER_DIFF_LIMIT = 100

  final val BLOCK_REWARD = 0

  final val SECP256K1N = BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)

  final val LONGEST_CHAIN = 192
}
