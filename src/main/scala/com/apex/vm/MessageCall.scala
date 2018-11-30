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
 * FileName: MessageCall.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-30 上午11:56@version: 1.0
 *
 */

package com.apex.vm

import org.apex.vm.OpCode

/**
  * A wrapper for a message call from a contract to another account.
  * This can either be a normal CALL, CALLCODE, DELEGATECALL or POST call.
  */
case class MessageCall(opCode: OpCode.Value, gas: DataWord, codeAddress: DataWord,
                       endowment: DataWord, inDataOffs: DataWord, inDataSize: DataWord,
                       outDataOffs: DataWord = null, outDataSize: DataWord = null)
