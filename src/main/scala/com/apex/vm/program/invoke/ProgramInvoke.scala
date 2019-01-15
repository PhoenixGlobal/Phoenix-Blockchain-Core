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
 * FileName: ProgramInvoke.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-28 下午3:13@version: 1.0
 *
 */

package com.apex.vm.program.invoke

import com.apex.core.{BlockBase, DataBase}
import com.apex.vm.DataWord

trait ProgramInvoke {
  def getOwnerAddress: DataWord

  def getBalance: DataWord

  def getOriginAddress: DataWord

  def getCallerAddress: DataWord

  def getMinGasPrice: DataWord

  //  def getGas: DataWord
  def getGaslimit: DataWord

  def getGasLimitLong: Long

  def getCallValue: DataWord

  def getDataSize: DataWord

  def getDataValue(indexData: DataWord): DataWord

  def getDataCopy(offsetData: DataWord, lengthData: DataWord): Array[Byte]

  def getPrevHash: DataWord

  def getCoinbase: DataWord

  def getTimestamp: DataWord

  def getNumber: DataWord

  //def getDifficulty: DataWord

  def byTransaction: Boolean

  def byTestingSuite: Boolean

  def getCallDeep: Int

  def getDataBase: DataBase

  def getOrigDataBase: DataBase

  def getBlockStore: BlockBase

  def isStaticCall: Boolean
}
