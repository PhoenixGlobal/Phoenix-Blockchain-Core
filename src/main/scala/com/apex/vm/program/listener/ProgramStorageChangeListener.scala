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
 * FileName: ProgramStorageChangeListener.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-28 下午2:04@version: 1.0
 *
 */

package com.apex.vm.program.listener

import com.apex.vm.DataWord

class ProgramStorageChangeListener extends ProgramListenerAdaptor {
  private val diff = collection.mutable.Map.empty[DataWord, DataWord]

  override def onStoragePut(key: DataWord, value: DataWord): Unit = {
    diff.put(key, value)
  }

  override def onStorageClear(): Unit = {
    // TODO: ...
  }

  def getDiff = diff.clone()

  def merge(other: Map[DataWord, DataWord]): Unit = {
    other.foreach(p => diff.put(p._1, p._2))
  }
}
