/*
 *
 *
 *
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: PersistentableStack.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-9-10 下午5:11@version: 1.0
 */

package com.apex.storage

import com.apex.common.Serializable

class PersistentStack[A <: Serializable](db: LevelDbStorage)
                                        (implicit deserializer: Array[Byte] => A) {

  private var topIdx: Int = 0

  init()

  def size: Int = topIdx

  def isEmpty: Boolean = topIdx < 1

  def top(): A = {
    if (topIdx <= 0) throw new IndexOutOfBoundsException

    val bytes = db.get(topKey).get
    deserializer(bytes)
  }

  def pop(): Unit = {
    if (topIdx <= 0) throw new IndexOutOfBoundsException

    db.delete(topKey)
    topIdx -= 1
  }

  def push(item: A): Unit = {
    if (db.set(BigInt(topIdx).toByteArray, item.toBytes)) {
      topIdx += 1
    }
  }

  private def init() = {
    val last = db.last()
    if (!last.isEmpty) {
      topIdx = BigInt(last.get.getKey).toInt + 1
    }
  }

  private def topKey: Array[Byte] = {
    BigInt(topIdx - 1).toByteArray
  }

}
