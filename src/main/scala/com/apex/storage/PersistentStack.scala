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

// stack whose elements stored in LevlelDb
class PersistentStack[A <: Serializable](db: LevelDbStorage)
                                        (implicit deserializer: Array[Byte] => A) {

  private var topIdx: Int = 0

  init()

  // count of elements in this stack
  def size: Int = topIdx

  // whether stack contains elements
  def isEmpty: Boolean = topIdx < 1

  // return top element
  def top(): A = {
    if (topIdx <= 0) throw new IndexOutOfBoundsException

    val bytes = db.get(topKey).get
    deserializer(bytes)
  }

  // remove the top element
  def pop(): Unit = {
    if (topIdx <= 0) throw new IndexOutOfBoundsException

    db.delete(topKey)
    topIdx -= 1
  }

  // add a element on top
  def push(item: A): Unit = {
    if (db.set(BigInt(topIdx).toByteArray, item.toBytes)) {
      topIdx += 1
    }
  }

  private def init() = {
    val last = db.last()
    if (last._1 != null) {
      topIdx = BigInt(last._1).toInt + 1
    }
  }

  private def topKey: Array[Byte] = {
    BigInt(topIdx - 1).toByteArray
  }

}