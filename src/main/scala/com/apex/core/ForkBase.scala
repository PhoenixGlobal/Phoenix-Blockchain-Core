/*
 *
 *
 *
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: ForkBase.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-8-28 上午10:53@version: 1.0
 */

package com.apex.core

import com.apex.crypto.Ecdsa.PublicKey
import com.apex.crypto.UInt256
import com.apex.storage.LevelDbStorage

import collection.mutable.{ListBuffer, Map, Seq, SortedMap}

case class ForkItem(block: BlockHeader, master: Boolean, lastProducerHeight: Map[PublicKey, Int]) {
  private var _confirmedHeight: Int = -1

  def confirmedHeight: Int = {
    if (_confirmedHeight == -1) {
      val index = lastProducerHeight.size * 2 / 3
      _confirmedHeight = lastProducerHeight.values.toSeq.reverse(index)
    }
    _confirmedHeight
  }

  def toBytes: Array[Byte] = {
    throw new NotImplementedError()
  }
}

object ForkItem {
  def fromBytes(bytes: Array[Byte]): ForkItem = {
    throw new NotImplementedError()
  }
}

case class ConfirmedKey(confirmedHeight: Int, height: Int) extends Ordered[ConfirmedKey] {
  override def compare(that: ConfirmedKey): Int = {
    val i = confirmedHeight.compare(that.confirmedHeight)
    if (i == 0) {
      -height.compare(that.height)
    } else {
      -i
    }
  }
}

case class HeightKey(height: Int, master: Boolean) extends Ordered[HeightKey] {
  override def compare(that: HeightKey): Int = {
    val i = height.compare(that.height)
    if (i == 0) {
      -master.compare(that.master)
    } else {
      i
    }
  }
}

class ForkBase(dir: String) {
  private var _head: Option[ForkItem] = None

  val indexById = Map.empty[UInt256, ForkItem]
  val indexByPrev = SortedMap.empty[UInt256, UInt256]
  val indexByHeight = SortedMap.empty[HeightKey, UInt256]
  val indexByConfirmedHeight = SortedMap.empty[ConfirmedKey, UInt256]
  val db = LevelDbStorage.open(dir)

  db.scan((_, v) => {
    val item = ForkItem.fromBytes(v)
    createIndex(item)
  })

  def head(): Option[ForkItem] = {
    _head
  }

  def add(item: ForkItem): Boolean = {
    if (indexById.contains(item.block.id)) {
      false
    } else {
      if (!indexById.contains(item.block.prevBlock)) {
        false
      } else {
        insert(item)
        val id = indexByConfirmedHeight.head._2
        val headItem = indexById(id)
        _head = Some(headItem)
        removeConfirmed(headItem.confirmedHeight)
        true
      }
    }
  }

  private def removeConfirmed(height: Int) = {
    val items = Seq.empty[ForkItem]
    for (p <- indexByHeight if p._1.height < height) {
      if (indexById.contains(p._2)) {
        items :+ indexById(p._2)
      }
    }
    items.foreach(item => {
      if (item.master) {
        db.delete(item.block.id.toBytes)
      } else {
        removeFork(item.block.id)
      }
    })
  }

  private def removeFork(id: UInt256) = {
    val ids = ListBuffer.empty[ForkItem]

    var prevId = id
    while (indexByPrev.contains(prevId)) {
      val currId = indexByPrev(prevId)
      ids.append(indexById(prevId))
      prevId = currId
    }

    db.batchWrite(batch => {
      ids.map(_.block.id.toBytes).foreach(batch.delete)
      ids.foreach(deleteIndex)
    })
  }

  private def insert(item: ForkItem): Boolean = {
    if (db.set(item.block.id.toBytes, item.toBytes)) {
      createIndex(item)
      true
    } else {
      import collection.mutable.MultiMap

      false
    }
  }

  private def createIndex(item: ForkItem): Unit = {
    val blk = item.block
    indexById.put(blk.id, item)
    indexByPrev.put(blk.prevBlock, blk.id)
    indexByHeight.put(HeightKey(blk.index, item.master), blk.id)
    indexByConfirmedHeight.put(ConfirmedKey(item.confirmedHeight, blk.index), blk.id)
  }

  private def deleteIndex(item: ForkItem): Unit = {
    val blk = item.block
    indexById.remove(blk.id)
    indexByPrev.remove(blk.prevBlock)
    indexByHeight.remove(HeightKey(blk.index, item.master))
    indexByConfirmedHeight.remove(ConfirmedKey(item.confirmedHeight, blk.index))
  }
}
