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

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.common.ApexLogging
import com.apex.crypto.Ecdsa.PublicKey
import com.apex.crypto.UInt256
import com.apex.exceptions.UnExpectedError
import com.apex.settings.{ForkBaseSettings, Witness}
import com.apex.storage.{Batch, LevelDbStorage}

import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map, Seq, SortedMap}

class MultiMap[K, V] extends mutable.Iterable[(K, V)] {
  private val container = Map.empty[K, ListBuffer[V]]

  override def size: Int = container.values.map(_.size).sum

  def contains(k: K): Boolean = {
    container.contains(k)
  }

  def get(k: K): Option[Seq[V]] = {
    container.get(k)
  }

  def put(k: K, v: V): Unit = {
    if (!container.contains(k)) {
      container.put(k, ListBuffer.empty)
    }
    container(k).append(v)
  }

  def remove(k: K): Option[Seq[V]] = {
    container.remove(k)
  }

  override def head: (K, V) = iterator.next()

  override def iterator: Iterator[(K, V)] = new MultiMapIterator(container)

  class MultiMapIterator(container: Map[K, ListBuffer[V]]) extends Iterator[(K, V)] {
    private val it = container.iterator

    private var it2: Option[Iterator[V]] = None
    private var k: Option[K] = None

    override def hasNext: Boolean = {
      if (it2.isEmpty || !it2.get.hasNext) {
        nextIt
      }

      it2.exists(_.hasNext)
    }

    override def next(): (K, V) = {
      if (!hasNext) throw new NoSuchElementException
      (k.get, it2.get.next())
    }

    private def nextIt = {
      if (it.hasNext) {
        val next = it.next()
        it2 = Some(next._2.iterator)
        k = Some(next._1)
      }
    }
  }

}

object MultiMap {
  def empty[K, V] = new MultiMap[K, V]
}

class SortedMultiMap1[K, V](implicit ord: Ordering[K]) extends Iterable[(K, V)] {
  private val container = SortedMap.empty[K, ListBuffer[V]]

  override def size: Int = container.values.map(_.size).sum

  def contains(k: K) = {
    container.contains(k)
  }

  def get(k: K): Option[Seq[V]] = {
    container.get(k)
  }

  def put(k: K, v: V): Unit = {
    if (!container.contains(k)) {
      container.put(k, ListBuffer.empty[V])
    }
    container(k).append(v)
  }

  def remove(k: K): Option[Seq[V]] = {
    container.remove(k)
  }

  override def head: (K, V) = iterator.next()

  override def iterator: Iterator[(K, V)] = new SortedMultiMap1Iterator(container)

  class SortedMultiMap1Iterator(val map: SortedMap[K, ListBuffer[V]]) extends Iterator[(K, V)] {
    private val it = map.iterator

    private var it2: Option[Iterator[V]] = None
    private var k: Option[K] = None

    override def hasNext: Boolean = {
      if (it2.isEmpty || !it2.get.hasNext) {
        nextIt
      }
      it2.exists(_.hasNext)
    }

    override def next(): (K, V) = {
      if (!hasNext) throw new NoSuchElementException
      (k.get, it2.get.next())
    }

    private def nextIt = {
      if (it.hasNext) {
        val next = it.next()
        it2 = Some(next._2.iterator)
        k = Some(next._1)
      }
    }
  }

}

class SortedMultiMap2[K1, K2, V](implicit ord1: Ordering[K1], ord2: Ordering[K2]) extends Iterable[(K1, K2, V)] {
  private val container = SortedMap.empty[K1, SortedMultiMap1[K2, V]]

  override def size: Int = container.values.map(_.size).sum

  def contains(k1: K1, k2: K2): Boolean = {
    container.contains(k1) && container(k1).contains(k2)
  }

  def get(k1: K1, k2: K2): Option[Seq[V]] = {
    container.get(k1).flatMap(_.get(k2))
  }

  def put(k1: K1, k2: K2, v: V): Unit = {
    if (!container.contains(k1)) {
      container.put(k1, SortedMultiMap.empty[K2, V])
    }
    container(k1).put(k2, v)
  }

  def remove(k1: K1, k2: K2): Option[Seq[V]] = {
    container.get(k1).flatMap(c => {
      val v = c.remove(k2)
      if (c.isEmpty) {
        container.remove(k1)
      }
      v
    })
  }

  override def head: (K1, K2, V) = iterator.next()

  override def iterator: Iterator[(K1, K2, V)] = new SortedMultiMap2Iterator(container)

  class SortedMultiMap2Iterator[K1, K2, V](val map: SortedMap[K1, SortedMultiMap1[K2, V]]) extends Iterator[(K1, K2, V)] {
    private val it = map.iterator

    private var it2: Option[Iterator[(K2, V)]] = None
    private var k1: Option[K1] = None

    override def hasNext: Boolean = {
      if (it2.isEmpty || !it2.get.hasNext) {
        nextIt
      }
      it2.exists(_.hasNext)
    }

    override def next(): (K1, K2, V) = {
      if (!hasNext) throw new NoSuchElementException
      val next = it2.get.next()
      (k1.get, next._1, next._2)
    }

    private def nextIt = {
      if (it.hasNext) {
        val next = it.next()
        it2 = Some(next._2.iterator)
        k1 = Some(next._1)
      }
    }
  }

}

object SortedMultiMap {
  def empty[A, B]()(implicit ord: Ordering[A]): SortedMultiMap1[A, B] = new SortedMultiMap1[A, B]

  def empty[A, B, C]()(implicit ord1: Ordering[A], ord2: Ordering[B]): SortedMultiMap2[A, B, C] = new SortedMultiMap2[A, B, C]
}

case class ForkItem(block: Block, lastProducerHeight: mutable.Map[PublicKey, Int], master: Boolean = false) extends com.apex.common.Serializable {
  private var _confirmedHeight: Int = -1

  def id(): UInt256 = block.id()

  def prev(): UInt256 = block.prev()

  def confirmedHeight: Int = {
    if (_confirmedHeight == -1) {
      val index = lastProducerHeight.size * 2 / 3
      val lastHeights = lastProducerHeight.values.toSeq.sorted(Ordering[Int].reverse)
      _confirmedHeight = lastHeights(index)
    }
    _confirmedHeight
  }

  def toBytes: Array[Byte] = {
    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    serialize(os)
    bs.toByteArray
  }

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.write(block)
    os.writeBoolean(master)
    os.writeVarInt(lastProducerHeight.size)
    lastProducerHeight.foreach(p => {
      os.writeByteArray(p._1.toBin)
      os.writeVarInt(p._2)
    })
  }
}

object ForkItem {
  def deserialize(is: DataInputStream): ForkItem = {
    import com.apex.common.Serializable._
    val block = is.readObj(Block.deserialize)
    val master = is.readBoolean
    val lastProducerHeight = Map.empty[PublicKey, Int]
    for (_ <- 1 to is.readVarInt) {
      lastProducerHeight += PublicKey(is.readByteArray) -> is.readVarInt
    }
    ForkItem(block, lastProducerHeight, master)
  }

  def fromBytes(bytes: Array[Byte]): ForkItem = {
    val bs = new ByteArrayInputStream(bytes)
    val is = new DataInputStream(bs)
    deserialize(is)
  }
}

class ForkBase(settings: ForkBaseSettings,
               witnesses: Array[Witness],
               onConfirmed: Block => Unit,
               onSwitch: (Seq[ForkItem], Seq[ForkItem]) => Unit) extends ApexLogging {
  private val db = LevelDbStorage.open(settings.dir)
  private val forkStore = new ForkItemStore(db, settings.cacheSize)

  private val indexById = Map.empty[UInt256, ForkItem]
  private val indexByPrev = MultiMap.empty[UInt256, UInt256]
  private val indexByHeight = SortedMultiMap.empty[Int, Boolean, UInt256]()(implicitly[Ordering[Int]], implicitly[Ordering[Boolean]].reverse)
  private val indexByConfirmedHeight = SortedMultiMap.empty[Int, Int, UInt256]()(implicitly[Ordering[Int]].reverse, implicitly[Ordering[Int]].reverse)

  private var _head: Option[ForkItem] = None

  init()

  def head(): Option[ForkItem] = {
    _head
  }

  def get(id: UInt256): Option[ForkItem] = {
    indexById.get(id)
  }

  def get(height: Int): Option[ForkItem] = {
    indexByHeight.get(height, true)
      .flatMap(_.headOption)
      .flatMap(get)
  }

  //TODO
  def getNext(id: UInt256): Option[UInt256] = {
    indexByPrev.get(id)
      .map(a => a.map(indexById.get))
      .map(a => a.flatten.find(_.master).map(_.block.id))
      .flatten
  }

  def add(block: Block): Boolean = {
    def makeItem(heights: IMap[PublicKey, Int], master: Boolean) = {
      val lph = Map.empty[PublicKey, Int]
      heights.foreach(lph +=)
      val pub = block.header.producer
      if (lph.contains(pub)) {
        lph.put(pub, block.height)
      }
      ForkItem(block, lph, master)
    }

    if (_head.isEmpty) {
      val prodHeights = witnesses.map(_.pubkey -> 0).toMap
      val item = makeItem(prodHeights, true)
      add(item)
    } else {
      if (!indexById.contains(block.id)) {
        indexById.get(block.prev).exists(prev => {
          val prodHeights = prev.lastProducerHeight.toMap
          val master = _head.get.id.equals(block.prev)
          val item = makeItem(prodHeights, master)
          add(item)
        })
      } else {
        false
      }
    }
  }

  def add(item: ForkItem): Boolean = {
    def maybeReplaceHead: (ForkItem, ForkItem) = {
      val old = _head
      _head = indexByConfirmedHeight.headOption.map(_._3).flatMap(indexById.get)
      require(_head.isDefined)
      (old.orElse(_head).get, _head.get)
    }

    if (insert(item)) {
      val (oldItem, newItem) = maybeReplaceHead
      if (newItem.prev.equals(oldItem.id)) {
        removeConfirmed(item.confirmedHeight)
      } else if (!newItem.id.equals(oldItem.id)) {
        switch(oldItem, newItem)
      }
      true
    } else {
      false
    }
  }

  def switch(from: ForkItem, to: ForkItem): Unit = {
    val (originFork, newFork) = getForks(from, to)

    val items = ListBuffer.empty[ForkItem]

    def switchFork(batch: Batch) = {
      for (item <- originFork) {
        val newItem = item.copy(master = false)
        forkStore.set(newItem.block.id, newItem, batch)
        items.append(newItem)
      }
      for (item <- newFork) {
        val newItem = item.copy(master = true)
        forkStore.set(newItem.block.id, newItem, batch)
        items.append(newItem)
      }
    }

    if (db.batchWrite(switchFork)) {
      items.foreach(updateIndex)
      onSwitch(originFork, newFork)
    }
  }

  def removeFork(id: UInt256): Boolean = {
    indexById.get(id).forall(item => {
      val queue = ListBuffer(item)

      def getAncestors(ancestors: Seq[UInt256]): Seq[ForkItem] = {
        ancestors.map(indexById.get).filter(_.isDefined).map(_.get)
      }

      def removeAll(batch: Batch): Unit = {
        var i = 0

        while (i < queue.size) {
          val toRemove = queue(i)
          val toRemoveId = toRemove.block.id
          val ancestors = indexByPrev.get(toRemoveId).map(getAncestors)
          ancestors.foreach(queue.appendAll)
          forkStore.delete(toRemoveId, batch)
          i += 1
        }
      }

      if (db.batchWrite(removeAll)) {
        queue.foreach(deleteIndex)
        true
      } else {
        false
      }
    })
  }

  def close(): Unit = {
    db.close()
  }

  private def init() = {
    forkStore.foreach((_, item) => createIndex(item))
    _head = indexByConfirmedHeight
      .headOption.map(_._3)
      .flatMap(indexById.get)
  }

  private def removeConfirmed(height: Int): Unit = {
    def tryConfirm(key: (Int, Boolean, UInt256)): Boolean = {
      if (key._1 < height) {
        indexById.get(key._3).exists(item => {
          if (item.master) {
            onConfirmed(item.block)
          }
          if (db.batchWrite(batch => forkStore.delete(item.block.id, batch))) {
            deleteIndex(item)
            true
          } else {
            false
          }
        })
      } else {
        false
      }
    }

    while (indexByHeight.headOption.exists(tryConfirm)) {
      indexByHeight.headOption.foreach(h => log.debug(s"head item: ${h.toString}"))
    }
  }

  private def insert(item: ForkItem): Boolean = {
    if (forkStore.set(item.block.id, item)) {
      createIndex(item)
      true
    } else {
      false
    }
  }

  private def getForks(x: ForkItem, y: ForkItem): (Seq[ForkItem], Seq[ForkItem]) = {
    def getPrev(item: ForkItem): ForkItem = {
      val prev = get(item.block.prev)
      require(prev.isDefined)
      prev.get
    }

    var (a, b) = (x, y)
    if (a.block.id.equals(b.block.id)) {
      (Seq.empty, Seq.empty)
    } else {
      val xs = ListBuffer.empty[ForkItem]
      val ys = ListBuffer.empty[ForkItem]
      while (a.block.height > b.block.height) {
        xs.append(a)
        a = getPrev(a)
      }
      while (b.block.height > a.block.height) {
        ys.append(b)
        b = getPrev(b)
      }
      while (!a.block.id.equals(b.block.id)) {
        xs.append(a)
        ys.append(b)
        a = getPrev(a)
        b = getPrev(b)
      }
      (xs, ys)
    }
  }

  private def createIndex(item: ForkItem): Unit = {
    val blk = item.block
    indexById.put(blk.id, item)
    indexByPrev.put(blk.prev, blk.id)
    indexByHeight.put(blk.height, item.master, blk.id)
    indexByConfirmedHeight.put(item.confirmedHeight, blk.height, blk.id)
  }

  private def deleteIndex(item: ForkItem): Unit = {
    val blk = item.block
    indexById.remove(blk.id)
    indexByPrev.remove(blk.prev)
    indexByHeight.remove(blk.height, item.master)
    indexByConfirmedHeight.remove(item.confirmedHeight, blk.height)
  }

  private def updateIndex(newItem: ForkItem): Unit = {
    val id = newItem.block.id
    val height = newItem.block.height
    val branch = newItem.master
    indexByHeight.remove(height, !branch)
      .map(_.filterNot(_.equals(id)))
      .foreach(_.foreach(id => indexByHeight.put(height, branch, id)))
    indexByHeight.put(height, branch, id)
    indexById.put(id, newItem)
  }
}