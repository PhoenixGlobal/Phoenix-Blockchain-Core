package com.apex.common

import io.iohk.iodb.ByteArrayWrapper
import com.apex.core.ModifierId
import com.apex.core.structure.MemoryPool
import scala.collection.concurrent.TrieMap
import scala.util.{Success, Try}


case class SimpleBoxStructureMemPool(unconfirmed: TrieMap[ByteArrayWrapper, SimpleBoxStructure])
  extends MemoryPool[SimpleBoxStructure, SimpleBoxStructureMemPool] with ApexLogging {
  override type NVCT = SimpleBoxStructureMemPool

  private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)

  override def getById(id: ModifierId): Option[SimpleBoxStructure] =
  unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleBoxStructure] = ids.flatMap(getById)

  override def put(tx: SimpleBoxStructure): Try[SimpleBoxStructureMemPool] = Success {
    unconfirmed.put(key(tx.id), tx)
    this
  }

  override def put(txs: Iterable[SimpleBoxStructure]): Try[SimpleBoxStructureMemPool] = Success(putWithoutCheck(txs))

  override def putWithoutCheck(txs: Iterable[SimpleBoxStructure]): SimpleBoxStructureMemPool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    this
  }

  override def remove(tx: SimpleBoxStructure): SimpleBoxStructureMemPool = {
    unconfirmed.remove(key(tx.id))
    this
  }

  override def take(limit: Int): Iterable[SimpleBoxStructure] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(condition: (SimpleBoxStructure) => Boolean): SimpleBoxStructureMemPool = {
    unconfirmed.retain { (k, v) =>
      condition(v)
    }
    this
  }

  override def size: Int = unconfirmed.size
}


object SimpleBoxStructureMemPool {
  lazy val emptyPool: SimpleBoxStructureMemPool = SimpleBoxStructureMemPool(TrieMap())
}