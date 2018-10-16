package com.apex.storage

import java.util.Map.Entry

import scala.collection.mutable.ListBuffer

trait Storage[Key, Value] {
  def containsKey(key: Key): Boolean = get(key).isDefined

  def get(key: Key): Option[Value]

  def set(key: Key, value: Value, batch: Batch): Boolean

  def delete(key: Key, batch: Batch): Boolean

  def batchWrite(action: Batch => Unit): Boolean

  def last(): Option[Entry[Array[Byte], Array[Byte]]]

  def scan(func: (Key, Value) => Unit): Unit

  def find(prefix: Array[Byte], func: (Key, Value) => Unit): Unit

  def newSession(): Unit

  def commit(revision: Int): Unit

  def commit(): Unit

  def rollBack(): Unit

  def close(): Unit

  def revision(): Int

  def uncommitted(): Seq[Int]
}

trait LowLevelDBIterator {
  def seek(prefix: Array[Byte]): Unit

  def next(): (Array[Byte], Array[Byte])

  def hasNext(): Boolean
}

trait LowLevelWriteBatch {
  def set(key: Array[Byte], value: Array[Byte]): Unit

  def delete(key: Array[Byte]): Unit

  def close(): Unit
}

trait LowLevelDB {
  def get(key: Array[Byte]): Array[Byte]

  def set(key: Array[Byte], value: Array[Byte]): Unit

  def delete(key: Array[Byte]): Unit

  def iterator(): LowLevelDBIterator

  def batchWrite(action: LowLevelWriteBatch => Unit)
}

trait BatchItem

case class DeleteOperationItem(key: Array[Byte]) extends BatchItem

case class PutOperationItem(key: Array[Byte], value: Array[Byte]) extends BatchItem

class Batch(val ops: ListBuffer[BatchItem] = ListBuffer.empty[BatchItem]) {
  def put(key: Array[Byte], value: Array[Byte]): Batch = {
    ops.append(PutOperationItem(key, value))
    this
  }

  def delete(key: Array[Byte]): Batch = {
    ops.append(DeleteOperationItem(key))
    this
  }
}