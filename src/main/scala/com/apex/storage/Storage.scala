package com.apex.storage

import org.iq80.leveldb.WriteBatch

import scala.collection.mutable.ListBuffer

trait Storage[Key, Value] {
  def containsKey(key: Key): Boolean = get(key).isDefined

  def get(key: Key): Option[Value]

  def set(key: Key, value: Value, batch: Batch): Boolean

  def delete(key: Key, batch: Batch): Unit

  def scan(func: (Key, Value) => Unit): Unit

  def find(prefix: Array[Byte], func: (Key, Value) => Unit): Unit

  def newSession(): Unit

  def commit(revision: Int = -1): Unit

  def rollBack(): Unit

  def close(): Unit
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