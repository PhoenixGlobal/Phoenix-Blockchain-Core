package com.apex.storage

trait Storage[Key, Value] {

  def set(key: Key, value: Value): Boolean

  def get(key: Key): Option[Value]

  def delete(key: Key): Unit

  def scan(func: (Key, Value) => Unit): Unit

  def find(prefix: Array[Byte], func: (Key, Value) => Unit): Unit

  def commit(): Unit

  def close(): Unit

  def containsKey(key: Key): Boolean = get(key).isDefined
}
