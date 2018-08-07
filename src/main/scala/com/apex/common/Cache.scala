package com.apex.common

trait Cache[K, V] {
  def size(): Int

  def contains(key: K): Boolean

  def get(key: K): Option[V]

  def set(key: K, value: V): Unit

  def delete(key: K): Unit
}

class LRUCache[K, V](val capacity: Int) extends Cache[K, V] {

  class LRUMap[K, V] extends java.util.LinkedHashMap[K, V](capacity, 1.0f, true) {
    override def removeEldestEntry(eldest: java.util.Map.Entry[K, V]): Boolean = {
      size() > capacity
    }
  }

  val container = new LRUMap[K, V]

  override def size(): Int = container.size()

  override def contains(key: K): Boolean = {
    container.containsKey(key)
  }

  override def get(key: K): Option[V] = {
    val value = container.get(key)
    if (value == null) {
      None
    } else {
      Some(value)
    }
  }

  override def set(key: K, value: V): Unit = {
    container.put(key, value)
  }

  override def delete(key: K): Unit = {
    container.remove(key)
  }
}
