package com.apex.test

import com.apex.core.LRUCache
import org.junit.Test

@Test
class CacheTest {
  @Test
  def testLRUCacheSetGetDelete = {
    val cache = new LRUCache[Int, Int](10)
    cache.set(1, 1)
    assert(cache.get(1).get == 1)
    cache.set(1, 2)
    assert(cache.get(1).get == 2)
    cache.delete(1)
    assert(cache.get(1).isEmpty)
  }

  @Test
  def testLRUCacheLRU1 = {
    val cache = new LRUCache[Int, Int](10)
    for (i <- 1 to 11) {
      cache.set(i, i)
    }
    println(cache.size())
    assert(cache.size() == 10)
    assert(cache.contains(1) == false)
    for (i <- 2 to 11) {
      assert(cache.get(i).get == i)
    }
  }

  @Test
  def testLRUCacheLRU2 = {
    val cache = new LRUCache[Int, Int](10)
    for (i <- 1 to 10) {
      cache.set(i, i)
    }
    cache.get(1)
    cache.set(11, 11)
    assert(cache.get(1).get == 1)
    assert(cache.get(2).isEmpty)
  }
}
