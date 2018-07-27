package com.apex.storage

import com.apex.common.ApexLogging
import com.apex.core.Block

import scala.collection.concurrent.TrieMap
import scala.util.Try

class BlockChainStorage(val path: String) extends ApexLogging {

  private val database = LevelDbStorage.open(path)

  private val BlocksCacheSizeLimit: Int = 2000

  private var blocksCacheSize: Int = 0

  private val blocksCache: TrieMap[Int, Option[Block]] = TrieMap.empty

  def readBlock(height: Int): Option[Block] = {

    //缓存区块的最大数目
    if (blocksCacheSize > BlocksCacheSizeLimit) {
      blocksCacheSize = 0
      blocksCache.clear()
    } else {
      blocksCacheSize = blocksCacheSize + 1
    }

    // 从缓存读取/更新
    //    blocksCache.getOrElseUpdate(height, Try(database.get(height)).get.flatMap(b => Block.parseFrom(b).toOption))
    throw new NotImplementedError()
  }

  def writeBlock(block: Block): Try[Boolean] = Try {
    database.set(block.id.toBytes, block.toBytes)
  }

  def scan(func: (Array[Byte], Array[Byte]) => Unit): Unit = {
    database.scan(func)
  }
}
