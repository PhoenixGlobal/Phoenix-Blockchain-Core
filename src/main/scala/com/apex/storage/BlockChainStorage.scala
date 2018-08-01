package com.apex.storage

import java.io.{ByteArrayInputStream, DataInputStream}

import com.apex.common.ApexLogging
import com.apex.core.Block
import com.apex.crypto.UInt256

import scala.collection.concurrent.TrieMap
import scala.util.Try

class BlockChainStorage(val path: String) extends ApexLogging {

  private val database = LevelDbStorage.open(path)

  private val BlocksCacheSizeLimit: Int = 2000

  private var blocksCacheSize: Int = 0

  private val blocksCache: TrieMap[Int, Option[Block]] = TrieMap.empty

  def readBlock(id: UInt256): Option[Block] = {
    database.get(id.toBytes) match {
      case Some(bytes) => {
        val bis = new ByteArrayInputStream(bytes)
        val is = new DataInputStream(bis)
        Some(Block.deserialize(is))
      }
      case None => None
    }
  }

  def writeBlock(block: Block): Try[Boolean] = Try {
    database.set(block.id.toBytes, block.toBytes)
  }

  def scan(func: (Array[Byte], Array[Byte]) => Unit): Unit = {
    database.scan(func)
  }
}
