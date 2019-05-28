/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: BlockBase.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-9-27 上午11:32@version: 1.0
 *
 */

package com.apex.core

import com.apex.common.ApexLogging
import com.apex.crypto.UInt256
import com.apex.settings.BlockBaseSettings
import com.apex.storage.Storage

class BlockBase(settings: BlockBaseSettings,
                lightNode: Boolean = false,
                maxBlockCount: Int = 1000) extends ApexLogging {
  private val db = Storage.openTemp(settings.dbType, settings.dir)

  private val blockStore = new BlockStore(db, settings.cacheSize)
  private val blockIdStore = new BlockIdStore(db, settings.cacheSize)
  private val heightStore = new HeightStore(db, settings.cacheSize)
  private val headBlockStore = new HeadBlockStore(db)
  private val receiptStore = new ReceiptStore(db, settings.cacheSize)

  private val blockCacheStore = new BlockCacheStore(db, settings.cacheSize)

  def head(): Option[BlockHeader] = {
    headBlockStore.get()
  }

  def add(block: Block): Unit = {
    require(head.forall(_.id.equals(block.prev)))

    db.batchWrite(batch => {
      blockStore.set(block.id, block, batch)
      blockIdStore.set(block.id, block.height(), batch)
      heightStore.set(block.height, block.id, batch)
      headBlockStore.set(block.header, batch)

      blockCacheStore.delete(block.height(), batch)
    })

    if (lightNode && block.height() > (maxBlockCount + 1)) {
      delete(block.height() - maxBlockCount)
    }
  }

  private def delete(height: Long): Unit = {
    val block = getBlock(height)
    if (block.isDefined) {
      log.info(s"delete block ${block.get.height()} ${block.get.shortId()}")
      db.batchWrite(batch => {
        blockStore.delete(block.get.id, batch)
        //blockIdStore.delete(block.get.id, batch)
        //heightStore.delete(block.get.height(), batch)
      })
    }
  }

  def cacheAdd(block: Block): Unit = {
    require(block.height() > head().get.index)

    blockCacheStore.set(block.height(), block)
  }

  def cacheGetBlock(height: Long): Option[Block] = {
    blockCacheStore.get(height)
  }

  def getBlock(id: UInt256): Option[Block] = {
    blockStore.get(id)
  }

  def getBlock(height: Long): Option[Block] = {
    heightStore.get(height).flatMap(getBlock)
  }

  def containBlock(id: UInt256): Boolean = {
    blockIdStore.contains(id)
  }

  def getBlockHash(height: Long): Option[UInt256] = {
    heightStore.get(height)
  }

  // TODO: This function need be called under some fork cases
  def deleteReceipt(txid: UInt256) = {
    receiptStore.delete(txid)
  }

  // get tx receipt
  def getReceipt(txid: UInt256): Option[TransactionReceipt] = {
    receiptStore.get(txid)
  }

  // set or update tx receipt
  def setReceipt(txid: UInt256, receipt: TransactionReceipt) = {
    if (lightNode == false)
      receiptStore.set(txid, receipt)
  }

  def close(): Unit = {
    db.close()
  }
}