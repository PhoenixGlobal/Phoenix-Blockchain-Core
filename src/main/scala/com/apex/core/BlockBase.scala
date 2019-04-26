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

import com.apex.crypto.UInt256
import com.apex.settings.BlockBaseSettings
import com.apex.storage.Storage

class BlockBase(settings: BlockBaseSettings) {
  private val db = Storage.openTemp(settings.dbType, settings.dir)

  private val blockStore = new BlockStore(db, settings.cacheSize)
  private val heightStore = new HeightStore(db, settings.cacheSize)
  private val headBlockStore = new HeadBlockStore(db)
  private val receiptStore = new ReceiptStore(db, settings.cacheSize)

  def head(): Option[BlockHeader] = {
    headBlockStore.get()
  }

  def add(block: Block): Unit = {
    require(head.forall(_.id.equals(block.prev)))

    db.batchWrite(batch => {
      blockStore.set(block.id, block, batch)
      heightStore.set(block.height, block.id, batch)
      headBlockStore.set(block.header, batch)
    })
  }

  def getBlock(id: UInt256): Option[Block] = {
    blockStore.get(id)
  }

  def getBlock(height: Long): Option[Block] = {
    heightStore.get(height).flatMap(getBlock)
  }

  def containBlock(id: UInt256): Boolean = {
    blockStore.contains(id)
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
    receiptStore.set(txid, receipt)
  }

  def close(): Unit = {
    db.close()
  }
}