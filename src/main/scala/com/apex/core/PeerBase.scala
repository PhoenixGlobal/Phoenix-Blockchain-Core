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

import com.apex.settings.PeerBaseSettings
import com.apex.storage.{Storage}

class PeerBase(settings: PeerBaseSettings) {

  private val db = Storage.open(settings.dbType, settings.dir)
  private val peerStore = new PeerStore(db, settings.cacheSize)

  def getGasLimit(): Option[BigInt] = {
    peerStore.get("gas_limit")
  }

  def containGasLimit(): Boolean = {
    peerStore.contains("gas_limit")
  }

  def setGasLimit(gasLimit: BigInt): Boolean = {
    peerStore.contains("gas_limit")
    db.batchWrite(batch => {
      peerStore.set("gas_limit", gasLimit, batch)
    })
  }

  def close(): Unit = {
    db.close()
  }
}