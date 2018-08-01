/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: BlockchainIterator.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-31 下午1:55@version: 1.0
 */

package com.apex.core

import com.apex.crypto.UInt256

class BlockchainIterator(chain: Blockchain) extends Iterator[Block] {
  var id = chain.getLatestHeader.id

  override def hasNext: Boolean = !id.equals(UInt256.Zero)

  override def next(): Block = {
    if (id.equals(UInt256.Zero)) throw new NoSuchElementException
    val blk = chain.getBlock(id).getOrElse(null)
    id = blk.header.prevBlock
    blk
  }
}
