package com.apex.core

import com.apex.crypto.UInt256

class BlockSummary(val block: Block, val txReceiptsMap: scala.collection.mutable.Map[UInt256, Option[TransactionReceipt]]) {

}
