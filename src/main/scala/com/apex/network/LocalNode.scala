/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: LocalNode.scala
 *
 * @author: shan.huang@chinapex.com: 2018-7-25 下午1:06@version: 1.0
 */

package com.apex.network

import collection.mutable.Map

import com.apex.common.ApexLogging
import com.apex.crypto.UInt256
import com.apex.core.{BlockChain, Block, Transaction}

class LocalNode extends ApexLogging {
  
  private val memPool : Map[UInt256, Transaction] = Map.empty
  
  private val connectedMax : Int = 10
  
  // connectedPeers
  // unconnectedPeers
  // badPeers
  
  //def AcceptPeers() = {  }
  
  def addTransaction(tx: Transaction) : Boolean = {
     //lock (Blockchain.Default.PersistLock)
     //lock (mem_pool)
     if (memPool.contains(tx.id)) return false
     if (BlockChain.Current.containsTransaction(tx.id)) return false
     //if (!tx.Verify(mem_pool.Values)) return false;
     memPool.put(tx.id, tx)
     //CheckMemPool()    
     true
  }
  
  def getMemoryPool() : Seq[Transaction] = {
     memPool.values.toSeq
  }
  
  def getTransaction(hash: UInt256) : Option[Transaction] = {

     return memPool.get(hash)    
  }
  
  def containsTransaction(tx: Transaction) : Boolean = {
    
     return memPool.contains(tx.id)    
  }
  
  def removeTransactionsInBlock(block: Block) = {
     //TODO
  }

}

object LocalNode { 
   final val default = new LocalNode()
}
