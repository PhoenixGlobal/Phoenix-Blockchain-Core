package com.apex

import akka.actor.ActorSystem
import com.apex.consensus.Genesis
import com.apex.core.{Block, Blockchain, Transaction}
import com.apex.crypto.{Fixed8, UInt256}
import com.apex.network.LocalNode

import scala.collection.mutable.StringBuilder
import com.apex.network.rpc.RpcServer
import com.apex.wallets.Wallet

import scala.concurrent.duration.Duration
import scala.io.StdIn


object MainEntry {

  def main(args: Array[String]): Unit = {

    //    val block1 = Blockchain.Current.produceBlock(Seq.empty)

    Wallet.importPrivKeyFromWIF("Kx45GeUBSMPReYQwgXiKhG9FzNXrnCeutJp4yjTd5kKxCitadm3C")
    //val tx = Wallet.makeTransaction("APQKUqPcJEUwRdwoxpoGQnkrRGstSXkgebk", UInt256.Zero, new Fixed8(230000L)).get

    //LocalNode.default.addTransaction(tx)
    //    val block2 = Blockchain.Current.produceBlock(LocalNode.default.getMemoryPool())
    val task = LocalNode.default.beginProduce(Genesis.config)
//    producer.wait()
    //    val block3 = Blockchain.Current.produceBlock(Seq.empty)

    RpcServer.run()

    //    val block = new Block()
    //
    //    System.out.print("Hello BlockChain: " + block.getClass.toString)
    //    System.out.println(" #" + block.header.index)
    System.out.println("Press RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    RpcServer.stop() //System.out.println("main end...")
    task.cancel()
  }
}
