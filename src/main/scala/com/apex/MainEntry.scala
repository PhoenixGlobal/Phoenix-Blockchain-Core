package com.apex

import com.apex.core.Block
import scala.collection.mutable.StringBuilder
import com.apex.network.rpc.RpcServer
import scala.io.StdIn


object MainEntry{

  def main(args: Array[String]): Unit = {
    
    RpcServer.run()

//    val block = new Block()
//
//    System.out.print("Hello BlockChain: " + block.getClass.toString)
//    System.out.println(" #" + block.header.index)
    System.out.println("Press RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    RpcServer.stop()
    
    //System.out.println("main end...")    

  }
}
