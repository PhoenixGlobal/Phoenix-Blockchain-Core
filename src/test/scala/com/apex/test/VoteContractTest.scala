package com.apex.test

import com.apex.test.ResourcePrepare.BlockChainPrepare
import org.junit.{AfterClass, Test}

import scala.reflect.io.Directory

class VoteContractTest extends BlockChainPrepare {
  Directory("VoteContractTest").deleteRecursively()

  @Test
  def testCreateChain():Unit = {
    val baseDir = "VoteContractTest/testCreateChain"
    When.createChain(baseDir){}
    try{
    }
    finally {
      chain.close()
    }
  }

  def When = this
  def Then = this
  def And  =this
  def Given= this


}

object VoteContractTest {
  @AfterClass
  def cleanUp: Unit = {
    println("clean Directory")
    Directory("VoteContractTest").deleteRecursively()
  }
}
