/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: WalletTest.scala
 *
 * @author: shan.huang@chinapex.com: 2018-07-27 下午4:06@version: 1.0
 */

package com.apex.test

import com.apex.crypto.{BinaryData}
import org.junit.Test
import com.apex.wallets.Wallet

@Test
class WalletTest {   
  
  @Test
  def testToAddress = {     
    //val data = new Array[Byte](20)
    //new SecureRandom().nextBytes(data) 
    
    //20 bytes data
    val data1 = BinaryData("f54a5851e9372b87810a8e60cdd2e7cfd80b6e31")
    val address1 = Wallet.toAddress(data1)    
    assert(address1 == "APEX1dQ3F4LTJ3J8VXDtfpe9LLSLLWkyWmBked") 
    
    //System.out.println(address)   
  }
  
  @Test
  def testToScriptHash = {     
    val hash = Wallet.toScriptHash("APEX1dQ3F4LTJ3J8VXDtfpe9LLSLLWkyWmBked").get    
    assert(hash.data sameElements BinaryData("f54a5851e9372b87810a8e60cdd2e7cfd80b6e31"))    
  }
  
  @Test
  def testPrivKeyToWIF = {    
    val privKey = BinaryData("1e99423a4ed27608a15a2616a2b0e9e52ced330ac530edcc32c8ffc6a526aedd")    
    val wif = Wallet.privKeyToWIF(privKey)    
    assert(wif == "KxFC1jmwwCoACiCAWZ3eXa96mBM6tb3TYzGmf6YwgdGWZgawvrtJ")        
  }
  
  @Test
  def testGetPrivKeyFromWIF = {
    val privKey = Wallet.getPrivKeyFromWIF("KxFC1jmwwCoACiCAWZ3eXa96mBM6tb3TYzGmf6YwgdGWZgawvrtJ").get
    assert(privKey sameElements BinaryData("1e99423a4ed27608a15a2616a2b0e9e52ced330ac530edcc32c8ffc6a526aedd"))
  }
  
}
