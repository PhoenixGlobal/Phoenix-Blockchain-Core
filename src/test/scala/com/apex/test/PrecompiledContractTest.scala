/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: PrecompiledContractTest.scala
 *
 * @author: shan.huang@chinapex.com: 18-11-30 下午7:40@version: 1.0
 *
 */

package com.apex.test

import com.apex.settings.{ContractSettings, DataBaseSettings}
import com.apex.vm.{DataWord, PrecompiledContracts, VM}
import org.junit.Assert.{assertArrayEquals, assertEquals}
import org.junit.Test
import org.spongycastle.util.encoders.Hex

@Test
class PrecompiledContractTest {

  val vmSettings = ContractSettings(0, false, false, false, false, true, false, false, false, false, false, false)

  @Test
  def identityTest1(): Unit = {
    val addr = DataWord.of("0000000000000000000000000000000000000000000000000000000000000004")
    val contract = PrecompiledContracts.getContractForAddress(addr, vmSettings)
    val data = Hex.decode("112233445566")
    val expected = Hex.decode("112233445566")
    val result = contract.execute(data)._2
    assertArrayEquals(expected, result)
  }

  @Test
  def sha256Test1: Unit = {
    val addr = DataWord.of("0000000000000000000000000000000000000000000000000000000000000002")
    val contract = PrecompiledContracts.getContractForAddress(addr, vmSettings)
    val data = null
    val expected = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    val result = contract.execute(data)._2
    assertEquals(expected, Hex.toHexString(result))
  }

  @Test
  def sha256Test2: Unit = {
    val addr = DataWord.of("0000000000000000000000000000000000000000000000000000000000000002")
    val contract = PrecompiledContracts.getContractForAddress(addr, vmSettings)
    val data = new Array[Byte](0)
    val expected = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    val result = contract.execute(data)._2
    assertEquals(expected, Hex.toHexString(result))
  }

  @Test
  def sha256Test3: Unit = {
    val addr = DataWord.of("0000000000000000000000000000000000000000000000000000000000000002")
    val contract = PrecompiledContracts.getContractForAddress(addr, vmSettings)
    val data = Hex.decode("112233")
    val expected = "49ee2bf93aac3b1fb4117e59095e07abe555c3383b38d608da37680a406096e8"
    val result = contract.execute(data)._2
    assertEquals(expected, Hex.toHexString(result))
  }

  @Test
  def Ripempd160Test1(): Unit = {
    val addr = DataWord.of("0000000000000000000000000000000000000000000000000000000000000003")
    val contract = PrecompiledContracts.getContractForAddress(addr, vmSettings)
    val data = Hex.decode("0000000000000000000000000000000000000000000000000000000000000001")
    val expected = "000000000000000000000000ae387fcfeb723c3f5964509af111cf5a67f30661"
    val result = contract.execute(data)._2
    assertEquals(expected, Hex.toHexString(result))
  }

  @Test
  def ecRecoverTest1(): Unit = {
    val data = Hex.decode("18c547e4f7b0f325ad1e56f57e26c745b09a3e503d86e00e5255ff7f715d3d1c000000000000000000000000000000000000000000000000000000000000001c73b1693892219d736caba55bdb67216e485557ea6b6af75f37096c9aa6a5a75feeb940b1d03b21e36b0e47e79769f095fe2ab855bd91e3a38756b7d75a9c4549")
    val addr = DataWord.of("0000000000000000000000000000000000000000000000000000000000000001")
    val contract = PrecompiledContracts.getContractForAddress(addr, vmSettings)
    val expected = "000000000000000000000000ae387fcfeb723c3f5964509af111cf5a67f30661"
    val result = contract.execute(data)._2
    System.out.println(Hex.toHexString(result))
  }

}

