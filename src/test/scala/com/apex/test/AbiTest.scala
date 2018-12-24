package com.apex.test

import junit.framework.TestCase.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import java.io.IOException
import com.apex.solidity.Abi
import com.apex.solidity.Abi.Entry.EntryType

@Test
class AbiTest {
  @Test
  @throws[IOException]
  def simpleTest(): Unit = {
    val contractAbi = "[{" + "\"name\":\"simpleFunction\"," + "\"constant\":true," + "\"payable\":true," + "\"type\":\"function\"," + "\"inputs\": [{\"name\":\"_in\", \"type\":\"bytes32\"}]," + "\"outputs\":[{\"name\":\"_out\",\"type\":\"bytes32\"}]}]"
    val abi = Abi.fromJson(contractAbi)
    assertEquals(abi.size, 1)
    val onlyFunc = abi.get(0)
    assertEquals(onlyFunc.entryType, EntryType.function)
    assertEquals(onlyFunc.inputs.size, 1)
    assertEquals(onlyFunc.outputs.size, 1)
    assertTrue(onlyFunc.payable)
    assertTrue(onlyFunc.constant.get)
  }

  @Test
  @throws[IOException]
  def simpleLegacyTest(): Unit = {
    val contractAbi = "[{" + "\"name\":\"simpleFunction\"," + "\"constant\":true," + "\"type\":\"function\"," + "\"inputs\": [{\"name\":\"_in\", \"type\":\"bytes32\"}]," + "\"outputs\":[{\"name\":\"_out\",\"type\":\"bytes32\"}]}]"
    val abi = Abi.fromJson(contractAbi)
    assertEquals(abi.size, 1)
    val onlyFunc = abi.get(0)
    assertEquals(onlyFunc.entryType, EntryType.function)
    assertEquals(onlyFunc.inputs.size, 1)
    assertEquals(onlyFunc.outputs.size, 1)
    assertTrue(onlyFunc.constant.get)
  }
}


