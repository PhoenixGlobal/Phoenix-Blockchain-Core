/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: VMTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-30 下午7:40@version: 1.0
 *
 */

package com.apex.test

import java.util

import com.apex.core.DataBase
import com.apex.crypto.Ecdsa.PublicKey
import com.apex.crypto.{BinaryData, Crypto, UInt160}
import com.apex.settings.{ContractSettings, DataBaseSettings}
import com.apex.solidity.Abi
import com.apex.solidity.compiler.SolidityCompiler
import com.apex.vm.hook.VMHook
import com.apex.vm.program.{Program, ProgramResult}
import com.apex.vm.program.invoke.{ProgramInvoke, ProgramInvokeImpl}
import com.apex.vm.{DataWord, VM}
import org.apex.vm.OpCode
import org.bouncycastle.util.encoders.Hex
import org.junit.{AfterClass, Test}

import scala.reflect.io.Directory

@Test
class VMTest {
  import VMTest._

  @Test
  def test1: Unit = {
    val _ =
      "contract D {\n   " +
        " mapping(address => uint) test;\n\n   " +
        " function set(uint i) public {\n        " +
        "   test[msg.sender] = i;\n   " +
        " }\n\n   " +
        " function get() public returns (uint) {\n        " +
        "   return test[msg.sender];\n   " +
        " }\n" +
        "}"
    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"i\",\"type\":\"uint256\"}],\"name\":\"set\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"get\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]")
    val (contract, result) = VMTest.deploy(author, BinaryData("608060405234801561001057600080fd5b50610169806100206000396000f3fe608060405260043610610046576000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11461004b5780636d4ce63c14610086575b600080fd5b34801561005757600080fd5b506100846004803603602081101561006e57600080fd5b81019080803590602001909291905050506100b1565b005b34801561009257600080fd5b5061009b6100f7565b6040518082815260200191505060405180910390f35b806000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555050565b60008060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205490509056fea165627a7a72305820b698ee72285f30209458db25efe0fe5632e08930f7659a0ac5208217d38d92500029"))
    assert(result.getHReturn.sameElements(BinaryData("608060405260043610610046576000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11461004b5780636d4ce63c14610086575b600080fd5b34801561005757600080fd5b506100846004803603602081101561006e57600080fd5b81019080803590602001909291905050506100b1565b005b34801561009257600080fd5b5061009b6100f7565b6040518082815260200191505060405180910390f35b806000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555050565b60008060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205490509056fea165627a7a72305820b698ee72285f30209458db25efe0fe5632e08930f7659a0ac5208217d38d92500029")))
    val setResult = VMTest.call(caller, contract, result.getHReturn, abi.encode("set(0x777)"))
    assert(sucess(setResult))
    val getResult = VMTest.call(caller, contract, result.getHReturn, abi.encode("get()"))
    assert(sucess(getResult))
    assert(DataWord.of(getResult.getHReturn).value == 0x777)
  }

  @Test
  def test2: Unit = {
    val _ = "contract Ownable {\n" +
      "  address public owner;\n" +
      "\n" +
      "  function constructor() public {\n" +
      "    owner = msg.sender;\n" +
      "  }\n" +
      "\n" +
      "  modifier onlyOwner() {\n" +
      "    require(msg.sender == owner);\n" +
      "    _;\n" +
      "  }\n" +
      "\n" +
      "  function transferOwnership(address newOwner) onlyOwner public {\n" +
      "    if (newOwner != address(0)) {\n" +
      "      owner = newOwner;\n" +
      "    }\n" +
      "  }\n" +
      "}"
    val abi = Abi.fromJson("[{\"constant\":true,\"inputs\":[],\"name\":\"owner\",\"outputs\":[{\"name\":\"\",\"type\":\"address\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"newOwner\",\"type\":\"address\"}],\"name\":\"transferOwnership\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"inputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"constructor\"}]")
    val (contract, result) = VMTest.deploy(author, BinaryData("608060405234801561001057600080fd5b50336000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550610219806100606000396000f3fe608060405260043610610046576000357c0100000000000000000000000000000000000000000000000000000000900480638da5cb5b1461004b578063f2fde38b146100a2575b600080fd5b34801561005757600080fd5b506100606100f3565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b3480156100ae57600080fd5b506100f1600480360360208110156100c557600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610118565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151561017357600080fd5b600073ffffffffffffffffffffffffffffffffffffffff168173ffffffffffffffffffffffffffffffffffffffff161415156101ea57806000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055505b5056fea165627a7a7230582013ec64e541b38e025ab583499d3764fcdcba9507625a990f73cb75ec6131a5ad0029"))
    assert(sucess(result))
    assert(result.getHReturn.sameElements(BinaryData("608060405260043610610046576000357c0100000000000000000000000000000000000000000000000000000000900480638da5cb5b1461004b578063f2fde38b146100a2575b600080fd5b34801561005757600080fd5b506100606100f3565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b3480156100ae57600080fd5b506100f1600480360360208110156100c557600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610118565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151561017357600080fd5b600073ffffffffffffffffffffffffffffffffffffffff168173ffffffffffffffffffffffffffffffffffffffff161415156101ea57806000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055505b5056fea165627a7a7230582013ec64e541b38e025ab583499d3764fcdcba9507625a990f73cb75ec6131a5ad0029")))
    val result1 = VMTest.call(caller, contract, result.getHReturn, abi.encode("owner"))
    assert(sucess(result1))
    assert(DataWord.of(result1.getHReturn).toUInt160.equals(author))
    val result2 = VMTest.call(caller, contract, result.getHReturn, abi.encode(s"transferOwnership('${caller.toString}')"))
    assert(!sucess(result2))
    val result3 = VMTest.call(caller, contract, result.getHReturn, abi.encode("owner"))
    assert(sucess(result3))
    assert(DataWord.of(result3.getHReturn).toUInt160.equals(author))
    val result4 = VMTest.call(author, contract, result.getHReturn, abi.encode(s"transferOwnership('${caller.toString}')"))
    assert(sucess(result4))
    val result5 = VMTest.call(author, contract, result.getHReturn, abi.encode("owner"))
    assert(sucess(result5))
    assert(DataWord.of(result5.getHReturn).toUInt160.equals(caller))
  }

  private def sucess(getResult: ProgramResult) = {
    !getResult.isRevert && getResult.getException == null
  }
}

object VMTest {
  private val dir = "VMTest"
  private val settings = DataBaseSettings(dir, true, 10)
  private val dataBase = new DataBase(settings)

  val caller = PublicKey("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322").pubKeyHash
  val author = PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8").pubKeyHash
  val contractAddress = Crypto.calcNewAddr(author, BigInt(1).toByteArray)

  def deploy(caller: UInt160, code: Array[Byte]) = {
    val vmSettings = ContractSettings(0, false)
    val contract = Crypto.calcNewAddr(author, BigInt(1).toByteArray)
    val invoker = VMTest.createInvoker(caller, contract, Array.empty)
    val program = new Program(vmSettings, code, invoker)
    val result = VM.play(vmSettings, VMHook.EMPTY, program)
    (contract, result)
  }

  def call(caller: UInt160, contract: UInt160, code: Array[Byte], signature: Array[Byte]) = {
    val vmSettings = ContractSettings(0, false)
    val invoker = VMTest.createInvoker(caller, contract, signature)
    val program = new Program(vmSettings, code, invoker)
    val result = VM.play(vmSettings, VMHook.EMPTY, program)
    result
  }

  def createInvoker(caller: UInt160, contract: UInt160, data: Array[Byte]): ProgramInvoke = {
    new ProgramInvokeImpl(
      DataWord.of(contract),
      DataWord.of(caller),
      DataWord.of(caller),
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.of(Int.MaxValue),
      DataWord.ZERO,
      data,
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.ZERO,
      dataBase,
      dataBase,
      null)
  }

  @AfterClass
  def cleanUp: Unit = {
    try {
      dataBase.close()
      Directory(dir).deleteRecursively()
    } catch {
      case _: Throwable =>
    }
  }
}
