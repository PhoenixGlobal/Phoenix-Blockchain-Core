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

    // deploy contract
    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"i\",\"type\":\"uint256\"}],\"name\":\"set\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"get\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]")
    val (contract, result) = VMTest.deploy(author, BinaryData("608060405234801561001057600080fd5b50610169806100206000396000f3fe608060405260043610610046576000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11461004b5780636d4ce63c14610086575b600080fd5b34801561005757600080fd5b506100846004803603602081101561006e57600080fd5b81019080803590602001909291905050506100b1565b005b34801561009257600080fd5b5061009b6100f7565b6040518082815260200191505060405180910390f35b806000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555050565b60008060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205490509056fea165627a7a72305820b698ee72285f30209458db25efe0fe5632e08930f7659a0ac5208217d38d92500029"))
    assert(result.getHReturn.sameElements(BinaryData("608060405260043610610046576000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11461004b5780636d4ce63c14610086575b600080fd5b34801561005757600080fd5b506100846004803603602081101561006e57600080fd5b81019080803590602001909291905050506100b1565b005b34801561009257600080fd5b5061009b6100f7565b6040518082815260200191505060405180910390f35b806000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555050565b60008060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205490509056fea165627a7a72305820b698ee72285f30209458db25efe0fe5632e08930f7659a0ac5208217d38d92500029")))

    // call set(0x777)
    val setResult = VMTest.call(caller, contract, result.getHReturn, abi.encode("set(0x777)"))
    assert(success(setResult))

    // call get() must return 0x777
    val getResult = VMTest.call(caller, contract, result.getHReturn, abi.encode("get()"))
    assert(success(getResult))
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

    // deploy contract
    val abi = Abi.fromJson("[{\"constant\":true,\"inputs\":[],\"name\":\"owner\",\"outputs\":[{\"name\":\"\",\"type\":\"address\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"newOwner\",\"type\":\"address\"}],\"name\":\"transferOwnership\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"inputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"constructor\"}]")
    val (contract, result) = VMTest.deploy(author, BinaryData("608060405234801561001057600080fd5b50336000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550610219806100606000396000f3fe608060405260043610610046576000357c0100000000000000000000000000000000000000000000000000000000900480638da5cb5b1461004b578063f2fde38b146100a2575b600080fd5b34801561005757600080fd5b506100606100f3565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b3480156100ae57600080fd5b506100f1600480360360208110156100c557600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610118565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151561017357600080fd5b600073ffffffffffffffffffffffffffffffffffffffff168173ffffffffffffffffffffffffffffffffffffffff161415156101ea57806000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055505b5056fea165627a7a7230582013ec64e541b38e025ab583499d3764fcdcba9507625a990f73cb75ec6131a5ad0029"))
    assert(success(result))
    assert(result.getHReturn.sameElements(BinaryData("608060405260043610610046576000357c0100000000000000000000000000000000000000000000000000000000900480638da5cb5b1461004b578063f2fde38b146100a2575b600080fd5b34801561005757600080fd5b506100606100f3565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b3480156100ae57600080fd5b506100f1600480360360208110156100c557600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610118565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151561017357600080fd5b600073ffffffffffffffffffffffffffffffffffffffff168173ffffffffffffffffffffffffffffffffffffffff161415156101ea57806000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055505b5056fea165627a7a7230582013ec64e541b38e025ab583499d3764fcdcba9507625a990f73cb75ec6131a5ad0029")))

    // call owner
    val result1 = VMTest.call(caller, contract, result.getHReturn, abi.encode("owner"))
    assert(success(result1))
    assert(DataWord.of(result1.getHReturn).toUInt160.equals(author))

    // call transferOwnership from caller will not pass check
    val result2 = VMTest.call(caller, contract, result.getHReturn, abi.encode(s"transferOwnership('${caller.toString}')"))
    assert(!success(result2))
    // check owner
    val result3 = VMTest.call(caller, contract, result.getHReturn, abi.encode("owner"))
    assert(success(result3))
    assert(DataWord.of(result3.getHReturn).toUInt160.equals(author))

    // call transferOwnership from author
    val result4 = VMTest.call(author, contract, result.getHReturn, abi.encode(s"transferOwnership('${caller.toString}')"))
    assert(success(result4))
    // check owner
    val result5 = VMTest.call(author, contract, result.getHReturn, abi.encode("owner"))
    assert(success(result5))
    assert(DataWord.of(result5.getHReturn).toUInt160.equals(caller))
  }

  @Test
  def test3 = {
    println(author)
    println(caller)
    dataBase.addBalance(author, 1000)
    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[],\"name\":\"get\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"inputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"constructor\"}]")
    val (contract, result) = VMTest.deploy(author, BinaryData("60806040523460008190555060b8806100196000396000f3fe6080604052600436106039576000357c0100000000000000000000000000000000000000000000000000000000900480636d4ce63c14603e575b600080fd5b348015604957600080fd5b5060506052565b005b3373ffffffffffffffffffffffffffffffffffffffff166108fc6000549081150290604051600060405180830381858888f193505050505056fea165627a7a7230582011e47efa4ca24196900f9d7796e7f5ae67211af5775e1bb2f30eefa9f6dd35fd0029"))
    assert(success(result))
    assert(result.getHReturn.sameElements(BinaryData("6080604052600436106039576000357c0100000000000000000000000000000000000000000000000000000000900480636d4ce63c14603e575b600080fd5b348015604957600080fd5b5060506052565b005b3373ffffffffffffffffffffffffffffffffffffffff166108fc6000549081150290604051600060405180830381858888f193505050505056fea165627a7a7230582011e47efa4ca24196900f9d7796e7f5ae67211af5775e1bb2f30eefa9f6dd35fd0029")))
    val getMoneyResult = VMTest.call(caller, contract, result.getHReturn, abi.encode("get()"))
    assert(success(getMoneyResult))
  }

  @Test
  def test4 = {
    val abi = Abi.fromJson("[{\"constant\":true,\"inputs\":[{\"name\":\"\",\"type\":\"address\"}],\"name\":\"registrantsPaid\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"organizer\",\"outputs\":[{\"name\":\"\",\"type\":\"address\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"recipient\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"name\":\"refundTicket\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"destroy\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"newquota\",\"type\":\"uint256\"}],\"name\":\"changeQuota\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"quota\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"numRegistrants\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"buyTicket\",\"outputs\":[{\"name\":\"success\",\"type\":\"bool\"}],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"function\"},{\"inputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"constructor\"},{\"anonymous\":false,\"inputs\":[{\"indexed\":false,\"name\":\"_from\",\"type\":\"address\"},{\"indexed\":false,\"name\":\"_amount\",\"type\":\"uint256\"}],\"name\":\"Deposit\",\"type\":\"event\"},{\"anonymous\":false,\"inputs\":[{\"indexed\":false,\"name\":\"_to\",\"type\":\"address\"},{\"indexed\":false,\"name\":\"_amount\",\"type\":\"uint256\"}],\"name\":\"Refund\",\"type\":\"event\"}]")
    val (contract, result) = VMTest.deploy(author, BinaryData("608060405234801561001057600080fd5b50336000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055506101f46003819055506000600281905550610696806100716000396000f3fe608060405260043610610088576000357c01000000000000000000000000000000000000000000000000000000009004806313381fbf1461008d57806361203265146100f2578063705099b91461014957806383197ef0146101a4578063a977c71e146101bb578063cebe09c9146101f6578063ec3a6f7314610221578063edca914c1461024c575b600080fd5b34801561009957600080fd5b506100dc600480360360208110156100b057600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff16906020019092919050505061026e565b6040518082815260200191505060405180910390f35b3480156100fe57600080fd5b50610107610286565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b34801561015557600080fd5b506101a26004803603604081101561016c57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803590602001909291905050506102ab565b005b3480156101b057600080fd5b506101b9610486565b005b3480156101c757600080fd5b506101f4600480360360208110156101de57600080fd5b8101908080359060200190929190505050610517565b005b34801561020257600080fd5b5061020b61057d565b6040518082815260200191505060405180910390f35b34801561022d57600080fd5b50610236610583565b6040518082815260200191505060405180910390f35b610254610589565b604051808215151515815260200191505060405180910390f35b60016020528060005260406000206000915090505481565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151561030657610482565b80600160008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020541415610481576000309050818173ffffffffffffffffffffffffffffffffffffffff163110151561047a578273ffffffffffffffffffffffffffffffffffffffff166108fc839081150290604051600060405180830381858888f1935050505015156103b257600080fd5b6000600160008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550600260008154809291906001900391905055507fbb28353e4598c3b9199101a66e0989549b659a59a54d2c27fbb183f1932c8e6d8383604051808373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018281526020019250505060405180910390a161047f565b600080fd5b505b5b5050565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff161415610515576000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16ff5b565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff161415156105725761057a565b806003819055505b50565b60035481565b60025481565b60006003546002541015156105a15760009050610667565b34600160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506002600081548092919060010191905055507fe1fffcc4923d04b559f4d29a8bfc6cda04eb5b0d3c460751c2402c5c5cc9109c3334604051808373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018281526020019250505060405180910390a1600190505b9056fea165627a7a7230582023ab6a68ba782f367d69eae1f5bd8ede14a444f34d941cc39f9f3192f11aac170029"))
    assert(success(result))
    assert(result.getHReturn.sameElements(BinaryData("608060405260043610610088576000357c01000000000000000000000000000000000000000000000000000000009004806313381fbf1461008d57806361203265146100f2578063705099b91461014957806383197ef0146101a4578063a977c71e146101bb578063cebe09c9146101f6578063ec3a6f7314610221578063edca914c1461024c575b600080fd5b34801561009957600080fd5b506100dc600480360360208110156100b057600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff16906020019092919050505061026e565b6040518082815260200191505060405180910390f35b3480156100fe57600080fd5b50610107610286565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b34801561015557600080fd5b506101a26004803603604081101561016c57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803590602001909291905050506102ab565b005b3480156101b057600080fd5b506101b9610486565b005b3480156101c757600080fd5b506101f4600480360360208110156101de57600080fd5b8101908080359060200190929190505050610517565b005b34801561020257600080fd5b5061020b61057d565b6040518082815260200191505060405180910390f35b34801561022d57600080fd5b50610236610583565b6040518082815260200191505060405180910390f35b610254610589565b604051808215151515815260200191505060405180910390f35b60016020528060005260406000206000915090505481565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151561030657610482565b80600160008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020541415610481576000309050818173ffffffffffffffffffffffffffffffffffffffff163110151561047a578273ffffffffffffffffffffffffffffffffffffffff166108fc839081150290604051600060405180830381858888f1935050505015156103b257600080fd5b6000600160008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550600260008154809291906001900391905055507fbb28353e4598c3b9199101a66e0989549b659a59a54d2c27fbb183f1932c8e6d8383604051808373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018281526020019250505060405180910390a161047f565b600080fd5b505b5b5050565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff161415610515576000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16ff5b565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff161415156105725761057a565b806003819055505b50565b60035481565b60025481565b60006003546002541015156105a15760009050610667565b34600160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506002600081548092919060010191905055507fe1fffcc4923d04b559f4d29a8bfc6cda04eb5b0d3c460751c2402c5c5cc9109c3334604051808373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018281526020019250505060405180910390a1600190505b9056fea165627a7a7230582023ab6a68ba782f367d69eae1f5bd8ede14a444f34d941cc39f9f3192f11aac170029")))
    separate
    val buyTicket = VMTest.call(caller, contract, result.getHReturn, abi.encode("buyTicket()"), 10)
    assert(success(buyTicket))
    assert(DataWord.of(buyTicket.getHReturn).value == 1)
    separate
    println(caller.toString)
    val refund = VMTest.call(author, contract, result.getHReturn, abi.encode(s"refundTicket('${caller.toString}', 10)"))
    assert(success(refund))
  }

  private def success(getResult: ProgramResult) = {
    !getResult.isRevert && getResult.getException == null
  }

  private def separate() = {
    println()
    println("****************")
    println()
  }
}

object VMTest {
  private val dir = "VMTest"
  private val settings = DataBaseSettings(dir, true, 10)
  private val dataBase = new DataBase(settings)

  val caller = PublicKey("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322").pubKeyHash
  val author = PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8").pubKeyHash
  val contractAddress = Crypto.calcNewAddr(author, BigInt(1).toByteArray)

  def deploy(caller: UInt160, code: Array[Byte], value: Int = 0) = {
    val vmSettings = ContractSettings(0, false)
    val contract = Crypto.calcNewAddr(author, BigInt(1).toByteArray)
    val invoker = VMTest.createInvoker(caller, contract, Array.empty, value)
    val program = new Program(vmSettings, code, invoker)
    val result = VM.play(vmSettings, VMHook.EMPTY, program)
    (contract, result)
  }

  def call(caller: UInt160, contract: UInt160, code: Array[Byte], signature: Array[Byte], value: Int = 0) = {
    val vmSettings = ContractSettings(0, false)
    val invoker = VMTest.createInvoker(caller, contract, signature, value)
    val program = new Program(vmSettings, code, invoker)
    val result = VM.play(vmSettings, VMHook.EMPTY, program)
    result
  }

  def createInvoker(caller: UInt160, contract: UInt160, data: Array[Byte], value: Int): ProgramInvoke = {
    new ProgramInvokeImpl(
      DataWord.of(contract),
      DataWord.of(caller),
      DataWord.of(caller),
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.of(Int.MaxValue),
      DataWord.of(value),
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
