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

import com.apex.core.DataBase
import com.apex.crypto.Ecdsa.PublicKey
import com.apex.crypto.{BinaryData, Crypto, UInt160}
import com.apex.settings.{ContractSettings, DataBaseSettings}
import com.apex.solidity.Abi
import com.apex.vm.hook.VMHook
import com.apex.vm.program.invoke.{ProgramInvoke, ProgramInvokeImpl}
import com.apex.vm.program.{Program, ProgramResult}
import com.apex.vm.{DataWord, VM}
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
    val _ =
      "contract Ownable {\n" +
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
    val _ =
      "contract G {\n" +
      "    uint value;\n" +
      "    constructor() payable public {\n" +
      "        value = msg.value;\n" +
      "    }\n" +
      "\n" +
      "    function get(uint amount) payable public {\n" +
      "        if (value >= amount) {\n" +
      "            msg.sender.transfer(amount);\n" +
      "        }\n" +
      "    }" +
      "\n}"

    // author has 1000
    dataBase.addBalance(author, 1000)

    // author deploy a contract and send it 500
    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"amount\",\"type\":\"uint256\"}],\"name\":\"get\",\"outputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"function\"},{\"inputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"constructor\"}]")
    val (contract, result) = VMTest.deploy(author, BinaryData("60806040523460008190555060f5806100196000396000f3fe6080604052600436106039576000357c0100000000000000000000000000000000000000000000000000000000900480639507d39a14603e575b600080fd5b348015604957600080fd5b50607360048036036020811015605e57600080fd5b81019080803590602001909291905050506075565b005b8060005410151560c6573373ffffffffffffffffffffffffffffffffffffffff166108fc829081150290604051600060405180830381858888f1935050505015801560c4573d6000803e3d6000fd5b505b5056fea165627a7a723058204d8c453a38511784d625551b11ee4c7fc439074acd2bc27b3c63bce2aa6cb5ac0029"), 500)
    assert(success(result))
    assert(result.getHReturn.sameElements(BinaryData("6080604052600436106039576000357c0100000000000000000000000000000000000000000000000000000000900480639507d39a14603e575b600080fd5b348015604957600080fd5b50607360048036036020811015605e57600080fd5b81019080803590602001909291905050506075565b005b8060005410151560c6573373ffffffffffffffffffffffffffffffffffffffff166108fc829081150290604051600060405180830381858888f1935050505015801560c4573d6000803e3d6000fd5b505b5056fea165627a7a723058204d8c453a38511784d625551b11ee4c7fc439074acd2bc27b3c63bce2aa6cb5ac0029")))
    assert(dataBase.getBalance(author).exists(_.value == 500))

    // caller can not get more than 500 from contract
    val getMoneyResult1 = VMTest.call(caller, contract, result.getHReturn, abi.encode("get(501)"))
    assert(success(getMoneyResult1))
    assert(dataBase.getBalance(contract).exists(_.value == 500))
    assert(dataBase.getBalance(caller).isEmpty)

    // caller can not get less than 0 from contract
    val getMoneyResult2 = VMTest.call(caller, contract, result.getHReturn, abi.encode("get(-1)"))
    assert(getMoneyResult2.isRevert)
    assert(dataBase.getBalance(contract).exists(_.value == 500))
    assert(dataBase.getBalance(caller).isEmpty)

    // caller get 100 from contract, contract has 400 left
    val getMoneyResult3 = VMTest.call(caller, contract, result.getHReturn, abi.encode("get(100)"))
    assert(success(getMoneyResult3))
    assert(dataBase.getBalance(contract).exists(_.value == 400))
    assert(dataBase.getBalance(caller).exists(_.value == 100))
  }

  @Test
  def test4 = {
    val _ =
      "contract SuicideTest {\n" +
      "    address payable owner;\n" +
      "\n" +
      "    modifier onlyOwner() {\n" +
      "        require(msg.sender == owner);\n" +
      "        _;\n" +
      "    }\n" +
      "\n" +
      "    constructor() payable public {\n" +
      "        owner = msg.sender;\n" +
      "    }\n" +
      "\n" +
      "    function destroy() onlyOwner public {\n" +
      "        selfdestruct(owner);\n" +
      "    }\n" +
      "}"

    // author has 1000
    dataBase.addBalance(author, 1000)

    // author deploy a contract and send it 500
    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[],\"name\":\"destroy\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"inputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"constructor\"}]")
    val (contract, result) = VMTest.deploy(author, BinaryData("6080604052336000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550610112806100536000396000f3fe6080604052600436106039576000357c01000000000000000000000000000000000000000000000000000000009004806383197ef014603e575b600080fd5b348015604957600080fd5b5060506052565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151560ac57600080fd5b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16fffea165627a7a72305820855a7b5ead6876153fd7b3f61d6f451d7ec98c0a99acef41c5eb71476307146f0029"), 500)
    assert(success(result))
    assert(result.getHReturn.sameElements(BinaryData("6080604052600436106039576000357c01000000000000000000000000000000000000000000000000000000009004806383197ef014603e575b600080fd5b348015604957600080fd5b5060506052565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151560ac57600080fd5b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16fffea165627a7a72305820855a7b5ead6876153fd7b3f61d6f451d7ec98c0a99acef41c5eb71476307146f0029")))
    assert(dataBase.getBalance(author).exists(_.value == 500))

    // caller can not destruct contract
    val result1 = VMTest.call(caller, contract, result.getHReturn, abi.encode("destroy()"))
    assert(result1.isRevert)
    assert(dataBase.getBalance(contract).exists(_.value == 500))
    assert(dataBase.getBalance(caller).isEmpty)

    // author destruct contract
    val result2 = VMTest.call(author, contract, result.getHReturn, abi.encode("destroy()"))
    assert(success(result2))
    println(dataBase.getBalance(contract))
    println(dataBase.getBalance(author))
    assert(dataBase.getBalance(author).exists(_.value == 1000))
//    assert(dataBase.getBalance(contract).isEmpty)
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
  val vmSettings = ContractSettings(0, false)

  def deploy(caller: UInt160, code: Array[Byte], value: Int = 0) = {
    val contract = Crypto.calcNewAddr(author, BigInt(1).toByteArray)
    if (value > 0 ) dataBase.transfer(caller, contract, value)
    val invoker = VMTest.createInvoker(caller, contract, Array.empty, value)
    val program = new Program(vmSettings, code, invoker, Long.MaxValue)
    val result = VM.play(vmSettings, VMHook.EMPTY, program)
    (contract, result)
  }

  def call(caller: UInt160, contract: UInt160, code: Array[Byte], signature: Array[Byte], value: Int = 0) = {
    if (value > 0 ) dataBase.transfer(caller, contract, value)
    val invoker = VMTest.createInvoker(caller, contract, signature, value)
    val program = new Program(vmSettings, code, invoker, Long.MaxValue)
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
