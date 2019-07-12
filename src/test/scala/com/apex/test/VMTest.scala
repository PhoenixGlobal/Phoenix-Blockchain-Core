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
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.crypto.{BinaryData, Crypto, UInt160}
import com.apex.settings._
import com.apex.solidity.Abi
import com.apex.vm.hook.VMHook
import com.apex.vm.program.invoke.{ProgramInvoke, ProgramInvokeImpl}
import com.apex.vm.program.{Program, ProgramResult}
import com.apex.vm.{DataWord, VM}
import org.junit.{After, Before, Test}

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
    val (contract, result) = VMTest.deploy(dataBase, author, BinaryData("608060405234801561001057600080fd5b50610169806100206000396000f3fe608060405260043610610046576000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11461004b5780636d4ce63c14610086575b600080fd5b34801561005757600080fd5b506100846004803603602081101561006e57600080fd5b81019080803590602001909291905050506100b1565b005b34801561009257600080fd5b5061009b6100f7565b6040518082815260200191505060405180910390f35b806000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555050565b60008060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205490509056fea165627a7a72305820b698ee72285f30209458db25efe0fe5632e08930f7659a0ac5208217d38d92500029"))
    assert(result.getHReturn.sameElements(BinaryData("608060405260043610610046576000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11461004b5780636d4ce63c14610086575b600080fd5b34801561005757600080fd5b506100846004803603602081101561006e57600080fd5b81019080803590602001909291905050506100b1565b005b34801561009257600080fd5b5061009b6100f7565b6040518082815260200191505060405180910390f35b806000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555050565b60008060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205490509056fea165627a7a72305820b698ee72285f30209458db25efe0fe5632e08930f7659a0ac5208217d38d92500029")))

    // call set(0x777)
    val setResult = VMTest.call(dataBase, caller, contract, result.getHReturn, abi.encode("set(0x777)"))
    assert(success(setResult))

    // call get() must return 0x777
    val getResult = VMTest.call(dataBase, caller, contract, result.getHReturn, abi.encode("get()"))
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
    val (contract, result) = VMTest.deploy(dataBase, author, BinaryData("608060405234801561001057600080fd5b50336000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550610219806100606000396000f3fe608060405260043610610046576000357c0100000000000000000000000000000000000000000000000000000000900480638da5cb5b1461004b578063f2fde38b146100a2575b600080fd5b34801561005757600080fd5b506100606100f3565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b3480156100ae57600080fd5b506100f1600480360360208110156100c557600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610118565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151561017357600080fd5b600073ffffffffffffffffffffffffffffffffffffffff168173ffffffffffffffffffffffffffffffffffffffff161415156101ea57806000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055505b5056fea165627a7a7230582013ec64e541b38e025ab583499d3764fcdcba9507625a990f73cb75ec6131a5ad0029"))
    assert(success(result))
    assert(result.getHReturn.sameElements(BinaryData("608060405260043610610046576000357c0100000000000000000000000000000000000000000000000000000000900480638da5cb5b1461004b578063f2fde38b146100a2575b600080fd5b34801561005757600080fd5b506100606100f3565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b3480156100ae57600080fd5b506100f1600480360360208110156100c557600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610118565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151561017357600080fd5b600073ffffffffffffffffffffffffffffffffffffffff168173ffffffffffffffffffffffffffffffffffffffff161415156101ea57806000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055505b5056fea165627a7a7230582013ec64e541b38e025ab583499d3764fcdcba9507625a990f73cb75ec6131a5ad0029")))

    // call owner
    val result1 = VMTest.call(dataBase, caller, contract, result.getHReturn, abi.encode("owner"))
    assert(success(result1))
    assert(DataWord.of(result1.getHReturn).toUInt160.equals(author))

    // call transferOwnership from caller will not pass check
    val result2 = VMTest.call(dataBase, caller, contract, result.getHReturn, abi.encode(s"transferOwnership('${caller.address}')"))
    assert(!success(result2))
    // check owner
    val result3 = VMTest.call(dataBase, caller, contract, result.getHReturn, abi.encode("owner"))
    assert(success(result3))
    assert(DataWord.of(result3.getHReturn).toUInt160.equals(author))

    // call transferOwnership from author
    val result4 = VMTest.call(dataBase, author, contract, result.getHReturn, abi.encode(s"transferOwnership('${caller.address}')"))
    assert(success(result4))
    // check owner
    val result5 = VMTest.call(dataBase, author, contract, result.getHReturn, abi.encode("owner"))
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
    val (contract, result) = VMTest.deploy(dataBase, author, BinaryData("60806040523460008190555060f5806100196000396000f3fe6080604052600436106039576000357c0100000000000000000000000000000000000000000000000000000000900480639507d39a14603e575b600080fd5b348015604957600080fd5b50607360048036036020811015605e57600080fd5b81019080803590602001909291905050506075565b005b8060005410151560c6573373ffffffffffffffffffffffffffffffffffffffff166108fc829081150290604051600060405180830381858888f1935050505015801560c4573d6000803e3d6000fd5b505b5056fea165627a7a723058204d8c453a38511784d625551b11ee4c7fc439074acd2bc27b3c63bce2aa6cb5ac0029"), 500)
    assert(success(result))
    assert(result.getHReturn.sameElements(BinaryData("6080604052600436106039576000357c0100000000000000000000000000000000000000000000000000000000900480639507d39a14603e575b600080fd5b348015604957600080fd5b50607360048036036020811015605e57600080fd5b81019080803590602001909291905050506075565b005b8060005410151560c6573373ffffffffffffffffffffffffffffffffffffffff166108fc829081150290604051600060405180830381858888f1935050505015801560c4573d6000803e3d6000fd5b505b5056fea165627a7a723058204d8c453a38511784d625551b11ee4c7fc439074acd2bc27b3c63bce2aa6cb5ac0029")))
    assert(dataBase.getBalance(author).exists(_.value == 500))

    // caller can not get more than 500 from contract
    val getMoneyResult1 = VMTest.call(dataBase, caller, contract, result.getHReturn, abi.encode("get(501)"))
    assert(success(getMoneyResult1))
    assert(dataBase.getBalance(contract).exists(_.value == 500))
    assert(dataBase.getBalance(caller).isEmpty)

    // caller can not get less than 0 from contract
    val getMoneyResult2 = VMTest.call(dataBase, caller, contract, result.getHReturn, abi.encode("get(-1)"))
    assert(getMoneyResult2.isRevert)
    assert(dataBase.getBalance(contract).exists(_.value == 500))
    assert(dataBase.getBalance(caller).isEmpty)

    // caller get 100 from contract, contract has 400 left
    val getMoneyResult3 = VMTest.call(dataBase, caller, contract, result.getHReturn, abi.encode("get(100)"))
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
    println(dataBase.getBalance(author))
    dataBase.addBalance(author, 1000)
    println(dataBase.getBalance(author))
    // author deploy a contract and send it 500
    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[],\"name\":\"destroy\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"inputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"constructor\"}]")
    val (contract, result) = VMTest.deploy(dataBase, author, BinaryData("6080604052336000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550610112806100536000396000f3fe6080604052600436106039576000357c01000000000000000000000000000000000000000000000000000000009004806383197ef014603e575b600080fd5b348015604957600080fd5b5060506052565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151560ac57600080fd5b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16fffea165627a7a72305820855a7b5ead6876153fd7b3f61d6f451d7ec98c0a99acef41c5eb71476307146f0029"), 500)
    assert(success(result))
    assert(result.getHReturn.sameElements(BinaryData("6080604052600436106039576000357c01000000000000000000000000000000000000000000000000000000009004806383197ef014603e575b600080fd5b348015604957600080fd5b5060506052565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151560ac57600080fd5b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16fffea165627a7a72305820855a7b5ead6876153fd7b3f61d6f451d7ec98c0a99acef41c5eb71476307146f0029")))
    println(dataBase.getBalance(contract))
    println(dataBase.getBalance(author))
    assert(dataBase.getBalance(contract).exists(_.value == 500))
    assert(dataBase.getBalance(author).exists(_.value == 500))

    // caller can not destruct contract
    val result1 = VMTest.call(dataBase, caller, contract, result.getHReturn, abi.encode("destroy()"))
    assert(result1.isRevert)
    assert(dataBase.getBalance(contract).exists(_.value == 500))
    assert(dataBase.getBalance(caller).isEmpty)

    // author destruct contract
    val result2 = VMTest.call(dataBase, author, contract, result.getHReturn, abi.encode("destroy()"))
    assert(success(result2))
    println(dataBase.getBalance(contract))
    println(dataBase.getBalance(author))
    assert(dataBase.getBalance(author).exists(_.value == 1000))
    //    assert(dataBase.getBalance(contract).isEmpty)
  }

  @Test
  def testBNB: Unit = {
    val abi = Abi.fromJson("[{\"constant\":true,\"inputs\":[],\"name\":\"name\",\"outputs\":[{\"name\":\"\",\"type\":\"string\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_spender\",\"type\":\"address\"},{\"name\":\"_value\",\"type\":\"uint256\"}],\"name\":\"approve\",\"outputs\":[{\"name\":\"success\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"totalSupply\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_from\",\"type\":\"address\"},{\"name\":\"_to\",\"type\":\"address\"},{\"name\":\"_value\",\"type\":\"uint256\"}],\"name\":\"transferFrom\",\"outputs\":[{\"name\":\"success\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"decimals\",\"outputs\":[{\"name\":\"\",\"type\":\"uint8\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"amount\",\"type\":\"uint256\"}],\"name\":\"withdrawEther\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_value\",\"type\":\"uint256\"}],\"name\":\"burn\",\"outputs\":[{\"name\":\"success\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_value\",\"type\":\"uint256\"}],\"name\":\"unfreeze\",\"outputs\":[{\"name\":\"success\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[{\"name\":\"\",\"type\":\"address\"}],\"name\":\"balanceOf\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"owner\",\"outputs\":[{\"name\":\"\",\"type\":\"address\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"symbol\",\"outputs\":[{\"name\":\"\",\"type\":\"string\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_to\",\"type\":\"address\"},{\"name\":\"_value\",\"type\":\"uint256\"}],\"name\":\"transfer\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[{\"name\":\"\",\"type\":\"address\"}],\"name\":\"freezeOf\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_value\",\"type\":\"uint256\"}],\"name\":\"freeze\",\"outputs\":[{\"name\":\"success\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[{\"name\":\"\",\"type\":\"address\"},{\"name\":\"\",\"type\":\"address\"}],\"name\":\"allowance\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_addr\",\"type\":\"address\"}],\"name\":\"getBalance\",\"outputs\":[{\"name\":\"balance\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"inputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"constructor\"},{\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"fallback\"},{\"anonymous\":false,\"inputs\":[{\"indexed\":true,\"name\":\"from\",\"type\":\"address\"},{\"indexed\":true,\"name\":\"to\",\"type\":\"address\"},{\"indexed\":false,\"name\":\"value\",\"type\":\"uint256\"}],\"name\":\"Transfer\",\"type\":\"event\"},{\"anonymous\":false,\"inputs\":[{\"indexed\":true,\"name\":\"from\",\"type\":\"address\"},{\"indexed\":false,\"name\":\"value\",\"type\":\"uint256\"}],\"name\":\"Burn\",\"type\":\"event\"},{\"anonymous\":false,\"inputs\":[{\"indexed\":true,\"name\":\"from\",\"type\":\"address\"},{\"indexed\":false,\"name\":\"value\",\"type\":\"uint256\"}],\"name\":\"Freeze\",\"type\":\"event\"},{\"anonymous\":false,\"inputs\":[{\"indexed\":true,\"name\":\"from\",\"type\":\"address\"},{\"indexed\":false,\"name\":\"value\",\"type\":\"uint256\"}],\"name\":\"Unfreeze\",\"type\":\"event\"}]")
    val (contract, result) = VMTest.deploy(dataBase, author, BinaryData("60606040526f98765000000000000000000000000000600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506f987650000000000000000000000000006003819055506040805190810160405280600881526020017f746573744170657800000000000000000000000000000000000000000000000081525060009080519060200190620000bc9291906200016e565b506040805190810160405280600281526020017f7441000000000000000000000000000000000000000000000000000000000000815250600190805190602001906200010a9291906200016e565b506003600260006101000a81548160ff021916908360ff16021790555033600460006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055506200021d565b828054600181600116156101000203166002900490600052602060002090601f016020900481019282601f10620001b157805160ff1916838001178555620001e2565b82800160010185558215620001e2579182015b82811115620001e1578251825591602001919060010190620001c4565b5b509050620001f19190620001f5565b5090565b6200021a91905b8082111562000216576000816000905550600101620001fc565b5090565b90565b6115bb806200022d6000396000f3006060604052600436106100e6576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806306fdde03146100e8578063095ea7b31461017657806318160ddd146101d057806323b872dd146101f9578063313ce567146102725780633bed33ce146102a157806342966c68146102c45780636623fc46146102ff57806370a082311461033a5780638da5cb5b1461038757806395d89b41146103dc578063a9059cbb1461046a578063cd4217c1146104ac578063d7a78db8146104f9578063dd62ed3e14610534578063f8b2cb4f146105a0575b005b34156100f357600080fd5b6100fb6105ed565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561013b578082015181840152602081019050610120565b50505050905090810190601f1680156101685780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b341561018157600080fd5b6101b6600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803590602001909190505061068b565b604051808215151515815260200191505060405180910390f35b34156101db57600080fd5b6101e3610726565b6040518082815260200191505060405180910390f35b341561020457600080fd5b610258600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803590602001909190505061072c565b604051808215151515815260200191505060405180910390f35b341561027d57600080fd5b610285610b6e565b604051808260ff1660ff16815260200191505060405180910390f35b34156102ac57600080fd5b6102c26004808035906020019091905050610b81565b005b34156102cf57600080fd5b6102e56004808035906020019091905050610c42565b604051808215151515815260200191505060405180910390f35b341561030a57600080fd5b6103206004808035906020019091905050610d96565b604051808215151515815260200191505060405180910390f35b341561034557600080fd5b610371600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050610f64565b6040518082815260200191505060405180910390f35b341561039257600080fd5b61039a610f7c565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b34156103e757600080fd5b6103ef610fa2565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561042f578082015181840152602081019050610414565b50505050905090810190601f16801561045c5780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b341561047557600080fd5b6104aa600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091908035906020019091905050611040565b005b34156104b757600080fd5b6104e3600480803573ffffffffffffffffffffffffffffffffffffffff169060200190919050506112e9565b6040518082815260200191505060405180910390f35b341561050457600080fd5b61051a6004808035906020019091905050611301565b604051808215151515815260200191505060405180910390f35b341561053f57600080fd5b61058a600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803573ffffffffffffffffffffffffffffffffffffffff169060200190919050506114cf565b6040518082815260200191505060405180910390f35b34156105ab57600080fd5b6105d7600480803573ffffffffffffffffffffffffffffffffffffffff169060200190919050506114f4565b6040518082815260200191505060405180910390f35b60008054600181600116156101000203166002900480601f0160208091040260200160405190810160405280929190818152602001828054600181600116156101000203166002900480156106835780601f1061065857610100808354040283529160200191610683565b820191906000526020600020905b81548152906001019060200180831161066657829003601f168201915b505050505081565b6000808211151561069b57600080fd5b81600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506001905092915050565b60035481565b60008073ffffffffffffffffffffffffffffffffffffffff168373ffffffffffffffffffffffffffffffffffffffff161415151561076957600080fd5b60008211151561077857600080fd5b81600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054101515156107c657600080fd5b600560008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054011015151561085557600080fd5b600760008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482111515156108e057600080fd5b610929600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361153d565b600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506109b5600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205483611556565b600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550610a7e600760008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361153d565b600760008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055508273ffffffffffffffffffffffffffffffffffffffff168473ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef846040518082815260200191505060405180910390a3600190509392505050565b600260009054906101000a900460ff1681565b600460009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16141515610bdd57600080fd5b600460009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166108fc829081150290604051600060405180830381858888f193505050501515610c3f57600080fd5b50565b600081600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205410151515610c9257600080fd5b600082111515610ca157600080fd5b610cea600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361153d565b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550610d396003548361153d565b6003819055503373ffffffffffffffffffffffffffffffffffffffff167fcc16f5dbb4873280815c1ee09dbd06736cffcc184412cf7a71a0fdb75d397ca5836040518082815260200191505060405180910390a260019050919050565b600081600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205410151515610de657600080fd5b600082111515610df557600080fd5b610e3e600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361153d565b600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550610eca600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205483611556565b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055503373ffffffffffffffffffffffffffffffffffffffff167f2cfce4af01bcb9d6cf6c84ee1b7c491100b8695368264146a94d71e10a63083f836040518082815260200191505060405180910390a260019050919050565b60056020528060005260406000206000915090505481565b600460009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b60018054600181600116156101000203166002900480601f0160208091040260200160405190810160405280929190818152602001828054600181600116156101000203166002900480156110385780601f1061100d57610100808354040283529160200191611038565b820191906000526020600020905b81548152906001019060200180831161101b57829003601f168201915b505050505081565b600073ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff161415151561107c57600080fd5b60008111151561108b57600080fd5b80600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054101515156110d957600080fd5b600560008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205481600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054011015151561116857600080fd5b6111b1600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548261153d565b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555061123d600560008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482611556565b600560008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055508173ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef836040518082815260200191505060405180910390a35050565b60066020528060005260406000206000915090505481565b600081600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020541015151561135157600080fd5b60008211151561136057600080fd5b6113a9600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361153d565b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550611435600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205483611556565b600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055503373ffffffffffffffffffffffffffffffffffffffff167ff97a274face0b5517365ad396b1fdba6f68bd3135ef603e44272adba3af5a1e0836040518082815260200191505060405180910390a260019050919050565b6007602052816000526040600020602052806000526040600020600091509150505481565b6000600560008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020549050919050565b600061154b83831115611580565b818303905092915050565b60008082840190506115768482101580156115715750838210155b611580565b8091505092915050565b80151561158c57600080fd5b505600a165627a7a723058201af50914efd1875e54c0660a90a679c61a604f98b248ab4c99df466a17bb1c2b0029"), 500)
    assert(success(result))
    assert(result.getHReturn.sameElements(BinaryData("6060604052600436106100e6576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806306fdde03146100e8578063095ea7b31461017657806318160ddd146101d057806323b872dd146101f9578063313ce567146102725780633bed33ce146102a157806342966c68146102c45780636623fc46146102ff57806370a082311461033a5780638da5cb5b1461038757806395d89b41146103dc578063a9059cbb1461046a578063cd4217c1146104ac578063d7a78db8146104f9578063dd62ed3e14610534578063f8b2cb4f146105a0575b005b34156100f357600080fd5b6100fb6105ed565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561013b578082015181840152602081019050610120565b50505050905090810190601f1680156101685780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b341561018157600080fd5b6101b6600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803590602001909190505061068b565b604051808215151515815260200191505060405180910390f35b34156101db57600080fd5b6101e3610726565b6040518082815260200191505060405180910390f35b341561020457600080fd5b610258600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803590602001909190505061072c565b604051808215151515815260200191505060405180910390f35b341561027d57600080fd5b610285610b6e565b604051808260ff1660ff16815260200191505060405180910390f35b34156102ac57600080fd5b6102c26004808035906020019091905050610b81565b005b34156102cf57600080fd5b6102e56004808035906020019091905050610c42565b604051808215151515815260200191505060405180910390f35b341561030a57600080fd5b6103206004808035906020019091905050610d96565b604051808215151515815260200191505060405180910390f35b341561034557600080fd5b610371600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050610f64565b6040518082815260200191505060405180910390f35b341561039257600080fd5b61039a610f7c565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b34156103e757600080fd5b6103ef610fa2565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561042f578082015181840152602081019050610414565b50505050905090810190601f16801561045c5780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b341561047557600080fd5b6104aa600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091908035906020019091905050611040565b005b34156104b757600080fd5b6104e3600480803573ffffffffffffffffffffffffffffffffffffffff169060200190919050506112e9565b6040518082815260200191505060405180910390f35b341561050457600080fd5b61051a6004808035906020019091905050611301565b604051808215151515815260200191505060405180910390f35b341561053f57600080fd5b61058a600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803573ffffffffffffffffffffffffffffffffffffffff169060200190919050506114cf565b6040518082815260200191505060405180910390f35b34156105ab57600080fd5b6105d7600480803573ffffffffffffffffffffffffffffffffffffffff169060200190919050506114f4565b6040518082815260200191505060405180910390f35b60008054600181600116156101000203166002900480601f0160208091040260200160405190810160405280929190818152602001828054600181600116156101000203166002900480156106835780601f1061065857610100808354040283529160200191610683565b820191906000526020600020905b81548152906001019060200180831161066657829003601f168201915b505050505081565b6000808211151561069b57600080fd5b81600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506001905092915050565b60035481565b60008073ffffffffffffffffffffffffffffffffffffffff168373ffffffffffffffffffffffffffffffffffffffff161415151561076957600080fd5b60008211151561077857600080fd5b81600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054101515156107c657600080fd5b600560008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054011015151561085557600080fd5b600760008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482111515156108e057600080fd5b610929600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361153d565b600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506109b5600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205483611556565b600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550610a7e600760008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361153d565b600760008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055508273ffffffffffffffffffffffffffffffffffffffff168473ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef846040518082815260200191505060405180910390a3600190509392505050565b600260009054906101000a900460ff1681565b600460009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16141515610bdd57600080fd5b600460009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166108fc829081150290604051600060405180830381858888f193505050501515610c3f57600080fd5b50565b600081600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205410151515610c9257600080fd5b600082111515610ca157600080fd5b610cea600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361153d565b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550610d396003548361153d565b6003819055503373ffffffffffffffffffffffffffffffffffffffff167fcc16f5dbb4873280815c1ee09dbd06736cffcc184412cf7a71a0fdb75d397ca5836040518082815260200191505060405180910390a260019050919050565b600081600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205410151515610de657600080fd5b600082111515610df557600080fd5b610e3e600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361153d565b600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550610eca600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205483611556565b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055503373ffffffffffffffffffffffffffffffffffffffff167f2cfce4af01bcb9d6cf6c84ee1b7c491100b8695368264146a94d71e10a63083f836040518082815260200191505060405180910390a260019050919050565b60056020528060005260406000206000915090505481565b600460009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b60018054600181600116156101000203166002900480601f0160208091040260200160405190810160405280929190818152602001828054600181600116156101000203166002900480156110385780601f1061100d57610100808354040283529160200191611038565b820191906000526020600020905b81548152906001019060200180831161101b57829003601f168201915b505050505081565b600073ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff161415151561107c57600080fd5b60008111151561108b57600080fd5b80600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054101515156110d957600080fd5b600560008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205481600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054011015151561116857600080fd5b6111b1600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548261153d565b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555061123d600560008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482611556565b600560008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055508173ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef836040518082815260200191505060405180910390a35050565b60066020528060005260406000206000915090505481565b600081600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020541015151561135157600080fd5b60008211151561136057600080fd5b6113a9600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361153d565b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550611435600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205483611556565b600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055503373ffffffffffffffffffffffffffffffffffffffff167ff97a274face0b5517365ad396b1fdba6f68bd3135ef603e44272adba3af5a1e0836040518082815260200191505060405180910390a260019050919050565b6007602052816000526040600020602052806000526040600020600091509150505481565b6000600560008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020549050919050565b600061154b83831115611580565b818303905092915050565b60008082840190506115768482101580156115715750838210155b611580565b8091505092915050565b80151561158c57600080fd5b505600a165627a7a723058201af50914efd1875e54c0660a90a679c61a604f98b248ab4c99df466a17bb1c2b0029")))

    println("|---------------------->")
    val result1 = VMTest.call(dataBase, author, contract, result.getHReturn, abi.encode(s"transfer('${caller.address}', 0x10)"), 0, 2977128)
    assert(success(result1))
    println(s"|----------------------> gas used: ${result1.getGasUsed}")

    val result2 = VMTest.call(dataBase, author, contract, result.getHReturn, abi.encode(s"balanceOf('${caller.address}')"))
    assert(success(result2))
    assert(DataWord.of(result2.getHReturn).value == 0x10)
  }

  @Test
  def testCREATE = {
    val _ =
      "contract A {\n" +
        "    constructor() payable public {}\n" +
        "}\n" +
        "\n" +
        "contract B {\n" +
        "    function createA() public returns (A) {\n" +
        "        return (new A).value(1)();\n" +
        "    }\n" +
        "}"

    dataBase.addBalance(author, 1000)
    dataBase.addBalance(caller, 1000)
    println(s"author balance: ${dataBase.getBalance(author).get}")
    println(s"caller balance: ${dataBase.getBalance(caller).get}")
    println("-----------------------------------------")

    val (contractB, result) = deploy(dataBase, author, BinaryData("608060405261013f806100136000396000f3fe6080604052600436106039576000357c01000000000000000000000000000000000000000000000000000000009004806355e4647614603e575b600080fd5b348015604957600080fd5b5060506092565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b60006001609c60be565b6040518091039082f08015801560b6573d6000803e3d6000fd5b509050905090565b6040516046806100ce8339019056fe608060405260358060116000396000f3fe6080604052600080fdfea165627a7a7230582009670f2c71915aea1f9f4b413ef3f3a9e8a2e5705c7ee12447e6d79f530887900029a165627a7a72305820defaf630cfde09f05e55e329daf74aee9905bfda54844b60b6b4fd142db7f2300029"), 2)
    assert(success(result))
    assert(result.getHReturn.sameElements(BinaryData("6080604052600436106039576000357c01000000000000000000000000000000000000000000000000000000009004806355e4647614603e575b600080fd5b348015604957600080fd5b5060506092565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b60006001609c60be565b6040518091039082f08015801560b6573d6000803e3d6000fd5b509050905090565b6040516046806100ce8339019056fe608060405260358060116000396000f3fe6080604052600080fdfea165627a7a7230582009670f2c71915aea1f9f4b413ef3f3a9e8a2e5705c7ee12447e6d79f530887900029a165627a7a72305820defaf630cfde09f05e55e329daf74aee9905bfda54844b60b6b4fd142db7f2300029")))
    val callResult = call(dataBase, caller, contractB, result.getHReturn, Crypto.sha3("createA()".getBytes).take(4), 0, 1000000)
    assert(success(callResult))
    val contractA = DataWord.of(callResult.getHReturn).toUInt160
    println(s"gas used: ${callResult.getGasUsed}")
    println(s"author balance: ${dataBase.getBalance(author).get}")
    println(s"caller balance: ${dataBase.getBalance(caller).get}")
    println(s"contractB balance: ${dataBase.getBalance(contractB).get}")
    println(s"contractA balance: ${dataBase.getBalance(contractB).get}")
    println(s"author nonce: ${dataBase.getNonce(author)}")
    println(s"caller nonce: ${dataBase.getNonce(caller)}")
    println(s"contractB nonce: ${dataBase.getNonce(contractB)}")
    println(s"contractA nonce: ${dataBase.getNonce(contractA)}")
    val callResult2 = call(dataBase, caller, contractB, result.getHReturn, Crypto.sha3("createA()".getBytes).take(4), 0, 1000000)
    assert(success(callResult2))
    val contractA2 = DataWord.of(callResult2.getHReturn).toUInt160
    println("------------------------------------------")
    println(s"gas used: ${callResult.getGasUsed}")
    println(s"author balance: ${dataBase.getBalance(author).get}")
    println(s"caller balance: ${dataBase.getBalance(caller).get}")
    println(s"contractB balance: ${dataBase.getBalance(contractB).get}")
    println(s"contractA balance: ${dataBase.getBalance(contractA).get}")
    println(s"contractA2 balance: ${dataBase.getBalance(contractA2).get}")
    println(s"author nonce: ${dataBase.getNonce(author)}")
    println(s"caller nonce: ${dataBase.getNonce(caller)}")
    println(s"contractB nonce: ${dataBase.getNonce(contractB)}")
    println(s"contractA nonce: ${dataBase.getNonce(contractA)}")
    println(s"contractA2 nonce: ${dataBase.getNonce(contractA2)}")
  }

  private def separate() = {
    println()
    println("****************")
    println()
  }

  private var dataBase: DataBase = null

  @Before
  def populate(): Unit = {
    dataBase = new DataBase(settings, _consensusSettings)
  }

  @After
  def clearUp(): Unit = {
    try {
      dataBase.close()
      Directory(dir).deleteRecursively()
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }

  @Test
  def testvm1: Unit = {
    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"x\",\"type\":\"uint256\"}],\"name\":\"set\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"get\",\"outputs\":[{\"name\":\"retVal\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"}]")

    val code = "60806040526001600460006101000a81548160ff02191690831515021790555034801561002b57600080fd5b506101588061003b6000396000f30060806040526004361061004c576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806360fe47b1146100515780636d4ce63c1461007e575b600080fd5b34801561005d57600080fd5b5061007c600480360381019080803590602001909291905050506100a9565b005b34801561008a57600080fd5b50610093610123565b6040518082815260200191505060405180910390f35b6001810160008190555060018103600181905550600181026002819055506001818115156100d357fe5b046003819055506003811080156100ea5750600681115b156101205760011515600460009054906101000a900460ff161515141561011857600060038190555061011f565b6001810390505b5b50565b600080549050905600a165627a7a72305820f6d79bc2d5ec3849bdb0459bba238a89602730e37022c881b385493704ed63050029"
    val call = abi.encode("get()")
    val (deployGasUsed1, callGasUsed1) = deployThenCall(code, call, true)
    val (deployGasUsed2, callGasUsed2) = deployThenCall(code, call, false)
    println(s"deploy: $deployGasUsed1 $deployGasUsed2")
    println(s"call: $callGasUsed1 $callGasUsed2")
    assert(deployGasUsed1 == deployGasUsed2)
    assert(callGasUsed1 == callGasUsed2)
  }

  @Test
  def testvm2: Unit = {

    val _ =
      "contract SimpleStorage {\n\n    function sdiv() returns (int) {\n        int a =10;\n        int b = 2;\n        \n       return a/b;\n    }\n    \n}"
    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[],\"name\":\"sdiv\",\"outputs\":[{\"name\":\"\",\"type\":\"int256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]")

    val code = "608060405234801561001057600080fd5b5060b78061001f6000396000f300608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063d5cfd270146044575b600080fd5b348015604f57600080fd5b506056606c565b6040518082815260200191505060405180910390f35b6000806000600a9150600290508082811515608357fe5b0592505050905600a165627a7a72305820f5175c5663211063e5b45e52b81e281701450e24afb798e65c1017aa8272d65a0029"

    val call1 = abi.encode("sdiv()")
    val (deployGasUsed3, callGasUsed3) = deployThenCall(code, call1, true)
    val (deployGasUsed4, callGasUsed4) = deployThenCall(code, call1, false)
    println(s"deploy: $deployGasUsed3 $deployGasUsed4")
    println(s"call: $callGasUsed3 $callGasUsed4")
    assert(deployGasUsed3 == deployGasUsed4)
    assert(callGasUsed3 == callGasUsed4)
  }


  @Test
  def testvm3: Unit = {

    val _ =
      "contract SimpleStorage {\n\n    function addmodtest(uint a, uint b, uint c)  returns (uint){\n      uint ret= addmod(a,b,c);\n      return ret;\n    }\n    \n}"

    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"a\",\"type\":\"uint256\"},{\"name\":\"b\",\"type\":\"uint256\"},{\"name\":\"c\",\"type\":\"uint256\"}],\"name\":\"addmodtest\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]")
    val code = "608060405234801561001057600080fd5b5060de8061001f6000396000f300608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680634a2ea12e146044575b600080fd5b348015604f57600080fd5b5060806004803603810190808035906020019092919080359060200190929190803590602001909291905050506096565b6040518082815260200191505060405180910390f35b6000808280151560a257fe5b84860890508091505093925050505600a165627a7a723058200e5fbe3002883d55df8ec42b7767068430029c68cb2bed74f243b472ab04e9d40029"

    val call2 = abi.encode("addmodtest(1,1)")
    val (deployGasUsed5, callGasUsed5) = deployThenCall(code, call2, true)
    val (deployGasUsed6, callGasUsed6) = deployThenCall(code, call2, false)
    println(s"deploy: $deployGasUsed5 $deployGasUsed6")
    println(s"call: $callGasUsed5 $callGasUsed6")
    assert(deployGasUsed5 == deployGasUsed6)
    assert(callGasUsed5 == callGasUsed6)

  }


  @Test
  def testvm4: Unit = {

    val _ =
      "contract SimpleStorage {\n\n    function mulmodtest(uint a, uint b, uint c)  returns (uint){\n      uint ret= mulmod(a,b,c);\n      \n      return ret;\n    }\n    \n}"

    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"a\",\"type\":\"uint256\"},{\"name\":\"b\",\"type\":\"uint256\"},{\"name\":\"c\",\"type\":\"uint256\"}],\"name\":\"mulmodtest\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]")

    val code = "608060405234801561001057600080fd5b5060de8061001f6000396000f300608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063edddcf2a146044575b600080fd5b348015604f57600080fd5b5060806004803603810190808035906020019092919080359060200190929190803590602001909291905050506096565b6040518082815260200191505060405180910390f35b6000808280151560a257fe5b84860990508091505093925050505600a165627a7a7230582053bf8d479863e2252f85416b7767b53942acc4202e51ac9b58ffa6665f4195350029"

    val call3 = abi.encode("mulmodtest(2,2)")
    val (deployGasUsed7, callGasUsed7) = deployThenCall(code, call3, true)
    val (deployGasUsed8, callGasUsed8) = deployThenCall(code, call3, false)
    println(s"deploy: $deployGasUsed7 $deployGasUsed8")
    println(s"call: $callGasUsed7 $callGasUsed8")
    assert(deployGasUsed7 == deployGasUsed8)
    assert(callGasUsed7 == callGasUsed8)
  }


  @Test
  def testvm5: Unit = {

    val _ =
      "contract SimpleStorage {\n\n    function modtest(uint a, uint b) returns (uint) {\n       return a%b;\n    }\n    \n}"

    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"a\",\"type\":\"uint256\"},{\"name\":\"b\",\"type\":\"uint256\"}],\"name\":\"modtest\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]")

    val code = "608060405234801561001057600080fd5b5060cd8061001f6000396000f300608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063607dcc1f146044575b600080fd5b348015604f57600080fd5b5060766004803603810190808035906020019092919080359060200190929190505050608c565b6040518082815260200191505060405180910390f35b60008183811515609857fe5b069050929150505600a165627a7a723058200c04baf167cea4a07df8df1952073357aa4de0ee83a13a8ae56c83b7608a0b280029"
    val call = abi.encode("modtest(2,1)")
    val (deployGasUsed1, callGasUsed1) = deployThenCall(code, call, true)
    val (deployGasUsed2, callGasUsed2) = deployThenCall(code, call, false)
    println(s"deploy: $deployGasUsed1 $deployGasUsed2")
    println(s"call: $callGasUsed1 $callGasUsed2")
    assert(deployGasUsed1 == deployGasUsed2)
    assert(callGasUsed1 == callGasUsed2)

  }

  @Test
  def testvm6: Unit = {

    val _ =
      "contract SimpleStorage {\n\n\tfunction sltgt(){\n        int a=10;\n        int b=5;\n        \n        assert(a>b);\n        assert(b<a);\n        \n        int c=a^b;\n    }\n    \n}0"

    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[],\"name\":\"sltgt\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]")
    val code = "608060405234801561001057600080fd5b5060af8061001f6000396000f300608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063f2d91475146044575b600080fd5b348015604f57600080fd5b5060566058565b005b6000806000600a9250600591508183131515606f57fe5b8282121515607957fe5b81831890505050505600a165627a7a723058208a53cebbcce0bda52e6c247af9a05e0efa7bbbe96db3345c808a84b405941e770029"

    val call3 = abi.encode("sltgt()")
    val (deployGasUsed7, callGasUsed7) = deployThenCall(code, call3, true)
    val (deployGasUsed8, callGasUsed8) = deployThenCall(code, call3, false)
    println(s"deploy: $deployGasUsed7 $deployGasUsed8")
    println(s"call: $callGasUsed7 $callGasUsed8")
    assert(deployGasUsed7 == deployGasUsed8)
    assert(callGasUsed7 == callGasUsed8)
  }

  @Test
  def testvm7: Unit = {
    val _ =
      "contract SimpleStorage {\n\n\tfunction memsore8test(uint v){\n        \n        assembly {\n       mstore8(0x80, v)\n        }\n    }\n    \n}"

    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"v\",\"type\":\"uint256\"}],\"name\":\"memsore8test\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]")
    val code = "6080604052348015600f57600080fd5b5060a18061001e6000396000f300608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063985abd49146044575b600080fd5b348015604f57600080fd5b50606c60048036038101908080359060200190929190505050606e565b005b80608053505600a165627a7a72305820f97c723b11eb7c59be2051142bf2ab2a1c6cf0b2dbdb3530c51b19cdd38f24500029"

    val call3 = abi.encode("memsore8test(8)")
    val (deployGasUsed7, callGasUsed7) = deployThenCall(code, call3, true)
    val (deployGasUsed8, callGasUsed8) = deployThenCall(code, call3, false)
    println(s"deploy: $deployGasUsed7 $deployGasUsed8")
    println(s"call: $callGasUsed7 $callGasUsed8")
    assert(deployGasUsed7 == deployGasUsed8)
    assert(callGasUsed7 == callGasUsed8)
  }

  @Test
  def testvm8: Unit = {
    val _ =
      "contract Purchase {\n\tfunction getBalance() constant public returns(uint){\n\t\treturn this.balance;\n\t} \n}"

    val abi = Abi.fromJson("[{\"constant\":true,\"inputs\":[],\"name\":\"getBalance\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"}]")
    val code = "6080604052348015600f57600080fd5b5060b78061001e6000396000f300608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806312065fe0146044575b600080fd5b348015604f57600080fd5b506056606c565b6040518082815260200191505060405180910390f35b60003073ffffffffffffffffffffffffffffffffffffffff16319050905600a165627a7a7230582086ab234bf457d6cac98fce110e5688da12a0ea23b98e67d9d4227592791fd9200029"

    val call3 = abi.encode("getBalance()")
    val (deployGasUsed7, callGasUsed7) = deployThenCall(code, call3, true)
    val (deployGasUsed8, callGasUsed8) = deployThenCall(code, call3, false)
    println(s"deploy: $deployGasUsed7 $deployGasUsed8")
    println(s"call: $callGasUsed7 $callGasUsed8")
    assert(deployGasUsed7 == deployGasUsed8)
    assert(callGasUsed7 == callGasUsed8)
  }

  @Test
  def testvm9: Unit = {
    val _ =
      "contract Purchase {\n\t function create2_test(uint x) public view returns(uint) {\n        assembly{\n            \n            let v := 100\n            let g := add(v, 2)\n            \n            create2(v, g, g, v)\n            \n            return v\n        }\n        \n    } \n}"

    val abi = Abi.fromJson("[{\"constant\":true,\"inputs\":[{\"name\":\"x\",\"type\":\"uint256\"}],\"name\":\"create2_test\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"}]")
    val code = "608060405234801561001057600080fd5b5060bd8061001f6000396000f300608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063fa440fa8146044575b600080fd5b348015604f57600080fd5b50606c600480360381019080803590602001909291905050506082565b6040518082815260200191505060405180910390f35b600060646002810181818284fbf300a165627a7a72305820b9c57f5053064c64ead062f172a9970f1fbf09c65ba36089dd0c1d2f595c80390029"

    val call3 = abi.encode("create2_test(1)")
    val (deployGasUsed7, callGasUsed7) = deployThenCall(code, call3, true)
    val (deployGasUsed8, callGasUsed8) = deployThenCall(code, call3, false)
    println(s"deploy: $deployGasUsed7 $deployGasUsed8")
    println(s"call: $callGasUsed7 $callGasUsed8")
    assert(deployGasUsed7 == deployGasUsed8)
    assert(callGasUsed7 == callGasUsed8)
  }

  @Test
  def testvm10: Unit = {
    val _ =
      "contract C{\n    uint public c = 10;\n}\n \ncontract D{\n    C c = new C();\n    \n    function getDataUsingAccessor() returns (uint){\n        return c.c();\n    }\n}"

    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[],\"name\":\"getDataUsingAccessor\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]")
    val code = "608060405261000c61007a565b604051809103906000f080158015610028573d6000803e3d6000fd5b506000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff16021790555034801561007457600080fd5b50610089565b60405160c1806101fc83390190565b610164806100986000396000f300608060405260043610610041576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680632616ee4d14610046575b600080fd5b34801561005257600080fd5b5061005b610071565b6040518082815260200191505060405180910390f35b60008060009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1663c3da42b86040518163ffffffff167c0100000000000000000000000000000000000000000000000000000000028152600401602060405180830381600087803b1580156100f857600080fd5b505af115801561010c573d6000803e3d6000fd5b505050506040513d602081101561012257600080fd5b81019080805190602001909291905050509050905600a165627a7a72305820349df8a541290f26d540ef62a913f0af77c62be23ed869c37799715e18a31fd800296080604052600a600055348015601457600080fd5b50609e806100236000396000f300608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063c3da42b8146044575b600080fd5b348015604f57600080fd5b506056606c565b6040518082815260200191505060405180910390f35b600054815600a165627a7a723058208ef790293eb50531eda90ad8dbae05de9cafc7d432a851b0f34f25f34ba94faf0029"

    //    val call3 = abi.encode("OwnedToken('test')")
    val call3 = abi.encode("getDataUsingAccessor()")
    val (deployGasUsed7, callGasUsed7) = deployThenCall(code, call3, true)
    val (deployGasUsed8, callGasUsed8) = deployThenCall(code, call3, false)
    println(s"deploy: $deployGasUsed7 $deployGasUsed8")
    println(s"call: $callGasUsed7 $callGasUsed8")
    assert(deployGasUsed7 == deployGasUsed8)
    assert(callGasUsed7 == callGasUsed8)
  }

  @Test
  def testvmcall: Unit = {
    val _ =
      "contract CallTest{\n    \n    function callByFun(address addr)returns (bool){\n        bytes4 methodId = bytes4(keccak256(\"increaseAge(string,uint256)\"));\n        return addr.call(methodId,\"jack\", 1);\n    }\n}"

    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"addr\",\"type\":\"address\"}],\"name\":\"callByFun\",\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]")

    val code = Array(
      "6080604052600a60005534801561001557600080fd5b50610150806100256000396000f30060806040526004361061004c576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063967e6e6514610051578063f9ea5e791461007c575b600080fd5b34801561005d57600080fd5b50610066610103565b6040518082815260200191505060405180910390f35b34801561008857600080fd5b506100ed600480360381019080803590602001908201803590602001908080601f01602080910402602001604051908101604052809392919081815260200183838082843782019150505050505091929192908035906020019092919050505061010c565b6040518082815260200191505060405180910390f35b60008054905090565b600080600081546001019190508190559050929150505600a165627a7a7230582071dbf0e4ebc31969873bec3dd6b8091f73515a3dbeb2751875eff8545c7d03df0029",
      "608060405234801561001057600080fd5b506101bf806100206000396000f300608060405260043610610041576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063d11f17ad14610046575b600080fd5b34801561005257600080fd5b50610087600480360381019080803573ffffffffffffffffffffffffffffffffffffffff1690602001909291905050506100a1565b604051808215151515815260200191505060405180910390f35b60008060405180807f696e63726561736541676528737472696e672c75696e74323536290000000000815250601b019050604051809103902090508273ffffffffffffffffffffffffffffffffffffffff16817c0100000000000000000000000000000000000000000000000000000000900460016040518263ffffffff167c010000000000000000000000000000000000000000000000000000000002815260040180807f6a61636b000000000000000000000000000000000000000000000000000000008152506020018260ff1681526020019150506000604051808303816000875af1925050509150509190505600a165627a7a72305820bc6435274015b42921833a005e13e317703c62d7bdeadf7af92c75d93ffdd2950029")


    //    val call3 = abi.encode("OwnedToken('test')")
    val call3 = abi.encode("callByFun('11cee5dd2b22ebd96a42e4c9318d6bf050c9b3b7')")
    val (deployGasUsed7, callGasUsed7) = deployThenCall(code, call3, true)
    val (deployGasUsed8, callGasUsed8) = deployThenCall(code, call3, false)
    println(s"deploy: $deployGasUsed7 $deployGasUsed8")
    println(s"call: $callGasUsed7 $callGasUsed8")
    assert(deployGasUsed7 == deployGasUsed8)
    assert(callGasUsed7 == callGasUsed8)
  }

  @Test
  def testvm11: Unit = {
    // 调用call
    val _ =
      "contract G {\n\tuint value;\n\tconstructor() payable public {\n\t\tvalue = msg.value;\n\t}\n\t\n\tfunction get(uint amount) payable public {\n\t\tmsg.sender.transfer(amount);\n\t}\n}"

    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"amount\",\"type\":\"uint256\"}],\"name\":\"get\",\"outputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"function\"},{\"inputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"constructor\"}]")
    val code = "60806040523460008190555060d7806100196000396000f300608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680639507d39a146044575b600080fd5b6060600480360381019080803590602001909291905050506062565b005b3373ffffffffffffffffffffffffffffffffffffffff166108fc829081150290604051600060405180830381858888f1935050505015801560a7573d6000803e3d6000fd5b50505600a165627a7a723058208a21277ee24fe49f11b52171582e8f62350b55b492567a166f26f8bef9dfa05f0029"

    val call3 = abi.encode("get(1)")
    val (deployGasUsed7, callGasUsed7) = deployThenCall(code, call3, true)
    val (deployGasUsed8, callGasUsed8) = deployThenCall(code, call3, false)
    println(s"deploy: $deployGasUsed7 $deployGasUsed8")
    println(s"call: $callGasUsed7 $callGasUsed8")
    assert(deployGasUsed7 == deployGasUsed8)
    assert(callGasUsed7 == callGasUsed8)
  }

  def test12: Unit = {
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
    val code = "608060405234801561001057600080fd5b50610169806100206000396000f3fe608060405260043610610046576000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11461004b5780636d4ce63c14610086575b600080fd5b34801561005757600080fd5b506100846004803603602081101561006e57600080fd5b81019080803590602001909291905050506100b1565b005b34801561009257600080fd5b5061009b6100f7565b6040518082815260200191505060405180910390f35b806000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555050565b60008060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205490509056fea165627a7a72305820b698ee72285f30209458db25efe0fe5632e08930f7659a0ac5208217d38d92500029"


    val call3 = abi.encode("set(0x777)")
    val (deployGasUsed7, callGasUsed7) = deployThenCall(code, call3, true)
    val (deployGasUsed8, callGasUsed8) = deployThenCall(code, call3, false)
    println(s"deploy: $deployGasUsed7 $deployGasUsed8")
    println(s"call: $callGasUsed7 $callGasUsed8")
    assert(deployGasUsed7 == deployGasUsed8)
    assert(callGasUsed7 == callGasUsed8)


    val call4 = abi.encode("get()")
    val (deployGasUsed1, callGasUsed1) = deployThenCall(code, call4, true)
    val (deployGasUsed2, callGasUsed2) = deployThenCall(code, call4, false)
    println(s"deploy: $deployGasUsed1 $deployGasUsed2")
    println(s"call: $callGasUsed1 $callGasUsed2")
    assert(deployGasUsed1 == deployGasUsed2)
    assert(callGasUsed1 == callGasUsed2)

  }

  @Test
  def test13: Unit = {
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
    val code = "608060405260043610610046576000357c0100000000000000000000000000000000000000000000000000000000900480638da5cb5b1461004b578063f2fde38b146100a2575b600080fd5b34801561005757600080fd5b506100606100f3565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b3480156100ae57600080fd5b506100f1600480360360208110156100c557600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610118565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614151561017357600080fd5b600073ffffffffffffffffffffffffffffffffffffffff168173ffffffffffffffffffffffffffffffffffffffff161415156101ea57806000806101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055505b5056fea165627a7a7230582013ec64e541b38e025ab583499d3764fcdcba9507625a990f73cb75ec6131a5ad0029"


    val call1 = abi.encode("owner")
    val (deployGasUsed1, callGasUsed1) = deployThenCall(code, call1, true)
    val (deployGasUsed2, callGasUsed2) = deployThenCall(code, call1, false)
    println(s"deploy: $deployGasUsed1 $deployGasUsed2")
    println(s"call: $callGasUsed1 $callGasUsed2")
    assert(deployGasUsed1 == deployGasUsed2)
    assert(callGasUsed1 == callGasUsed2)

    val call2 = abi.encode(s"transferOwnership('${caller.address}')")
    val (deployGasUsed3, callGasUsed3) = deployThenCall(code, call2, true)
    val (deployGasUsed4, callGasUsed4) = deployThenCall(code, call2, false)
    println(s"deploy: $deployGasUsed3 $deployGasUsed4")
    println(s"call: $callGasUsed3 $callGasUsed4")
    assert(deployGasUsed3 == deployGasUsed4)
    assert(callGasUsed3 == callGasUsed4)
  }

  @Test
  def testvm14: Unit = {
    val _ =
      "contract Purchase {\n\tfunction at(address _addr) public view returns (bytes memory o_code) {\n        assembly {\n            // retrieve the size of the code, this needs assembly\n            let size := extcodesize(_addr)\n            // allocate output byte array - this could also be done without assembly\n            // by using o_code = new bytes(size)\n            o_code := mload(0x40)\n            // new \"memory end\" including padding\n            mstore(0x40, add(o_code, and(add(add(size, 0x20), 0x1f), not(0x1f))))\n            // store length in memory\n            mstore(o_code, size)\n            // actually retrieve the code, this needs assembly\n            extcodecopy(_addr, add(o_code, 0x20), 0, size)\n\t\t\tcalldatacopy(add(o_code, 0x20), 0, size)\n        }\n    } \n}"

    val abi = Abi.fromJson("[{\"constant\":true,\"inputs\":[{\"name\":\"_addr\",\"type\":\"address\"}],\"name\":\"at\",\"outputs\":[{\"name\":\"o_code\",\"type\":\"bytes\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"}]")
    val code = "608060405234801561001057600080fd5b50610161806100206000396000f300608060405260043610610041576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063dce4a44714610046575b600080fd5b34801561005257600080fd5b50610087600480360381019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610102565b6040518080602001828103825283818151815260200191508051906020019080838360005b838110156100c75780820151818401526020810190506100ac565b50505050905090810190601f1680156100f45780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b6060813b6040519150601f19601f602083010116820160405280825280600060208401853c8060006020840137509190505600a165627a7a72305820637cbe6fa56b5aa193d537f7b695fe7fd8b773c71da1796fbcc62d4c1588efff0029"

    val call3 = abi.encode("at('APHmn5xqWTw7fmQ1TZZsMkt2rLYk5uuJNRg')")
    val (deployGasUsed7, callGasUsed7) = deployThenCall(code, call3, true)
    val (deployGasUsed8, callGasUsed8) = deployThenCall(code, call3, false)
    println(s"deploy: $deployGasUsed7 $deployGasUsed8")
    println(s"call: $callGasUsed7 $callGasUsed8")
    assert(deployGasUsed7 == deployGasUsed8)
    assert(callGasUsed7 == callGasUsed8)
  }

  @Test
  def test15 = {
    val _ =
      "contract SuicideTest {\n\n\tfunction destroy(address _addr) public {\n\t\tselfdestruct(_addr);\n\t}\n}"

    // author deploy a contract and send it 500
    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"_addr\",\"type\":\"address\"}],\"name\":\"destroy\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]")
    val code = "608060405234801561001057600080fd5b5060c88061001f6000396000f300608060405260043610603e576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168062f55d9d146043575b600080fd5b348015604e57600080fd5b506081600480360381019080803573ffffffffffffffffffffffffffffffffffffffff1690602001909291905050506083565b005b8073ffffffffffffffffffffffffffffffffffffffff16ff00a165627a7a723058208a4aff98295cf0b0c8782247310e4194905afe3f40213c93afeafb284b05584f0029"

    val call1 = abi.encode("destroy('11cee5dd2b22ebd96a42e4c9318d6bf050c9b3b7')")
    val (deployGasUsed1, callGasUsed1) = deployThenCall(code, call1, true)
    val (deployGasUsed2, callGasUsed2) = deployThenCall(code, call1, false)
    println(s"deploy: $deployGasUsed1 $deployGasUsed2")
    println(s"call: $callGasUsed1 $callGasUsed2")
    assert(deployGasUsed1 == deployGasUsed2)
    assert(callGasUsed1 == callGasUsed2)

  }

  @Test
  def test16 = {
    // 执行create2
    val _ =
      "contract Factory {\n\n    function createHolder(address _owner) {\n        \n        address h;\n        assembly{\n            h := create2(0, 0, 32, _owner)\n        }\n    }\n}"

    // author deploy a contract and send it 500
    val abi = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"_owner\",\"type\":\"address\"}],\"name\":\"createHolder\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]")
    val code = "608060405234801561001057600080fd5b5060bf8061001f6000396000f300608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806351e8054c146044575b600080fd5b348015604f57600080fd5b506082600480360381019080803573ffffffffffffffffffffffffffffffffffffffff1690602001909291905050506084565b005b6000816020600080fb905050505600a165627a7a723058202095d0114e0e839a0e8c92f5ac449a4bd70450e01493fed4c94028c276bde78f0029"

    val call1 = abi.encode("createHolder('11cee5dd2b22ebd96a42e4c9318d6bf050c9b3b7')")
    val (deployGasUsed1, callGasUsed1) = deployThenCall(code, call1, true)
    val (deployGasUsed2, callGasUsed2) = deployThenCall(code, call1, false)
    println(s"deploy: $deployGasUsed1 $deployGasUsed2")
    println(s"call: $callGasUsed1 $callGasUsed2")
    assert(deployGasUsed1 == deployGasUsed2)
    assert(callGasUsed1 == callGasUsed2)

  }

  @Test
  def test17: Unit = {
    val (_, result) = deploy(dataBase, author, BinaryData("6080604052600a6003553480156200001657600080fd5b5060405162004ff738038062004ff7833981018060405260c08110156200003c57600080fd5b81516020830151604084015160608501805193959294919391830192916401000000008111156200006c57600080fd5b820160208101848111156200008057600080fd5b81516401000000008111828201871017156200009b57600080fd5b50509291906020018051640100000000811115620000b857600080fd5b82016020810184811115620000cc57600080fd5b8151640100000000811182820187101715620000e757600080fd5b50506020909101516001600090815560048054600160a060020a0319163317905560088790559193509150869086908690869086908690841162000177576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252603081526020018062004fc76030913960400191505060405180910390fd5b60006200018d87640100000000620002d0810204565b90508015620001fd57604080517f08c379a000000000000000000000000000000000000000000000000000000000815260206004820152601a60248201527f53657474696e6720636f6d7074726f6c6c6572206661696c6564000000000000604482015290519081900360640190fd5b6200021064010000000062000468810204565b600a55670de0b6b3a7640000600b5562000233866401000000006200046d810204565b905080156200028e576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252602281526020018062004fa56022913960400191505060405180910390fd5b8351620002a3906001906020870190620006b6565b508251620002b9906002906020860190620006b6565b505060035550620007589950505050505050505050565b600454600090600160a060020a031633146200030557620002fd6001603e64010000000062000646810204565b905062000463565b600654604080517e7e3dd20000000000000000000000000000000000000000000000000000000081529051600160a060020a0392831692851691627e3dd2916004808301926020929190829003018186803b1580156200036457600080fd5b505afa15801562000379573d6000803e3d6000fd5b505050506040513d60208110156200039057600080fd5b505115156200040057604080517f08c379a000000000000000000000000000000000000000000000000000000000815260206004820152601c60248201527f6d61726b6572206d6574686f642072657475726e65642066616c736500000000604482015290519081900360640190fd5b60068054600160a060020a031916600160a060020a03858116918217909255604080519284168352602083019190915280517f7ac369dbd14fa5ea3f473ed67cc9d598964a77501540ba6751eb0b3decf5870d9281900390910190a160005b9150505b919050565b435b90565b6004546000908190600160a060020a03163314620004a5576200049c6001604164010000000062000646810204565b91505062000463565b620004b864010000000062000468810204565b600a5414620004d8576200049c600a604064010000000062000646810204565b600760009054906101000a9004600160a060020a0316905082600160a060020a0316632191f92a6040518163ffffffff167c010000000000000000000000000000000000000000000000000000000002815260040160206040518083038186803b1580156200054657600080fd5b505afa1580156200055b573d6000803e3d6000fd5b505050506040513d60208110156200057257600080fd5b50511515620005e257604080517f08c379a000000000000000000000000000000000000000000000000000000000815260206004820152601c60248201527f6d61726b6572206d6574686f642072657475726e65642066616c736500000000604482015290519081900360640190fd5b60078054600160a060020a031916600160a060020a03858116918217909255604080519284168352602083019190915280517fedffc32e068c7c95dfd4bdfd5c4d939a084d6b11c4199eac8436ed234d72f9269281900390910190a160006200045f565b60007f45b96fe442630264581b197e84bbada861235052c5a1aadfff9ea4e40a969aa08360108111156200067657fe5b83604c8111156200068357fe5b604080519283526020830191909152600082820152519081900360600190a1826010811115620006af57fe5b9392505050565b828054600181600116156101000203166002900490600052602060002090601f016020900481019282601f10620006f957805160ff191683800117855562000729565b8280016001018555821562000729579182015b82811115620007295782518255916020019190600101906200070c565b50620007379291506200073b565b5090565b6200046a91905b8082111562000737576000815560010162000742565b61483d80620007686000396000f3fe60806040526004361061026b5760003560e060020a9004806395d89b4111610148578063d40e8f4a116100ba578063f2b3abbd1161007e578063f2b3abbd146108e4578063f3fdb15a14610917578063f851a4401461092c578063f8f9da2814610941578063fca7820b14610956578063fe9c44ae146109805761026b565b8063d40e8f4a146107f8578063db006a7514610844578063dd62ed3e1461086e578063e5974619146108a9578063e9c714f2146108cf5761026b565b8063aae40a2a1161010c578063aae40a2a146106bc578063b2a02ff1146106ea578063b71d1a0c1461072d578063bd6d894d14610760578063c37f68e214610775578063c5ebeaec146107ce5761026b565b806395d89b411461061157806395dd919314610626578063a6afed9514610659578063a9059cbb1461066e578063aa5af0fd146106a75761026b565b80634576b5db116101e1578063675d972c116101a5578063675d972c146105605780636c540baf1461057557806370a082311461058a57806373acee98146105bd578063852a12e3146105d25780638f840ddd146105fc5761026b565b80634576b5db146104d157806347bd3718146105045780634e4d9fea146105195780635fe3b56714610521578063601a0bf1146105365761026b565b806318160ddd1161023357806318160ddd146103eb578063182df0f51461040057806323b872dd146104155780632678224714610458578063313ce567146104895780633af9e6691461049e5761026b565b806306fdde03146102b2578063095ea7b31461033c5780631249c58b14610389578063173b99041461039157806317bfdfbc146103b8575b6102b061027734610995565b60408051808201909152600b81527f6d696e74206661696c65640000000000000000000000000000000000000000006020820152610a29565b005b3480156102be57600080fd5b506102c7610ca4565b6040805160208082528351818301528351919283929083019185019080838360005b838110156103015781810151838201526020016102e9565b50505050905090810190601f16801561032e5780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b34801561034857600080fd5b506103756004803603604081101561035f57600080fd5b50600160a060020a038135169060200135610d31565b604080519115158252519081900360200190f35b6102b0610d9e565b34801561039d57600080fd5b506103a6610dac565b60408051918252519081900360200190f35b3480156103c457600080fd5b506103a6600480360360208110156103db57600080fd5b5035600160a060020a0316610db2565b3480156103f757600080fd5b506103a6610e6c565b34801561040c57600080fd5b506103a6610e72565b34801561042157600080fd5b506103756004803603606081101561043857600080fd5b50600160a060020a03813581169160208101359091169060400135610ed8565b34801561046457600080fd5b5061046d610f42565b60408051600160a060020a039092168252519081900360200190f35b34801561049557600080fd5b506103a6610f51565b3480156104aa57600080fd5b506103a6600480360360208110156104c157600080fd5b5035600160a060020a0316610f57565b3480156104dd57600080fd5b506103a6600480360360208110156104f457600080fd5b5035600160a060020a0316610fc8565b34801561051057600080fd5b506103a6611143565b6102b0611149565b34801561052d57600080fd5b5061046d61118e565b34801561054257600080fd5b506103a66004803603602081101561055957600080fd5b503561119d565b34801561056c57600080fd5b506103a66111d7565b34801561058157600080fd5b506103a66111dd565b34801561059657600080fd5b506103a6600480360360208110156105ad57600080fd5b5035600160a060020a03166111e3565b3480156105c957600080fd5b506103a66111fe565b3480156105de57600080fd5b506103a6600480360360208110156105f557600080fd5b50356112b6565b34801561060857600080fd5b506103a66112c1565b34801561061d57600080fd5b506102c76112c7565b34801561063257600080fd5b506103a66004803603602081101561064957600080fd5b5035600160a060020a031661131f565b34801561066557600080fd5b506103a661137f565b34801561067a57600080fd5b506103756004803603604081101561069157600080fd5b50600160a060020a038135169060200135611730565b3480156106b357600080fd5b506103a6611799565b6102b0600480360360408110156106d257600080fd5b50600160a060020a038135811691602001351661179f565b3480156106f657600080fd5b506103a66004803603606081101561070d57600080fd5b50600160a060020a038135811691602081013590911690604001356117e6565b34801561073957600080fd5b506103a66004803603602081101561075057600080fd5b5035600160a060020a0316611acc565b34801561076c57600080fd5b506103a6611b60565b34801561078157600080fd5b506107a86004803603602081101561079857600080fd5b5035600160a060020a0316611c19565b604080519485526020850193909352838301919091526060830152519081900360800190f35b3480156107da57600080fd5b506103a6600480360360208110156107f157600080fd5b5035611cae565b34801561080457600080fd5b5061082b6004803603602081101561081b57600080fd5b5035600160a060020a0316611cb9565b6040805192835260208301919091528051918290030190f35b34801561085057600080fd5b506103a66004803603602081101561086757600080fd5b5035611cd2565b34801561087a57600080fd5b506103a66004803603604081101561089157600080fd5b50600160a060020a0381358116916020013516611cdd565b6102b0600480360360208110156108bf57600080fd5b5035600160a060020a0316611d08565b3480156108db57600080fd5b506103a6611d51565b3480156108f057600080fd5b506103a66004803603602081101561090757600080fd5b5035600160a060020a0316611e4d565b34801561092357600080fd5b5061046d611e87565b34801561093857600080fd5b5061046d611e96565b34801561094d57600080fd5b506103a6611ea5565b34801561096257600080fd5b506103a66004803603602081101561097957600080fd5b5035611f87565b34801561098c57600080fd5b50610375611fc1565b60008054600101808255816109a861137f565b905080156109ce576109c68160108111156109bf57fe5b601d611fc6565b9250506109dc565b6109d8338561202c565b9250505b6000548114610a23576040805160e560020a62461bcd02815260206004820152600a60248201526000805160206146d7833981519152604482015290519081900360640190fd5b50919050565b811515610a3557610ca0565b606081516005016040519080825280601f01601f191660200182016040528015610a66576020820181803883390190505b50905060005b8251811015610ac4578281815181101515610a8357fe5b90602001015160f860020a900460f860020a028282815181101515610aa457fe5b906020010190600160f860020a031916908160001a905350600101610a6c565b81517f200000000000000000000000000000000000000000000000000000000000000090839083908110610af457fe5b906020010190600160f860020a031916908160001a90535081517f280000000000000000000000000000000000000000000000000000000000000090839060018401908110610b3f57fe5b906020010190600160f860020a031916908160001a905350600a840460300160f860020a028282600201815181101515610b7557fe5b906020010190600160f860020a031916908160001a905350600a840660300160f860020a028282600301815181101515610bab57fe5b906020010190600160f860020a031916908160001a90535081517f290000000000000000000000000000000000000000000000000000000000000090839060048401908110610bf657fe5b906020010190600160f860020a031916908160001a905350818415610c9c5760405160e560020a62461bcd0281526004018080602001828103825283818151815260200191508051906020019080838360005b83811015610c61578181015183820152602001610c49565b50505050905090810190601f168015610c8e5780820380516001836020036101000a031916815260200191505b509250505060405180910390fd5b5050505b5050565b60018054604080516020600284861615610100026000190190941693909304601f81018490048402820184019092528181529291830182828015610d295780601f10610cfe57610100808354040283529160200191610d29565b820191906000526020600020905b815481529060010190602001808311610d0c57829003601f168201915b505050505081565b336000818152601060209081526040808320600160a060020a03871680855290835281842086905581518681529151939493909284927f8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925929081900390910190a360019150505b92915050565b610daa61027734610995565b565b60095481565b6000805460010180825581610dc561137f565b14610e1a576040805160e560020a62461bcd02815260206004820152601660248201527f61636372756520696e746572657374206661696c656400000000000000000000604482015290519081900360640190fd5b610e238361131f565b91506000548114610a23576040805160e560020a62461bcd02815260206004820152600a60248201526000805160206146d7833981519152604482015290519081900360640190fd5b600e5481565b6000806000610e7f6124a6565b90925090506000826003811115610e9257fe5b14610ed15760405160e560020a62461bcd0281526004018080602001828103825260358152602001806147856035913960400191505060405180910390fd5b9150505b90565b6000805460010180825581610eef33878787612554565b1491505b6000548114610f3a576040805160e560020a62461bcd02815260206004820152600a60248201526000805160206146d7833981519152604482015290519081900360640190fd5b509392505050565b600554600160a060020a031681565b60035481565b6000610f616145bf565b602060405190810160405280610f75611b60565b9052600160a060020a0384166000908152600f6020526040812054919250908190610fa19084906127f2565b90925090506000826003811115610fb457fe5b14610fbe57600080fd5b925050505b919050565b600454600090600160a060020a03163314610ff057610fe96001603e611fc6565b9050610fc3565b600654604080517e7e3dd20000000000000000000000000000000000000000000000000000000081529051600160a060020a0392831692851691627e3dd2916004808301926020929190829003018186803b15801561104e57600080fd5b505afa158015611062573d6000803e3d6000fd5b505050506040513d602081101561107857600080fd5b505115156110d0576040805160e560020a62461bcd02815260206004820152601c60248201527f6d61726b6572206d6574686f642072657475726e65642066616c736500000000604482015290519081900360640190fd5b6006805473ffffffffffffffffffffffffffffffffffffffff1916600160a060020a03858116918217909255604080519284168352602083019190915280517f7ac369dbd14fa5ea3f473ed67cc9d598964a77501540ba6751eb0b3decf5870d9281900390910190a160005b9392505050565b600c5481565b610daa61115534612846565b60408051808201909152601281527f7265706179426f72726f77206661696c656400000000000000000000000000006020820152610a29565b600654600160a060020a031681565b60008054600101808255816111b061137f565b905080156111ce576109c68160108111156111c757fe5b602f611fc6565b6109d884612882565b60085481565b600a5481565b600160a060020a03166000908152600f602052604090205490565b600080546001018082558161121161137f565b14611266576040805160e560020a62461bcd02815260206004820152601660248201527f61636372756520696e746572657374206661696c656400000000000000000000604482015290519081900360640190fd5b600c54915060005481146112b2576040805160e560020a62461bcd02815260206004820152600a60248201526000805160206146d7833981519152604482015290519081900360640190fd5b5090565b6000610d9882612a06565b600d5481565b6002805460408051602060018416156101000260001901909316849004601f81018490048402820184019092528181529291830182828015610d295780601f10610cfe57610100808354040283529160200191610d29565b600080600061132d84612a43565b9092509050600082600381111561134057fe5b1461113c5760405160e560020a62461bcd0281526004018080602001828103825260378152602001806146f76037913960400191505060405180910390fd5b60006113896145d1565b600754600160a060020a03166315f240536113a2612af9565b600c54600d546040518463ffffffff1660e060020a028152600401808481526020018381526020018281526020019350505050604080518083038186803b1580156113ec57600080fd5b505afa158015611400573d6000803e3d6000fd5b505050506040513d604081101561141657600080fd5b50805160209182015160408401819052918301526601c6bf526340001015611488576040805160e560020a62461bcd02815260206004820152601c60248201527f626f72726f772072617465206973206162737572646c79206869676800000000604482015290519081900360640190fd5b6020810151156114ab576114a3600560028360200151612b25565b915050610ed5565b6114b3612b8b565b60608201819052600a546114c79190612b8f565b60808301819052828260038111156114db57fe5b60038111156114e657fe5b90525060009050815160038111156114fa57fe5b1461150157fe5b61152260206040519081016040528083604001518152508260800151612bb2565b60a083018190528282600381111561153657fe5b600381111561154157fe5b905250600090508151600381111561155557fe5b14611576576114a3600960068360000151600381111561157157fe5b612b25565b6115868160a00151600c546127f2565b60c083018190528282600381111561159a57fe5b60038111156115a557fe5b90525060009050815160038111156115b957fe5b146115d5576114a3600960018360000151600381111561157157fe5b6115e58160c00151600c54612c1a565b60e08301819052828260038111156115f957fe5b600381111561160457fe5b905250600090508151600381111561161857fe5b14611634576114a3600960048360000151600381111561157157fe5b6116566020604051908101604052806009548152508260c00151600d54612c40565b61010083018190528282600381111561166b57fe5b600381111561167657fe5b905250600090508151600381111561168a57fe5b146116a6576114a3600960058360000151600381111561157157fe5b6116b98160a00151600b54600b54612c40565b6101208301819052828260038111156116ce57fe5b60038111156116d957fe5b90525060009050815160038111156116ed57fe5b14611709576114a3600960038360000151600381111561157157fe5b6060810151600a55610120810151600b5560e0810151600c556101000151600d5550600090565b600080546001018082558161174733338787612554565b1491505b6000548114611792576040805160e560020a62461bcd02815260206004820152600a60248201526000805160206146d7833981519152604482015290519081900360640190fd5b5092915050565b600b5481565b610ca06117ad833484612c9c565b60408051808201909152601681527f6c6971756964617465426f72726f77206661696c6564000000000000000000006020820152610a29565b60008054600101808255600654604080517fd02f7351000000000000000000000000000000000000000000000000000000008152306004820152336024820152600160a060020a03888116604483015287811660648301526084820187905291518593929092169163d02f73519160a480820192602092909190829003018186803b15801561187457600080fd5b505afa158015611888573d6000803e3d6000fd5b505050506040513d602081101561189e57600080fd5b5051905080156118bd576118b56003601a83612b25565b925050610ef3565b85600160a060020a031685600160a060020a031614156118e3576118b56006601b611fc6565b600160a060020a0385166000908152600f60205260408120548190819061190a9088612b8f565b9093509150600083600381111561191d57fe5b14611940576119356009601985600381111561157157fe5b955050505050610ef3565b600160a060020a0389166000908152600f60205260409020546119639088612c1a565b9093509050600083600381111561197657fe5b1461198e576119356009601885600381111561157157fe5b600160a060020a038089166000818152600f60209081526040808320879055938d168083529184902085905583518b815293519193600080516020614765833981519152929081900390910190a3600654604080517f6d35bf91000000000000000000000000000000000000000000000000000000008152306004820152336024820152600160a060020a038c811660448301528b81166064830152608482018b905291519190921691636d35bf919160a480830192600092919082900301818387803b158015611a5e57600080fd5b505af1158015611a72573d6000803e3d6000fd5b5060009250611a7f915050565b9550505050506000548114610f3a576040805160e560020a62461bcd02815260206004820152600a60248201526000805160206146d7833981519152604482015290519081900360640190fd5b600454600090600160a060020a03163314611aed57610fe960016044611fc6565b60058054600160a060020a0384811673ffffffffffffffffffffffffffffffffffffffff19831681179093556040805191909216808252602082019390935281517fca4f2f25d0898edd99413412fb94012f9e54ec8142f9b093e7720646a95b16a9929181900390910190a1600061113c565b6000805460010180825581611b7361137f565b14611bc8576040805160e560020a62461bcd02815260206004820152601660248201527f61636372756520696e746572657374206661696c656400000000000000000000604482015290519081900360640190fd5b611bd0610e72565b915060005481146112b2576040805160e560020a62461bcd02815260206004820152600a60248201526000805160206146d7833981519152604482015290519081900360640190fd5b600160a060020a0381166000908152600f6020526040812054819081908190818080611c4489612a43565b935090506000816003811115611c5657fe5b14611c745760095b975060009650869550859450611ca79350505050565b611c7c6124a6565b925090506000816003811115611c8e57fe5b14611c9a576009611c5e565b5060009650919450925090505b9193509193565b6000610d9882612dab565b6011602052600090815260409020805460019091015482565b6000610d9882612de6565b600160a060020a03918216600090815260106020908152604080832093909416825291909152205490565b611d4e611d158234612e1c565b60408051808201909152601881527f7265706179426f72726f77426568616c66206661696c656400000000000000006020820152610a29565b50565b600554600090600160a060020a031633141580611d6c575033155b15611d8457611d7d60016000611fc6565b9050610ed5565b6004805460058054600160a060020a0380821673ffffffffffffffffffffffffffffffffffffffff1980861682179687905590921690925560408051938316808552949092166020840152815190927ff9ffabca9c8276e99321725bcb43fb076a6c66a54b7f21c4e8146d8519b417dc92908290030190a160055460408051600160a060020a038085168252909216602083015280517fca4f2f25d0898edd99413412fb94012f9e54ec8142f9b093e7720646a95b16a99281900390910190a160009250505090565b600080611e5861137f565b90508015611e7e57611e76816010811115611e6f57fe5b603f611fc6565b915050610fc3565b61113c83612eaa565b600754600160a060020a031681565b600454600160a060020a031681565b60075460009081908190600160a060020a03166315f24053611ec5612af9565b600c54600d546040518463ffffffff1660e060020a028152600401808481526020018381526020018281526020019350505050604080518083038186803b158015611f0f57600080fd5b505afa158015611f23573d6000803e3d6000fd5b505050506040513d6040811015611f3957600080fd5b50805160209091015190925090508115610ed15760405160e560020a62461bcd02815260040180806020018281038252603781526020018061472e6037913960400191505060405180910390fd5b6000805460010180825581611f9a61137f565b90508015611fb8576109c6816010811115611fb157fe5b6045611fc6565b6109d88461302f565b600181565b60007f45b96fe442630264581b197e84bbada861235052c5a1aadfff9ea4e40a969aa0836010811115611ff557fe5b83604c81111561200157fe5b604080519283526020830191909152600082820152519081900360600190a182601081111561113c57fe5b600654604080517f4ef4c3e1000000000000000000000000000000000000000000000000000000008152306004820152600160a060020a03858116602483015260448201859052915160009384931691634ef4c3e1916064808301926020929190829003018186803b1580156120a157600080fd5b505afa1580156120b5573d6000803e3d6000fd5b505050506040513d60208110156120cb57600080fd5b5051905080156120ea576120e26003601e83612b25565b915050610d98565b6120f2612b8b565b600a5414612106576120e2600a6021611fc6565b61210e61462b565b61211885856130d2565b8190601081111561212557fe5b9081601081111561213257fe5b90525060008151601081111561214457fe5b1461215f578051612156906025611fc6565b92505050610d98565b6121676124a6565b604083018190526020830182600381111561217e57fe5b600381111561218957fe5b90525060009050816020015160038111156121a057fe5b146121bc57612156600960208360200151600381111561157157fe5b6121d9846020604051908101604052808460400151815250613104565b60608301819052602083018260038111156121f057fe5b60038111156121fb57fe5b905250600090508160200151600381111561221257fe5b1461222e576121566009601f8360200151600381111561157157fe5b61223e600e548260600151612c1a565b608083018190526020830182600381111561225557fe5b600381111561226057fe5b905250600090508160200151600381111561227757fe5b1461229357612156600960238360200151600381111561157157fe5b600160a060020a0385166000908152600f602052604090205460608201516122bb9190612c1a565b60a08301819052602083018260038111156122d257fe5b60038111156122dd57fe5b90525060009050816020015160038111156122f457fe5b1461231057612156600960228360200151600381111561157157fe5b61231a858561311b565b8190601081111561232757fe5b9081601081111561233457fe5b90525060008151601081111561234657fe5b14612358578051612156906024611fc6565b6080810151600e5560a0810151600160a060020a0386166000818152600f602090815260409182902093909355606080850151825193845293830188905282820193909352517f4c209b5fc8ad50758f13e2e1088ba56a560dff690a1c6fef26394f4c03821c4f929181900390910190a160608101516040805191825251600160a060020a0387169130916000805160206147658339815191529181900360200190a36006546060820151604080517f41c728b9000000000000000000000000000000000000000000000000000000008152306004820152600160a060020a038981166024830152604482018990526064820193909352905191909216916341c728b991608480830192600092919082900301818387803b15801561247c57600080fd5b505af1158015612490573d6000803e3d6000fd5b506000925061249d915050565b95945050505050565b600080600e54600014156124c1575050600854600090612550565b60006124cb612af9565b905060006124d76145bf565b60006124e884600c54600d54613138565b9350905060008160038111156124fa57fe5b1461250e5794506000935061255092505050565b61251a83600e54613176565b92509050600081600381111561252c57fe5b146125405794506000935061255092505050565b5051600094509250612550915050565b9091565b600654604080517feabe7d91000000000000000000000000000000000000000000000000000000008152306004820152600160a060020a0386811660248301526044820185905291516000938493169163eabe7d91916064808301926020929190829003018186803b1580156125c957600080fd5b505afa1580156125dd573d6000803e3d6000fd5b505050506040513d60208110156125f357600080fd5b5051905080156126125761260a6003604983612b25565b9150506127ea565b83600160a060020a031685600160a060020a031614156126385761260a6002604a611fc6565b6000600160a060020a038781169087161415612657575060001961267f565b50600160a060020a038086166000908152601060209081526040808320938a16835292905220545b60008060008061268f8589612b8f565b909450925060008460038111156126a257fe5b146126c0576126b36009604a611fc6565b96505050505050506127ea565b600160a060020a038a166000908152600f60205260409020546126e39089612b8f565b909450915060008460038111156126f657fe5b14612707576126b36009604b611fc6565b600160a060020a0389166000908152600f602052604090205461272a9089612c1a565b9094509050600084600381111561273d57fe5b1461274e576126b36009604c611fc6565b600160a060020a03808b166000908152600f6020526040808220859055918b1681522081905560001985146127a657600160a060020a03808b166000908152601060209081526040808320938f168352929052208390555b88600160a060020a03168a600160a060020a03166000805160206147658339815191528a6040518082815260200191505060405180910390a3600096505050505050505b949350505050565b60008060006127ff6145bf565b6128098686612bb2565b9092509050600082600381111561281c57fe5b1461282d575091506000905061283f565b600061283882613226565b9350935050505b9250929050565b600080546001018082558161285961137f565b90508015612877576109c681601081111561287057fe5b6035611fc6565b6109d8333386613235565b60045460009081908190600160a060020a031633146128b0576128a760016030611fc6565b92505050610fc3565b6128b8612b8b565b600a54146128cc576128a7600a6032611fc6565b836128d5612af9565b10156128e7576128a7600e6031611fc6565b600d548411156128fd576128a760026033611fc6565b50600d54838103908111156129465760405160e560020a62461bcd0281526004018080602001828103825260248152602001806147ee6024913960400191505060405180910390fd5b600d81905560045461296190600160a060020a03168561368c565b9150600082601081111561297157fe5b146129b05760405160e560020a62461bcd0281526004018080602001828103825260238152602001806146b46023913960400191505060405180910390fd5b60045460408051600160a060020a03909216825260208201869052818101839052517f3bad0c59cf2f06e7314077049f48a93578cd16f5ef92329f1dab1420a99c177e9181900360600190a16000949350505050565b6000805460010180825581612a1961137f565b90508015612a37576109c6816010811115612a3057fe5b6026611fc6565b6109d8336000866136ce565b600160a060020a0381166000908152601160205260408120805482918291829182911515612a7c575060009450849350612af492505050565b612a8c8160000154600b54613c17565b90945092506000846003811115612a9f57fe5b14612ab4575091935060009250612af4915050565b612ac2838260010154613c5a565b90945091506000846003811115612ad557fe5b14612aea575091935060009250612af4915050565b5060009450925050505b915091565b60008080612b08303134612b8f565b90925090506000826003811115612b1b57fe5b14610ed157600080fd5b60007f45b96fe442630264581b197e84bbada861235052c5a1aadfff9ea4e40a969aa0846010811115612b5457fe5b84604c811115612b6057fe5b604080519283526020830191909152818101859052519081900360600190a18360108111156127ea57fe5b4390565b600080838311612ba657506000905081830361283f565b5060039050600061283f565b6000612bbc6145bf565b600080612bcd866000015186613c17565b90925090506000826003811115612be057fe5b14612bff5750604080516020810190915260008152909250905061283f565b60408051602081019091529081526000969095509350505050565b600080838301848110612c325760009250905061283f565b50600291506000905061283f565b6000806000612c4d6145bf565b612c578787612bb2565b90925090506000826003811115612c6a57fe5b14612c7b5750915060009050612c94565b612c8d612c8782613226565b86612c1a565b9350935050505b935093915050565b6000805460010180825581612caf61137f565b90508015612ccd576118b5816010811115612cc657fe5b600f611fc6565b83600160a060020a031663a6afed956040518163ffffffff1660e060020a028152600401602060405180830381600087803b158015612d0b57600080fd5b505af1158015612d1f573d6000803e3d6000fd5b505050506040513d6020811015612d3557600080fd5b505190508015612d55576118b5816010811115612d4e57fe5b6010611fc6565b612d6133878787613c89565b9250506000548114610f3a576040805160e560020a62461bcd02815260206004820152600a60248201526000805160206146d7833981519152604482015290519081900360640190fd5b6000805460010180825581612dbe61137f565b90508015612ddc576109c6816010811115612dd557fe5b6008611fc6565b6109d833856141c6565b6000805460010180825581612df961137f565b90508015612e10576109c6816010811115612a3057fe5b6109d8338560006136ce565b6000805460010180825581612e2f61137f565b90508015612e5557612e4d816010811115612e4657fe5b6034611fc6565b92505061174b565b612e60338686613235565b9250506000548114611792576040805160e560020a62461bcd02815260206004820152600a60248201526000805160206146d7833981519152604482015290519081900360640190fd5b6004546000908190600160a060020a03163314612ecd57611e7660016041611fc6565b612ed5612b8b565b600a5414612ee957611e76600a6040611fc6565b600760009054906101000a9004600160a060020a0316905082600160a060020a0316632191f92a6040518163ffffffff1660e060020a02815260040160206040518083038186803b158015612f3d57600080fd5b505afa158015612f51573d6000803e3d6000fd5b505050506040513d6020811015612f6757600080fd5b50511515612fbf576040805160e560020a62461bcd02815260206004820152601c60248201527f6d61726b6572206d6574686f642072657475726e65642066616c736500000000604482015290519081900360640190fd5b6007805473ffffffffffffffffffffffffffffffffffffffff1916600160a060020a03858116918217909255604080519284168352602083019190915280517fedffc32e068c7c95dfd4bdfd5c4d939a084d6b11c4199eac8436ed234d72f9269281900390910190a1600061113c565b600454600090600160a060020a0316331461305057610fe960016046611fc6565b613058612b8b565b600a541461306c57610fe9600a6047611fc6565b670de0b6b3a764000082111561308857610fe960026048611fc6565b6009805490839055604080518281526020810185905281517faaa68312e2ea9d50e16af5068410ab56e1a1fd06037b1a35664812c30f821460929181900390910190a1600061113c565b600033600160a060020a038416146130ec57506001610d98565b3482146130fb57506002610d98565b50600092915050565b60008060006131116145bf565b6128098686614560565b600033600160a060020a0384161461312f57fe5b3482146130fb57fe5b6000806000806131488787612c1a565b9092509050600082600381111561315b57fe5b1461316c5750915060009050612c94565b612c8d8186612b8f565b60006131806145bf565b60008061319586670de0b6b3a7640000613c17565b909250905060008260038111156131a857fe5b146131c75750604080516020810190915260008152909250905061283f565b6000806131d48388613c5a565b909250905060008260038111156131e757fe5b146132095750604080516020810190915260008152909450925061283f915050565b604080516020810190915290815260009890975095505050505050565b51670de0b6b3a7640000900490565b600654604080517f24008a62000000000000000000000000000000000000000000000000000000008152306004820152600160a060020a0386811660248301528581166044830152606482018590529151600093849316916324008a62916084808301926020929190829003018186803b1580156132b257600080fd5b505afa1580156132c6573d6000803e3d6000fd5b505050506040513d60208110156132dc57600080fd5b5051905080156132fb576132f36003603783612b25565b91505061113c565b613303612b8b565b600a5414613317576132f3600a6038611fc6565b61331f61462b565b61332885612a43565b606083018190526020830182600381111561333f57fe5b600381111561334a57fe5b905250600090508160200151600381111561336157fe5b146133865761337d600960368360200151600381111561157157fe5b9250505061113c565b60001984141561339f57606081015160408201526133a7565b604081018490525b6133b58682604001516130d2565b819060108111156133c257fe5b908160108111156133cf57fe5b9052506000815160108111156133e157fe5b146133f357805161337d90603b611fc6565b61340581606001518260400151612b8f565b608083018190526020830182600381111561341c57fe5b600381111561342757fe5b905250600090508160200151600381111561343e57fe5b1461345a5761337d600960398360200151600381111561157157fe5b61346a600c548260400151612b8f565b60a083018190526020830182600381111561348157fe5b600381111561348c57fe5b90525060009050816020015160038111156134a357fe5b146134bf5761337d6009603a8360200151600381111561157157fe5b6134cd86826040015161311b565b819060108111156134da57fe5b908160108111156134e757fe5b9052506000815160108111156134f957fe5b1461354e576040805160e560020a62461bcd02815260206004820152601f60248201527f726570617920626f72726f77207472616e7366657220696e206661696c656400604482015290519081900360640190fd5b60808082018051600160a060020a03808916600081815260116020908152604091829020948555600b5460019095019490945560a080880151600c8190558289015196518351958f1686529585019390935283820195909552606083019390935293810193909352517f1a2a22cb034d26d1854bdc6666a5b91fe25efbbb5dcad3b0355478d6f5c362a19281900390910190a160065460408083015181517fefcb03dd000000000000000000000000000000000000000000000000000000008152306004820152600160a060020a038a8116602483015289811660448301526064820192909252915192169163efcb03dd9160848082019260009290919082900301818387803b15801561366157600080fd5b505af1158015613675573d6000803e3d6000fd5b5060009250613682915050565b9695505050505050565b604051600090600160a060020a0384169083156108fc0290849084818181858888f193505050501580156136c4573d6000803e3d6000fd5b5060009392505050565b60008215806136db575081155b151561371b5760405160e560020a62461bcd0281526004018080602001828103825260348152602001806147ba6034913960400191505060405180910390fd5b613723614645565b61372b6124a6565b604083018190526020830182600381111561374257fe5b600381111561374d57fe5b905250600090508160200151600381111561376457fe5b14613780576132f36009602a8360200151600381111561157157fe5b60008411156138045760608101849052604080516020810182529082015181526137aa90856127f2565b60808301819052602083018260038111156137c157fe5b60038111156137cc57fe5b90525060009050816020015160038111156137e357fe5b146137ff576132f3600960288360200151600381111561157157fe5b61387e565b613821836020604051908101604052808460400151815250613104565b606083018190526020830182600381111561383857fe5b600381111561384357fe5b905250600090508160200151600381111561385a57fe5b14613876576132f3600960298360200151600381111561157157fe5b608081018390525b6006546060820151604080517feabe7d91000000000000000000000000000000000000000000000000000000008152306004820152600160a060020a03898116602483015260448201939093529051600093929092169163eabe7d9191606480820192602092909190829003018186803b1580156138fb57600080fd5b505afa15801561390f573d6000803e3d6000fd5b505050506040513d602081101561392557600080fd5b50519050801561393c5761337d6003602783612b25565b613944612b8b565b600a54146139585761337d600a602b611fc6565b613968600e548360600151612b8f565b60a084018190526020840182600381111561397f57fe5b600381111561398a57fe5b90525060009050826020015160038111156139a157fe5b146139bd5761337d6009602d8460200151600381111561157157fe5b600160a060020a0386166000908152600f602052604090205460608301516139e59190612b8f565b60c08401819052602084018260038111156139fc57fe5b6003811115613a0757fe5b9052506000905082602001516003811115613a1e57fe5b14613a3a5761337d6009602c8460200151600381111561157157fe5b8160800151613a47612af9565b1015613a595761337d600e602e611fc6565b613a6786836080015161368c565b82906010811115613a7457fe5b90816010811115613a8157fe5b905250600082516010811115613a9357fe5b14613ae8576040805160e560020a62461bcd02815260206004820152601a60248201527f72656465656d207472616e73666572206f7574206661696c6564000000000000604482015290519081900360640190fd5b60a0820151600e5560c0820151600160a060020a0387166000818152600f602090815260409182902093909355608085015160608087015183519485529484019190915282820193909352517fe5b754fb1abb7f01b499791d0b820ae3b6af3424ac1c59768edb53f4ec31a929929181900390910190a1606082015160408051918252513091600160a060020a038916916000805160206147658339815191529181900360200190a360065460808301516060840151604080517f51dff989000000000000000000000000000000000000000000000000000000008152306004820152600160a060020a038b81166024830152604482019490945260648101929092525191909216916351dff98991608480830192600092919082900301818387803b15801561366157600080fd5b600080831515613c2c5750600090508061283f565b838302838582811515613c3b57fe5b0414613c4f5750600291506000905061283f565b60009250905061283f565b600080821515613c70575060019050600061283f565b60008385811515613c7d57fe5b04915091509250929050565b600654604080517f5fc7e71e000000000000000000000000000000000000000000000000000000008152306004820152600160a060020a0384811660248301528781166044830152868116606483015260848201869052915160009384931691635fc7e71e9160a4808301926020929190829003018186803b158015613d0e57600080fd5b505afa158015613d22573d6000803e3d6000fd5b505050506040513d6020811015613d3857600080fd5b505190508015613d4f5761260a6003601283612b25565b613d57612b8b565b600a5414613d6b5761260a600a6015611fc6565b613d73612b8b565b83600160a060020a0316636c540baf6040518163ffffffff1660e060020a02815260040160206040518083038186803b158015613daf57600080fd5b505afa158015613dc3573d6000803e3d6000fd5b505050506040513d6020811015613dd957600080fd5b505114613dec5761260a600a6011611fc6565b85600160a060020a031685600160a060020a03161415613e125761260a60066016611fc6565b831515613e255761260a60076014611fc6565b600654604080517fc488847b000000000000000000000000000000000000000000000000000000008152306004820152600160a060020a038681166024830152604482018890528251600094859492169263c488847b926064808301939192829003018186803b158015613e9857600080fd5b505afa158015613eac573d6000803e3d6000fd5b505050506040513d6040811015613ec257600080fd5b50805160209091015190925090508115613eed57613ee36004601384612b25565b93505050506127ea565b84600160a060020a03166370a08231886040518263ffffffff1660e060020a0281526004018082600160a060020a0316600160a060020a0316815260200191505060206040518083038186803b158015613f4657600080fd5b505afa158015613f5a573d6000803e3d6000fd5b505050506040513d6020811015613f7057600080fd5b5051811115613f8557613ee3600d601c611fc6565b6000613f92898989613235565b90508015613fbb57613fb0816010811115613fa957fe5b6017611fc6565b9450505050506127ea565b604080517fb2a02ff1000000000000000000000000000000000000000000000000000000008152600160a060020a038b811660048301528a8116602483015260448201859052915160009289169163b2a02ff191606480830192602092919082900301818787803b15801561402f57600080fd5b505af1158015614043573d6000803e3d6000fd5b505050506040513d602081101561405957600080fd5b5051905080156140b3576040805160e560020a62461bcd02815260206004820152601460248201527f746f6b656e207365697a757265206661696c6564000000000000000000000000604482015290519081900360640190fd5b60408051600160a060020a03808d168252808c1660208301528183018b9052891660608201526080810185905290517f298637f684da70674f26509b10f07ec2fbc77a335ab1e7d6215a4b2484d8bb529181900360a00190a1600654604080517f47ef3b3b000000000000000000000000000000000000000000000000000000008152306004820152600160a060020a038a811660248301528d811660448301528c81166064830152608482018c905260a48201879052915191909216916347ef3b3b9160c480830192600092919082900301818387803b15801561419757600080fd5b505af11580156141ab573d6000803e3d6000fd5b50600092506141b8915050565b9a9950505050505050505050565b600654604080517fda3d454c000000000000000000000000000000000000000000000000000000008152306004820152600160a060020a0385811660248301526044820185905291516000938493169163da3d454c916064808301926020929190829003018186803b15801561423b57600080fd5b505afa15801561424f573d6000803e3d6000fd5b505050506040513d602081101561426557600080fd5b50519050801561427c576120e26003600e83612b25565b614284612b8b565b600a5414614297576120e2600a80611fc6565b826142a0612af9565b10156142b2576120e2600e6009611fc6565b6142ba614683565b6142c385612a43565b60408301819052602083018260038111156142da57fe5b60038111156142e557fe5b90525060009050816020015160038111156142fc57fe5b1461431857612156600960078360200151600381111561157157fe5b614326816040015185612c1a565b606083018190526020830182600381111561433d57fe5b600381111561434857fe5b905250600090508160200151600381111561435f57fe5b1461437b576121566009600c8360200151600381111561157157fe5b614387600c5485612c1a565b608083018190526020830182600381111561439e57fe5b60038111156143a957fe5b90525060009050816020015160038111156143c057fe5b146143dc576121566009600b8360200151600381111561157157fe5b6143e6858561368c565b819060108111156143f357fe5b9081601081111561440057fe5b90525060008151601081111561441257fe5b14614467576040805160e560020a62461bcd02815260206004820152601a60248201527f626f72726f77207472616e73666572206f7574206661696c6564000000000000604482015290519081900360640190fd5b60608082018051600160a060020a038816600081815260116020908152604091829020938455600b54600190940193909355608080870151600c819055945182519384529383018a9052828201939093529381019290925291517f13ed6866d4e1ee6da46f845c46d7e54120883d75c5ea9a2dacc1c4ca8984ab80929181900390910190a1600654604080517f5c778605000000000000000000000000000000000000000000000000000000008152306004820152600160a060020a0388811660248301526044820188905291519190921691635c77860591606480830192600092919082900301818387803b15801561247c57600080fd5b600061456a6145bf565b60008061457f670de0b6b3a764000087613c17565b9092509050600082600381111561459257fe5b146145b15750604080516020810190915260008152909250905061283f565b612838818660000151613176565b60408051602081019091526000815290565b6040805161014081019091528060008152602001600081526020016000815260200160008152602001600081526020016146096145bf565b8152602001600081526020016000815260200160008152602001600081525090565b6040805160c0810190915280600081526020016000614609565b6040805160e0810190915280600081526020016000815260200160008152602001600081526020016000815260200160008152602001600081525090565b6040805160a08101909152806000815260200160008152602001600081526020016000815260200160008152509056fe726564756365207265736572766573207472616e73666572206f7574206661696c656472652d656e746572656400000000000000000000000000000000000000000000626f72726f7742616c616e636553746f7265643a20626f72726f7742616c616e636553746f726564496e7465726e616c206661696c6564626f72726f7752617465506572426c6f636b3a20696e746572657374526174654d6f64656c2e626f72726f7752617465206661696c6564ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef65786368616e67655261746553746f7265643a2065786368616e67655261746553746f726564496e7465726e616c206661696c65646f6e65206f662072656465656d546f6b656e73496e206f722072656465656d416d6f756e74496e206d757374206265207a65726f72656475636520726573657276657320756e657870656374656420756e646572666c6f77a165627a7a723058208b5afa02e721e447980273257cd39091a4b086f3b1a72c511d0cb5b0aa01d448002953657474696e6720696e7465726573742072617465206d6f64656c206661696c6564496e697469616c2065786368616e67652072617465206d7573742062652067726561746572207468616e207a65726f2e0000000000000000000000000ecf2e81f77ee7b825ae1a8b74f3c0331868ef1b000000000000000000000000e622db19d5bf1f4e61dd57fb11fe887100e5e59e000000000000000000000000000000000000000000a56fa5b99019a5c800000000000000000000000000000000000000000000000000000000000000000000c0000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000080000000000000000000000000000000000000000000000000000000000000013436f6d706f756e6420457468657220f09f93880000000000000000000000000000000000000000000000000000000000000000000000000000000000000000046345544800000000000000000000000000000000000000000000000000000000"))
    println(result.isRevert)
    println(result.getException)
    println(result.getGasUsed)
  }

  private def deployThenCall(code: String, call: Array[Byte], newStep: Boolean): (BigInt, BigInt) = {
    deployThenCall(Array(code), call, newStep)
  }

  private def deployThenCall(code: Array[String], call: Array[Byte], newStep: Boolean) = {
    println("|------begin deploy ------------------")
    val (contract, result) = code.map(x => {
      VMTest.newdeploy(dataBase, author, BinaryData(x), over = newStep)

    }).toSeq.head

    println(s"|------begin call $contract------------------")
    val getResult = VMTest.newcall(dataBase, caller, contract, result.getHReturn, call, over = newStep)
    (result.getGasUsed, getResult.getGasUsed)
  }
}

object VMTest {
  private val dir = "VMTest"
  private val settings = DataBaseSettings(dir, true, 10, DBType.LevelDB)
  //  private val dataBase = new DataBase(settings)

  val priv1 = new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))
  val priv2 = new PrivateKey(BinaryData("cc7b7fa6e706944fa2d75652065f95ef2f364316e172601320655aac0e648165"))
  val priv3 = new PrivateKey(BinaryData("db71fe7c0ac4ca3e8cef95bf55cf535eaa8fe0c80d18e0cb19af8d7071b8a184"))
  val priv4 = new PrivateKey(BinaryData("9456beec947b368eda4be03f6c306703d9b2eda49f661285944b4e1f07ae18f3"))

  val caller = new PrivateKey(BinaryData("3c8f96129e421363ce6f5631b29e51fdf8ee3eba951e0c19ea87843876c1b689")).publicKey.pubKeyHash
  val author = priv1.publicKey.pubKeyHash
  val contractAddress = Crypto.calcNewAddr(author, 1)
  val vmSettings = ContractSettings(0, false, Int.MaxValue)

  private val _witness1 = InitWitness("init1", priv1.publicKey.pubKeyHash)
  private val _witness2 = InitWitness("init2", priv2.publicKey.pubKeyHash)
  private val _witness3 = InitWitness("init3", priv3.publicKey.pubKeyHash)
  private val _witness4 = InitWitness("init4", priv4.publicKey.pubKeyHash)

  private val _consensusSettings = ConsensusSettings(500, 500, 1, 4, 63000, Array(_witness1, _witness2, _witness3, _witness4))

  private var nonce = 0

  def newNonce = {
    nonce += 1
    nonce
  }

  def deploy(dataBase: DataBase, caller: UInt160, code: Array[Byte], value: Int = 0, gasLimit: Long = Int.MaxValue) = {
    val tracking = dataBase.startTracking()
    val contract = Crypto.calcNewAddr(author, newNonce)
    if (value > 0) tracking.transfer(caller, contract, value)
    val invoker = createInvoker(tracking, dataBase, caller, contract, Array.empty, value, gasLimit)
    val program = new Program(vmSettings, code, invoker, Long.MaxValue)
    val result = VM.play(vmSettings, VMHook.EMPTY, program)
    if (success(result)) {
      tracking.commit()
    } else {
      tracking.rollBack()
    }
    (contract, result)
  }

  def call(dataBase: DataBase, caller: UInt160, contract: UInt160, code: Array[Byte],
           signature: Array[Byte], value: Int = 0, gasLimit: Long = Int.MaxValue) = {
    val tracking = dataBase.startTracking()
    if (value > 0) {
      tracking.transfer(caller, contract, value)
    }

    val invoker = createInvoker(tracking, dataBase, caller, contract, signature, value, gasLimit)
    val program = new Program(vmSettings, code, invoker, Long.MaxValue)
    val result = VM.play(vmSettings, VMHook.EMPTY, program)
    if (success(result)) {
      tracking.commit()
    } else {
      tracking.rollBack()
    }
    result
  }

  def newdeploy(dataBase: DataBase, caller: UInt160, code: Array[Byte], value: Int = 0, gasLimit: Long = Int.MaxValue, over: Boolean) = {
    val tracking = dataBase.startTracking()
    val contract = Crypto.calcNewAddr(author, newNonce)
    if (value > 0) tracking.transfer(caller, contract, value)
    val invoker = createInvoker(tracking, dataBase, caller, contract, Array.empty, value, gasLimit)
    val program = new Program(vmSettings, code, invoker, Long.MaxValue)
    val result = VM.play(vmSettings, VMHook.EMPTY, program)
    if (success(result)) {
      tracking.commit()
    } else {
      tracking.rollBack()
    }
    (contract, result)
  }

  def newcall(dataBase: DataBase, caller: UInt160, contract: UInt160, code: Array[Byte],
              signature: Array[Byte], value: Int = 0, gasLimit: Long = Int.MaxValue, over: Boolean) = {
    val tracking = dataBase.startTracking()
    if (value > 0) {
      tracking.transfer(caller, contract, value)
    }

    val invoker = createInvoker(tracking, dataBase, caller, contract, signature, value, gasLimit)
    val program = new Program(vmSettings, code, invoker, Long.MaxValue)
    val result = VM.play(vmSettings, VMHook.EMPTY, program)
    if (success(result)) {
      tracking.commit()
    } else {
      tracking.rollBack()
    }
    result
  }


  def createInvoker(tracking: DataBase, origin: DataBase, caller: UInt160, contract: UInt160,
                    data: Array[Byte], value: Int, gasLimit: Long): ProgramInvoke = {
    new ProgramInvokeImpl(DataWord.of(contract), DataWord.of(caller), DataWord.of(caller), DataWord.ZERO, DataWord.ZERO, DataWord.of(gasLimit), DataWord.of(value), data, DataWord.ZERO, DataWord.ZERO, DataWord.ZERO, DataWord.ZERO, DataWord.of(gasLimit), tracking, origin, null)
  }

  def success(getResult: ProgramResult) = {
    !getResult.isRevert && getResult.getException == null
  }
}
