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
import com.apex.settings.{ContractSettings, DBType, DataBaseSettings}
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

  private def separate() = {
    println()
    println("****************")
    println()
  }

  private var dataBase: DataBase = null

  @Before
  def populate(): Unit = {
    dataBase = new DataBase(settings)
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
}

object VMTest {
  private val dir = "VMTest"
  private val settings = DataBaseSettings(dir, true, 10, DBType.LevelDB)
  //  private val dataBase = new DataBase(settings)

  val caller = PublicKey("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322").pubKeyHash
  val author = PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8").pubKeyHash
  val contractAddress = Crypto.calcNewAddr(author, BigInt(1).toByteArray)
  val vmSettings = ContractSettings(0, false, Int.MaxValue)

  def deploy(dataBase: DataBase, caller: UInt160, code: Array[Byte], value: Int = 0, gasLimit: Long = Int.MaxValue) = {
    val tracking = dataBase.startTracking()
    val contract = Crypto.calcNewAddr(author, BigInt(1).toByteArray)
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

  def call(dataBase: DataBase, caller: UInt160, contract: UInt160, code: Array[Byte], signature: Array[Byte], value: Int = 0, gasLimit: Long = Int.MaxValue) = {
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

  def createInvoker(tracking: DataBase, origin: DataBase, caller: UInt160, contract: UInt160, data: Array[Byte], value: Int, gasLimit: Long): ProgramInvoke = {
    new ProgramInvokeImpl(
      DataWord.of(contract),
      DataWord.of(caller),
      DataWord.of(caller),
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.of(gasLimit),
      DataWord.of(value),
      data,
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.ZERO,
      tracking,
      origin,
      null)
  }

  def success(getResult: ProgramResult) = {
    !getResult.isRevert && getResult.getException == null
  }
}
