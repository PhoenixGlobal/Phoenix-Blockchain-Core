package com.apex.test

import java.time.Instant

import com.apex.consensus.ProducerUtil
import com.apex.core._
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, FixedNumber, MerkleTree, UInt160, UInt256}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.settings._
import com.apex.solidity.Abi
import com.apex.solidity.compiler.{CompilationResult, SolidityCompiler}
import com.apex.solidity.compiler.SolidityCompiler.Options.{ABI, BIN, INTERFACE, METADATA}
import com.apex.vm.DataWord
import org.junit.{AfterClass, Test}
import scala.reflect.io.Directory

@Test
class VMOpTest2 {  

  Directory("VMOpTest2").deleteRecursively()

  val _produceInterval = 2000

  val _minerAward: Double = 12.3

  val p1 = new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))
  val p2 = new PrivateKey(BinaryData("cc7b7fa6e706944fa2d75652065f95ef2f364316e172601320655aac0e648165"))
  val p3 = new PrivateKey(BinaryData("db71fe7c0ac4ca3e8cef95bf55cf535eaa8fe0c80d18e0cb19af8d7071b8a184"))
  val p4 = new PrivateKey(BinaryData("9456beec947b368eda4be03f6c306703d9b2eda49f661285944b4e1f07ae18f3"))

  val _witness1 = InitWitness("init1", p1.publicKey.address)
  val _witness2 = InitWitness("init2", p2.publicKey.address)
  val _witness3 = InitWitness("init3", p3.publicKey.address)
  val _witness4 = InitWitness("init4", p4.publicKey.address)

  val _miners = MinerSettings(Array(
    new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471")),
    new PrivateKey(BinaryData("cc7b7fa6e706944fa2d75652065f95ef2f364316e172601320655aac0e648165")),
    new PrivateKey(BinaryData("db71fe7c0ac4ca3e8cef95bf55cf535eaa8fe0c80d18e0cb19af8d7071b8a184")),
    new PrivateKey(BinaryData("9456beec947b368eda4be03f6c306703d9b2eda49f661285944b4e1f07ae18f3"))     ))

  val _consensusSettings = ConsensusSettings(_produceInterval, 500, 1, 4, 5, 2.1, 63000,
    Array(_witness1, _witness2, _witness3, _witness4))

  val _runtimeParas = RuntimeParas(100)

  val _acct1 = Ecdsa.PrivateKey.fromWIF("KwmuSp41VWBtGSWaQQ82ZRRSFzkJVTAyuDLQ9NzP9CPqLWirh4UQ").get
  val _acct2 = Ecdsa.PrivateKey.fromWIF("L32JpLopG2hWjEMSCkAjS1nUnPixVrDTPqFAGYbddQrtUjRfkjEP").get
  val _acct3 = Ecdsa.PrivateKey.fromWIF("KyUTLv2BeP9SJD6Sa8aHBVmuRkgw9eThjNGJDE4PySEgf2TvCQCn").get
  val _acct4 = Ecdsa.PrivateKey.fromWIF("L33Uh9L35pSoEqBPP43U6rQcD2xMpJ7F4b3QMjUMAL6HZhxUqEGq").get

  val contractAddr = Crypto.calcNewAddr(_acct1.publicKey.pubKeyHash, 0)

  private val minerCoinFrom = UInt160.Zero

  private def createChain(path: String): Blockchain = {
    val baseDir = s"VMOpTest2/$path"
    val chainSetting = ChainSettings(
      BlockBaseSettings(s"$baseDir/block", false, 0, DBType.LevelDB),
      DataBaseSettings(s"$baseDir/data", false, 0, DBType.LevelDB),
      ForkBaseSettings(s"$baseDir/fork", false, 0, DBType.LevelDB),
      _minerAward,
      GenesisSettings(Instant.EPOCH,
        "7a93d447bffe6d89e690f529a3a0bdff8ff6169172458e04849ef1d4eafd7f86",
        Array(CoinAirdrop(_acct1.publicKey.address, 123.12),
          CoinAirdrop(_acct2.publicKey.address, 234.2),
          CoinAirdrop(_acct3.publicKey.address, 234.2))
      )
    )

    new Blockchain(chainSetting, _consensusSettings, _runtimeParas, Notification())
  }

  @Test    // ADDMOD
  def testADDMOD(): Unit = {
    val chain = createChain("testADDMOD")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := addmod(2, 3, 3)\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == BigInt(2))
    }
    finally {
      chain.close()
    }
  }

  @Test    // MULMOD
  def testMULMOD(): Unit = {
    val chain = createChain("testMULMOD")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := mulmod(3, 4, 7)\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == BigInt(5))
    }
    finally {
      chain.close()
    }
  }

  @Test    // BLOCKHASH
  def testBLOCKHASH(): Unit = {
    val chain = createChain("testBLOCKHASH")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := blockhash(1)\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      chain.produceBlockFinalize()

      assert(chain.getHeight() == 1)
      val block1hash = chain.getBlock(1).get.id
      val block0hash = chain.getBlock(0).get.id

      blockTime += _produceInterval
      chain.startProduceBlock(_miners.findPrivKey(chain.getWitness(blockTime)).get, blockTime, Long.MaxValue)
      assert(chain.isProducingBlock())

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      val wefwef = chain.getReceipt(tx.id()).get
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output) == DataWord.of(block1hash))
    }
    finally {
      chain.close()
    }
  }

  @Test
  def testBalance: Unit = {
    val chain = createChain("testBalance")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() view public returns (uint r) {\n      return msg.sender.balance;\n    } \n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      val nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      chain.produceBlockFinalize()

      assert(chain.getHeight() == 1)
      //val block1hash = chain.getBlock(1).get.id
      //val block0hash = chain.getBlock(0).get.id

      blockTime += _produceInterval
      chain.startProduceBlock(_miners.findPrivKey(chain.getWitness(blockTime)).get, blockTime, Long.MaxValue)

      val deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      val txData = Abi.fromJson(abiString).encode(s"a()")
      val tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(FixedNumber(DataWord.of(chain.getReceipt(tx.id()).get.output).value) == (FixedNumber.fromDecimal(123.12) - FixedNumber(9100195)))
    }
    finally {
      chain.close()
    }
  }

//  @Test  // BALANCE Op
//  def testBALANCE: Unit = {
//        val _ =
//          "contract Purchase {\n\tfunction getBalance() constant public returns(uint){\n\t\treturn this.balance;\n\t} \n}"
//
//        val abi = Abi.fromJson("[{\"constant\":true,\"inputs\":[],\"name\":\"getBalance\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"}]")
//        val code = "6080604052348015600f57600080fd5b5060b78061001e6000396000f300608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806312065fe0146044575b600080fd5b348015604f57600080fd5b506056606c565b6040518082815260200191505060405180910390f35b60003073ffffffffffffffffffffffffffffffffffffffff16319050905600a165627a7a7230582086ab234bf457d6cac98fce110e5688da12a0ea23b98e67d9d4227592791fd9200029"
//
//        val call3 = abi.encode("getBalance()")
//        dataBase.addBalance(caller, 1000)
//
//        println("(caller address.asString)" + caller.address)
//        println("(authoer address.asString)" + PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8").pubKeyHash.address)
//        println(PublicKey("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322").pubKeyHash.address)
//
//        val (contract, result)= deploy(dataBase, caller, BinaryData(code))
//        val getResult = call(dataBase,  caller, contract, result.getHReturn, call3)
//
//        val program = new Program(VMOpTest.vmSettings, BinaryData(code), invoker, Long.MaxValue)
//
//        val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
//
//        vm.step(program)
//
//        val item1 = program.stackPop
//
//        assert(getResult.getHReturn == DataWord.of(1000))
//  }

  @Test    // SHL
  def testSHL(): Unit = {
    val chain = createChain("testSHL")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := shl(23, 3)\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == BigInt(184))
    }
    finally {
      chain.close()
    }
  }

  @Test    // SHR
  def testSHR(): Unit = {
    val chain = createChain("testSHR")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := shr(184, 3)\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == BigInt(23))
    }
    finally {
      chain.close()
    }
  }

  @Test    // SAR1
  def testSAR1(): Unit = {
    val chain = createChain("testSAR1")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := sar(184, 3)\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == BigInt(23))
    }
    finally {
      chain.close()
    }
  }

  @Test    // SAR2
  def testSAR2(): Unit = {
    val chain = createChain("testSAR2")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := sar(0x8000000000000000000000000000000000000000000000000000000000000000, 1)\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value ==
        BigInt("c000000000000000000000000000000000000000000000000000000000000000", 16))
    }
    finally {
      chain.close()
    }
  }

  @Test    // SAR3
  def testSAR3(): Unit = {
    val chain = createChain("testSAR3")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := sar(0x8000000000000000000000000000000000000000000000000000000000000000, 256)\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value ==
        BigInt("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", 16))
    }
    finally {
      chain.close()
    }
  }

  @Test    // SAR4
  def testSAR4(): Unit = {
    val chain = createChain("testSAR4")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := sar(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff, 0)\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value ==
        BigInt("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", 16))
    }
    finally {
      chain.close()
    }
  }

  @Test    // SAR5
  def testSAR5(): Unit = {
    val chain = createChain("testSAR5")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := sar(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff, 1)\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value ==
        BigInt("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", 16))
    }
    finally {
      chain.close()
    }
  }

  @Test    // ORIGIN
  def testORIGIN(): Unit = {
    val chain = createChain("testORIGIN")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := origin\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output) ==
        DataWord.of(_acct1.publicKey.pubKeyHash))
    }
    finally {
      chain.close()
    }
  }

  @Test    // CODESIZE
  def testCODESIZE(): Unit = {
    val chain = createChain("testCODESIZE")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := codesize\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      val eeeeee = chain.getReceipt(tx.id()).get
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value.intValue() ==
        154)  // !!
    }
    finally {
      chain.close()
    }
  }

  @Test    // SIGNEXTEND
  def testSIGNEXTEND(): Unit = {
    val chain = createChain("testSIGNEXTEND")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\n\ncontract A {\n  function a() public returns (uint r) {\n      assembly {\n              r := signextend(3, 0xab82345678)\n       }\n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("A").abi
      val binString = result.getContract("A").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value ==
        BigInt("ffffffffffffffffffffffffffffffffffffffffffffffffffffffff82345678", 16))  // !!
    }
    finally {
      chain.close()
    }
  }

}

object VMOpTest2 {
  @AfterClass
  def cleanUp: Unit = {
    println("clean Directory")
    Directory("VMOpTest2").deleteRecursively()
  }
}
