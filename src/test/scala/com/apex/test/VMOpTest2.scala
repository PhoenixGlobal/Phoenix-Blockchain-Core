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

  val _witness1 = Witness("init1",
    PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8").pubKeyHash)
    //Some(new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))))

  val _witness2 = Witness("init2",
    PublicKey("03c3333373adc0636b1d67d4bca3d8b34a53d663698119369981e67866250d3a74").pubKeyHash)
    //Some(new PrivateKey(BinaryData("cc7b7fa6e706944fa2d75652065f95ef2f364316e172601320655aac0e648165"))))

  val _witness3 = Witness("init3",
    PublicKey("020550de6ce7ed53ff018cccf1095893edba43f798252d6983e0fd2ca5af3ee0da").pubKeyHash)
    //Some(new PrivateKey(BinaryData("db71fe7c0ac4ca3e8cef95bf55cf535eaa8fe0c80d18e0cb19af8d7071b8a184"))))

  val _witness4 = Witness("init4",  // APPnx5YahVg1dTgeWkp1fE33ftvAaGbeQaR  L2C4Za8VSx2iBgszQarHx4YzqHvfumkHjbi6bNqvqst6mc8QcuZ7
    PublicKey("0246f896de22582786884d7d7ae27ef00cc8fed167bcdb8c305fbbc3dd9cca696c").pubKeyHash)
    //Some(new PrivateKey(BinaryData("9456beec947b368eda4be03f6c306703d9b2eda49f661285944b4e1f07ae18f3"))))

  val _miners = MinerSettings(Array(
    new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471")),
    new PrivateKey(BinaryData("cc7b7fa6e706944fa2d75652065f95ef2f364316e172601320655aac0e648165")),
    new PrivateKey(BinaryData("db71fe7c0ac4ca3e8cef95bf55cf535eaa8fe0c80d18e0cb19af8d7071b8a184")),
    new PrivateKey(BinaryData("9456beec947b368eda4be03f6c306703d9b2eda49f661285944b4e1f07ae18f3"))     ))

  val _consensusSettings = ConsensusSettings(_produceInterval, 500, 1, 4, 63000,
    Array(_witness1, _witness2, _witness3, _witness4))

  val _runtimeParas = RuntimeParas(100, 9000000)

  val _acct1 = Ecdsa.PrivateKey.fromWIF("KwmuSp41VWBtGSWaQQ82ZRRSFzkJVTAyuDLQ9NzP9CPqLWirh4UQ").get
  val _acct2 = Ecdsa.PrivateKey.fromWIF("L32JpLopG2hWjEMSCkAjS1nUnPixVrDTPqFAGYbddQrtUjRfkjEP").get
  val _acct3 = Ecdsa.PrivateKey.fromWIF("KyUTLv2BeP9SJD6Sa8aHBVmuRkgw9eThjNGJDE4PySEgf2TvCQCn").get
  val _acct4 = Ecdsa.PrivateKey.fromWIF("L33Uh9L35pSoEqBPP43U6rQcD2xMpJ7F4b3QMjUMAL6HZhxUqEGq").get

  private val minerCoinFrom = UInt160.Zero

  private def createChain(path: String): Blockchain = {
    val baseDir = s"VMOpTest2/$path"
    val chainSetting = ChainSettings(
      BlockBaseSettings(s"$baseDir/block", false, 0, DBType.LevelDB),
      DataBaseSettings(s"$baseDir/data", false, 0, DBType.LevelDB),
      ForkBaseSettings(s"$baseDir/fork", false, 0, DBType.LevelDB),
      PeerBaseSettings(s"$baseDir/peer", false, 0, DBType.LevelDB),
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
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
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
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
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

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error == "")
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output) == DataWord.of(block1hash))
    }
    finally {
      chain.close()
    }
  }

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
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
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
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
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
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
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
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
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
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
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
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
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
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
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
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
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
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
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
        FixedNumber(0), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      var txData = Abi.fromJson(abiString).encode(s"a()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        UInt160.fromBytes(BinaryData("7f97e6f4f660e6c09b894f34edae3626bf44039a")), FixedNumber.Zero,
        1, txData, FixedNumber(0), 9000000L, BinaryData.empty)
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
