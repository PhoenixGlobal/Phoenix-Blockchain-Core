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

import scala.collection.mutable.ArrayBuffer
import scala.reflect.io.Directory

@Test
class ContractTxTest {  

  Directory("ContractTxTest").deleteRecursively()

  val _produceInterval = 2000

  val _minerAward: Double = 12.3

  val priv1 = new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))
  val priv2 = new PrivateKey(BinaryData("cc7b7fa6e706944fa2d75652065f95ef2f364316e172601320655aac0e648165"))
  val priv3 = new PrivateKey(BinaryData("db71fe7c0ac4ca3e8cef95bf55cf535eaa8fe0c80d18e0cb19af8d7071b8a184"))
  val priv4 = new PrivateKey(BinaryData("9456beec947b368eda4be03f6c306703d9b2eda49f661285944b4e1f07ae18f3"))

  val _witness1 = InitWitness("init1", priv1.publicKey.address)
  val _witness2 = InitWitness("init2", priv2.publicKey.address)
  val _witness3 = InitWitness("init3", priv3.publicKey.address)
  val _witness4 = InitWitness("init4", priv4.publicKey.address)

  val _miners = MinerSettings(Array(priv1, priv2, priv3, priv4))

  val _consensusSettings = ConsensusSettings(_produceInterval, 500, 1, 4, 5, 2.1, 63000, Array(_witness1, _witness2, _witness3, _witness4))

  val _runtimeParas = RuntimeParas(100)

  val _acct1 = Ecdsa.PrivateKey.fromWIF("KwmuSp41VWBtGSWaQQ82ZRRSFzkJVTAyuDLQ9NzP9CPqLWirh4UQ").get
  val _acct2 = Ecdsa.PrivateKey.fromWIF("L32JpLopG2hWjEMSCkAjS1nUnPixVrDTPqFAGYbddQrtUjRfkjEP").get
  val _acct3 = Ecdsa.PrivateKey.fromWIF("KyUTLv2BeP9SJD6Sa8aHBVmuRkgw9eThjNGJDE4PySEgf2TvCQCn").get
  val _acct4 = Ecdsa.PrivateKey.fromWIF("L33Uh9L35pSoEqBPP43U6rQcD2xMpJ7F4b3QMjUMAL6HZhxUqEGq").get

  private val minerCoinFrom = UInt160.Zero

  val contractAddress = Crypto.calcNewAddr(_acct1.publicKey.pubKeyHash, 0)

  private def createChain(path: String): Blockchain = {
    val baseDir = s"ContractTxTest/$path"
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

  @Test
  def testCreateChain(): Unit = {
    val chain = createChain("testCreateChain")
    try {

      assert(chain.getHeight() == 0)

      assert(chain.getBalance(_acct1).get == FixedNumber.fromDecimal(123.12))

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      assert(chain.isProducingBlock())

      /*
      *
      * contract Faucet {
              uint value;
              // Give out ether to anyone who asks
              function set(uint withdraw_amount) public {
                  value = withdraw_amount;
              }
              function get() public  returns (uint)  {
                  return value;
              }
        }
      *
      * */

      val codebin = BinaryData("608060405234801561001057600080fd5b5060e68061001f6000396000f3fe6080604052600436106043576000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11460485780636d4ce63c14607f575b600080fd5b348015605357600080fd5b50607d60048036036020811015606857600080fd5b810190808035906020019092919050505060a7565b005b348015608a57600080fd5b50609160b1565b6040518082815260200191505060405180910390f35b8060008190555050565b6000805490509056fea165627a7a723058202c7cfe05b5e1b84938fa70727102e914fba062d91fde5a0f0a92613ad081732b0029")

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, codebin,
        FixedNumber(1), 9, BinaryData.empty)
      assert(!chain.addTransaction(deployTx))   // gas limit error

      deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, codebin,
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      val settt = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"withdraw_amount\",\"type\":\"uint256\"}],\"name\":\"set\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"get\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]").encode("set(123)")
      var setTx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        1, settt, FixedNumber(1), 9, BinaryData.empty)
      assert(!chain.addTransaction(setTx))   // gas limit error

      setTx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        1, settt, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(setTx))

      val gettt = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"withdraw_amount\",\"type\":\"uint256\"}],\"name\":\"set\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"get\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]").encode("get()")

      var getTx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        2, gettt, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(getTx))
      assert(chain.getTransactionFromPendingTxs(getTx.id).isDefined)

      var receipt = chain.getReceipt(getTx.id()).get

      assert(DataWord.of(receipt.output).longValue == 123)
      assert(chain.getBalance(_acct1).get == FixedNumber.fromDecimal(BigDecimal("123.119999999999822462")))
      assert(chain.getBalance(producer).get == FixedNumber.fromDecimal(BigDecimal("12.300000000000177538")))

      getTx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        3, gettt, FixedNumber(1000000000L), 21539, BinaryData.empty)
      assert(chain.addTransaction(getTx))   // require gas 21540, but only give 21539
      receipt = chain.getReceipt(getTx.id()).get
      assert(receipt.error.contains("Not enough gas"))
      // Fee = 0.000021539
      assert(chain.getBalance(_acct1).get == FixedNumber.fromDecimal(BigDecimal("123.119978460999822462")))
      assert(chain.getBalance(producer).get == FixedNumber.fromDecimal(BigDecimal("12.300021539000177538")))

    }
    finally {
      chain.close()
    }
  }

  @Test
  def testScheduleContractTx(): Unit = {
    val chain = createChain("testScheduleContractTx")
    try {

      assert(chain.getHeight() == 0)

      assert(chain.getBalance(_acct1).get == FixedNumber.fromDecimal(123.12))

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      assert(chain.isProducingBlock())

      /*
      *
      * contract Faucet {
              uint value;
              // Give out ether to anyone who asks
              function set(uint withdraw_amount) public {
                  value = withdraw_amount;
              }
              function get() public  returns (uint)  {
                  return value;
              }
        }
      *
      * */

      val codebin = BinaryData("608060405234801561001057600080fd5b5060e68061001f6000396000f3fe6080604052600436106043576000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11460485780636d4ce63c14607f575b600080fd5b348015605357600080fd5b50607d60048036036020811015606857600080fd5b810190808035906020019092919050505060a7565b005b348015608a57600080fd5b50609160b1565b6040518082815260200191505060405180910390f35b8060008190555050565b6000805490509056fea165627a7a723058202c7cfe05b5e1b84938fa70727102e914fba062d91fde5a0f0a92613ad081732b0029")

      val deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, codebin,
        FixedNumber(1), 9000000L, BinaryData.empty, executeTime = blockTime + 2750)
      assert(chain.addTransaction(deployTx))
      assert(chain.getScheduleTx().size ==1)
      val block1 = chain.produceBlockFinalize()

      blockTime += _produceInterval
      val witness1 = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(witness1).get, blockTime, Long.MaxValue)
      assert(chain.isProducingBlock())

      sleepTo(blockTime)
      val block2 = chain.produceBlockFinalize()
      assert(block2.isDefined)
      assert(block2.get.transactions.size == 1)

      assert(!chain.isProducingBlock())
      assert(chain.getHeight() == 2)
      assert(chain.getHeadTime() == blockTime)
      assert(chain.head.id() == block2.get.id())

      blockTime += _produceInterval
      val witness2 = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(witness2).get, blockTime, Long.MaxValue)
      assert(chain.isProducingBlock())

      sleepTo(blockTime)

      println("chain.blockTime()" + blockTime)

      val block3 = chain.produceBlockFinalize()
      assert(block3.isDefined)
      assert(chain.getScheduleTx().size == 0)

      blockTime += _produceInterval
      val witness3 = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(witness3).get, blockTime, Long.MaxValue)
      assert(chain.isProducingBlock())

      sleepTo(blockTime)

      println("chain.blockTime()" + blockTime)


      val settt = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"withdraw_amount\",\"type\":\"uint256\"}],\"name\":\"set\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"get\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]").encode("set(123)")
      val setTx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
      contractAddress, FixedNumber.Zero,
      1, settt, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(setTx))

      val gettt = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"withdraw_amount\",\"type\":\"uint256\"}],\"name\":\"set\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"get\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]").encode("get()")

      var getTx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
      contractAddress, FixedNumber.Zero,
      2, gettt, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(getTx))
      assert(chain.getTransactionFromPendingTxs(getTx.id).isDefined)

      var receipt = chain.getReceipt(getTx.id()).get

      assert(DataWord.of(receipt.output).longValue == 123)

      //assert(chain.getBalance(_acct1).get == FixedNumber.fromDecimal(BigDecimal("123.119999999662376462")))
      //assert(chain.getBalance(producer).get == FixedNumber.fromDecimal(BigDecimal("12.300000000337446000")))

    }
    finally {
      chain.close()
    }
  }

  @Test
  def testERC20(): Unit = {
    val chain = createChain("testERC20")
    try {

      assert(chain.getHeight() == 0)

      assert(chain.getBalance(_acct1).get == FixedNumber.fromDecimal(123.12))

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)
      assert(chain.isProducingBlock())

      val codebin = BinaryData("60806040523480156200001157600080fd5b506f98765000000000000000000000000000600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506f987650000000000000000000000000006003819055506040805190810160405280600881526020017f746573744170657800000000000000000000000000000000000000000000000081525060009080519060200190620000ca9291906200017c565b506040805190810160405280600281526020017f744100000000000000000000000000000000000000000000000000000000000081525060019080519060200190620001189291906200017c565b506003600260006101000a81548160ff021916908360ff16021790555033600460006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055506200022b565b828054600181600116156101000203166002900490600052602060002090601f016020900481019282601f10620001bf57805160ff1916838001178555620001f0565b82800160010185558215620001f0579182015b82811115620001ef578251825591602001919060010190620001d2565b5b509050620001ff919062000203565b5090565b6200022891905b80821115620002245760008160009055506001016200020a565b5090565b90565b6115ff806200023b6000396000f3006080604052600436106100e6576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806306fdde03146100e8578063095ea7b31461017857806318160ddd146101dd57806323b872dd14610208578063313ce5671461028d5780633bed33ce146102be57806342966c68146102eb5780636623fc461461033057806370a08231146103755780638da5cb5b146103cc57806395d89b4114610423578063a9059cbb146104b3578063cd4217c114610500578063d7a78db814610557578063dd62ed3e1461059c578063f8b2cb4f14610613575b005b3480156100f457600080fd5b506100fd61066a565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561013d578082015181840152602081019050610122565b50505050905090810190601f16801561016a5780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b34801561018457600080fd5b506101c3600480360381019080803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190505050610708565b604051808215151515815260200191505060405180910390f35b3480156101e957600080fd5b506101f26107a3565b6040518082815260200191505060405180910390f35b34801561021457600080fd5b50610273600480360381019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803590602001909291905050506107a9565b604051808215151515815260200191505060405180910390f35b34801561029957600080fd5b506102a2610bcd565b604051808260ff1660ff16815260200191505060405180910390f35b3480156102ca57600080fd5b506102e960048036038101908080359060200190929190505050610be0565b005b3480156102f757600080fd5b5061031660048036038101908080359060200190929190505050610ca8565b604051808215151515815260200191505060405180910390f35b34801561033c57600080fd5b5061035b60048036038101908080359060200190929190505050610dfa565b604051808215151515815260200191505060405180910390f35b34801561038157600080fd5b506103b6600480360381019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610fc6565b6040518082815260200191505060405180910390f35b3480156103d857600080fd5b506103e1610fde565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b34801561042f57600080fd5b50610438611004565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561047857808201518184015260208101905061045d565b50505050905090810190601f1680156104a55780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b3480156104bf57600080fd5b506104fe600480360381019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803590602001909291905050506110a2565b005b34801561050c57600080fd5b50610541600480360381019080803573ffffffffffffffffffffffffffffffffffffffff16906020019092919050505061132f565b6040518082815260200191505060405180910390f35b34801561056357600080fd5b5061058260048036038101908080359060200190929190505050611347565b604051808215151515815260200191505060405180910390f35b3480156105a857600080fd5b506105fd600480360381019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050611513565b6040518082815260200191505060405180910390f35b34801561061f57600080fd5b50610654600480360381019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050611538565b6040518082815260200191505060405180910390f35b60008054600181600116156101000203166002900480601f0160208091040260200160405190810160405280929190818152602001828054600181600116156101000203166002900480156107005780601f106106d557610100808354040283529160200191610700565b820191906000526020600020905b8154815290600101906020018083116106e357829003601f168201915b505050505081565b6000808211151561071857600080fd5b81600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506001905092915050565b60035481565b6000808373ffffffffffffffffffffffffffffffffffffffff1614156107ce57600080fd5b6000821115156107dd57600080fd5b81600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054101561082957600080fd5b600560008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020540110156108b657600080fd5b600760008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482111561093f57600080fd5b610988600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205483611581565b600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550610a14600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361159a565b600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550610add600760008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205483611581565b600760008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055508273ffffffffffffffffffffffffffffffffffffffff168473ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef846040518082815260200191505060405180910390a3600190509392505050565b600260009054906101000a900460ff1681565b600460009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16141515610c3c57600080fd5b600460009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166108fc829081150290604051600060405180830381858888f19350505050158015610ca4573d6000803e3d6000fd5b5050565b600081600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020541015610cf657600080fd5b600082111515610d0557600080fd5b610d4e600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205483611581565b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550610d9d60035483611581565b6003819055503373ffffffffffffffffffffffffffffffffffffffff167fcc16f5dbb4873280815c1ee09dbd06736cffcc184412cf7a71a0fdb75d397ca5836040518082815260200191505060405180910390a260019050919050565b600081600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020541015610e4857600080fd5b600082111515610e5757600080fd5b610ea0600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205483611581565b600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550610f2c600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361159a565b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055503373ffffffffffffffffffffffffffffffffffffffff167f2cfce4af01bcb9d6cf6c84ee1b7c491100b8695368264146a94d71e10a63083f836040518082815260200191505060405180910390a260019050919050565b60056020528060005260406000206000915090505481565b600460009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b60018054600181600116156101000203166002900480601f01602080910402602001604051908101604052809291908181526020018280546001816001161561010002031660029004801561109a5780601f1061106f5761010080835404028352916020019161109a565b820191906000526020600020905b81548152906001019060200180831161107d57829003601f168201915b505050505081565b60008273ffffffffffffffffffffffffffffffffffffffff1614156110c657600080fd5b6000811115156110d557600080fd5b80600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054101561112157600080fd5b600560008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205481600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020540110156111ae57600080fd5b6111f7600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482611581565b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550611283600560008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548261159a565b600560008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055508173ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef836040518082815260200191505060405180910390a35050565b60066020528060005260406000206000915090505481565b600081600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054101561139557600080fd5b6000821115156113a457600080fd5b6113ed600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205483611581565b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550611479600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548361159a565b600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055503373ffffffffffffffffffffffffffffffffffffffff167ff97a274face0b5517365ad396b1fdba6f68bd3135ef603e44272adba3af5a1e0836040518082815260200191505060405180910390a260019050919050565b6007602052816000526040600020602052806000526040600020600091509150505481565b6000600560008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020549050919050565b600061158f838311156115c4565b818303905092915050565b60008082840190506115ba8482101580156115b55750838210155b6115c4565b8091505092915050565b8015156115d057600080fd5b505600a165627a7a7230582007a730a96c00e2baa7635d39087613750cf3c76861b19dcfe03ef6b7634bd8e50029")

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, codebin,
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      val abiString = "[{\"constant\":true,\"inputs\":[],\"name\":\"name\",\"outputs\":[{\"name\":\"\",\"type\":\"string\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_spender\",\"type\":\"address\"},{\"name\":\"_value\",\"type\":\"uint256\"}],\"name\":\"approve\",\"outputs\":[{\"name\":\"success\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"totalSupply\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_from\",\"type\":\"address\"},{\"name\":\"_to\",\"type\":\"address\"},{\"name\":\"_value\",\"type\":\"uint256\"}],\"name\":\"transferFrom\",\"outputs\":[{\"name\":\"success\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"decimals\",\"outputs\":[{\"name\":\"\",\"type\":\"uint8\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"amount\",\"type\":\"uint256\"}],\"name\":\"withdrawEther\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_value\",\"type\":\"uint256\"}],\"name\":\"burn\",\"outputs\":[{\"name\":\"success\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_value\",\"type\":\"uint256\"}],\"name\":\"unfreeze\",\"outputs\":[{\"name\":\"success\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[{\"name\":\"\",\"type\":\"address\"}],\"name\":\"balanceOf\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"owner\",\"outputs\":[{\"name\":\"\",\"type\":\"address\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"symbol\",\"outputs\":[{\"name\":\"\",\"type\":\"string\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_to\",\"type\":\"address\"},{\"name\":\"_value\",\"type\":\"uint256\"}],\"name\":\"transfer\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[{\"name\":\"\",\"type\":\"address\"}],\"name\":\"freezeOf\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_value\",\"type\":\"uint256\"}],\"name\":\"freeze\",\"outputs\":[{\"name\":\"success\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[{\"name\":\"\",\"type\":\"address\"},{\"name\":\"\",\"type\":\"address\"}],\"name\":\"allowance\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_addr\",\"type\":\"address\"}],\"name\":\"getBalance\",\"outputs\":[{\"name\":\"balance\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"inputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"constructor\"},{\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"fallback\"},{\"anonymous\":false,\"inputs\":[{\"indexed\":true,\"name\":\"from\",\"type\":\"address\"},{\"indexed\":true,\"name\":\"to\",\"type\":\"address\"},{\"indexed\":false,\"name\":\"value\",\"type\":\"uint256\"}],\"name\":\"Transfer\",\"type\":\"event\"},{\"anonymous\":false,\"inputs\":[{\"indexed\":true,\"name\":\"from\",\"type\":\"address\"},{\"indexed\":false,\"name\":\"value\",\"type\":\"uint256\"}],\"name\":\"Burn\",\"type\":\"event\"},{\"anonymous\":false,\"inputs\":[{\"indexed\":true,\"name\":\"from\",\"type\":\"address\"},{\"indexed\":false,\"name\":\"value\",\"type\":\"uint256\"}],\"name\":\"Freeze\",\"type\":\"event\"},{\"anonymous\":false,\"inputs\":[{\"indexed\":true,\"name\":\"from\",\"type\":\"address\"},{\"indexed\":false,\"name\":\"value\",\"type\":\"uint256\"}],\"name\":\"Unfreeze\",\"type\":\"event\"}]"

      val wfewefewfwef = _acct1.publicKey.pubKeyHash  // c98bae4cc033c7e7bce053ebaa926bd61c120454
      val fwefwefwefwf = _acct2.publicKey.pubKeyHash  // 2ee607c3ed304353dd8a2d16636b46bd91d11152

      //  f8b2cb4f  getBalance
      //  a9059cbb  transfer

      val totalSupply = BigInt("98765000000000000000000000000000", 16)

      // getBalance of _acct1
      //var txData = BinaryData("f8b2cb4f000000000000000000000000c98bae4cc033c7e7bce053ebaa926bd61c120454")
      var txData = Abi.fromJson(abiString).encode(s"getBalance('${_acct1.publicKey.pubKeyHash.address}')")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        1, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == totalSupply)

      // getBalance of _acct2
      txData = Abi.fromJson(abiString).encode(s"getBalance('${_acct2.publicKey.pubKeyHash.address}')")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        2, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).longValue == 0)

      // transfer 501 from 1 to 2
      txData = Abi.fromJson(abiString).encode(s"transfer('${_acct2.publicKey.pubKeyHash.address}', 501)")
      //assert(txData sameElements BinaryData("a9059cbb0000000000000000000000002ee607c3ed304353dd8a2d16636b46bd91d1115200000000000000000000000000000000000000000000000000000000000001f5"))
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        3, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      var receipt = chain.getReceipt(tx.id()).get

      // getBalance of _acct1
      txData = Abi.fromJson(abiString).encode(s"getBalance('${_acct1.publicKey.pubKeyHash.address}')")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        4, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      val wefwef = chain.getReceipt(tx.id())
      var ercBalance1 = totalSupply - BigInt(501)
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == ercBalance1)

      // getBalance of _acct2
      txData = Abi.fromJson(abiString).encode(s"getBalance('${_acct2.publicKey.pubKeyHash.address}')")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        5, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == BigInt(501))

      //      //https://etherscan.io/tx/0x41bdc6c1fd6cd97acc37f97714f24fe328dcaaadd546c7981f77f0290a0868b6
      //      txData = BinaryData("a9059cbb000000000000000000000000df9c09d8e32864103ad0b157dfca613a222c51db000000000000000000000000000000000000000000000003cd346237eb1b6400")
      //      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
      //        contractAddress, "", FixedNumber.Zero,
      //        6, txData, FixedNumber(0), 9000000L, BinaryData.empty)
      //      assert(chain.addTransaction(tx))
      //      receipt = chain.getReceipt(tx.id()).get    //  gas should 37642

      // burn 1001
      txData = Abi.fromJson(abiString).encode(s"burn(1001)")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        6, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))

      ercBalance1 = ercBalance1 - BigInt(1001)

      // freeze 700
      txData = Abi.fromJson(abiString).encode(s"freeze(700)")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        7, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))

      ercBalance1 = ercBalance1 - BigInt(700)


      // getBalance of _acct1
      txData = Abi.fromJson(abiString).encode(s"getBalance('${_acct1.publicKey.pubKeyHash.address}')")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        8, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == ercBalance1)

      // unfreeze 500
      txData = Abi.fromJson(abiString).encode(s"unfreeze(500)")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        9, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))

      ercBalance1 = ercBalance1 + BigInt(500)

      // getBalance of _acct1
      txData = Abi.fromJson(abiString).encode(s"getBalance('${_acct1.publicKey.pubKeyHash.address}')")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        10, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == ercBalance1)


      // approve
      txData = Abi.fromJson(abiString).encode(s"approve('${_acct3.publicKey.pubKeyHash.address}', 2000)")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        11, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))


      // 3 tran from 1 to 4    2000
      txData = Abi.fromJson(abiString).encode(s"transferFrom('${_acct1.publicKey.pubKeyHash.address}', '${_acct4.publicKey.pubKeyHash.address}', 2000)")
      tx = new Transaction(TransactionType.Call, _acct3.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        0, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))

      ercBalance1 = ercBalance1 - BigInt(2000)

      // getBalance of _acct1
      txData = Abi.fromJson(abiString).encode(s"getBalance('${_acct1.publicKey.pubKeyHash.address}')")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        12, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == ercBalance1)

      // getBalance of _acct4
      txData = Abi.fromJson(abiString).encode(s"getBalance('${_acct4.publicKey.pubKeyHash.address}')")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        13, txData, FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == BigInt(2000))



//      // unfreeze 400  throw
//      txData = Abi.fromJson(abiString).encode(s"unfreeze(400)")
//      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
//        contractAddress, "", FixedNumber.Zero,
//        11, txData, FixedNumber(0), 9000000L, BinaryData.empty)
//      assert(chain.addTransaction(tx))
//

    }
    finally {
      chain.close()
    }
  }

  @Test
  def testGAS(): Unit = {
    val chain = createChain("testGAS")
    try {
      val contractSrc = "pragma solidity ^0.5.2;\n\ncontract supportPay {\n\n\n  function test(uint a) public returns (uint256) {  \n     uint sum = 0;\n  \n     for (uint i = 1; i <= 100; i++) {\n        sum += a;     \n     }      \n     return sum;\n  }\n\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("supportPay").abi
      val binString = result.getContract("supportPay").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      assert(chain.getBalance(_acct1).get == FixedNumber.fromDecimal(123.12))

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      assert(chain.getAccount(_acct1).get.nextNonce == 1)
      var balance1 = chain.getBalance(_acct1).get

      var txData = Abi.fromJson(abiString).encode(s"test(2)")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.Zero,
        1, txData, FixedNumber(100000),
        BigInt(28000), // require gas 29083
        BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.error.contains("Not enough gas"))

      assert(chain.getAccount(_acct1).get.nextNonce == 2)
      assert(chain.getBalance(_acct1).get == balance1 - FixedNumber(100000) * FixedNumber(28000))

      val efwefwef = chain.getReceipt(tx.id()).get

      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.One,
        2, BinaryData.empty, FixedNumber(100000),
        BigInt(28000), // require gas 29083
        BinaryData.empty)
      assert(chain.addTransaction(tx))
    }
    finally {
      chain.close()
    }
  }

  @Test
  def testTransfer1(): Unit = {
    val chain = createChain("testTransfer1")
    try {
        //      pragma solidity >=0.4.0 <0.7.0;
        //
        //      contract test3 {
        //
        //        function() external payable { }
        //
        //        function get3(uint a) public  {
        //          for (uint i = 1; i <= a; i++) {
        //            msg.sender.transfer(1);
        //          }
        //        }
        //      }
      val contractSrc = "pragma solidity >=0.4.0 <0.7.0;\n\ncontract test3 {\n\n  function() external payable { }\n\n  function get3(uint a) public  {\n     for (uint i = 1; i <= a; i++) {\n       msg.sender.transfer(1);\n     } \n  }  \n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("test3").abi
      val binString = result.getContract("test3").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      assert(chain.getBalance(_acct1).get == FixedNumber.fromDecimal(123.12))

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))
      assert(chain.getAccount(_acct1).get.nextNonce == 1)
      var balance1 = chain.getBalance(_acct1).get

      val contractAddr = contractAddress

      // transefer 1.2 coin, no call function
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.fromDecimal(1.2),
        1, BinaryData.empty, FixedNumber(1),9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      var receipt = chain.getReceipt(tx.id()).get
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.gasUsed == BigInt(21040L))
      assert(chain.getAccount(_acct1).get.nextNonce == 2)
      assert(chain.getBalance(contractAddr).get == FixedNumber.fromDecimal(1.2))
      assert(chain.getBalance(_acct1).get == balance1 - FixedNumber.fromDecimal(1.2) - FixedNumber(21040) )
      balance1 = chain.getBalance(_acct1).get


      var txData = Abi.fromJson(abiString).encode(s"get3(9)")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        2, txData, FixedNumber(1),9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      receipt = chain.getReceipt(tx.id()).get
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.gasUsed == BigInt(89776L))
      assert(chain.getAccount(_acct1).get.nextNonce == 3)
      assert(chain.getBalance(contractAddr).get == FixedNumber.fromDecimal(BigDecimal("1.199999999999999991")))
      assert(chain.getBalance(_acct1).get == balance1 + FixedNumber.fromDecimal(BigDecimal("0.000000000000000009")) - FixedNumber(89776) )

    }
    finally {
      chain.close()
    }
  }

  @Test
  def testTransfer2(): Unit = {
    val chain = createChain("testTransfer2")
    try {
      //      pragma solidity >=0.4.0 <0.7.0;
      //
      //      contract test3 {
      //
      //        function() external payable { }
      //
      //        function get3(uint a) public  {
      //          for (uint i = 1; i <= a; i++) {
      //            msg.sender.transfer(1);
      //          }
      //        }
      //      }
      val contractSrc = "pragma solidity >=0.4.0 <0.7.0;\n\ncontract test3 {\n\n  function() external payable { }\n\n  function get3(uint a) public  {\n     for (uint i = 1; i <= a; i++) {\n       msg.sender.transfer(1);\n     } \n  }  \n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("test3").abi
      val binString = result.getContract("test3").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      assert(chain.getBalance(_acct1).get == FixedNumber.fromDecimal(123.12))

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))
      assert(chain.getAccount(_acct1).get.nextNonce == 1)
      var balance1 = chain.getBalance(_acct1).get

      val contractAddr = contractAddress

      // transefer 1.2 coin, no call function
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.fromDecimal(1.2),
        1, BinaryData.empty, FixedNumber(1),9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      var receipt = chain.getReceipt(tx.id()).get
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.gasUsed == BigInt(21040L))
      assert(chain.getAccount(_acct1).get.nextNonce == 2)
      assert(chain.getBalance(contractAddr).get == FixedNumber.fromDecimal(1.2))
      assert(chain.getBalance(_acct1).get == balance1 - FixedNumber.fromDecimal(1.2) - FixedNumber(21040) )
      balance1 = chain.getBalance(_acct1).get

      var txData = Abi.fromJson(abiString).encode(s"get3(9)")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        2, txData, FixedNumber(1),
        89200L,    // require 89776
        BinaryData.empty)
      assert(chain.addTransaction(tx))
      receipt = chain.getReceipt(tx.id()).get
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.gasUsed == BigInt(89200))
      assert(chain.getAccount(_acct1).get.nextNonce == 3)
      assert(chain.getBalance(contractAddr).get == FixedNumber.fromDecimal(BigDecimal("1.2")))
      assert(chain.getBalance(_acct1).get == balance1 - FixedNumber(89200))

    }
    finally {
      chain.close()
    }
  }

  @Test
  def testTimeout1(): Unit = {
    val chain = createChain("testTimeout1")
    try {
      //      pragma solidity >=0.4.0 <0.7.0;
      //
      //      contract test3 {
      //
      //        function() external payable { }
      //
      //        function get3(uint a) public  {
      //          for (uint i = 1; i <= a; i++) {
      //            msg.sender.transfer(1);
      //          }
      //        }
      //      }
      val contractSrc = "pragma solidity >=0.4.0 <0.7.0;\n\ncontract test3 {\n\n  function() external payable { }\n\n  function get3(uint a) public  {\n     for (uint i = 1; i <= a; i++) {\n       msg.sender.transfer(1);\n     } \n  }  \n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("test3").abi
      val binString = result.getContract("test3").bin

      var nowTime = Instant.now.toEpochMilli
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      var producer = chain.getWitness(blockTime)
      sleepTo(blockTime - _produceInterval)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      assert(chain.getBalance(_acct1).get == FixedNumber.fromDecimal(123.12))

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))
      assert(chain.getAccount(_acct1).get.nextNonce == 1)
      var balance1 = chain.getBalance(_acct1).get

      val contractAddr = contractAddress

      // transefer 1.2 coin, no call function
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.fromDecimal(1.2),
        1, BinaryData.empty, FixedNumber(1),9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      var receipt = chain.getReceipt(tx.id()).get
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(chain.getReceipt(tx.id()).get.gasUsed == BigInt(21040L))
      assert(chain.getAccount(_acct1).get.nextNonce == 2)
      assert(chain.getBalance(contractAddr).get == FixedNumber.fromDecimal(1.2))
      assert(chain.getBalance(_acct1).get == balance1 - FixedNumber.fromDecimal(1.2) - FixedNumber(21040) )
      balance1 = chain.getBalance(_acct1).get

      chain.produceBlockFinalize()
      blockTime += _produceInterval
      producer = chain.getWitness(blockTime)
      sleepTo(blockTime - _produceInterval)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, blockTime - 10)

      println("=========== now test timeout tx =================")

      // get3(999999) make sure vm run long enough
      var txData = Abi.fromJson(abiString).encode(s"get3(999999)")
      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddr, FixedNumber.Zero,
        2, txData, FixedNumber(1), 9000000L, BinaryData.empty)

      sleepTo(blockTime - 110)  // allocate 100 ms to run this tx

      assert(!chain.addTransaction(tx))
      assert(chain.getAccount(_acct1).get.nextNonce == 2)
      assert(chain.getBalance(contractAddr).get == FixedNumber.fromDecimal(1.2))
      assert(chain.getBalance(_acct1).get == balance1)

    }
    finally {
      chain.close()
    }
  }

  @Test
  def testCALLVALUE(): Unit = {
    val chain = createChain("testCALLVALUE")
    try {
      /*
      *
      *   pragma solidity ^0.5.2;
          contract TestEth {
            function testE() public payable returns (uint256) {
               if(msg.value >= 10 ether) {  return 2;   }
               else {  return 3;   }
            }
          }
      *
      * */
      val contractSrc = "pragma solidity ^0.5.2;\n\ncontract TestEth {\n  function testE() public payable returns (uint256) {   \n     if(msg.value >= 10 ether) {  return 2;   }\n     else {  return 3;   }      \n  }\n}"
      val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
      val result = CompilationResult.parse(res.output)
      val abiString = result.getContract("TestEth").abi
      val binString = result.getContract("TestEth").bin

      var nowTime = Instant.now.toEpochMilli - 90000
      var blockTime = ProducerUtil.nextBlockTime(chain.getHeadTime(), nowTime, _produceInterval / 10, _produceInterval) //  chain.getHeadTime() + _consensusSettings.produceInterval
      blockTime += _produceInterval
      val producer = chain.getWitness(blockTime)
      chain.startProduceBlock(_miners.findPrivKey(producer).get, blockTime, Long.MaxValue)

      assert(chain.getBalance(_acct1).get == FixedNumber.fromDecimal(123.12))

      var deployTx = new Transaction(TransactionType.Deploy, _acct1.publicKey.pubKeyHash,
        UInt160.Zero, FixedNumber.Zero, 0, BinaryData(binString),
        FixedNumber(1), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      assert(chain.getAccount(_acct1).get.nextNonce == 1)
      var balance1 = chain.getBalance(_acct1).get

      var txData = Abi.fromJson(abiString).encode(s"testE()")
      var tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.fromDecimal(11),
        1, txData, FixedNumber(100000), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == BigInt(2))

      tx = new Transaction(TransactionType.Call, _acct1.publicKey.pubKeyHash,
        contractAddress, FixedNumber.fromDecimal(9),
        2, txData, FixedNumber(100000), 9000000L, BinaryData.empty)
      assert(chain.addTransaction(tx))
      assert(chain.getReceipt(tx.id()).get.status == 0)
      assert(DataWord.of(chain.getReceipt(tx.id()).get.output).value == BigInt(3))
    }
    finally {
      chain.close()
    }
  }


  def sleepTo(time: Long) = {
    val nowTime = Instant.now.toEpochMilli
    if (time > nowTime)
      Thread.sleep(time - nowTime)
  }

}

object ContractTxTest {

  @AfterClass
  def cleanUp: Unit = {
    println("clean Directory")
    Directory("ContractTxTest").deleteRecursively()
  }
}
