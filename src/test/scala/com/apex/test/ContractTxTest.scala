package com.apex.test

import java.time.Instant

import com.apex.consensus.ProducerUtil
import com.apex.core._
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, FixedNumber, MerkleTree, UInt160, UInt256}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.settings._
import com.apex.solidity.Abi
import com.apex.vm.DataWord
import org.junit.{AfterClass, Test}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.io.Directory

//======
//1
//priv key raw:           108fd85be78e0dcc96b93c9c53c30281115ee453e9c472654fad985dcd7b91db
//priv key WIF format:    KwmuSp41VWBtGSWaQQ82ZRRSFzkJVTAyuDLQ9NzP9CPqLWirh4UQ
//pub key (compressed):   024f3076ac9b2b8ef8bf1c303c3ac3057472c9fa391df62c543789f34017168c62
//pub key hash160:        c98bae4cc033c7e7bce053ebaa926bd61c120454
//Address:                APLLBNZjMq4tFANSNHsEoXwGJ7NVfi4BxUa
//======
//2
//priv key raw:           ad28867b93d8be8558d61ee58e971117142aefd28ebb54bf4f20792bfb7fab25
//priv key WIF format:    L32JpLopG2hWjEMSCkAjS1nUnPixVrDTPqFAGYbddQrtUjRfkjEP
//pub key (compressed):   02add2d02786ba350148aee109e67495d6c4c2dccd9b5aaa57ad866d7b6105ac8f
//pub key hash160:        2ee607c3ed304353dd8a2d16636b46bd91d11152
//Address:                AP6EUsRV9DCvbywpDFLXS3JhjRryFPe9Qo8
//======
//3
//priv key raw:           43427dd5def74e97ab0409eaec3d64aff0557707a6edc1004d1c6e08ea705b45
//priv key WIF format:    KyUTLv2BeP9SJD6Sa8aHBVmuRkgw9eThjNGJDE4PySEgf2TvCQCn
//pub key (compressed):   03d7a23792639a9824d1013151711b521653f0c72563d69481bd914db75b03309d
//pub key hash160:        00d08ce14267fbce154d2983b8723dd5ad2addd5
//Address:                AP22p497htDs1SKXHjwyCgrYuFtTgcP5F5w
//======
//4
//priv key raw:           adc2a556a1a1726ecce71eb38e306914af4d82f547a545eed677ba555409932f
//priv key WIF format:    L33Uh9L35pSoEqBPP43U6rQcD2xMpJ7F4b3QMjUMAL6HZhxUqEGq
//pub key (compressed):   03f5bb4aa2a0773d658e9df51865ffbfd41f98891bd1994337afae4dc27c1d1927
//pub key hash160:        da55e4f6f216187259e7bc56cbb1587edc5156b4
//Address:                APMrxRgE3ZBqw9nC249175AvjUcmYMCq7XX
//======
//5
//priv key raw:           2cd68534541d6babbc8edb7a15036f8cee5f3506d667aecc022ffa1ea1b1b269
//priv key WIF format:    KxisR46MUfkekvgfuuydTD91avsjxhoqs5S6Ech2uiG21RDUEbna
//pub key (compressed):   03300f032f1d130956f0ff994cbd52cab3fc4167c0f6e96f1b5714da10ed51c1bd
//pub key hash160:        63d15a28a09748540eca07aeabf3b831b3056ebe
//Address:                APB4Hur58nvWWHuSE5qNQFWYPjZaKZetFAe
//======
//6
//priv key raw:           0c165d5428409eef6e025d6fe827b3b0f595f5a1cc4051c9ff43a7f1e20fed56
//priv key WIF format:    KwdCy3jFbU5MK4ZMbgKLTDkenhQgCU44hevVVXAv7ZUZRYJypqCB
//pub key (compressed):   034a79dcbd7b6da30739516f00e0be2e8c511a9ee446cc7362b8a441504ec0e7bf
//pub key hash160:        3a01649c803b08d4a3522c3ca49dedf687d576f9
//Address:                AP7FD5j2aPj83dmb3xSJZVVJ1JwNLry74xa
//======
//7
//priv key raw:           29e286f51e12df09f46b451d606650a0cce4b0d6d42a5d2e5450ab7c8b58a2c3
//priv key WIF format:    Kxd8UDvptjG4AcT7k2ULe8tVkmfK5naXKc3gzrvRtg6msGLA3xLY
//pub key (compressed):   03a549ab71a681d09bd090ed67bad589e55b582a2bbbdf3fb7d68ad3a71bfee211
//pub key hash160:        3252379053bf9f8827896cf0a5ff2cbd6fdca06f
//Address:                AP6YaVweecsk2sLRpSdUEwiQ2yE9WvitBtr
//======

@Test
class ContractTxTest {  

  Directory("ContractTxTest").deleteRecursively()

  val _produceInterval = 2000

  val _minerAward: Double = 12.3

  val _witness1 = Witness("init1",
    PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8"),
    Some(new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))))
  val _witness2 = Witness("init2",
    PublicKey("03c3333373adc0636b1d67d4bca3d8b34a53d663698119369981e67866250d3a74"),
    Some(new PrivateKey(BinaryData("cc7b7fa6e706944fa2d75652065f95ef2f364316e172601320655aac0e648165"))))
  val _witness3 = Witness("init3",
    PublicKey("020550de6ce7ed53ff018cccf1095893edba43f798252d6983e0fd2ca5af3ee0da"),
    Some(new PrivateKey(BinaryData("db71fe7c0ac4ca3e8cef95bf55cf535eaa8fe0c80d18e0cb19af8d7071b8a184"))))
  val _witness4 = Witness("init4",  // APPnx5YahVg1dTgeWkp1fE33ftvAaGbeQaR  L2C4Za8VSx2iBgszQarHx4YzqHvfumkHjbi6bNqvqst6mc8QcuZ7
    PublicKey("0246f896de22582786884d7d7ae27ef00cc8fed167bcdb8c305fbbc3dd9cca696c"),
    Some(new PrivateKey(BinaryData("9456beec947b368eda4be03f6c306703d9b2eda49f661285944b4e1f07ae18f3"))))

  val _consensusSettings = ConsensusSettings(_produceInterval, 500, 1, Array(_witness1, _witness2, _witness3, _witness4))

  val _acct1 = Ecdsa.PrivateKey.fromWIF("KwmuSp41VWBtGSWaQQ82ZRRSFzkJVTAyuDLQ9NzP9CPqLWirh4UQ").get
  val _acct2 = Ecdsa.PrivateKey.fromWIF("L32JpLopG2hWjEMSCkAjS1nUnPixVrDTPqFAGYbddQrtUjRfkjEP").get
  val _acct3 = Ecdsa.PrivateKey.fromWIF("KyUTLv2BeP9SJD6Sa8aHBVmuRkgw9eThjNGJDE4PySEgf2TvCQCn").get
  val _acct4 = Ecdsa.PrivateKey.fromWIF("L33Uh9L35pSoEqBPP43U6rQcD2xMpJ7F4b3QMjUMAL6HZhxUqEGq").get

  private val minerCoinFrom = PublicKey(BinaryData("02866facba8742cd702b302021a9588e78b3cd96599a3b1c85688d6dc0a72585e6"))

  private def makeTx(from: PrivateKey,
                     to: UInt160,
                     amount: FixedNumber,
                     nonce: Long,
                     txType: TransactionType.Value = TransactionType.Transfer) = {

    val tx = new Transaction(txType, from.publicKey, to, "",
      amount, nonce, BinaryData.empty, FixedNumber.Zero, 0, BinaryData.empty)
    tx.sign(from)
    tx
  }

  private def makeBlock(preBlock: Block,
                        txs: Seq[Transaction],
                        award: Double = _minerAward): Block = {
    val blockTime = preBlock.header.timeStamp + _consensusSettings.produceInterval
    val miner = ProducerUtil.getWitness(blockTime, _consensusSettings)

    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
      miner.pubkey.pubKeyHash, "", FixedNumber.fromDecimal(award),
      preBlock.height + 1,
      BinaryData(Crypto.randomBytes(8)), // add random bytes to distinct different blocks with same block index during debug in some cases
      FixedNumber.Zero, 0, BinaryData.empty)

    val allTxs = ArrayBuffer.empty[Transaction]

    allTxs.append(minerTx)
    txs.foreach(allTxs.append(_))

    val header: BlockHeader = BlockHeader.build(preBlock.header.index + 1,
      blockTime, MerkleTree.root(allTxs.map(_.id)),
      preBlock.id(), miner.pubkey, miner.privkey.get)

    Block.build(header, allTxs)
  }

  private def makeBlockByTime(preBlock: Block,
                        //txs: Seq[Transaction],
                         blockTime: Long): Block = {
    //val blockTime = preBlock.header.timeStamp + _consensusSettings.produceInterval
    val miner = ProducerUtil.getWitness(blockTime, _consensusSettings)

    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
      miner.pubkey.pubKeyHash, "", FixedNumber.fromDecimal(_minerAward),
      preBlock.height + 1,
      BinaryData(Crypto.randomBytes(8)), // add random bytes to distinct different blocks with same block index during debug in some cases
      FixedNumber.Zero, 0, BinaryData.empty)

    val allTxs = ArrayBuffer.empty[Transaction]

    allTxs.append(minerTx)
    //txs.foreach(allTxs.append(_))

    val header: BlockHeader = BlockHeader.build(preBlock.header.index + 1,
      blockTime, MerkleTree.root(allTxs.map(_.id)),
      preBlock.id(), miner.pubkey, miner.privkey.get)

    Block.build(header, allTxs)
  }

  private def createChain(path: String): LevelDBBlockchain = {
    val baseDir = s"ContractTxTest/$path"
    val chainSetting = ChainSettings(
      BlockBaseSettings(s"$baseDir/block", false, 0),
      DataBaseSettings(s"$baseDir/data", false, 0),
      ForkBaseSettings(s"$baseDir/fork", false, 0),
      _minerAward,
      GenesisSettings(Instant.EPOCH,
        "03b4534b44d1da47e4b4a504a210401a583f860468dec766f507251a057594e682",
        "7a93d447bffe6d89e690f529a3a0bdff8ff6169172458e04849ef1d4eafd7f86",
        Array(CoinAirdrop(_acct1.publicKey.address, 123.12),
          CoinAirdrop(_acct2.publicKey.address, 234.2))
      )
    )

    Blockchain.populate(chainSetting, _consensusSettings, Notification())
  }

  @Test
  def testCreateChain(): Unit = {
    val chain = createChain("testCreateChain")
    try {

      assert(chain.getHeight() == 0)

      val balance1 = chain.getBalance(_acct1.publicKey.pubKeyHash)
      assert(balance1.get == FixedNumber.fromDecimal(123.12))

      val balance2 = chain.getBalance(_acct2.publicKey.pubKeyHash)
      assert(balance2.get == FixedNumber.fromDecimal(234.2))

      var blockTime = chain.getHeadTime() + _consensusSettings.produceInterval
      chain.startProduceBlock(ProducerUtil.getWitness(blockTime, _consensusSettings), blockTime)

      assert(chain.isProducingBlock())


      val codebin = BinaryData("608060405234801561001057600080fd5b5060e68061001f6000396000f3fe6080604052600436106043576000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11460485780636d4ce63c14607f575b600080fd5b348015605357600080fd5b50607d60048036036020811015606857600080fd5b810190808035906020019092919050505060a7565b005b348015608a57600080fd5b50609160b1565b6040518082815260200191505060405180910390f35b8060008190555050565b6000805490509056fea165627a7a723058202c7cfe05b5e1b84938fa70727102e914fba062d91fde5a0f0a92613ad081732b0029")

      val deployTx = new Transaction(TransactionType.Deploy, minerCoinFrom,
        UInt160.Zero, "", FixedNumber.fromDecimal(_minerAward),
        3,
        codebin,
        FixedNumber(1), 99999999L, BinaryData.empty)
      assert(chain.addTransaction(deployTx))

      val settt = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"withdraw_amount\",\"type\":\"uint256\"}],\"name\":\"set\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"get\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]").encode("set(123)")
      val setTx = new Transaction(TransactionType.Call, minerCoinFrom,
        UInt160.fromBytes(BinaryData("43d4118a551815ec937380219e3bf5057316376e")), "", FixedNumber.fromDecimal(_minerAward),
        4,
        settt,
        FixedNumber(1), 99999999L, BinaryData.empty)
      assert(chain.addTransaction(setTx))

      val gettt = Abi.fromJson("[{\"constant\":false,\"inputs\":[{\"name\":\"withdraw_amount\",\"type\":\"uint256\"}],\"name\":\"set\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"get\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]").encode("get()")
      val getTx = new Transaction(TransactionType.Call, minerCoinFrom,
        UInt160.fromBytes(BinaryData("43d4118a551815ec937380219e3bf5057316376e")), "", FixedNumber.fromDecimal(_minerAward),
        5,
        gettt,
        FixedNumber(1), 99999999L, BinaryData.empty)
      assert(chain.addTransaction(getTx))

      val receipt = chain.getReceipt(getTx.id()).get

      assert(DataWord.of(receipt.output).longValue == 123)


    }
    finally {
      chain.close()
    }
  }

}

object ContractTxTest {
  @AfterClass
  def cleanUp: Unit = {
    println("clean Directory")
    Directory("ContractTxTest").deleteRecursively()
  }
}
