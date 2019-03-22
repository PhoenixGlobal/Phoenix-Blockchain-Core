package com.apex.core

import java.time.Instant

import com.apex.common.ApexLogging
import com.apex.consensus.Vote
import com.apex.consensus.WitnessList
import com.apex.consensus.{ProducerUtil, WitnessInfo}
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKeyHash}
import com.apex.crypto.{BinaryData, Crypto, FixedNumber, MerkleTree, UInt160, UInt256}
import com.apex.settings.{ChainSettings, ConsensusSettings, RuntimeParas}
import com.apex.vm.GasCost
import play.api.libs.json.{JsValue, Json, Writes}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class ChainInfo(id: String)


class PendingState {
  var producerPrivKey: PrivateKey = _
  var blockTime: Long = _
  var startTime: Long = _
  var blockIndex: Long = _
  var stopProcessTxTime: Long = _
  var isProducingBlock = false

  val txs = ArrayBuffer.empty[Transaction]

  def set(producerPrivKey: PrivateKey, blockTime: Long, stopProcessTxTime: Long, blockIndex: Long) = {
    this.producerPrivKey = producerPrivKey
    this.blockTime = blockTime
    this.stopProcessTxTime = stopProcessTxTime
    this.startTime = Instant.now.toEpochMilli
    this.blockIndex = blockIndex
  }
}

case class AddTxResult(added: Boolean, result: String)

object AddTxResult {
  implicit val resultWrites = new Writes[AddTxResult] {
    override def writes(o: AddTxResult): JsValue = Json.obj(
      "added" -> o.added,
      "result" -> o.result
    )
  }
}

class Blockchain(chainSettings: ChainSettings,
                 consensusSettings: ConsensusSettings,
                 runtimeParas: RuntimeParas,
                 notification: Notification) extends Iterable[Block] with ApexLogging {

  log.info("Blockchain starting")

  private val genesisProducerPrivKey = new PrivateKey(BinaryData(chainSettings.genesis.privateKey))

  log.info("creating BlockBase")

  private val blockBase = new BlockBase(chainSettings.blockBase)

  log.info("creating DataBase")

  private val dataBase = new DataBase(chainSettings.dataBase)

  log.info("creating ForkBase")

  private val forkBase = new ForkBase(
    chainSettings.forkBase,
    //consensusSettings.initialWitness,
    onConfirmed,
    onSwitch)

  log.info("creating Genesis Block")

  private val minerCoinFrom = UInt160.Zero

  private val minerAward = FixedNumber.fromDecimal(chainSettings.minerAward)

  private val genesisBlock: Block = buildGenesisBlock()

  private val unapplyTxs = mutable.LinkedHashMap.empty[UInt256, Transaction]

  private var timeoutTx: Option[Transaction] = None

  private val pendingState = new PendingState

  private var mCurWitnessList: Option[WitnessList] = None
  private var mPrevWitnessList: Option[WitnessList] = None
  private var mPendingWitnessList: Option[WitnessList] = None

  private val DAY: Long = 1000 * 24 * 60 * 60

  populate()

  private def buildGenesisBlock(): Block = {
    val genesisTxs = ArrayBuffer.empty[Transaction]

    chainSettings.genesis.genesisCoinAirdrop.foreach(airdrop => {
      genesisTxs.append(new Transaction(TransactionType.Miner, minerCoinFrom,
        PublicKeyHash.fromAddress(airdrop.addr).get, FixedNumber.fromDecimal(airdrop.coins),
        0, consensusSettings.fingerprint(), FixedNumber.MinValue, 0, BinaryData.empty))
    })

    val genesisBlockHeader: BlockHeader = BlockHeader.build(0,
      chainSettings.genesis.timeStamp.toEpochMilli, MerkleTree.root(genesisTxs.map(_.id)),
      UInt256.Zero, genesisProducerPrivKey)

    Block.build(genesisBlockHeader, genesisTxs)
  }

  def Id: String = genesisBlock.id.toString

  override def iterator: Iterator[Block] = new BlockchainIterator(this)

  def close() = {
    log.info("blockchain closing")
    blockBase.close()
    dataBase.close()
    forkBase.close()
    log.info("blockchain closed")
  }

  def getChainInfo(): ChainInfo = {
    ChainInfo(genesisBlock.id.toString)
  }

  def getHeight(): Long = {
    forkBase.head.map(_.block.height).getOrElse(genesisBlock.height)
  }

  def getHeadTime(): Long = {
    forkBase.head.map(_.block.timeStamp).getOrElse(0)
  }

  def getLatestHeader(): BlockHeader = {
    forkBase.head.map(_.block.header).getOrElse(genesisBlock.header)
  }

  def getConfirmedHeader(): BlockHeader = blockBase.head().get

  def getConfirmedHeight(): Long = getConfirmedHeader.index

  def headTimeSinceGenesis(): Long = {
    getLatestHeader.timeStamp - genesisBlock.header.timeStamp
  }

  def getHeader(id: UInt256): Option[BlockHeader] = {
    forkBase.get(id).map(_.block.header).orElse(blockBase.getBlock(id).map(_.header))
  }

  def getHeader(height: Long): Option[BlockHeader] = {
    forkBase.get(height).map(_.block.header).orElse(blockBase.getBlock(height).map(_.header))
  }

  def getNextBlockId(id: UInt256): Option[UInt256] = {
    var target: Option[UInt256] = None
    val block = getBlock(id)
    if (block.isDefined) {
      val nextBlock = getBlock(block.get.height() + 1)
      if (nextBlock.isDefined)
        target = Some(nextBlock.get.id())
    }
    if (target == None) {
      target = forkBase.getNext(id)
    }
    target
  }

  def getBlock(id: UInt256): Option[Block] = {
    forkBase.get(id).map(_.block).orElse(blockBase.getBlock(id))
  }

  def getBlock(height: Long): Option[Block] = {
    forkBase.get(height).map(_.block).orElse(blockBase.getBlock(height))
  }

  def containsBlock(id: UInt256): Boolean = {
    forkBase.contains(id) || blockBase.containBlock(id)
  }

  def blockIsConfirmed(id: UInt256): Boolean = {
    blockBase.containBlock(id)
  }

  def getTransactionFromMempool(txid: UInt256): Option[Transaction] = {
    pendingState.txs.find(tx => tx.id().equals(txid)).orElse(unapplyTxs.get(txid))
  }

  def getTransactionFromPendingTxs(txid: UInt256): Option[Transaction] = {
    pendingState.txs.find(tx => tx.id().equals(txid))
  }

  def getTransactionFromUnapplyTxs(txid: UInt256): Option[Transaction] = {
    unapplyTxs.get(txid)
  }

  def startProduceBlock(producerPrivKey: PrivateKey, blockTime: Long, stopProcessTxTime: Long): Unit = {
    require(!isProducingBlock())
    val producer = producerPrivKey.publicKey.pubKeyHash
    val forkHead = forkBase.head.get
    pendingState.set(producerPrivKey, blockTime, stopProcessTxTime, forkHead.block.height + 1)
    log.debug(s"start block at: ${pendingState.startTime}  blockTime=${blockTime}  stopProcessTxTime=${stopProcessTxTime}")

    val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
      producer, minerAward, forkHead.block.height + 1,
      BinaryData(Crypto.randomBytes(8)), // add random bytes to distinct different blocks with same block index during debug in some cases
      FixedNumber.MinValue, 0, BinaryData.empty)

    dataBase.startSession()

    val applied = applyTransaction(minerTx, producer, stopProcessTxTime, blockTime, forkHead.block.height + 1)
    require(applied.added)
    pendingState.txs.append(minerTx)
    pendingState.isProducingBlock = true

    if (timeoutTx.isDefined) {
      log.info(s"try again for the old timeout tx ${timeoutTx.get.id().shortString()}")
      addTransaction(timeoutTx.get)
      timeoutTx = None // set to None even if addTransaction fail or timeout again.
    }
    val scheduleTxs = dataBase.getAllScheduleTx()
    if (scheduleTxs.nonEmpty) {
      scheduleTxs.foreach(scheduleTx => {
        if (blockTime >= scheduleTx.executeTime && Instant.now.toEpochMilli < stopProcessTxTime) {
          println("schedule execute time is" + scheduleTx.executeTime + "block time   " + blockTime)
          if (applyTransaction(scheduleTx, producer, stopProcessTxTime, blockTime, forkHead.block.height + 1).added)
            pendingState.txs.append(scheduleTx)
        }
      })
    }
    val badTxs = ArrayBuffer.empty[Transaction]

    unapplyTxs.foreach(p => {
      if (Instant.now.toEpochMilli < stopProcessTxTime) {
        if (applyTransaction(p._2, producer, stopProcessTxTime, blockTime, forkHead.block.height + 1).added)
          pendingState.txs.append(p._2)
        else
          badTxs.append(p._2)
      }
    })
    pendingState.txs.foreach(tx => unapplyTxs.remove(tx.id))
    badTxs.foreach(tx => unapplyTxs.remove(tx.id))
  }

  def isProducingBlock(): Boolean = {
    pendingState.isProducingBlock
  }

  private def addTransactionToUnapplyTxs(tx: Transaction): AddTxResult = {
    if (!unapplyTxs.contains(tx.id)) {
      unapplyTxs += (tx.id -> tx)
      AddTxResult(true, "added to mempool, pending process")
    }
    else
      AddTxResult(false, "same tx already exist in mempool")
  }

  def addTransaction(tx: Transaction): Boolean = addTransactionEx(tx).added

  def addTransactionEx(tx: Transaction): AddTxResult = {
    var result = AddTxResult(false, "error")

    if (tx.txType == TransactionType.Miner || tx.txType == TransactionType.Refund || tx.txType == TransactionType.Schedule) {
      result = AddTxResult(false, "tx type invalid")
    }
    else {
      if (tx.gasLimit > runtimeParas.txAcceptGasLimit) {
        log.info(s"tx: ${tx.id()}, set too heigh gas-limit, it should not above ${runtimeParas.txAcceptGasLimit}")
        result = AddTxResult(false, s"set too heigh gas-limit, it should not above ${runtimeParas.txAcceptGasLimit}")
      }
      else {
        if (isProducingBlock()) {
          if (Instant.now.toEpochMilli > pendingState.stopProcessTxTime)
            result = addTransactionToUnapplyTxs(tx)
          else {
            result = applyTransaction(tx, pendingState.producerPrivKey.publicKey.pubKeyHash,
              pendingState.stopProcessTxTime, pendingState.blockTime, pendingState.blockIndex)
            if (result.added)
              pendingState.txs.append(tx)
          }
        }
        else
          result = addTransactionToUnapplyTxs(tx)
      }
    }
    if (result.added)
      notification.broadcast(AddTransactionNotify(tx))
    else
      log.error(s"addTransaction error, txid=${tx.id.toString}  ${result.result}")
    result
  }

  def produceBlockFinalize(): Option[Block] = {
    val endTime = Instant.now.toEpochMilli
    if (!isProducingBlock()) {
      log.info("block canceled")
      None
    } else {
      log.debug(s"block time: ${pendingState.blockTime}, end time: $endTime, produce time: ${endTime - pendingState.startTime}")
      val forkHead = forkBase.head.get
      val merkleRoot = MerkleTree.root(pendingState.txs.map(_.id))
      val timeStamp = pendingState.blockTime
      val header = BlockHeader.build(
        forkHead.block.height + 1, timeStamp, merkleRoot,
        forkHead.block.id, pendingState.producerPrivKey)
      val block = Block.build(header, pendingState.txs.clone)
      pendingState.txs.clear()
      pendingState.isProducingBlock = false
      if (tryInsertBlock(block, false)) {
        log.info(s"block #${block.height} ${block.shortId} produced by ${block.producer.shortAddr} ${block.header.timeString()}")
        notification.broadcast(NewBlockProducedNotify(block))
        Some(block)
      } else {
        None
      }
    }
  }

  private def stopProduceBlock() = {
    pendingState.txs.foreach(tx => {
      if (tx.txType != TransactionType.Miner)
        unapplyTxs += (tx.id -> tx)
    })
    pendingState.txs.clear()
    pendingState.isProducingBlock = false
    dataBase.rollBack()
  }

  def tryInsertBlock(block: Block, doApply: Boolean = true): Boolean = {
    var inserted = false
    if (isProducingBlock())
      stopProduceBlock()

    if (forkBase.head.get.block.id.equals(block.id)) {
      inserted = false // same as head
    }
    else if (forkBase.head.get.block.id.equals(block.prev())) {
      if (doApply == false) { // check first !
        require(forkBase.add(block, getWitnessList(block)))
        inserted = true
      }
      else if (applyBlock(block)) {
        require(forkBase.add(block, getWitnessList(block)))
        inserted = true
      }
      else
        log.error(s"block ${block.height} ${block.shortId} apply error")
      if (inserted) {
        notification.broadcast(BlockAddedToHeadNotify(blockSummary(block)))
        checkUpdateWitnessList(block)
        dataBase.commit()
      }
    }
    else {
      log.info(s"try add received block to minor fork chain. block ${block.height} ${block.shortId}")
      if (forkBase.add(block, getWitnessList(block)) && forkBase.contains(block.id))
        inserted = true
      else
        log.error(s"fail add block ${block.height} ${block.shortId} to minor fork chain")
    }
    if (inserted) {
      block.transactions.foreach(tx => {
        unapplyTxs.remove(tx.id)
        if (timeoutTx.isDefined && timeoutTx.get.id() == tx.id())
          timeoutTx = None
      })
    }
    inserted
  }

  private def blockSummary(block: Block): BlockSummary = {
    val summary = mutable.Map[UInt256, Option[TransactionReceipt]]()
    block.transactions.foreach(tx => {
      summary.put(tx.id, dataBase.getReceipt(tx.id))
    })

    new BlockSummary(block, summary)
  }

  private def getWitnessList(block: Block): WitnessList = {
    if (block.timeStamp() > getBlock(mPendingWitnessList.get.generateInBlock).get.timeStamp())
      mCurWitnessList.get
    else
      mPrevWitnessList.get
    // will not get mPendingWitnessList
  }

  private def checkUpdateWitnessList(curblock: Block) = {
    val pendingWitnessList = mPendingWitnessList.get
    if (blockIsConfirmed(pendingWitnessList.generateInBlock) &&
      isLastBlockOfProducer(curblock.timeStamp()) &&
      curblock.timeStamp() - getBlock(pendingWitnessList.generateInBlock).get.timeStamp() >= consensusSettings.electeTime) {

      log.info(s"block ${curblock.height()} applied, it's time to electe new producers")
      val currentWitness = mCurWitnessList.get
      val allWitnesses = dataBase.getAllWitness().filter(_.register)
      new WitnessList(allWitnesses.toArray, UInt256.Zero).logInfo("current all Witness")
      currentWitness.logInfo("setPreviousWitnessList")
      dataBase.setPreviousWitnessList(currentWitness)
      mPrevWitnessList = dataBase.getPreviousWitnessList()
      val allWitnessesMap: Map[UInt160, WitnessInfo] = allWitnesses.map(w => w.addr -> w).toMap
      val updatedCurrentWitness = ArrayBuffer.empty[WitnessInfo]
      currentWitness.witnesses.foreach(oldInfo => {
        val newInfo = allWitnessesMap.get(oldInfo.addr)
        if (newInfo.isDefined)
          updatedCurrentWitness.append(newInfo.get)
      })

      require(updatedCurrentWitness.size <= consensusSettings.witnessNum)

      var newElectedWitnesses = mutable.Map.empty[UInt160, WitnessInfo]
      if (updatedCurrentWitness.size == consensusSettings.witnessNum)
        newElectedWitnesses = WitnessList.removeLeastVote(updatedCurrentWitness.toArray)
      else
        newElectedWitnesses = mutable.Map(updatedCurrentWitness.map(w => w.addr -> w).toMap.toSeq: _*)

      require(newElectedWitnesses.size < consensusSettings.witnessNum)

      val allWitnessesSorted = WitnessList.sortByVote(allWitnesses.toArray)
      val allWitnessIterator = allWitnessesSorted.iterator
      while (allWitnessIterator.hasNext && newElectedWitnesses.size < consensusSettings.witnessNum) {
        val witness = allWitnessIterator.next()
        if (!newElectedWitnesses.contains(witness.addr))
          newElectedWitnesses.update(witness.addr, witness)
      }
      require(newElectedWitnesses.size == consensusSettings.witnessNum)

      pendingWitnessList.logInfo("setCurrentWitnessList")
      dataBase.setCurrentWitnessList(pendingWitnessList)
      val newPending = WitnessList.create(newElectedWitnesses.toArray.map(_._2), curblock.id)
      newPending.logInfo("setPendingWitnessList")
      dataBase.setPendingWitnessList(newPending)

      mCurWitnessList = dataBase.getCurrentWitnessList()
      mPendingWitnessList = dataBase.getPendingWitnessList()
    }
  }

  def updateWitnessList(curblock: Block): Unit ={
    val pendingWitnessList = mPendingWitnessList.get

    log.info("it's time to electe new producers")
    val currentWitness = mCurWitnessList.get
    val allWitnesses = dataBase.getAllWitness().filter(_.register)
    new WitnessList(allWitnesses.toArray, UInt256.Zero).logInfo("current all Witness")
    currentWitness.logInfo("setPreviousWitnessList")
    dataBase.setPreviousWitnessList(currentWitness)
    mPrevWitnessList = dataBase.getPreviousWitnessList()
    //      val allWitnessesMap: Map[UInt160, WitnessInfo] = allWitnesses.map(w => w.addr -> w).toMap
    val updatedCurrentWitness = ArrayBuffer.empty[WitnessInfo]
    currentWitness.witnesses.foreach(oldInfo => {
      val newInfo = allWitnesses.find(_.addr == oldInfo.addr)
      if (newInfo.isDefined)
        updatedCurrentWitness.append(newInfo.get)
    })

    require(updatedCurrentWitness.size <= consensusSettings.witnessNum)

    var newElectedWitnesses = mutable.Map.empty[UInt160, WitnessInfo]
    if (updatedCurrentWitness.size == consensusSettings.witnessNum)
      newElectedWitnesses = WitnessList.removeLeastVote(updatedCurrentWitness.toArray)
    else
      newElectedWitnesses = mutable.Map(updatedCurrentWitness.map(w => w.addr -> w).toMap.toSeq: _*)

    require(newElectedWitnesses.size < consensusSettings.witnessNum)

    val allWitnessesSorted = WitnessList.sortByVote(allWitnesses.toArray)
    val allWitnessIterator = allWitnessesSorted.iterator
    while (allWitnessIterator.hasNext && newElectedWitnesses.size < consensusSettings.witnessNum) {
      val witness = allWitnessIterator.next()
      if (!newElectedWitnesses.contains(witness.addr))
        newElectedWitnesses.update(witness.addr, witness)
    }
    require(newElectedWitnesses.size == consensusSettings.witnessNum)

    pendingWitnessList.logInfo("setCurrentWitnessList")
    dataBase.setCurrentWitnessList(pendingWitnessList)
    val newPending = WitnessList.create(newElectedWitnesses.toArray.map(_._2), curblock.id)
    newPending.logInfo("setPendingWitnessList")
    dataBase.setPendingWitnessList(newPending)

    mCurWitnessList = dataBase.getCurrentWitnessList()
    mPendingWitnessList = dataBase.getPendingWitnessList()
  }

  private def applyBlock(block: Block, verify: Boolean = true, enableSession: Boolean = true): Boolean = {
    var applied = true

    if (!verify || verifyBlock(block)) {
      if (enableSession)
        dataBase.startSession()

      for (tx <- block.transactions if applied) {
        applied = applyTransaction(tx, block.producer, Long.MaxValue, block.header.timeStamp, block.height()).added
      }

      if (enableSession && !applied)
        dataBase.rollBack()
    }
    else
      applied = false
    if (!applied) {
      log.error(s"Block apply fail #${block.height()} ${block.shortId}")
    }
    applied
  }

  private def applyTransaction(tx: Transaction, blockProducer: UInt160,
                               stopTime: Long, blockTime: Long, blockIndex: Long): AddTxResult = {
    var txValid = AddTxResult(false, "error")
    //if tx is a schedule tx and it is time to execute,or tx is a normal transfer or miner tx, start execute tx directly
    tx.txType match {
      case TransactionType.Miner => txValid = applyMinerTransaction(tx, blockProducer, blockIndex)
      case TransactionType.Transfer => txValid = applyContractTransaction(tx, blockProducer, stopTime, blockTime, blockIndex)
      case TransactionType.Deploy => txValid = applyContractTransaction(tx, blockProducer, stopTime, blockTime, blockIndex)
      case TransactionType.Call => txValid = applyContractTransaction(tx, blockProducer, stopTime, blockTime, blockIndex)
      case TransactionType.Refund => txValid = applyRefundTransaction(tx, blockProducer, blockTime)
      case TransactionType.Schedule => txValid = applyScheduleTransaction(tx, blockProducer, stopTime, blockTime, blockIndex)
    }
    if (!txValid.added)
      log.error(s"applyTransaction fail, txid = ${tx.id.toString}  blockIndex = $blockIndex  reason = ${txValid.result}")
    txValid
  }

  private def applyRefundTransaction(tx: Transaction, blockProducer: UInt160, blockTime: Long): AddTxResult = {
    if (blockTime >= tx.executeTime && dataBase.getScheduleTx(tx.id()).isDefined) {
      dataBase.transfer(tx.from, tx.toPubKeyHash, tx.amount)
      dataBase.deleteScheduleTx(tx.id())
      AddTxResult(true, "success")
    } else {
      AddTxResult(false, "applyRefundTransaction error")
    }
  }

  private def applyScheduleTransaction(tx: Transaction, blockProducer: UInt160,
                                       stopTime: Long, blockTime: Long, blockIndex: Long): AddTxResult = {
    var txValid = AddTxResult(false, "error")
    if (dataBase.getScheduleTx(tx.id()).isDefined && blockTime >= tx.executeTime) {
      val originalTx = Transaction.fromBytes(tx.data)
      originalTx.txType match {
        case TransactionType.Transfer => txValid = applyContractTransaction(tx, blockProducer, stopTime, blockTime, blockIndex, originalTx)
        case TransactionType.Deploy => txValid = applyContractTransaction(tx, blockProducer, stopTime, blockTime, blockIndex, originalTx)
        case TransactionType.Call => txValid = applyContractTransaction(tx, blockProducer, stopTime, blockTime, blockIndex, originalTx)
      }
    }
    txValid
  }

  private def applyContractTransaction(tx: Transaction, blockProducer: UInt160,
                                       stopTime: Long, blockTime: Long, blockIndex: Long,
                                       originalTx: Transaction = null): AddTxResult = {
    if (originalTx != null) {
      val applied = applyContractTransactionExecutor(tx, blockProducer, stopTime, blockTime, blockIndex, Some(originalTx))
      if (applied.added)
        dataBase.deleteScheduleTx(tx.id())
      applied
    }
    else {
      if (blockTime >= tx.executeTime)
        applyContractTransactionExecutor(tx, blockProducer, stopTime, blockTime, blockIndex)
      else
        scheduleTxFirstExecute(tx, blockProducer, blockTime, blockIndex)
    }
  }

  private def applyContractTransactionExecutor(tx: Transaction, blockProducer: UInt160, stopTime: Long, blockTime: Long,
                                               blockIndex: Long, originalTx: Option[Transaction] = None): AddTxResult = {
    var applied = AddTxResult(false, "error")
    val cacheTrack = dataBase.startTracking()
    val executor = new TransactionExecutor(originalTx.getOrElse(tx), blockProducer,
      cacheTrack, stopTime, blockTime, blockIndex, this, originalTx.isDefined)

    executor.init()
    executor.execute()
    executor.go()
    val summary = executor.finalization()
    val receipt = executor.getReceipt

    if (executor.getResult.isBlockTimeout) {
      log.error(s"tx ${tx.id.shortString()} executor time out")
      if (isProducingBlock() && originalTx.isEmpty)
        timeoutTx = Some(tx)
      applied = AddTxResult(false, "executor time out")
    }
    else if (receipt.isValid())
      applied = AddTxResult(true, "success")
    else if (originalTx.isDefined)
      applied = AddTxResult(true, "success")
    else
      applied = AddTxResult(false, "TransactionExecutor error")

    if (applied.added) {
      cacheTrack.commit()
      if (originalTx.isDefined)
        dataBase.setReceipt(tx.id(), TransactionReceipt(tx.id, receipt.txType, receipt.from, receipt.to, receipt.blockIndex, receipt.gasUsed, receipt.output, receipt.status, receipt.error))
      else
        dataBase.setReceipt(tx.id(), receipt)
    }
    applied
  }

  private def applyMinerTransaction(tx: Transaction, blockProducer: UInt160, blockIndex: Long): AddTxResult = {
    dataBase.transfer(tx.from, tx.toPubKeyHash, tx.amount)
    dataBase.increaseNonce(tx.from)
    dataBase.setReceipt(tx.id(), TransactionReceipt(tx.id(), tx.txType, tx.from, blockProducer,
      blockIndex, 0, BinaryData.empty, 0, ""))
    AddTxResult(true, "success")
  }

  private def scheduleTxFirstExecute(tx: Transaction, blockProducer: UInt160,
                                     blockTime: Long, blockIndex: Long): AddTxResult = {
    var txValid = AddTxResult(true, "success")

    val fromAccount = dataBase.getAccount(tx.from).getOrElse(Account.newAccount(tx.from))
    if (tx.nonce != fromAccount.nextNonce) {
      log.info(s"tx ${tx.id().shortString()} nonce ${tx.nonce} invalid, expect ${fromAccount.nextNonce}")
      txValid = AddTxResult(false, s"nonce ${tx.nonce} invalid, expect ${fromAccount.nextNonce}")
    }
    val scheduleTx = new Transaction(TransactionType.Schedule, tx.from, tx.toPubKeyHash, tx.amount, tx.nonce, tx.toBytes,
      tx.gasPrice, tx.gasLimit, BinaryData.empty, tx.version, tx.executeTime)
    val scheduleFee = FixedNumber(BigInt(GasCost.SSTORE)) * scheduleTx.toBytes.size * tx.gasPrice *
      ((tx.executeTime - blockTime) / DAY + 1) + FixedNumber(BigInt(GasCost.TRANSACTION)) * tx.gasPrice

    if (scheduleFee > fromAccount.balance) txValid = AddTxResult(false, "schedule fee not enough")
    if (txValid.added) {
      dataBase.transfer(tx.from, blockProducer, scheduleFee)
      dataBase.setScheduleTx(scheduleTx.id, scheduleTx)
      dataBase.increaseNonce(tx.from)
      dataBase.setReceipt(tx.id(), TransactionReceipt(tx.id(), tx.txType, tx.from, tx.toPubKeyHash,
        blockIndex, tx.transactionCost(), BinaryData.empty, 0, ""))
    }
    txValid
  }

  private def verifyBlock(block: Block): Boolean = {
    var isValid = false
    if (!verifyHeader(block.header)) {
      log.error("verifyBlock error: verifyHeader fail")
    }
    else if (block.transactions.size == 0) {
      log.error("verifyBlock error: block.transactions.size == 0")
    }
    else if (!block.merkleRoot().equals(block.header.merkleRoot)) {
      log.error("verifyBlock error: merkleRoot not equals")
    }
    else if (!verifyTxTypeAndSignature(block.transactions)) {
      log.error("verifyBlock error: verifyTxTypeAndSignature fail")
    }
    else
      isValid = true
    isValid
  }

  private def verifyTxTypeAndSignature(txs: Seq[Transaction]): Boolean = {
    var isValid = true
    var minerTxNum = 0
    txs.foreach(tx => {
      if (tx.txType == TransactionType.Miner) {
        minerTxNum += 1
        if (tx.amount.value != minerAward.value)
          isValid = false
      }
      else {
        if (tx.txType == TransactionType.Schedule) {
          if (dataBase.getScheduleTx(tx.id()).isEmpty) isValid = false
        }
        else if (!tx.verifySignature())
          isValid = false
      }
    })
    if (minerTxNum > 1)
      isValid = false
    isValid
  }

  private def verifyHeader(header: BlockHeader): Boolean = {
    var isValid = false
    val prevBlock = forkBase.get(header.prevBlock)
    val now = Instant.now.toEpochMilli
    if (prevBlock.isEmpty) {
      log.error("verifyHeader error: prevBlock not found")
    }
    else if (header.timeStamp <= prevBlock.get.block.header.timeStamp) {
      log.error(s"verifyHeader error: timeStamp not valid  ${header.timeStamp}  ${prevBlock.get.block.header.timeStamp}")
    }
    else if (header.timeStamp - now > 2000) {
      log.error(s"verifyHeader error: timeStamp too far in future. now=$now timeStamp=${header.timeStamp}")
    }
    else if (header.index != prevBlock.get.block.height() + 1) {
      log.error(s"verifyHeader error: index error ${header.index} ${prevBlock.get.block.height()}")
    }
    else if (!isProducerValid(header.timeStamp, header.producer)) {
      log.error("verifyHeader error: producer not valid")
    }
    else if (!header.verifySig()) {
      log.error("verifyHeader error: verifySig fail")
    }
    else {
      // verify merkleRoot in verifyBlock()
      isValid = true
    }
    isValid
  }

  def getBalance(address: UInt160): Option[FixedNumber] = {
    dataBase.getBalance(address)
  }

  def getBalance(privKey: PrivateKey): Option[FixedNumber] = {
    getBalance(privKey.publicKey.pubKeyHash)
  }

  def getAccount(address: UInt160): Option[Account] = {
    dataBase.getAccount(address)
  }

  def getAccount(privKey: PrivateKey): Option[Account] = {
    dataBase.getAccount(privKey.publicKey.pubKeyHash)
  }

  def getWitness(address: UInt160): Option[WitnessInfo] = {
    dataBase.getWitness(address)
  }

  def setWitness(witnessInfo: WitnessInfo){
    dataBase.createWitness(witnessInfo)
  }

  def getScheduleTx(): ArrayBuffer[Transaction] = {
    dataBase.getAllScheduleTx()
  }

  def getVote(address: UInt160): Option[Vote] = {
    dataBase.getVote(address)
  }

  def getReceipt(txid: UInt256): Option[TransactionReceipt] = {
    dataBase.getReceipt(txid)
  }

  private def updateWitnessLists() = {
    mPrevWitnessList = dataBase.getPreviousWitnessList()
    mCurWitnessList = dataBase.getCurrentWitnessList()
    mPendingWitnessList = dataBase.getPendingWitnessList()
  }

  private def populate(): Unit = {
    def initGenesisWitness() = {
      val witnesses = ArrayBuffer.empty[WitnessInfo]
      consensusSettings.initialWitness.foreach(w => {
        val initWitness = new WitnessInfo(w.pubkeyHash, true, w.name)
        witnesses.append(initWitness)
        dataBase.createWitness(initWitness)
      })
      require(dataBase.getAllWitness().count(_.register) == consensusSettings.witnessNum)
      val witnessList = WitnessList.create(witnesses.toArray, genesisBlock.id())
      require(witnessList.witnesses.size == consensusSettings.witnessNum)
      dataBase.setPreviousWitnessList(witnessList)
      dataBase.setCurrentWitnessList(witnessList)
      dataBase.setPendingWitnessList(witnessList)

      updateWitnessLists()
    }

    log.info("chain populate")
    if (forkBase.head.isEmpty) {
      initGenesisWitness()
      applyBlock(genesisBlock, false, false)
      blockBase.add(genesisBlock)
      forkBase.add(genesisBlock, dataBase.getCurrentWitnessList().get)
      notification.broadcast(BlockAddedToHeadNotify(blockSummary(genesisBlock)))
    }

    updateWitnessLists()

    require(forkBase.head.isDefined)

    forkBase.switchState.foreach(resolveSwitchFailure)

    forkBase.head.foreach(resolveDbUnConsistent)

    require(forkBase.head.map(_.block.height).get >= blockBase.head.map(_.index).get)

    val latestHeader = forkBase.head.get.block.header

    log.info(s"populate() latest block ${latestHeader.index} ${latestHeader.shortId()}")
  }

  private def resolveSwitchFailure(state: SwitchState): Unit = {
    val oldBranch = forkBase.getBranch(state.oldHead, state.forkPoint)
    val newBranch = forkBase.getBranch(state.newHead, state.forkPoint)
    val result = onSwitch(oldBranch, newBranch, state)
    forkBase.endSwitch(oldBranch, newBranch, result)
  }

  private def resolveDbUnConsistent(head: ForkItem): Unit = {
    while (dataBase.revision > head.height + 1) {
      dataBase.rollBack()
    }
  }

  private def onConfirmed(block: Block): Unit = {
    if (block.height > 0) {
      log.info(s"confirm block ${block.height} (${block.shortId})")
      dataBase.commit(block.height)
      blockBase.add(block)
    }
    notification.broadcast(BlockConfirmedNotify(block))
  }

  private def onSwitch(from: Seq[ForkItem], to: Seq[ForkItem], switchState: SwitchState): SwitchResult = {
    def printChain(title: String, fork: Seq[ForkItem]): Unit = {
      log.info(s"$title: ${fork.map(_.block.shortId).mkString(" <- ")}")
    }

    printChain("old chain", from)
    printChain("new chain", to)

    require(dataBase.revision == from.last.height + 1)
    while (dataBase.revision > switchState.height + 1) {
      dataBase.rollBack()
      updateWitnessLists()
    }

    var appliedCount = 0
    var continueApply = true
    for (item <- to if continueApply) {
      if (applyBlock(item.block)) {
        checkUpdateWitnessList(item.block)
        dataBase.commit()
        appliedCount += 1
      }
      else
        continueApply = false
    }

    if (appliedCount < to.size) {
      while (dataBase.revision > switchState.height + 1) {
        dataBase.rollBack()
        updateWitnessLists()
      }
      from.foreach(item => applyBlock(item.block))
      SwitchResult(false, to(appliedCount))
    } else {
      var fromBlocksSummary = Seq[BlockSummary]()
      var toBlocksSummary = Seq[BlockSummary]()
      from.foreach(item => fromBlocksSummary = fromBlocksSummary :+ blockSummary(item.block))
      to.foreach(item => toBlocksSummary = toBlocksSummary :+ blockSummary(item.block))
      notification.broadcast(ForkSwitchNotify(fromBlocksSummary, toBlocksSummary))
      SwitchResult(true)
    }
  }

  // "timeMs": time from 1970 in ms, should be divided evenly with no remainder by settings.produceInterval
  def getWitness(timeMs: Long): UInt160 = {
    require(ProducerUtil.isTimeStampValid(timeMs, consensusSettings.produceInterval))
    require(timeMs > getBlock(mCurWitnessList.get.generateInBlock).get.timeStamp())
    val slot = timeMs / consensusSettings.produceInterval
    var index = slot % (consensusSettings.witnessNum * consensusSettings.producerRepetitions)
    index /= consensusSettings.producerRepetitions
    mCurWitnessList.get.witnesses(index.toInt).addr
  }

  def isLastBlockOfProducer(timeMs: Long): Boolean = {
    require(ProducerUtil.isTimeStampValid(timeMs, consensusSettings.produceInterval))
    val slot = timeMs / consensusSettings.produceInterval
    val index = slot % (consensusSettings.witnessNum * consensusSettings.producerRepetitions)
    (index + 1) % consensusSettings.producerRepetitions == 0
  }

  private def isProducerValid(timeStamp: Long, producer: UInt160): Boolean = {
    var isValid = false
    if (getWitness(timeStamp).data sameElements producer.data) {
      if (ProducerUtil.isTimeStampValid(timeStamp, consensusSettings.produceInterval)) {
        isValid = true
      }
    }
    isValid
  }

  def getProducer(address: UInt160): Option[WitnessInfo] = {
    dataBase.getWitness(address)
  }

  def getProducers(listType: String): WitnessList = {
    // 判断类型值,调用不同的数据库进行查询操作
    listType match {
      case "all" => {
        val witnessInfo = dataBase.getAllWitness()
        WitnessList.create(witnessInfo.toArray, UInt256.Zero)
      }
      case "active" => {
        dataBase.getCurrentWitnessList().get
      }
      case "pending" => {
        dataBase.getPendingWitnessList().get
      }
      case "previous" => {
        dataBase.getPreviousWitnessList().get
      }
      case _ => null
    }
  }
}
