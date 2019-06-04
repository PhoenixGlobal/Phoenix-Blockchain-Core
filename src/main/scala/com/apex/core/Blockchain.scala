package com.apex.core

import java.io.{ByteArrayInputStream, DataInputStream}
import java.time.Instant

import com.apex.common.{ApexLogging, Helper}
import com.apex.consensus._
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKeyHash}
import com.apex.crypto.{BinaryData, Crypto, FixedNumber, MerkleTree, UInt160, UInt256}
import com.apex.proposal._
import com.apex.settings.{ChainSettings, ConsensusSettings, RuntimeParas}
import com.apex.vm.GasCost

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

class Blockchain(chainSettings: ChainSettings,
                 consensusSettings: ConsensusSettings,
                 runtimeParas: RuntimeParas,
                 notification: Notification,
                 forceStartProduce: Boolean = false) extends Iterable[Block] with ApexLogging {

  log.info(s"Blockchain starting, forceStartProduce=${forceStartProduce}")

  private val genesisProducerPrivKey = new PrivateKey(BinaryData(chainSettings.genesis.privateKey))

  log.info(s"creating BlockBase, lightNodeMode=${chainSettings.lightNode}")

  private val blockBase = new BlockBase(chainSettings.blockBase,
    chainSettings.lightNode, consensusSettings.witnessNum * consensusSettings.producerRepetitions)

  log.info("creating DataBase")

  private val dataBase = new DataBase(chainSettings.dataBase, consensusSettings)

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

  private val txPool = new TransactionPool(notification)

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
    getLatestHeader.timeStamp - genesisBlock.timeStamp
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

  def getBlockHash(height: Long): Option[UInt256] = {
    forkBase.get(height).map(_.block.id).orElse(blockBase.getBlockHash(height))
  }

  def getBlockHeight(id: UInt256): Option[Long] = {
    forkBase.get(id).map(_.block.height()).orElse(blockBase.getBlockHeight(id))
  }

  def containsBlock(id: UInt256): Boolean = {
    forkBase.contains(id) || blockBase.containBlock(id)
  }

  def containsBlock(block: Block): Boolean = {
    containsBlock(block.id)
  }

  def blockIsConfirmed(id: UInt256): Boolean = {
    blockBase.containBlock(id)
  }

  def getTransactionFromMempool(txid: UInt256): Option[Transaction] = {
    pendingState.txs.find(tx => tx.id().equals(txid)).orElse(txPool.get(txid))
  }

  def getTransactionFromPendingTxs(txid: UInt256): Option[Transaction] = {
    pendingState.txs.find(tx => tx.id().equals(txid))
  }

  def getTransactionFromUnapplyTxs(txid: UInt256): Option[Transaction] = {
    txPool.get(txid)
  }

  def txPoolTxNumber: Int = txPool.txNumber

  def startProduceBlock(producerPrivKey: PrivateKey, blockTime: Long, stopProcessTxTime: Long): Unit = {
    if (isProducingBlock()) {
      log.error(s"start produce block fail because already in producing, ${blockTime} ${pendingState.blockTime} ${pendingState.blockIndex}")
    }
    require(!isProducingBlock())
    val forkHead = forkBase.head.get
    if (forceStartProduce == false && forkBase.forkItemNum() > 50 * consensusSettings.witnessNum * consensusSettings.producerRepetitions) {
      log.error(s"too many unconfirmed block, abort produce new block. forkItemNum=${forkBase.forkItemNum()} head=${forkHead.block.height()} ${forkHead.block.shortId()}")
    }
    else {
      val producer = producerPrivKey.publicKey.pubKeyHash
      pendingState.set(producerPrivKey, blockTime, stopProcessTxTime, forkHead.block.height + 1)
      log.debug(s"start block at: ${pendingState.startTime}  blockTime=${blockTime}  stopProcessTxTime=${stopProcessTxTime}")

      val minerTx = new Transaction(TransactionType.Miner, minerCoinFrom,
        producer, dataBase.getMinerAward(), forkHead.block.height + 1,
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

      txPool.getSortedTxs().foreach(p => {
        if (Instant.now.toEpochMilli < stopProcessTxTime) {
          val applyResult = applyTransaction(p.tx, producer, stopProcessTxTime, blockTime, forkHead.block.height + 1)
          applyResult match {
            case AddTxSucceed => pendingState.txs.append(p.tx)
            case InvalidNonce(expected, actual) if actual > expected =>
            case _ => badTxs.append(p.tx)
          }
        }
      })
      txPool.remove(pendingState.txs)
      txPool.remove(badTxs)
    }
  }

  def isProducingBlock(): Boolean = {
    pendingState.isProducingBlock
  }

  private def addTransactionToUnapplyTxs(tx: Transaction): AddTxResult = {
    if (!txPool.contains(tx)) {
      if (txPool.add(tx))
        AddedToMempool
      else
        MempoolFull
    }
    else SameTx
  }

  private def validateTransaction(tx: Transaction): AddTxResult = {
    var result: AddTxResult = new AddTxResult(true, "")

    if (tx.gasLimit > runtimeParas.txAcceptGasLimit) {
      log.info(s"tx: ${tx.id()}, set too heigh gas-limit, it should not above ${runtimeParas.txAcceptGasLimit}")
      result = HeighGasLimit(runtimeParas.txAcceptGasLimit)
    }
    else if (dataBase.getNonce(tx.sender()) > tx.nonce) {
      result = InvalidNonce(dataBase.getNonce(tx.sender()), tx.nonce)
    }
    else
      result = checkTxGas(tx)

    result
  }

  def addTransaction(tx: Transaction): Boolean = addTransactionEx(tx).added

  def addTransactionEx(tx: Transaction): AddTxResult = {
    var result = validateTransaction(tx)

    if (result.added) {
      if (tx.txType == TransactionType.Miner || tx.txType == TransactionType.Refund || tx.txType == TransactionType.Schedule) {
        result = InvalidType
      }
      else {
        if (isProducingBlock()) {
          if (Instant.now.toEpochMilli > pendingState.stopProcessTxTime)
            result = addTransactionToUnapplyTxs(tx)
          else {
            result = applyTransaction(tx, pendingState.producerPrivKey.publicKey.pubKeyHash,
              pendingState.stopProcessTxTime, pendingState.blockTime, pendingState.blockIndex)

            result match {
              case AddTxSucceed => pendingState.txs.append(tx)
              case InvalidNonce(expected, actual) if actual > expected => result = addTransactionToUnapplyTxs(tx)
              case _ =>
            }
          }
        }
        else
          result = addTransactionToUnapplyTxs(tx)
      }
    }
    if (!result.added)
      log.error(s"addTransaction error, from=${tx.from.shortAddr} txid=${tx.id.toString}  ${result.result}")

    result
  }

  def produceBlockFinalize(checkTime: Boolean = false): Option[Block] = {
    val endTime = Instant.now.toEpochMilli
    if (!isProducingBlock()) {
      log.info("block canceled")
      None
    }
    else if (checkTime && endTime - pendingState.blockTime > 2000) {
      log.error("produceBlockFinalize too late, block canceled")
      stopProduceBlock()
      None
    }
    else {
      log.debug(s"block time: ${pendingState.blockTime}, end time: $endTime, produce time: ${endTime - pendingState.startTime}")
      val forkHead = forkBase.head.get
      val merkleRoot = MerkleTree.root(pendingState.txs.map(_.id))
      val header = BlockHeader.build(
        forkHead.block.height + 1, pendingState.blockTime, merkleRoot,
        forkHead.block.id, pendingState.producerPrivKey)
      val block = Block.build(header, pendingState.txs.clone)
      pendingState.txs.clear()
      pendingState.isProducingBlock = false
      if (tryInsertBlock(block, false)) {
        log.info(s"block #${block.height} ${block.shortId} produced by ${block.producer.shortAddr} ${block.header.timeString()} txNum=${block.transactions.size}")
        notification.broadcast(NewBlockProducedNotify(block))
        Some(block)
      } else {
        None
      }
    }
  }

  def stopProduceBlock() = {
    pendingState.txs.foreach(tx => {
      if (tx.txType != TransactionType.Miner)
        txPool.add(tx)
    })
    pendingState.txs.clear()
    pendingState.isProducingBlock = false
    dataBase.rollBack()
  }

  def tryInsertBlock(block: Block, doApply: Boolean = true): Boolean = {
    var inserted = false
    if (isProducingBlock()) {
      log.info(s"tryInsertBlock ${block.height} ${block.shortId} but we are producing, stop produce block ${pendingState.blockIndex}")
      stopProduceBlock()
    }

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
      txPool.checkRemoveTimeoutTxs
      txPool.remove(block.transactions)
      block.transactions.foreach(tx => {
        if (timeoutTx.isDefined && timeoutTx.get.id() == tx.id())
          timeoutTx = None
      })
    }
    inserted
  }

  def tryInsertCacheBlock(height: Long): Boolean = {
    var inserted = false
    blockBase.cacheGetBlock(height).foreach(b => {
      //log.info(s"found block ${b.height()} ${b.shortId()} in local cache")
      if (tryInsertBlock(b)) {
        inserted = true
        log.info(s"success insert cache block #${b.height} ${b.shortId} by ${b.producer.shortAddr} txNum=${b.transactions.size}")
      }
      else {
        log.info(s"fail insert cache block #${b.height} ${b.shortId} by ${b.producer.shortAddr} txNum=${b.transactions.size}")
      }
    })
    inserted
  }

  def addBlockToCache(block: Block) = {
    if (block.height() > getConfirmedHeight()) {
      log.info(s"add block ${block.height} ${block.shortId} to cache")
      blockBase.cacheAdd(block)
    }
  }

  private def blockSummary(block: Block): BlockSummary = {
    val summary = mutable.Map[UInt256, Option[TransactionReceipt]]()
    block.transactions.foreach(tx => {
      summary.put(tx.id, blockBase.getReceipt(tx.id))
    })

    new BlockSummary(block, summary)
  }

  private def getWitnessList(block: Block): WitnessList = {
    if (block.timeStamp() > mPendingWitnessList.get.generateTime)
      mCurWitnessList.get
    else
      mPrevWitnessList.get
    // will not get mPendingWitnessList
  }

  private def checkUpdateWitnessList(curblock: Block) = {
    val pendingWitnessList = mPendingWitnessList.get
    if (blockIsConfirmed(pendingWitnessList.generateInBlock) &&
      isLastBlockOfProducer(curblock.timeStamp()) &&
      curblock.timeStamp() - pendingWitnessList.generateTime >= consensusSettings.electeTime) {

      log.info(s"block ${curblock.height()} applied, it's time to electe new producers")
      val currentWitness = mCurWitnessList.get
      val allWitnesses = dataBase.getAllWitness().filter(_.register)
      new WitnessList(allWitnesses.toArray, UInt256.Zero, 0).logInfo("current all Witness")
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
      val newPending = WitnessList.create(newElectedWitnesses.toArray.map(_._2), curblock.id, curblock.timeStamp())
      newPending.logInfo("setPendingWitnessList")
      dataBase.setPendingWitnessList(newPending)

      mCurWitnessList = dataBase.getCurrentWitnessList()
      mPendingWitnessList = dataBase.getPendingWitnessList()
    }
    checkUpdateProposalVote(curblock)
  }

  private def checkUpdateProposalVote(curBlock: Block) = {
    val prevBlock = getBlock(curBlock.prev()).get

    if (isStartOfNewMinutes(prevBlock, curBlock)) {
      log.info(s"block ${curBlock.height()} ${curBlock.header.timeString()} is start of new week")
      dataBase.setWitnessBlockCountNewWeek()
    }
    dataBase.witnessBlockCountAdd(curBlock.producer())

    ProposalList.sortByActiveTime(dataBase.getAllProposal().toArray).foreach(p => {
      if (p.status == ProposalStatus.PendingActive) {
        if (curBlock.timeStamp() >= p.activeTime)
          activeProposal(p)
      }
      else if (curBlock.timeStamp() >= p.endVoteTime) {
        val agreeCount = dataBase.getProposalVoteList().agreeCount(p.proposalID)
        log.info(s"block ${curBlock.height()}, proposal ${p.proposalID} vote time end, agreeCount=${agreeCount}")
        if (agreeCount > 2 * consensusSettings.witnessNum / 3) {
          log.info("proposal pass")
          if (curBlock.timeStamp() >= p.activeTime)
            activeProposal(p)
          else {
            log.info(s"set to PendingActive, active time is ${Helper.timeString(p.activeTime)}")
            dataBase.setProposal(p.setNewStatus(ProposalStatus.PendingActive))
          }
        }
        else {
          log.info("proposal fail")
          deleteProposal(p)
        }
      }
    })
  }

  private def deleteProposal(p: Proposal): Unit = {
    log.info(s"now delete proposal ${p.proposalID}")
    dataBase.deleteProposal(p.proposalID)
    dataBase.deleteProposalVote(p.proposalID)
  }

  private def activeProposal(p: Proposal) = {
    log.info(s"now active proposal ${p.proposalID}")
    val bs = new ByteArrayInputStream(p.proposalValue)
    val is = new DataInputStream(bs)
    if (p.proposalType == ProposalType.BlockAward) {
      val newValue = FixedNumber.deserialize(is)
      log.info(s"new Block Award is ${newValue}")
      dataBase.setMinerAward(newValue)
    }
    else if (p.proposalType == ProposalType.TxMinGasPrice) {
      val newValue = FixedNumber.deserialize(is)
      log.info(s"new TxMinGasPrice is ${newValue}")
      dataBase.setMinGasPrice(newValue)
    }
    else if (p.proposalType == ProposalType.TxMaxGasLimit) {
      val newValue = FixedNumber.deserialize(is)
      log.info(s"new TxMaxGasLimit is ${newValue.value}")
      dataBase.setTxMaxGasLimit(newValue)
    }
    deleteProposal(p)
  }

  private def isStartOfNewWeek(prevBlock: Block, curBlock: Block): Boolean = {
    val oneWeek: Long = 7 * 24 * 3600 * 1000
    if (Helper.weekNumOfYear(prevBlock.timeStamp()) != Helper.weekNumOfYear(curBlock.timeStamp()))
      true
    else if (curBlock.timeStamp() - prevBlock.timeStamp() > oneWeek) // rare case
      true
    else
      false
  }

  // just for test, should use isStartOfNewWeek()
  private def isStartOfNewMinutes(prevBlock: Block, curBlock: Block): Boolean = {
    val timeGap = 300 * 1000
    (curBlock.timeStamp() / timeGap) > (prevBlock.timeStamp() / timeGap)
  }

  private def applyBlock(block: Block, verify: Boolean = true, enableSession: Boolean = true): Boolean = {
    var applied = true

    if (!verify || verifyBlock(block)) {
      if (enableSession)
        dataBase.startSession()

      for (tx <- block.transactions if applied) {
        applied = applyTransaction(tx, block.producer, Long.MaxValue, block.timeStamp, block.height()).added
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

  private def checkTxGas(tx: Transaction): AddTxResult = {
    if (tx.txType == TransactionType.Miner)
      AddTxSucceed
    else if (dataBase.getMinGasPrice() > tx.gasPrice)
      GasPriceTooLow(tx.gasPrice)
    else if (tx.gasLimit > dataBase.getTxMaxGasLimit().value)
      HeighGasLimit(dataBase.getTxMaxGasLimit().value.toLong)
    else
      AddTxSucceed
  }

  private def applyTransaction(tx: Transaction, blockProducer: UInt160,
                               stopTime: Long, blockTime: Long, blockIndex: Long): AddTxResult = {
    var txValid = checkTxGas(tx)

    if (txValid.added) {
      tx.txType match {
        case TransactionType.Miner => txValid = applyMinerTransaction(tx, blockProducer, blockIndex)
        case TransactionType.Transfer | TransactionType.Deploy | TransactionType.Call =>
          txValid = applyContractTransaction(tx, blockProducer, stopTime, blockTime, blockIndex)
        case TransactionType.Refund => txValid = applyRefundTransaction(tx, blockProducer, blockTime, blockIndex)
        case TransactionType.Schedule => txValid = applyScheduleTransaction(tx, blockProducer, stopTime, blockTime, blockIndex)
      }
    }
    if (!txValid.added)
      log.error(s"applyTransaction fail, from=${tx.from.shortAddr} txid=${tx.id.toString}  blockIndex=$blockIndex  reason=${txValid.result}")
    txValid
  }

  private def applyRefundTransaction(tx: Transaction, blockProducer: UInt160,
                                     blockTime: Long, blockIndex: Long): AddTxResult = {
    if (blockTime >= tx.executeTime && dataBase.getScheduleTx(tx.id()).isDefined) {
      dataBase.transfer(tx.from, tx.toPubKeyHash, tx.amount)
      dataBase.deleteScheduleTx(tx.id())

      blockBase.setReceipt(tx.id(), TransactionReceipt(tx.id(), tx.txType, tx.from, tx.toPubKeyHash,
        blockIndex, 0, BinaryData.empty, 0, ""))

      AddTxSucceed
    } else RefundTxError
  }

  private def applyScheduleTransaction(tx: Transaction, blockProducer: UInt160,
                                       stopTime: Long, blockTime: Long, blockIndex: Long): AddTxResult = {
    var txValid = new AddTxResult(false, "error")
    if (dataBase.getScheduleTx(tx.id()).isDefined && blockTime >= tx.executeTime) {
      val originalTx = Transaction.fromBytes(tx.data)
      applyContractTransaction(tx, blockProducer, stopTime, blockTime, blockIndex, originalTx)
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
    var applied = new AddTxResult(false, "error")
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
      applied = ExecutorTimeout
    }
    else if (receipt.isValid())
      applied = AddTxSucceed
    else if (originalTx.isDefined)
      applied = AddTxSucceed
    else
      executor.executorResult match {
        case InvalidNonce(expected, actual) => applied = InvalidNonce(expected, actual)
        case _ => applied = ExecuteError(receipt.error)
      }

    if (applied.added) {
      cacheTrack.commit()
      if (originalTx.isDefined)
        blockBase.setReceipt(tx.id(), TransactionReceipt(tx.id, receipt.txType, receipt.from, receipt.to, receipt.blockIndex, receipt.gasUsed, receipt.output, receipt.status, receipt.error))
      else
        blockBase.setReceipt(tx.id(), receipt)
    }
    applied
  }

  private def applyMinerTransaction(tx: Transaction, blockProducer: UInt160, blockIndex: Long): AddTxResult = {
    dataBase.transfer(tx.from, tx.toPubKeyHash, tx.amount)
    dataBase.increaseNonce(tx.from)
    blockBase.setReceipt(tx.id(), TransactionReceipt(tx.id(), tx.txType, tx.from, blockProducer,
      blockIndex, 0, BinaryData.empty, 0, ""))
    AddTxSucceed
  }

  private def scheduleTxFirstExecute(tx: Transaction, blockProducer: UInt160,
                                     blockTime: Long, blockIndex: Long): AddTxResult = {
    var txValid: AddTxResult = AddTxSucceed

    val fromAccount = dataBase.getAccount(tx.from).getOrElse(Account.newAccount(tx.from))
    if (tx.nonce != fromAccount.nextNonce) {
      log.info(s"tx ${tx.id().shortString()} nonce ${tx.nonce} invalid, expect ${fromAccount.nextNonce}")
      txValid = InvalidNonce(fromAccount.nextNonce, tx.nonce)
    }
    val scheduleTx = new Transaction(TransactionType.Schedule, tx.from, tx.toPubKeyHash, tx.amount, tx.nonce, tx.toBytes,
      tx.gasPrice, tx.gasLimit, BinaryData.empty, tx.version, tx.executeTime)
    val scheduleFee = FixedNumber(BigInt(GasCost.SSTORE)) * scheduleTx.toBytes.size * tx.gasPrice *
      ((tx.executeTime - blockTime) / DAY + 1) + FixedNumber(BigInt(GasCost.TRANSACTION)) * tx.gasPrice

    if (scheduleFee > fromAccount.balance) txValid = ScheduleFeeNotEnough
    if (txValid.added) {
      dataBase.transfer(tx.from, blockProducer, scheduleFee)
      dataBase.setScheduleTx(scheduleTx.id, scheduleTx)
      dataBase.increaseNonce(tx.from)
      blockBase.setReceipt(tx.id(), TransactionReceipt(tx.id(), tx.txType, tx.from, tx.toPubKeyHash,
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
        if (tx.amount.value != dataBase.getMinerAward().value) {
          log.error(s"miner award ${tx.amount} not valid, should be ${dataBase.getMinerAward()}")
          isValid = false
        }
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
    else if (header.timeStamp <= prevBlock.get.block.timeStamp) {
      log.error(s"verifyHeader error: timeStamp not valid  ${header.timeStamp}  ${prevBlock.get.block.timeStamp}")
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

  def setWitness(witnessInfo: WitnessInfo) {
    dataBase.setWitness(witnessInfo)
  }

  def getScheduleTx(): ArrayBuffer[Transaction] = {
    dataBase.getAllScheduleTx()
  }

  def getWitnessVote(address: UInt160): Option[WitnessVote] = {
    dataBase.getWitnessVote(address)
  }

  def getWitnessVoteInfo(address: UInt160): WitnessVoteInfo = {
    val txs = dataBase.getAllScheduleTx().filter(tx => tx.toPubKeyHash == address)
    val w = getWitnessVote(address).getOrElse(new WitnessVote(address))
    new WitnessVoteInfo(w.voter, w.targetMap, txs, w.version)
  }

  def getReceipt(txid: UInt256): Option[TransactionReceipt] = {
    blockBase.getReceipt(txid)
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
        dataBase.setWitness(initWitness)
      })
      require(dataBase.getAllWitness().count(_.register) == consensusSettings.witnessNum)
      val witnessList = WitnessList.create(witnesses.toArray, genesisBlock.id(), genesisBlock.timeStamp())
      require(witnessList.witnesses.size == consensusSettings.witnessNum)
      dataBase.setPreviousWitnessList(witnessList)
      dataBase.setCurrentWitnessList(witnessList)
      dataBase.setPendingWitnessList(witnessList)

      updateWitnessLists()
    }

    log.info("chain populate")
    if (forkBase.head.isEmpty) {
      dataBase.setMinerAward(minerAward)
      dataBase.setMinGasPrice(FixedNumber.MinValue)
      dataBase.setTxMaxGasLimit(FixedNumber(9000000))
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
    log.info(s"resolveSwitchFailure")
    state.logInfo()
    val oldBranch = forkBase.getBranch(state.oldHead, state.forkPoint)
    val newBranch = forkBase.getBranch(state.newHead, state.forkPoint)
    val result = onSwitch(oldBranch, newBranch, state, true)
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

  private def onSwitch(from: Seq[ForkItem], to: Seq[ForkItem],
                       switchState: SwitchState, isInit: Boolean = false): SwitchResult = {
    def printChain(title: String, fork: Seq[ForkItem]): Unit = {
      log.info(s"$title: ${fork.map(_.block.shortId).mkString(" <- ")}")
    }

    printChain("old chain", from)
    printChain("new chain", to)
    log.info(s"dataBase.revision=${dataBase.revision}")
    log.info(s"from.last.height + 1 = ${from.last.height + 1}")
    log.info(s"switchState.height + 1 = ${switchState.height + 1}")

    if (isInit) {
      require(dataBase.revision >= switchState.height + 1)
    }
    else {
      require(dataBase.revision == from.last.height + 1)
    }
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
    require(timeMs > mCurWitnessList.get.generateTime)
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
        val liveWitnessInfo = witnessInfo.filter(p => p.register)
        WitnessList.create(liveWitnessInfo.toArray, UInt256.Zero, 0)
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

  def getProducerVotes(producer: UInt160): AddressVoteList = {
    dataBase.getWitnessAllVoter(producer)
  }

  def getProposal(id: UInt256): Option[Proposal] = {
    dataBase.getProposal(id)
  }

  def getProposalList(): ProposalList = {
    new ProposalList(dataBase.getAllProposal().toArray)
  }

  def getProposalVoteList(): ProposalVoteList = {
    dataBase.getProposalVoteList()
  }
}
