package com.apex.plugins.mongodb

import java.time.Instant

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import com.apex.common.ApexLogging
import com.apex.core._

import scala.collection.immutable.IndexedSeq
import org.mongodb.scala._
import org.mongodb.scala.model.Aggregates._
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Projections._
import org.mongodb.scala.model.Sorts._
import org.mongodb.scala.model.Updates._
import org.mongodb.scala.model._
import org.mongodb.scala.bson.BsonDateTime
import com.apex.plugins.mongodb.Helpers._
import com.apex.settings.ApexSettings
import play.api.libs.json.Json

import scala.concurrent.ExecutionContext

class MongodbPlugin(settings: ApexSettings)
                   (implicit ec: ExecutionContext) extends Actor with ApexLogging {

  private val mongoClient: MongoClient = MongoClient(settings.plugins.mongodb.uri)

  private val database: MongoDatabase = mongoClient.getDatabase("apex")

  private val blockCol: MongoCollection[Document] = database.getCollection("block")
  private val txCol: MongoCollection[Document] = database.getCollection("transaction")
  private val accountCol: MongoCollection[Document] = database.getCollection("account")
  private val tpsHourCol: MongoCollection[Document] = database.getCollection("tps_hour")
  private val tpsTenSecCol: MongoCollection[Document] = database.getCollection("tps_tensec")

  init()

  override def postStop(): Unit = {
    log.info("mongodb plugin stopped")
    super.postStop()
  }

  override def receive: Receive = {
    case NewBlockProducedNotify(block) => {

    }
    case BlockAddedToHeadNotify(block) => {
      addBlock(block)
    }
    case BlockConfirmedNotify(block) => {
      blockCol.updateOne(equal("blockHash", block.id.toString), set("confirmed", true)).results()
      block.transactions.foreach(tx => {
        txCol.updateOne(equal("txHash", tx.id.toString), set("confirmed", true)).results()
      })
    }
    case AddTransactionNotify(tx) => {
      if (findTransaction(tx) == false)
        addTransaction(tx, None)
    }
    case ForkSwitchNotify(from, to) => {
      log.info("MongodbPlugin got ForkSwitchNotify")
      from.foreach(item => removeBlock(item.block))
      to.foreach(item => addBlock(item.block))
    }
    case a: Any => {
      log.info(s"${sender().toString}, ${a.toString}")
    }
  }

  private def findTransaction(tx: Transaction): Boolean = {
    txCol.find(equal("txHash", tx.id.toString)).results().size > 0
  }

  private def removeBlock(block: Block) = {
    log.info(s"MongodbPlugin remove block ${block.height()}  ${block.shortId()}")
    blockCol.deleteOne(equal("blockHash", block.id().toString)).results()
    block.transactions.foreach(tx => {
      //txCol.updateOne(equal("txHash", tx.id.toString), set("refBlockHash", "")).results()
      //txCol.updateOne(equal("txHash", tx.id.toString), set("refBlockHeight", Int.MaxValue)).results()
      //txCol.updateOne(equal("txHash", tx.id.toString), set("refBlockTime", BsonDateTime(0))).results()
      //txCol.updateOne(equal("txHash", tx.id.toString), set("confirmed", false)).results()
      txCol.deleteOne(equal("txHash", tx.id.toString)).results()
    })
    updateTps(block, false)
  }

  private def addBlock(block: Block) = {
    log.info(s"MongodbPlugin add block ${block.height()}  ${block.shortId()}")
    val newBlock: Document = Document(
      "height" -> block.height(),
      "blockHash" -> block.id().toString,
      "timeStamp" -> BsonDateTime(block.timeStamp()),
      "prevBlock" -> block.prev().toString,
      "producer" -> block.header.producer.address,
      "producerSig" -> block.header.producerSig.toString,
      "version" -> block.header.version,
      "merkleRoot" -> block.header.merkleRoot.toString,
      "txNum" -> block.transactions.size,
      "txHashs" -> block.transactions.map(tx => tx.id.toString),
      "createdAt" -> BsonDateTime(Instant.now.toEpochMilli),
      "confirmed" -> false)

    blockCol.insertOne(newBlock).results()

    block.transactions.foreach(tx => {
      if (findTransaction(tx)) {
        txCol.updateOne(equal("txHash", tx.id.toString), set("refBlockHash", block.id.toString)).results()
        txCol.updateOne(equal("txHash", tx.id.toString), set("refBlockHeight", block.height)).results()
        txCol.updateOne(equal("txHash", tx.id.toString), set("refBlockTime", BsonDateTime(block.timeStamp()))).results()
        updateAccout(tx, block)
      }
      else
        addTransaction(tx, Some(block))
    })
    updateTps(block, true)
  }

  private def updateAccout(tx: Transaction, block: Block) = {
    val option = UpdateOptions()
    option.upsert(true)
    if (tx.from.address.length > 0) {
      accountCol.updateOne(equal("address", tx.from.address), set("timeStamp", BsonDateTime(block.timeStamp())), option).results()
    }
    accountCol.updateOne(equal("address", tx.toAddress()), set("timeStamp", BsonDateTime(block.timeStamp())), option).results()
  }

  private def updateTps(block: Block, isIncrease: Boolean) = {
    val option = UpdateOptions()
    option.upsert(true)

    val tenSec: Long = 10000
    val oneHour: Long = 3600000

    val time10s: Long = block.header.timeStamp / tenSec * tenSec
    val timeHour: Long = block.header.timeStamp / oneHour * oneHour

    if (isIncrease)
      tpsHourCol.updateOne(equal("timeStamp", BsonDateTime(timeHour)), inc("txs", block.transactions.size), option).results()
    else
      tpsHourCol.updateOne(equal("timeStamp", BsonDateTime(timeHour)), inc("txs", -block.transactions.size), option).results()

    if (isIncrease)
      tpsTenSecCol.updateOne(equal("timeStamp", BsonDateTime(time10s)), inc("txs", block.transactions.size), option).results()
    else
      tpsTenSecCol.updateOne(equal("timeStamp", BsonDateTime(time10s)), inc("txs", -block.transactions.size), option).results()

  }

  private def addTransaction(tx: Transaction, block: Option[Block]) = {
    var newTx: Document = Document(
      "txHash" -> tx.id.toString,
      "type" -> tx.txType.toString,
      "from" -> { if (tx.txType == TransactionType.Miner) "" else tx.from.address },
      "to" ->  tx.toAddress,
      "amount" -> tx.amount.toString,
      "nonce" -> tx.nonce.toString,
      "data" -> tx.data.toString,
      "gasPrice" -> tx.gasPrice.toString,
      "gasLimit" -> tx.gasLimit.longValue(),
      "signature" -> tx.signature.toString,
      "version" -> tx.version,
      "createdAt" -> BsonDateTime(Instant.now.toEpochMilli),
      "confirmed" -> false)

    if (block.isDefined) {
      newTx += ("refBlockHash" -> block.get.id.toString,
                "refBlockHeight" -> block.get.height,
                "refBlockTime" -> BsonDateTime(block.get.timeStamp()))
      updateAccout(tx, block.get)
    }
    else {
      newTx += ("refBlockHeight" -> Int.MaxValue)
    }
    txCol.insertOne(newTx).results()
  }

  private def init() = {
    log.info("init mongo")

    try {
      if (blockCol.countDocuments().headResult() == 0) {
        log.info("creating mongo db")

        blockCol.createIndex(ascending("height")).results()
        blockCol.createIndex(ascending("blockHash")).results()

        txCol.createIndex(ascending("txHash")).results()
        txCol.createIndex(ascending("refBlockHeight")).results()
        txCol.createIndex(ascending("from")).results()
        txCol.createIndex(ascending("to")).results()

        accountCol.createIndex(ascending("address")).results()
        accountCol.createIndex(ascending("timeStamp")).results()

        tpsHourCol.createIndex(ascending("timeStamp")).results()

        tpsTenSecCol.createIndex(ascending("timeStamp")).results()
      }
    }
    catch {
      case e: Throwable => {
        log.error(s"init mongo error: ${e.getMessage}")
      }
    }
  }
}

object MongodbPluginRef {
  def props(settings: ApexSettings)
           (implicit ec: ExecutionContext): Props = {
    Props(new MongodbPlugin(settings))
  }

  def apply(settings: ApexSettings)
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef = {
    system.actorOf(props(settings))
  }

  def apply(settings: ApexSettings,
            name: String)
           (implicit system: ActorContext, ec: ExecutionContext): ActorRef = {
    system.actorOf(props(settings), name)
  }
}