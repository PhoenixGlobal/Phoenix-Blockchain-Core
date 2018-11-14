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

  private val mongoClient: MongoClient = MongoClient()

  private val database: MongoDatabase = mongoClient.getDatabase("apex")

  private val blockCol: MongoCollection[Document] = database.getCollection("block")

  private val txCol: MongoCollection[Document] = database.getCollection("transaction")


  //collection.drop().results()

  //  val doc: Document = Document("name" -> "MongoDB", "type" -> "database",
  //    "count" -> 1, "info" -> Document("x" -> 203, "y" -> 102))
  //
  //  collection.insertOne(doc).results()

  init()

  override def receive: Receive = {
    case NewBlockProducedNotify(block) => {
      val newBlock: Document = Document(
        "height" -> block.height(),
        "blockHash" -> block.id().toString,
        "timeStamp" -> BsonDateTime(block.timeStamp()),
        "prevBlock" -> block.prev().toString,
        "producer" -> block.header.producer.toString,
        "producerSig" -> block.header.producerSig.toString,
        "version" -> block.header.version,
        "merkleRoot" -> block.header.merkleRoot.toString,
        "txNum" -> block.transactions.size,
        "txHashs" -> block.transactions.map(tx => tx.id.toString),
        "createdAt" -> BsonDateTime(Instant.now.toEpochMilli),
        "confirmed" -> false)

      blockCol.insertOne(newBlock).results()

      block.transactions.foreach(tx => addTransaction(tx, Some(block)))
    }
    case BlockConfirmedNotify(block) => {

      blockCol.updateOne(equal("blockHash", block.id.toString), set("confirmed", true)).results()
      block.transactions.foreach(tx => {
        txCol.updateOne(equal("txHash", tx.id.toString), set("confirmed", true)).results()
        //txCol.updateOne(equal("txHash", tx.id.toString), set("refBlockHash", block.id.toString)).results()
        //txCol.updateOne(equal("txHash", tx.id.toString), set("refBlockHeight", block.height)).results()
      })

    }
    case a: Any => {
      log.info(s"${sender().toString}, ${a.toString}")
    }
  }

  private def findBlock() = {

  }

  private def findTransaction() = {

  }

  private def addTransaction(tx: Transaction, block: Some[Block]) = {
    var newTx: Document = Document(
      "txHash" -> tx.id.toString,
      "type" -> tx.txType.toString,
      "from" -> { if (tx.txType == TransactionType.Miner) "" else tx.fromAddress },
      "to" ->  tx.toAddress,
      "toName" -> tx.toName,
      "amount" -> tx.amount.toString,
      "assetId" -> tx.assetId.toString,
      "nonce" -> tx.nonce.toString,
      "data" -> tx.data.toString,
      "signature" -> tx.signature.toString,
      "version" -> tx.version,
      "createdAt" -> BsonDateTime(Instant.now.toEpochMilli),
      "confirmed" -> false)

    if (block.isDefined) {
      newTx += ("refBlockHash" -> block.get.id.toString,
                "refBlockHeight" -> block.get.height)
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