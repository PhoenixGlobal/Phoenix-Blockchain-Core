package com.apex.plugins.mongodb

import java.time.Instant

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import com.apex.common.ApexLogging
import com.apex.core.NewBlockProducedNotify

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

  private val collection: MongoCollection[Document] = database.getCollection("block")

  //collection.drop().results()

  //  val doc: Document = Document("name" -> "MongoDB", "type" -> "database",
  //    "count" -> 1, "info" -> Document("x" -> 203, "y" -> 102))
  //
  //  collection.insertOne(doc).results()

  override def receive: Receive = {
    case NewBlockProducedNotify(block) => {
      val newBlock: Document = Document("height" -> block.height(),
        "id" -> block.id().toString,
        "timeStamp" -> BsonDateTime(block.timeStamp()),
        "prevBlock" -> block.prev().toString,
        "producer" -> block.header.producer.toString,
        "producerSig" -> block.header.producerSig.toString,
        "merkleRoot" -> block.header.merkleRoot.toString,
        "txNum" -> block.transactions.size,
        "transactions" -> block.transactions.map(tx => tx.id.toString),
        "createdAt" -> BsonDateTime(Instant.now.toEpochMilli),
        "confirmed" -> false)

      collection.insertOne(newBlock).results()
    }
    case a: Any => {
      log.info(s"${sender().toString}, ${a.toString}")
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