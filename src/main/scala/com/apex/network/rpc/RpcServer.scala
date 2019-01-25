/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: RpcServer.scala
 *
 * @author: shan.huang@chinapex.com: 2018-08-01 下午4:06@version: 1.0
 */

package com.apex.network.rpc

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.apex.common.ApexLogging
import com.apex.consensus.{Vote, WitnessInfo, WitnessList}
import com.apex.core.{Account, Block, TransactionReceipt}
import com.apex.settings.{RPCSettings, SecretRPCSettings}
import play.api.libs.json._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.concurrent.duration._

object RpcServer extends ApexLogging {

  implicit val system = ActorSystem("rpc")
  implicit val executionContext = system.dispatcher
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(5.seconds)

  private var bindingFuture: Future[Http.ServerBinding] = null
  private var secretBindingFuture: Future[Http.ServerBinding] = null

  def run(rpcSettings: RPCSettings, secretRPCSettings : SecretRPCSettings, nodeRef: ActorRef) = {
    val route =
      path("getblock") {
        post {
          entity(as[String]) { data =>
            Json.parse(data).validate[GetBlockByHeightCmd] match {
              case cmd: JsSuccess[GetBlockByHeightCmd] => {
                val f = (nodeRef ? cmd.get)
                  .mapTo[Option[Block]]
                  .map(_.map(Json.toJson(_).toString).getOrElse(JsNull.toString))
                complete(f)
              }
              case _: JsError => {
                Json.parse(data).validate[GetBlockByIdCmd] match {
                  case cmd: JsSuccess[GetBlockByIdCmd] => {
                    val f = (nodeRef ? cmd.get)
                      .mapTo[Option[Block]]
                      .map(_.map(Json.toJson(_).toString).getOrElse(JsNull.toString))
                    complete(f)
                  }
                  case _: JsError => {
                    //                    log.error("", idError)
                    complete(HttpEntity(ContentTypes.`application/json`, Json.parse( """ {"result": "Error"}""").toString()))
                  }
                }
              }
            }
          }
        }
      } ~
        path("getblocks") {
          post {
            entity(as[String]) { _ =>
              val f = (nodeRef ? GetBlocksCmd()).mapTo[ArrayBuffer[Block]].map(Json.toJson(_).toString)
              complete(f)
            }
          }
        } ~
        path("showaccount") { // getaccount
          post {
            entity(as[String]) { data =>
              Json.parse(data).validate[GetAccountCmd] match {
                case cmd: JsSuccess[GetAccountCmd] => {
                  val f = (nodeRef ? cmd.value)
                    .mapTo[Option[Account]]
                    .map(_.map(Json.toJson(_).toString).getOrElse(JsNull.toString))
                  complete(f)
                }
                case e: JsError => {
                  println(e)
                  complete(HttpEntity(ContentTypes.`application/json`, Json.parse( """{"result": "Error"}""").toString()))
                }
              }
            }
          }
        } ~
        path("getContract") {
          post {
            entity(as[String]) { data =>
              Json.parse(data).validate[GetContractByIdCmd] match {
                case cmd: JsSuccess[GetContractByIdCmd] => {
                  val f = (nodeRef ? cmd.get)
                    .mapTo[Option[TransactionReceipt]]
                    .map(_.map(Json.toJson(_).toString).getOrElse(JsNull.toString))
                  complete(f)
                }
                case _: JsError => {
                  //                    log.error("", idError)
                  complete(HttpEntity(ContentTypes.`application/json`, Json.parse( """ {"result": "Error"}""").toString()))
                }
              }
            }
          }
        } ~
        path("sendrawtransaction") {
          post {
            entity(as[String]) { data =>
              Json.parse(data).validate[SendRawTransactionCmd] match {
                case cmd: JsSuccess[SendRawTransactionCmd] => {
                  val f = (nodeRef ? cmd.value).mapTo[Boolean].map(Json.toJson(_).toString)
                  complete(f)
                }
                case e: JsError => {
                  println(e)
                  complete(HttpEntity(ContentTypes.`application/json`, Json.parse( """{"result": "Error"}""").toString()))
                }
              }
            }
          }
        } ~
        path("getblockheight") {
          post {
            entity(as[String]) { _ =>
              val f = (nodeRef ? GetBlockCountCmd()).mapTo[Long].map(GetBlockCountResult(_)).map(Json.toJson(_).toString)
              complete(f)
            }
          }
        } ~
        path("getProducers") {
          post {
            entity(as[String]) { data =>
              Json.parse(data).validate[GetProducersCmd] match {
                case cmd: JsSuccess[GetProducersCmd] => {
                  val f = (nodeRef ? cmd.get).mapTo[Option[WitnessList]].map(Json.toJson(_).toString())
                  complete(f)
                }
                case _: JsError => {
                  complete(HttpEntity(ContentTypes.`application/json`, Json.parse( """ {"result": "Error"}""").toString()))
                }
              }
            }
          }
        } ~
        path("getProducer") {
          post {
            entity(as[String]) { data =>
              Json.parse(data).validate[GetProducerCmd] match {
                case cmd: JsSuccess[GetProducerCmd] => {
                  val f = (nodeRef ? cmd.value)
                    .mapTo[Option[WitnessInfo]]
                    .map(_.map(Json.toJson(_).toString).getOrElse(JsNull.toString))
                  complete(f)
                }
                case e: JsError => {
                  complete(HttpEntity(ContentTypes.`application/json`, Json.parse( """{"result": "Error"}""").toString()))
                }
              }
            }
          }
        }

    val secretRoute =
      path("getGasLimit") {
        post {
          entity(as[String]) {_ =>
            val f = (nodeRef ? GetGasLimitCmd()).mapTo[Long].map(Json.toJson(_).toString)
            complete(f)
          }
        }
      } ~
        path("setGasLimit") {
          post {
            entity(as[String]) { data =>
              Json.parse(data).validate[SetGasLimitCmd] match {
                case cmd: JsSuccess[SetGasLimitCmd] => {
                  val f = (nodeRef ? cmd.get).mapTo[Boolean].map(Json.toJson(_).toString)
                  complete(f)
                }
                case _: JsError => {
                  complete(HttpEntity(ContentTypes.`application/json`, Json.parse( """ {"result": "Error"}""").toString()))
                }
              }
            }
          }
        }

    bindingFuture = Http().bindAndHandle(route, rpcSettings.host, rpcSettings.port)
    secretBindingFuture = Http().bindAndHandle(secretRoute, secretRPCSettings.host, secretRPCSettings.port)
    //    println(s"Server online at http://${rpcSettings.host}:${rpcSettings.port}/\n")
    //  StdIn.readLine() // let it run until user presses return
  }

  def stop() = {
    log.info("stopping rpc server")
    system.terminate.foreach(_ => log.info("rpc server stopped"))
  }

}


