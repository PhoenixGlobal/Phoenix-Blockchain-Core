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
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.StandardRoute
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import spray.json.DefaultJsonProtocol._
import com.apex.common.ApexLogging
import com.apex.core.Block
import com.apex.crypto.UInt256
import com.apex.settings.RPCSettings
import play.api.libs.json._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{Await, ExecutionContext, Future}

object RpcServer extends ApexLogging {

  implicit val system = ActorSystem("rpc")
  implicit val materializer = ActorMaterializer()
  implicit val timeout: Timeout = 5.seconds
  //def main(args: Array[String]) {
  //  def run(node: Node) {

  private var bindingFuture: Future[Http.ServerBinding] = null


  def run(rpcSettings: RPCSettings, nodeRef: ActorRef) = {
    implicit val executionContext = system.dispatcher

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
                    val f = (nodeRef ? cmd)
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
              // FIXME
              val f = (nodeRef ? GetBlocks).mapTo[ArrayBuffer[Block]].map(Json.toJson(_).toString)

              complete(f)
            }
          }
        } ~
        path("getaccount") {   // todo
          post {
            entity(as[String]) { _ =>
              val f = (nodeRef ? GetBlocks).mapTo[ArrayBuffer[Block]].map(Json.toJson(_).toString)

              complete(f)
            }
          }
        } ~
        path("produceblock") {
          post {
            entity(as[String]) { _ =>
              //            Blockchain.Current.produceBlock(LocalNode.default.getMemoryPool())
              //            LocalNode.default.clearMemoryPool()
              complete(HttpEntity(ContentTypes.`application/json`, Json.parse( """ {"result": "OK"}""").toString))
            }
          }
        } ~
        path("send") {
          post {
            entity(as[String]) { data =>
              Json.parse(data).validate[SendCmd] match {
                case cmd: JsSuccess[SendCmd] => {
                  val f = (nodeRef ? cmd.value)
                    .mapTo[UInt256].map(_.toString)
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
        path("getblockcount") {
          post {
            entity(as[String]) { _ =>
              val f = (nodeRef ? GetBlockCountCmd()).mapTo[Int].map(GetBlockCountResult(_)).map(Json.toJson(_).toString)
              complete(f)
            }
          }
        } ~
        path("importprivkey") {
          post {
            entity(as[String]) { data =>
              Json.parse(data).validate[ImportPrivKeyCmd] match {
                case cmd: JsSuccess[ImportPrivKeyCmd] => {
                  complete(HttpEntity(ContentTypes.`application/json`, (nodeRef ! ImportPrivKeyCmd).toString))
                }
                case e: JsError => {
                  println(e)
                  complete(HttpEntity(ContentTypes.`application/json`, Json.parse( """{"result": "Error"}""").toString()))
                }
              }
            }
          }
        } ~
        path("getbalance") {
          post {
            entity(as[String]) { data =>
              Json.parse(data).validate[GetBalanceCmd] match {
                case cmd: JsSuccess[GetBalanceCmd] => {
                  //                Json.toJson((nodeRef ! cmd.value).asInstanceOf[Int])
                  complete(HttpEntity(ContentTypes.`application/json`, (nodeRef ! cmd.value).toString))
                }
                case e: JsError => {
                  println(e)
                  complete(HttpEntity(ContentTypes.`application/json`, Json.parse( """{"result": "Error"}""").toString()))
                }
              }
            }
          }
        }

    bindingFuture = Http().bindAndHandle(route, rpcSettings.host, rpcSettings.port)
    println(s"Server online at http://${rpcSettings.host}:${rpcSettings.port}/\n")
    //  StdIn.readLine() // let it run until user presses return
  }

  def stop() = {
    println(s"RpcServer stop")
    if (bindingFuture != null) {
      implicit val executionContext = system.dispatcher
      bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
    }
  }

}


