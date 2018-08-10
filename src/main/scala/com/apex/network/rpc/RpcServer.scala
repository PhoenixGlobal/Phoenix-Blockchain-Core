/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: RpcServer.scala
 *
 * @author: shan.huang@chinapex.com: 2018-08-01 下午4:06@version: 1.0
 */

package com.apex.network.rpc

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import com.apex.core.Blockchain
import com.apex.network.LocalNode

import scala.io.StdIn
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

object RpcServer {
  
  implicit val apexSystem = ActorSystem("apex-system")
  implicit val materializer = ActorMaterializer()
   
  //def main(args: Array[String]) {
  def run() {    

    implicit val executionContext = apexSystem.dispatcher

    val route =
      path("getblock") {
        post {
          entity(as[String]) { data =>
            Json.parse(data).validate[GetBlockCmd] match {
              case cmd: JsSuccess[GetBlockCmd] => {
                complete(HttpEntity(ContentTypes.`application/json`, cmd.get.run.toString))
              }
              case e: JsError => {
                println(e)
                complete(HttpEntity(ContentTypes.`application/json`, Json.parse( """ {"result": "Error"}""").toString()))
              }
            }
          }
        }
      } ~
      path("produceblock") {
        post {
          entity(as[String]) { data =>
            val block4 = Blockchain.Current.produceBlock(LocalNode.default.getMemoryPool())
            complete(HttpEntity(ContentTypes.`application/json`, Json.parse( """ {"result": "OK"}""").toString))
          }
        }
      } ~
      path("send") {
        post {
          entity(as[String]) { data =>
            Json.parse(data).validate[SendCmd] match {
              case cmd: JsSuccess[SendCmd] => {
                complete(HttpEntity(ContentTypes.`application/json`, cmd.get.run.toString))
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
          entity(as[String]) { data =>
            val count = Blockchain.Current.getLatestHeader.index + 1
            complete(HttpEntity(ContentTypes.`application/json`, Json.parse(s"""{"result": "$count"}""").toString))
          }
        }
      }

    val bbindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\n")
    //StdIn.readLine() // let it run until user presses return

    //bindingFuture
    //  .flatMap(_.unbind())  
    //  .onComplete(_ => apexSystem.terminate())  
  }
  
  def stop() = {
    println(s"RpcServer stop")
    materializer.shutdown()
    Await.result(apexSystem.terminate(), Duration.Inf)
  }
}


