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
import scala.io.StdIn
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import com.apex.core.Blockchain
import com.apex.network.LocalNode
import play.api.libs.json.{JsValue, Json, JsArray, JsObject}


object RpcServer {
  
  implicit val apexSystem = ActorSystem("apex-system")
  implicit val materializer = ActorMaterializer()
   
  //def main(args: Array[String]) {
  def run() {    

    implicit val executionContext = apexSystem.dispatcher

    val route =
      path("") {
        get {
          parameters('method, 'params.?) { (method, params) =>            
            if (method == "getblock") {              
              if (params == None)  {
                complete(HttpEntity(ContentTypes.`application/json`, s"Method $method missing params"))
              }
              else {         
                val paramJsValue: JsValue = Json.parse(params.getOrElse(""))
                val paramJsValueSeq = paramJsValue.asOpt[JsArray].get.value
                val p1 = paramJsValueSeq(0).toString()
                if (p1.length() > 20) {
                  //  http://localhost:8080/?method=getblock&params=["00000000000000000d2347b601158f40c741dbd9353918903f057cedd82f49d1",1]
                  println("get block by hash")
                }
                else {
                  //  http://localhost:8080/?method=getblock&params=[12345,1]
                  println("get block by height")                  
                }
                
                //test
                val json: JsValue = Json.parse("""
                {
                  "hash" : "00000000000000000d2347b601158f40c741dbd9353918903f057cedd82f49d1",
                  "previousblockhash" : "0000000000000000017d4e729a41e2232bf1246a2bca4795071dc15c36504002",
                  "confirmations" : 1,
                  "size" : 19274,
                  "height" : 12345,
                  "version" : 2,
                  "merkleroot" : "9d32282c5b4831e56d9150287e9601e17f03b74820b9d1a353a294645b42af86",
                  "tx" : [
                      "a654b2fce469dfe51e621612cf310fed69f8c1ece82ffb953ba767303ee6c88c",
                      "5bc0f574b4e85d5dbe6fe4bb1801e3e0f00f0ad239ccc524eb9404e9a88c484c",
                      "93936276b3eff8ab22021a40c3c810c5aaf5a2fcc4cf886e239ca5f1a0edfa15",
                      "5b348cc1c5348a1714ea9ed3c34462ac9d569979a9613d7bab82c8eda5822d5a"
                      ]
                  }  """)
              
                complete(HttpEntity(ContentTypes.`application/json`, json.toString()))                
              }                           
            } 
            else if (method == "produceblock") {
              
              Blockchain.Current.produceBlock(LocalNode.default.getMemoryPool())     
              
              complete(HttpEntity(ContentTypes.`application/json`, s"method=$method params=$params"))              
            } 
            else if (method == "sendrawtransaction") {       
              
              complete(HttpEntity(ContentTypes.`application/json`, s"method=$method params=$params"))              
            } 
            else if (method == "getblockcount") {       
              
              complete(HttpEntity(ContentTypes.`application/json`, s"method=$method params=$params"))              
            } 
            else {
              complete(HttpEntity(ContentTypes.`application/json`, s"Unknown method: $method"))
            }
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


