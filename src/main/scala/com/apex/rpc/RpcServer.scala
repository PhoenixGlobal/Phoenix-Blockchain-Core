/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: RpcServer.scala
 *
 * @author: shan.huang@chinapex.com: 2018-08-01 下午4:06@version: 1.0
 */

package com.apex.rpc

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.apex.common.ApexLogging
import com.apex.consensus.{WitnessVote, WitnessInfo, WitnessList}
import com.apex.core._
import com.apex.rpc.RpcServer.sussesRes
import com.apex.settings.ApexSettings
import com.typesafe.config.Config
import play.api.libs.json._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success, Try}

object RpcServer extends ApexLogging {

  implicit val timeout = Timeout(5.seconds)

  private implicit var system: ActorSystem = _
  private implicit var materializer: ActorMaterializer = _
  private implicit var dispatcher: ExecutionContextExecutor = _

  private var bindingFuture: Future[Http.ServerBinding] = _

  def run(settings: ApexSettings, config: Config, nodeRef: ActorRef, gasPricePlugin: ActorRef) = {

    system = ActorSystem("RPC", config)
    materializer = ActorMaterializer()
    dispatcher = getDispatcher("apex.actor.rpc-dispatcher")
    system.registerOnTermination(log.info("rpc terminated"))

    val rpcSettings = settings.rpc
    val route =
      path("getblock") {
        post {
          entity(as[String]) { data =>
            Json.parse(data).validate[GetBlockByHeightCmd] match {
              case cmd: JsSuccess[GetBlockByHeightCmd] => {
                val f = (nodeRef ? cmd.get)
                  .mapTo[Try[Option[Block]]]
                  .map(s =>
                    s match {
                      case Success(blockOption) => {
                        blockOption match {
                          case Some(block) => sussesRes(Json.prettyPrint(Block.blockWrites.writes(block)))
                          case None => sussesRes("")
                        }
                      }
                      case Failure(e) => error500Res(e.getMessage)
                    })
                complete(f)
              }
              case _: JsError => {
                Json.parse(data).validate[GetBlockByIdCmd] match {
                  case cmd: JsSuccess[GetBlockByIdCmd] => {
                    val f = (nodeRef ? cmd.get)
                      .mapTo[Try[Option[Block]]]
                      .map(s =>
                        s match {
                          case Success(blockOption) => {
                            blockOption match {
                              case Some(block) => sussesRes(Json.prettyPrint(Block.blockWrites.writes(block)))
                              case None => sussesRes("")
                            }
                          }
                          case Failure(e) => error500Res(e.getMessage)
                        })
                    complete(f)
                  }
                  case _: JsError => {
                    //  log.error("", idError)
                    complete(HttpEntity(ContentTypes.`application/json`, error400Res))
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
              val f = (nodeRef ? GetBlocksCmd())
                .mapTo[Try[ArrayBuffer[Block]]]
                .map(s =>
                  s match {
                    case Success(blocks) => {
                      sussesRes(Json.prettyPrint(Json.toJson(blocks)))
                    }
                    case Failure(e) => error500Res(e.getMessage)
                  }
                )
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
                    .mapTo[Try[Option[Account]]]
                    .map(s =>
                      s match {
                        case Success(accountOption) => {
                          accountOption match {
                            case Some(account) => sussesRes(Json.prettyPrint(Account.accountWrites.writes(account)))
                            case None => sussesRes("")
                          }
                        }
                        case Failure(e) => error500Res(e.getMessage)
                      })
                  complete(f)
                }
                case e: JsError => {
                  println(e)
                  complete(HttpEntity(ContentTypes.`application/json`, error400Res))
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
                    .mapTo[Try[Option[TransactionReceipt]]]
                    .map(s =>
                      s match {
                        case Success(contractOption) => {
                          contractOption match {
                            case Some(contract) => sussesRes(Json.prettyPrint(TransactionReceipt.TransactionReceiptWrites.writes(contract)))
                            case None => sussesRes("")
                          }
                        }
                        case Failure(e) => error500Res(e.getMessage)
                      })
                  complete(f)
                }
                case _: JsError => {
                  //  log.error("", idError)
                  complete(HttpEntity(ContentTypes.`application/json`, error400Res))
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
                  log.info("send transaction: " + Json.toJson(cmd.value.rawTx).toString())
                  val f = (nodeRef ? cmd.value)
                    .mapTo[Try[AddTxResult]]
                    .map(s =>
                      s match {
                        case Success(sendTx) => sussesRes(Json.prettyPrint(AddTxResult.resultWrites.writes(sendTx)))
                        case Failure(e) => error500Res(e.getMessage)
                      })
                  complete(f)
                }
                case e: JsError => {
                  println(e)
                  complete(HttpEntity(ContentTypes.`application/json`, error400Res))
                }
              }
            }
          }
        } ~
        path("getblockheight") {
          post {
            entity(as[String]) { _ =>
              val f = (nodeRef ? GetBlockCountCmd())
                .mapTo[Try[Long]]
                .map(s =>
                  s match {
                    case Success(blockheight) => sussesRes(blockheight.toString)
                    case Failure(e) => error500Res(e.getMessage)
                  })
              complete(f)
            }
          }
        } ~
        path("getLatesBlockInfo") {
          post {
            entity(as[String]) { _ =>
              val f = (nodeRef ? GetLatesBlockInfoCmd())
                .mapTo[Try[BlockHeader]]
                .map(s =>
                  s match {
                    case Success(blockHeader) => sussesRes(Json.prettyPrint(BlockHeader.blockHeaderWrites.writes(blockHeader)))
                    case Failure(e) => error500Res(e.getMessage)
                  })
              complete(f)
            }
          }
        } ~
        path("getProducers") {
          post {
            entity(as[String]) { data =>
              Json.parse(data).validate[GetProducersCmd] match {
                case cmd: JsSuccess[GetProducersCmd] => {
                  val f = (nodeRef ? cmd.get)
                    .mapTo[Try[WitnessList]]
                    .map(s =>
                      s match {
                        case Success(witnessList) => sussesRes(Json.prettyPrint(WitnessList.witnessListWrites.writes(witnessList)))
                        case Failure(e) => error500Res(e.getMessage)
                      })
                  complete(f)
                }
                case _: JsError => {
                  complete(HttpEntity(ContentTypes.`application/json`, error400Res))
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
                    .mapTo[Try[Option[WitnessInfo]]]
                    .map(s =>
                      s match {
                        case Success(producerOption) => {
                          producerOption match {
                            case Some(producer) => sussesRes(Json.prettyPrint(WitnessInfo.witnessInfoWrites.writes(producer)))
                            case None => sussesRes("")
                          }
                        }
                        case Failure(e) => error500Res(e.getMessage)
                      })
                  complete(f)
                }
                case e: JsError => {
                  complete(HttpEntity(ContentTypes.`application/json`, error400Res()))
                }
              }
            }
          }
        } ~
        path("getVote") {
          post {
            entity(as[String]) { data =>
              Json.parse(data).validate[GetVotesCmd] match {
                case cmd: JsSuccess[GetVotesCmd] => {
                  val f = (nodeRef ? cmd.value)
                    .mapTo[Try[Option[WitnessVote]]]
                    .map(s =>
                      s match {
                        case Success(producerOption) => {
                          producerOption match {
                            case Some(vote) => sussesRes(Json.prettyPrint(WitnessVote.witnessVoteWrites.writes(vote)))
                            case None => sussesRes("")
                          }
                        }
                        case Failure(e) => error500Res(e.getMessage)
                      })
                  complete(f)
                }
                case e: JsError => {
                  complete(HttpEntity(ContentTypes.`application/json`, error400Res()))
                }
              }
            }
          }
        } ~
        path("getAverageGasPrice") {
          post {
            entity(as[String]) { _ =>
              val f = (gasPricePlugin ? GetAverageCmd)
                .mapTo[Try[String]]
                .map(s => s match {
                  case Success(average) => sussesRes(average)
                  case Failure(e) => error500Res(e.getMessage)
                })
              complete(f)
            }
          }
        }

    bindingFuture = Http().bindAndHandle(route, rpcSettings.host, rpcSettings.port)
    //    println(s"Server online at http://${rpcSettings.host}:${rpcSettings.port}/\n")
    //  StdIn.readLine() // let it run until user presses return
  }

  def sussesRes(res: String): String = {
    resultString(new ExecResult(result = res))
  }

  def error500Res(message: String): String = {
    resultString(new ExecResult(false, 500, message))
  }

  def error400Res(): String = {
    resultString(new ExecResult(false, 400, "Param check error"))
  }

  private def resultString(execResult: ExecResult): String = {
    //       Json.prettyPrint(ExecResult.resultWrites.writes(execResult))

    val ret = "{\r\n\"succeed\":" + execResult.succeed + ",\r\n" +
      "\"status\":" + execResult.status + ",\r\n" +
      "\"message\":\"" + execResult.message + "\",\r\n" +
      "\"result\":" + execResult.result + "\r\n}"

    ret
  }

  /*def completeFuture()*/

  private def getDispatcher(id: String) = {
    if (system.dispatchers.hasDispatcher(id)) {
      system.dispatchers.lookup(id)
    } else {
      system.dispatcher
    }
  }

  //  def stop() = {
  //    log.info("stopping rpc server")
  //    system.terminate.foreach(_ => log.info("rpc server stopped"))
  //  }

}


