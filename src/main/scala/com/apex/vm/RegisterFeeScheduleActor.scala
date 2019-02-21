//package com.apex.vm
//
//import java.text.SimpleDateFormat
//import java.util.Date
//
//import akka.actor.{Actor, ActorSystem, Kill, PoisonPill, Props}
//import com.apex.consensus.{RegisterData, VoteData}
//import com.apex.core.{DataBase, Transaction}
//import com.apex.crypto.{FixedNumber, UInt160}
//
//import scala.concurrent.ExecutionContext
//import scala.concurrent.duration._
//
//case class RefundToRegister(track: DataBase, registerData: RegisterData, registerSpend: FixedNumber)
//class RegisterFeeScheduleActor extends Actor{
//
//  override def receive: Receive = {
//
//    case RefundToRegister(track, voteData, tx) => refundToRegister(track, voteData, tx)
//    case _ => None
//  }
//
//  private def refundToRegister(track: DataBase, registerData: RegisterData, registerSpend: FixedNumber): Unit ={
//    println(track.getBalance(registerData.registerAccount))
//    println(track.getBalance(new UInt160(PrecompiledContracts.registerNodeAddr.getLast20Bytes)))
//    track.addBalance(registerData.registerAccount, registerSpend.value)
//    track.addBalance(new UInt160(PrecompiledContracts.registerNodeAddr.getLast20Bytes), -registerSpend.value)
//    println("222222222222222222222222222:" + new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date))
//    println(track.getBalance(registerData.registerAccount))
//    println(track.getBalance(new UInt160(PrecompiledContracts.registerNodeAddr.getLast20Bytes)))
//    self ! PoisonPill
//  }
//}
//
//object RegisterFeeScheduleActor {
//
//
//  def apply(track: DataBase, registerData: RegisterData, registerSpend: FixedNumber): Unit ={
//    val system = ActorSystem("RegisterFeeSystemActor")
//
//    val registerFeeScheduleActor = system.actorOf(Props(new RegisterFeeScheduleActor()), name = "RegisterFeeSystemActor")
//    implicit val executionContext: ExecutionContext = system.dispatcher
//
//    println("11111111111111111111111:" + new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date))
//    system.scheduler.scheduleOnce(5 seconds){
//      registerFeeScheduleActor ! RefundToRegister(track, registerData, registerSpend)
//    }
//  }
//}
//
//
