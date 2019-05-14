package com.apex.consensus

import com.apex.common.ApexLogging
import com.apex.crypto.{FixedNumber, UInt160}
import play.api.libs.json.{JsValue, Json, Writes}

class AddressVote(val addr: UInt160, val vote: FixedNumber) extends ApexLogging {

}

object AddressVote {

  implicit val addressVoteWrites = new Writes[AddressVote] {
    override def writes(o: AddressVote): JsValue = {
      Json.obj(
        "addr" -> o.addr,
        "vote" -> o.vote.toString
      )
    }
  }

}

class AddressVoteList(val addrs: Array[AddressVote]) extends ApexLogging {

}

object AddressVoteList {

  implicit val addressVoteListWrites = new Writes[AddressVoteList] {
    override def writes(o: AddressVoteList): JsValue = {
      Json.obj(
        "addrs" -> o.addrs
      )
    }
  }

}