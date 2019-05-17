package com.apex.proposal

import play.api.libs.json.{JsValue, Json, Writes}

class ProposalList(val proposals: Array[Proposal],
                   val version: Int = 0x01) {

}

object ProposalList {

  def sortByActiveTime(proposals: Array[Proposal]): Array[Proposal] = {
    proposals.sortWith((p1, p2) => {
      p1.activeTime < p2.activeTime
    })
  }

  implicit val proposalListWrites = new Writes[ProposalList] {
    override def writes(o: ProposalList): JsValue = {
      Json.obj(
        "proposals" -> o.proposals,
        "version" -> o.version
      )
    }
  }

}
