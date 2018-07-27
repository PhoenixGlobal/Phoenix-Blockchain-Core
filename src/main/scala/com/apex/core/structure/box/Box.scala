package com.apex.core.structure.box

import com.apex.core.serialization.BytesSerializable
import com.apex.core.structure.box.proposition.Proposition
import com.apex.core.authds._

trait Box[P <: Proposition] extends BytesSerializable {
  val value: Box.Amount
  val proposition: P

  val id: ADKey
}

object Box {
  type Amount = Long
}

