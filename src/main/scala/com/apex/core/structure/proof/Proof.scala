package com.apex.core.structure.proof

import com.apex.core.serialization.BytesSerializable
import com.apex.core.structure.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import com.apex.core.structure.state.Secret


trait Proof[P <: Proposition] extends BytesSerializable {
  def isValid(proposition: P, message: Array[Byte]): Boolean
}

trait ProofOfKnowledge[S <: Secret, P <: ProofOfKnowledgeProposition[S]] extends Proof[P]
