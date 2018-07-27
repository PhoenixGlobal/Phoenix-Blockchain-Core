package com.apex.core.structure.box.proposition

import com.apex.core.serialization.BytesSerializable
import com.apex.core.structure.state.Secret

trait Proposition extends BytesSerializable

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition

