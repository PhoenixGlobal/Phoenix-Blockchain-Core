package com.apex.core.consensus

import com.apex.core.serialization.BytesSerializable

trait SyncInfo extends BytesSerializable {
  def startingPoints: History.ModifierIds
}


