package com.apex.network

import com.apex.network.message.MessageSpec

import scala.reflect.runtime.universe.TypeTag

object NetworkControllerSharedMessages {
  object ReceivableMessages {
    case class DataFromPeer[DT: TypeTag](spec: MessageSpec[DT], data: DT, source: ConnectedPeer)
  }
}
