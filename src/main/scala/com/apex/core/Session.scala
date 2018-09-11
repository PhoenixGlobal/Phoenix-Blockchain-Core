/*
 *
 *
 *
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Session.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-9-11 下午5:09@version: 1.0
 */

package com.apex.core

import java.io.{DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.UInt256

import scala.collection.mutable.Map

class DbTrack[K <: Serializable, V <: Serializable](val insert: Map[K, V] = Map.empty[K, V],
                                                    val update: Map[K, V] = Map.empty[K, V],
                                                    val delete: Map[K, V] = Map.empty[K, V])
  extends Serializable {
  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeMap(insert.toMap)
    os.writeMap(update.toMap)
    os.writeMap(delete.toMap)
  }
}

object DbTrack {
  def deserialize[K <: Serializable, V <: Serializable](is: DataInputStream)(implicit kdeser: DataInputStream => K, vdeser: DataInputStream => V): DbTrack[K, V] = {
    import com.apex.common.Serializable._
    val dbTrack = new DbTrack[K, V]
    is.readMap(kdeser, vdeser).foreach(p => dbTrack.insert.put(p._1, p._2))
    is.readMap(kdeser, vdeser).foreach(p => dbTrack.update.put(p._1, p._2))
    is.readMap(kdeser, vdeser).foreach(p => dbTrack.delete.put(p._1, p._2))
    dbTrack
  }
}

class Session {
  val a = new DbTrack[UInt256, UInt256]()
}
