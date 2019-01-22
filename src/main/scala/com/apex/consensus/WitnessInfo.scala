/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: WitnessInfo.scala
 *
 * @author: shan.huang@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.consensus

import java.io.{DataInputStream, DataOutputStream}

import com.apex.crypto.{FixedNumber, UInt160}

case class WitnessInfo( name: String = "",
                   addr: UInt160 = UInt160.Zero,
                   url: String = "",
                   country: String = "",
                   address: String = "",
                   longitude: Int = 0, //BigDecimal,
                   latitude: Int = 0, //BigDecimal,
                   var voteCounts: FixedNumber = FixedNumber.Zero,
                   version: Int = 0x01) extends com.apex.common.Serializable {

  def updateVoteCounts(voteCounts: FixedNumber): WitnessInfo = {
    this.voteCounts = this.voteCounts + voteCounts
    this
  }

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)
    os.writeString(name)
    os.write(addr)
    os.writeString(url)
    os.writeString(country)
    os.writeString(address)
    os.writeInt(longitude)
    os.writeInt(latitude)
    os.write(voteCounts)
  }
}

object WitnessInfo {

  def deserialize(is: DataInputStream): WitnessInfo = {
    import com.apex.common.Serializable._

    val version = is.readInt()
    val name = is.readString()
    val addr = UInt160.deserialize(is)
    val url = is.readString()
    val country = is.readString()
    val address = is.readString()
    val longitude = is.readInt()
    val latitude = is.readInt()
    val voteCounts = FixedNumber.deserialize(is)

    new WitnessInfo(name, addr, url, country, address, longitude, latitude, voteCounts, version)
  }

}