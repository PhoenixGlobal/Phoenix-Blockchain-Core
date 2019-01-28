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
import play.api.libs.json.{JsValue, Json, Writes}

case class WitnessInfo(addr: UInt160,
                       isGenesisWitness: Boolean = false,
                       name: String = "",
                       url: String = "",
                       country: String = "",
                       address: String = "",
                       longitude: Int = 0,
                       latitude: Int = 0,
                       voteCounts: FixedNumber = FixedNumber.Zero,
                       version: Int = 0x01) extends com.apex.common.Serializable {

  def updateVoteCounts(votes: FixedNumber): WitnessInfo = {
    val witness = this.copy(voteCounts = this.voteCounts + votes)
    witness
  }

  def setVoteCounts(votes: FixedNumber): WitnessInfo = {
    val witness = this.copy(voteCounts = votes)
    witness
  }

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)
    os.write(addr)
    os.writeBoolean(isGenesisWitness)
    os.writeString(name)
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
    val addr = UInt160.deserialize(is)
    val isGenesisWitness = is.readBoolean()
    val name = is.readString()
    val url = is.readString()
    val country = is.readString()
    val address = is.readString()
    val longitude = is.readInt()
    val latitude = is.readInt()
    val voteCounts = FixedNumber.deserialize(is)

    new WitnessInfo(addr, isGenesisWitness, name, url, country, address, longitude, latitude, voteCounts, version)

  }

  implicit val witnessInfoWrites = new Writes[WitnessInfo] {
    override def writes(o: WitnessInfo): JsValue = {
      Json.obj(

        "addr" -> o.addr.address,
        "isGenesisWitness" -> o.isGenesisWitness.toString,
        "name" -> o.name.toString,
        "url" -> o.url,
        "country" ->  o.country,
        "address" -> o.address,
        "longitude" -> o.longitude,
        "latitude" -> o.latitude,
        "voteCounts" -> o.voteCounts.toString,
        "version" -> o.version
      )
    }
  }

}