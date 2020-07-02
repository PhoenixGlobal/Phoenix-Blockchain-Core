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
                       register: Boolean = true,
                       frozen: Boolean = false,
                       version: Int = 0x01,
                       ownerInfo: OwnerInfo = OwnerInfo()) extends com.apex.common.Serializable {

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
    os.writeBoolean(register)
    os.writeBoolean(frozen)
    os.write(ownerInfo)
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
    val register = is.readBoolean()
    val frozen = is.readBoolean()
    val owner = OwnerInfo.deserialize(is)
    new WitnessInfo(addr, isGenesisWitness, name, url, country, address, longitude, latitude, voteCounts,register, frozen, version,owner)

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
        "register" -> o.register.toString,
        "frozen" -> o.frozen.toString,
        "version" -> o.version
      )
    }
  }

}

object OwnerInfo{
  def deserialize(is: DataInputStream): OwnerInfo = {
    val ownerAddress = UInt160.deserialize(is)

    OwnerInfo(ownerAddress)

  }

  implicit val witnessInfoWrites = new Writes[OwnerInfo] {
    override def writes(o: OwnerInfo): JsValue = {
      Json.obj(

        "ownerAddress" -> o.ownerAddress.address
      )
    }
  }
}

case class OwnerInfo(var ownerAddress: UInt160 = null) extends com.apex.common.Serializable{


  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.write(ownerAddress)
  }

}