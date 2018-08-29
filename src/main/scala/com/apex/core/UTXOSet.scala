//package com.apex.core
//
//import java.io.{DataInputStream, DataOutputStream}
//
//import com.apex.common.Serializable
//import com.apex.crypto.UInt256
//
//class UTXOSet(val utxoStore: UTXOStore) {
//  def contains(txId: UInt256, index: Int): Boolean = {
//    utxoStore.contains(UTXOKey(txId, index))
//  }
//
//  def contains(key: UTXOKey): Boolean = {
//    utxoStore.contains(key)
//  }
//
//  def get(txId: UInt256, index: Int): Option[TransactionOutput] = {
//    utxoStore.get(UTXOKey(txId, index))
//  }
//
//  def get(key: UTXOKey): Option[TransactionOutput] = {
//    utxoStore.get(key)
//  }
//
//  def foreach(func: (UTXOKey, TransactionOutput) => Unit): Unit = {
//    utxoStore.foreach(func)
//  }
//}
//
//case class UTXOKey(val txId: UInt256, val index: Int) extends Serializable {
//  override def serialize(os: DataOutputStream): Unit = {
//    os.write(txId)
//    os.writeInt(index)
//  }
//}
//
//object UTXOKey {
//  val Size: Int = UInt256.Size + 4
//
//  def deserialize(is: DataInputStream): UTXOKey = {
//    import com.apex.common.Serializable._
//    UTXOKey(
//      is.readObj(UInt256.deserialize),
//      is.readInt()
//    )
//  }
//}
