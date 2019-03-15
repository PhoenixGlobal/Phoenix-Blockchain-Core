/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: ProgramInvokeImpl.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-30 下午5:06@version: 1.0
 *
 */

package com.apex.vm.program.invoke

import com.apex.core.{BlockBase, DataBase, Blockchain}
import com.apex.vm.DataWord

class ProgramInvokeImpl(address: DataWord,
                        origin: DataWord,
                        caller: DataWord,
                        balance: DataWord,
                        gasPrice: DataWord,
                        gas: DataWord,
                        callValue: DataWord,
                        msgData: Array[Byte],
                        lastHash: DataWord,
                        coinBase: DataWord,
                        timestamp: DataWord,
                        number: DataWord,
                        gasLimit: DataWord,
                        dataBase: DataBase,
                        origDataBase: DataBase,
                        chain: Blockchain,
                        callDeep: Int = 0,
                        staticCall: Boolean = false,
                        testingSuite: Boolean = false,
                        byTx: Boolean = true) extends ProgramInvoke {

  /** ***************/
  /** *  msg data ***/
  /** ***************/
  /* NOTE: In the protocol there is no restriction on the maximum message data,
   * However msgData here is a byte[] and this can't hold more than 2^32-1
   */
  private val MAX_MSG_DATA = BigInt(Int.MaxValue)

  /*           ADDRESS op         */
  override def getOwnerAddress: DataWord = address

  /*           BALANCE op         */
  override def getBalance: DataWord = balance

  /*           ORIGIN op         */
  override def getOriginAddress: DataWord = origin

  /*           CALLER op         */
  override def getCallerAddress: DataWord = caller

  /*           GASPRICE op       */
  override def getMinGasPrice: DataWord = gasPrice

  /*           GAS op       */
  override def getGas: DataWord = gas

  override def getGasLong: Long = gas.longValueSafe

  /*     GASLIMIT op    */
  override def getGasLimit: DataWord = gasLimit

  /*          CALLVALUE op    */
  override def getCallValue: DataWord = callValue

  /*  CALLDATASIZE */
  override def getDataSize: DataWord = {
    if (msgData == null || msgData.length == 0) {
      DataWord.ZERO
    } else {
      val size = msgData.length
      DataWord.of(size)
    }
  }

  /*     CALLDATALOAD  op   */
  override def getDataValue(indexData: DataWord): DataWord = {
    val tempIndex = indexData.value
    val index = tempIndex.intValue
    // possible overflow is caught below
    var size = 32 // maximum datavalue size

    if (msgData == null || index >= msgData.length || tempIndex.compareTo(MAX_MSG_DATA) == 1) {
      DataWord.ZERO
    } else {
      if (index + size > msgData.length) {
        size = msgData.length - index
      }

      val data = new Array[Byte](32)
      System.arraycopy(msgData, index, data, 0, size)
      DataWord.of(data)
    }
  }

  /*  CALLDATACOPY */
  override def getDataCopy(offsetData: DataWord, lengthData: DataWord): Array[Byte] = {
    val offset = offsetData.intValueSafe
    var length = lengthData.intValueSafe

    val data = new Array[Byte](length)

    if (msgData == null || offset > msgData.length) {
      data
    } else {
      if (offset + length > msgData.length) {
        length = msgData.length - offset
      }

      System.arraycopy(msgData, offset, data, 0, length)
      data
    }
  }

  /*     PREVHASH op    */
  override def getPrevHash: DataWord = lastHash

  /*     COINBASE op    */
  override def getCoinBase: DataWord = coinBase

  /*     TIMESTAMP op    */
  override def getTimestamp: DataWord = timestamp

  /*     NUMBER op    */
  override def getNumber: DataWord = number

  /*     DIFFICULTY op    */
  override def getDifficulty: DataWord = DataWord.ZERO

  override def byTransaction: Boolean = byTx

  override def byTestingSuite: Boolean = testingSuite

  override def getCallDeep: Int = callDeep

  override def isStaticCall: Boolean = staticCall

  override def getDataBase: DataBase = dataBase

  override def getOrigDataBase: DataBase = origDataBase

  override def getChain: Blockchain = chain
}
