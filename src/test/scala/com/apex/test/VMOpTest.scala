/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: VMOpTest.scala
 *
 * @author: shan.huang@chinapex.com: 18-11-30 下午7:40@version: 1.0
 *
 */

package com.apex.test

import com.apex.core.DataBase
import com.apex.crypto.Ecdsa.PublicKey
import com.apex.crypto.{BinaryData, Crypto, UInt160}
import com.apex.settings.{ContractSettings, DBType, DataBaseSettings}
import com.apex.solidity.Abi
import com.apex.test.VMTest.author
import com.apex.vm.hook.VMHook
import com.apex.vm.program.invoke.{ProgramInvoke, ProgramInvokeImpl}
import com.apex.vm.program.{Program, ProgramResult}
import com.apex.vm.{DataWord, VM}
import org.junit.{After, Before, Test}

import scala.reflect.io.Directory

@Test
class VMOpTest {

  import VMOpTest._

  val invoker = new ProgramInvokeImpl(
    DataWord.of(BinaryData("2341DE5527492BCB42EC68DFEDF0742A98EC3F1E")),
    DataWord.of(caller),
    DataWord.of(caller),
    DataWord.of(10),
    DataWord.of(30),
    DataWord.of(900000),
    DataWord.of(0),
    Array.empty,
    DataWord.ZERO,
    DataWord.of(BinaryData("E559DE5527492BCB42EC68D07DF0742A98EC3F1E")),
    DataWord.of(BinaryData("0000000000000000000000000000000000000123")),
    DataWord.of(BinaryData("0000000000000000000000000000000000000111")),
    null,
    null,
    null,
    null)


  @Test  // COINBASE Op
  def test1: Unit = {


    val program = new Program(VMOpTest.vmSettings, BinaryData("41"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("E559DE5527492BCB42EC68D07DF0742A98EC3F1E")))
  }

  @Test  // GASPRICE Op
  def testGASPRICE: Unit = {


    val program = new Program(VMOpTest.vmSettings, BinaryData("3a"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("000000000000000000000000000000000000001e")))
  }

  @Test  // GAS Op
  def testGAS: Unit = {


    val program = new Program(VMOpTest.vmSettings, BinaryData("5a"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("00000000000000000000000000000000000dbb9e")))
  }

  @Test  // TIMESTAMP Op
  def testTimeStamp: Unit = {

    val program = new Program(VMOpTest.vmSettings, BinaryData("42"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("0000000000000000000000000000000000000123")))
  }

  @Test  // NUMBER Op
  def testNUMBER: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("43"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("0000000000000000000000000000000000000111")))
  }

  @Test  // DIFFICULTY Op
  def testDIFFICULTY: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("44"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("0000000000000000000000000000000000000000")))
  }

  @Test  // GASLIMIT Op
  def testGASLIMIT: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("45"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("00000000000000000000000000000000000DBBA0")))
  }

  @Test  // ADDRESS Op
  def testADDRESS: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("30"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("2341DE5527492BCB42EC68DFEDF0742A98EC3F1E")))
  }


  private var dataBase: DataBase = null

  @Before
  def populate(): Unit = {
    dataBase = new DataBase(settings)
  }

  @After
  def clearUp(): Unit = {
    try {
      dataBase.close()
      Directory(dir).deleteRecursively()
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }
}

object VMOpTest {
  private val dir = "VMOpTest"
  private val settings = DataBaseSettings(dir, true, 10, DBType.LevelDB)
  //  private val dataBase = new DataBase(settings)

  val caller = PublicKey("0345ffbf8dc9d8ff15785e2c228ac48d98d29b834c2e98fb8cfe6e71474d7f6322").pubKeyHash
  val author = PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8").pubKeyHash
  val contractAddress = Crypto.calcNewAddr(author, BigInt(1).toByteArray)
  val vmSettings = ContractSettings(0, false, Int.MaxValue)

  def deploy(dataBase: DataBase, caller: UInt160, code: Array[Byte], value: Int = 0, gasLimit: Long = Int.MaxValue) = {
    val tracking = dataBase.startTracking()
    val contract = Crypto.calcNewAddr(author, BigInt(1).toByteArray)
    if (value > 0) tracking.transfer(caller, contract, value)
    val invoker = createInvoker(tracking, dataBase, caller, contract, Array.empty, value, gasLimit)
    val program = new Program(vmSettings, code, invoker, Long.MaxValue)
    val result = VM.play(vmSettings, VMHook.EMPTY, program)
    if (success(result)) {
      tracking.commit()
    } else {
      tracking.rollBack()
    }
    (contract, result)
  }

  def call(dataBase: DataBase, caller: UInt160, contract: UInt160, code: Array[Byte],
           signature: Array[Byte], value: Int = 0, gasLimit: Long = Int.MaxValue) = {
    val tracking = dataBase.startTracking()
    if (value > 0) {
      tracking.transfer(caller, contract, value)
    }

    val invoker = createInvoker(tracking, dataBase, caller, contract, signature, value, gasLimit)
    val program = new Program(vmSettings, code, invoker, Long.MaxValue)
    val result = VM.play(vmSettings, VMHook.EMPTY, program)
    if (success(result)) {
      tracking.commit()
    } else {
      tracking.rollBack()
    }
    result
  }

  def createInvoker(tracking: DataBase, origin: DataBase, caller: UInt160, contract: UInt160,
                    data: Array[Byte], value: Int, gasLimit: Long): ProgramInvoke = {
    new ProgramInvokeImpl(
      DataWord.of(contract),
      DataWord.of(caller),
      DataWord.of(caller),
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.of(gasLimit),
      DataWord.of(value),
      data,
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.ZERO,
      DataWord.ZERO,
      tracking,
      origin,
      null,
      null)
  }

  def success(getResult: ProgramResult) = {
    !getResult.isRevert && getResult.getException == null
  }
}
