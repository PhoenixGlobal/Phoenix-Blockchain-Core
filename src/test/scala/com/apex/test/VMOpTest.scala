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
import com.apex.crypto.Ecdsa.{PrivateKey, PublicKey}
import com.apex.crypto.{BinaryData, Crypto, UInt160}
import com.apex.settings._
import com.apex.solidity.Abi
import com.apex.test.VMTest.author
import com.apex.vm
import com.apex.vm.hook.VMHook
import com.apex.vm.program.invoke.{ProgramInvoke, ProgramInvokeImpl}
import com.apex.vm.program.{Program, ProgramResult}
import com.apex.vm.{DataWord, VM}
import org.apex.vm.BytecodeCompiler
import org.junit.{After, Before, Test}

import scala.reflect.io.Directory

@Test
class VMOpTest {

  import VMOpTest._

  private def invoker(): ProgramInvoke = invoker(Array.empty)

  private def invoker2(): ProgramInvoke = invoker(BinaryData(
    "00000000000000000000000000000000000000000000000000000000000000A1" +
      "00000000000000000000000000000000000000000000000000000000000000B1"))

  private def invoker(msgData: Array[Byte] = Array.empty): ProgramInvoke =
    new ProgramInvokeImpl(DataWord.of(BinaryData("2341DE5527492BCB42EC68DFEDF0742A98EC3F1E")),
      DataWord.of(caller),
      DataWord.of(caller),
      DataWord.of(10),
      DataWord.of(30),
      DataWord.of(900000),
      DataWord.of(0),
      msgData,
      DataWord.ZERO,
      DataWord.of(BinaryData("E559DE5527492BCB42EC68D07DF0742A98EC3F1E")),
      DataWord.of(BinaryData("0000000000000000000000000000000000000123")),
      DataWord.of(BinaryData("0000000000000000000000000000000000000111")),
      DataWord.of(1000000), null, null, null)


  @Test // COINBASE Op
  def test1: Unit = {

    val program = new Program(VMOpTest.vmSettings, BinaryData("41"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("E559DE5527492BCB42EC68D07DF0742A98EC3F1E")))
  }

  @Test // GASPRICE Op
  def testGASPRICE: Unit = {

    val program = new Program(VMOpTest.vmSettings, BinaryData("3a"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("000000000000000000000000000000000000001e")))
  }

  @Test // GAS Op
  def testGAS: Unit = {

    val program = new Program(VMOpTest.vmSettings, BinaryData("5a"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("00000000000000000000000000000000000dbb9e")))
  }

  @Test // TIMESTAMP Op
  def testTimeStamp: Unit = {

    val program = new Program(VMOpTest.vmSettings, BinaryData("42"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("0000000000000000000000000000000000000123")))
  }

  @Test // NUMBER Op
  def testNUMBER: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("43"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("0000000000000000000000000000000000000111")))
  }

  @Test // DIFFICULTY Op
  def testDIFFICULTY: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("44"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("0000000000000000000000000000000000000000")))
  }

  @Test // GASLIMIT Op
  def testGASLIMIT: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("45"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("00000000000000000000000000000000000F4240")))
  }

  @Test // ADDRESS Op
  def testADDRESS: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("30"), invoker, Long.MaxValue)

    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)

    vm.step(program)

    val item1 = program.stackPop

    assert(item1 == DataWord.of(BinaryData("2341DE5527492BCB42EC68DFEDF0742A98EC3F1E")))
  }

  @Test // MSIZE
  def testMSIZE_1: Unit = {
    val program = new Program(VMOpTest.vmSettings, BytecodeCompiler.compile("MSIZE"), invoker, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    val item1 = program.stackPop
    assert(item1 == DataWord.of(BinaryData("0000000000000000000000000000000000000000000000000000000000000000")))
  }

  @Test // MSIZE
  def testMSIZE_2: Unit = {
    val program = new Program(VMOpTest.vmSettings, BytecodeCompiler.compile("PUSH1 0x20 PUSH1 0x30 MSTORE MSIZE"), invoker, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    val item1 = program.stackPop
    assert(item1 == DataWord.of(BinaryData("0000000000000000000000000000000000000000000000000000000000000060")))
  }

  @Test // BYTE
  def testBYTE_1: Unit = {
    val program = new Program(VMOpTest.vmSettings, BytecodeCompiler.compile("PUSH6 0xAABBCCDDEEFF PUSH1 0x1E BYTE"), invoker, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    val item1 = program.stackPop
    assert(item1 == DataWord.of(BinaryData("00000000000000000000000000000000000000000000000000000000000000EE")))
  }

  @Test // BYTE
  def testBYTE_2: Unit = {
    val program = new Program(VMOpTest.vmSettings, BytecodeCompiler.compile("PUSH6 0xAABBCCDDEEFF PUSH1 0x20 BYTE"), invoker, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    val item1 = program.stackPop
    assert(item1 == DataWord.of(BinaryData("0000000000000000000000000000000000000000000000000000000000000000")))
  }

  @Test // BYTE
  def testBYTE_3: Unit = {
    val program = new Program(VMOpTest.vmSettings, BytecodeCompiler.compile("PUSH6 0xAABBCCDDEE3A PUSH1 0x1F BYTE"), invoker, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    val item1 = program.stackPop
    assert(item1 == DataWord.of(BinaryData("000000000000000000000000000000000000000000000000000000000000003A")))
  }

  @Test(expected = classOf[vm.exceptions.StackTooSmallException]) // BYTE
  def testBYTE_4: Unit = {
    val program = new Program(VMOpTest.vmSettings, BytecodeCompiler.compile("PUSH6 0xAABBCCDDEE3A BYTE"), invoker, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    try {
      vm.step(program)
      vm.step(program)
      vm.step(program)
    } finally {
      assert(program.isStopped)
    }
  }

  @Test // PC
  def testPC_1: Unit = {
    val program = new Program(VMOpTest.vmSettings, BytecodeCompiler.compile("PC"), invoker, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    val item1 = program.stackPop
    assert(item1 == DataWord.of(BinaryData("0000000000000000000000000000000000000000000000000000000000000000")))
  }

  @Test // PC
  def testPC_2: Unit = {
    val program = new Program(VMOpTest.vmSettings, BytecodeCompiler.compile("PUSH1 0x22 PUSH1 0xAA MSTORE PUSH1 0xAA NOT PC"), invoker, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    val item1 = program.stackPop
    assert(item1 == DataWord.of(BinaryData("0000000000000000000000000000000000000000000000000000000000000008")))
  }

  @Test // CALLDATACOPY
  def testCALLDATACOPY_1: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("60206000600037"), invoker2, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    assert(program.getMemory sameElements BinaryData("00000000000000000000000000000000000000000000000000000000000000A1"))
  }

  @Test // CALLDATACOPY
  def testCALLDATACOPY_2: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("60406000600037"), invoker2, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    assert(program.getMemory sameElements
      BinaryData("00000000000000000000000000000000000000000000000000000000000000A1" +
        "00000000000000000000000000000000000000000000000000000000000000B1"))
  }

  @Test // CALLDATACOPY
  def testCALLDATACOPY_3: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("60406004600037"), invoker2, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    assert(program.getMemory sameElements
      BinaryData(
        "000000000000000000000000000000000000000000000000000000A100000000" +
          "000000000000000000000000000000000000000000000000000000B100000000"))
  }

  @Test // CALLDATACOPY
  def testCALLDATACOPY_4: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("60406000600437"), invoker2, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    assert(program.getMemory sameElements
      BinaryData(
        "0000000000000000000000000000000000000000000000000000000000000000" +
          "000000A100000000000000000000000000000000000000000000000000000000" +
          "000000B100000000000000000000000000000000000000000000000000000000"))
  }

  @Test // CALLDATACOPY
  def testCALLDATACOPY_5: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("60406000600437"), invoker2, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    vm.step(program)
    assert(program.getMemory sameElements
      BinaryData(
        "0000000000000000000000000000000000000000000000000000000000000000" +
          "000000A100000000000000000000000000000000000000000000000000000000" +
          "000000B100000000000000000000000000000000000000000000000000000000"))
  }

  @Test(expected = classOf[vm.exceptions.StackTooSmallException]) // CALLDATACOPY
  def testCALLDATACOPY_6: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("6040600037"), invoker2, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    try {
      vm.step(program)
      vm.step(program)
      vm.step(program)
    } finally {
      assert(program.isStopped)
    }
  }

  @Test(expected = classOf[vm.exceptions.OutOfGasException]) // CALLDATACOPY
  def testCALLDATACOPY_7: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("6020600073CC0929EB16730E7C14FEFC63006AC2D794C5795637"), invoker2, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    try {
      vm.step(program)
      vm.step(program)
      vm.step(program)
      vm.step(program)
    } finally {
      assert(program.isStopped)
    }
  }

  @Test // CODESIZE
  def testCODESIZE_1: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("385E60076000396000605f556014600054601e60205463abcddcba6040545b51602001600a5254516040016014525451606001601e5254516080016028525460a052546016604860003960166000f26000603f556103e75660005460005360200235"), invoker, Long.MaxValue)
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    val item1 = program.stackPop
    assert(item1 == DataWord.of(BinaryData("0000000000000000000000000000000000000000000000000000000000000062")))
  }

  @Test // EXTCODESIZE  // todo: test is not testing EXTCODESIZE
  def testEXTCODESIZE_1: Unit = {
    val program = new Program(VMOpTest.vmSettings, BinaryData("73471FD3AD3E9EEADEEC4608B92D16CE6B500704CC395E60076000396000605f556014600054601e60205463abcddcba6040545b51602001600a5254516040016014525451606001601e5254516080016028525460a052546016604860003960166000f26000603f556103e75660005460005360200235"), invoker, Long.MaxValue) // Push address on the stack and perform EXTCODECOPY
    val vm = new VM(VMOpTest.vmSettings, VMHook.EMPTY)
    vm.step(program)
    val item1 = program.stackPop
    assert(item1 == DataWord.of(BinaryData("000000000000000000000000471FD3AD3E9EEADEEC4608B92D16CE6B500704CC")))
  }

  private var dataBase: DataBase = null

  @Before
  def populate(): Unit = {
    dataBase = new DataBase(settings, _consensusSettings)
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


  private val _witness1 = Witness("init1",
    PublicKey("022ac01a1ea9275241615ea6369c85b41e2016abc47485ec616c3c583f1b92a5c8").pubKeyHash)
  //Some(new PrivateKey(BinaryData("efc382ccc0358f468c2a80f3738211be98e5ae419fc0907cb2f51d3334001471"))))

  private val _witness2 = Witness("init2",
    PublicKey("03c3333373adc0636b1d67d4bca3d8b34a53d663698119369981e67866250d3a74").pubKeyHash)
  //Some(new PrivateKey(BinaryData("cc7b7fa6e706944fa2d75652065f95ef2f364316e172601320655aac0e648165"))))

  private val _witness3 = Witness("init3",
    PublicKey("020550de6ce7ed53ff018cccf1095893edba43f798252d6983e0fd2ca5af3ee0da").pubKeyHash)
  //Some(new PrivateKey(BinaryData("db71fe7c0ac4ca3e8cef95bf55cf535eaa8fe0c80d18e0cb19af8d7071b8a184"))))

  private val _witness4 = Witness("init4", // APPnx5YahVg1dTgeWkp1fE33ftvAaGbeQaR  L2C4Za8VSx2iBgszQarHx4YzqHvfumkHjbi6bNqvqst6mc8QcuZ7
    PublicKey("0246f896de22582786884d7d7ae27ef00cc8fed167bcdb8c305fbbc3dd9cca696c").pubKeyHash)
  //Some(new PrivateKey(BinaryData("9456beec947b368eda4be03f6c306703d9b2eda49f661285944b4e1f07ae18f3"))))

  private val _consensusSettings = ConsensusSettings(500, 500, 1, 4, 63000, Array(_witness1, _witness2, _witness3, _witness4))

}
