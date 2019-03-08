package com.apex.test


import com.apex.solidity.compiler.CompilationResult
import com.apex.solidity.compiler.SolidityCompiler
import com.apex.solidity.compiler.SolidityCompiler.Options._
import org.junit.Assert
import org.junit.Test
import java.io.IOException
import java.nio.file.Path
import java.nio.file.Paths
import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.core.StringContains.containsString


//object CompilerTest {
//  @throws[Exception]
//  def main(args: Array[String]): Unit = {
//    new CompilerTest().simpleTest()
//  }
//}

@Test
class CompilerTest {
  @Test
  @throws[IOException]
  def solc_getVersion_shouldWork(): Unit = {
    val version = SolidityCompiler.runGetVersionOutput
    // ##### May produce 2 lines:
    //solc, the solidity compiler commandline interface
    //Version: 0.4.7+commit.822622cf.mod.Darwin.appleclang
    System.out.println(version)
    assertThat(version, containsString("Version:"))
  }

  @Test
  @throws[IOException]
  def simpleTest(): Unit = {
    val contract = "pragma solidity ^0.5.2;\n" + "\n" + "contract a {\n" + "\n" + "        mapping(address => string) private mailbox;\n" + "\n" + "        event Mailed(address from, string message);\n" + "        event Read(address from, string message);\n" + "\n" + "}"
    val res = SolidityCompiler.compile(contract.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
    System.out.println("Out: '" + res.output + "'")
    System.out.println("Err: '" + res.errors + "'")
    val result = CompilationResult.parse(res.output)
    if (result.getContract("a") != null)
      System.out.println(result.getContract("a").bin)
    else
      Assert.fail()
  }

  @Test
  @throws[IOException]
  def defaultFuncTest(): Unit = {
    val contractSrc = "pragma solidity ^0.5.2;\n" + "contract a {" + "        function() external { }" + "}"
    val res = SolidityCompiler.compile(contractSrc.getBytes, true, Seq(ABI, BIN, INTERFACE, METADATA))
    System.out.println("Out: '" + res.output + "'")
    System.out.println("Err: '" + res.errors + "'")
    val result = CompilationResult.parse(res.output)
    val a = result.getContract("a")
    //val contract = new CallTransaction.Contract(a.abi)
    //System.out.print(contract.functions(0).toString)
  }

  @Test
  @throws[IOException]
  def compileFilesTest(): Unit = {
    val source = Paths.get("src", "test", "resources", "solidity", "file1.sol")
    val res: SolidityCompiler.Result = SolidityCompiler.compile(source.toFile, true, Seq(ABI, BIN, INTERFACE, METADATA))
    System.out.println("Out: '" + res.output + "'")
    System.out.println("Err: '" + res.errors + "'")
    val result = CompilationResult.parse(res.output)
    Assert.assertEquals("test1", result.getContractName)
    Assert.assertEquals(source.toAbsolutePath, result.getContractPath)
    val a = result.getContract(source, "test1")
    //val contract = new CallTransaction.Contract(a.abi)
    //System.out.print(contract.functions(0).toString)
  }

  @Test
  @throws[IOException]
  def compileFilesWithImportTest(): Unit = {
    val source = Paths.get("src", "test", "resources", "solidity", "file2.sol")
    val res: SolidityCompiler.Result = SolidityCompiler.compile(source.toFile, true, Seq(ABI, BIN, INTERFACE, METADATA))
    System.out.println("Out: '" + res.output + "'")
    System.out.println("Err: '" + res.errors + "'")
    val result = CompilationResult.parse(res.output)
    val a = result.getContract(source, "test2")
    //val contract = new CallTransaction.Contract(a.abi)
    //System.out.print(contract.functions(0).toString)
  }

  @Test
  @throws[IOException]
  def compileFilesWithImportFromParentFileTest(): Unit = {
    val source = Paths.get("src", "test", "resources", "solidity", "foo", "file3.sol")
    val allowPathsOption = new SolidityCompiler.AllowPaths(Seq(source.getParent.getParent.toFile))
    val res: SolidityCompiler.Result = SolidityCompiler.compile(source.toFile, true, Seq(ABI, BIN, INTERFACE, METADATA, allowPathsOption))
    System.out.println("Out: '" + res.output + "'")
    System.out.println("Err: '" + res.errors + "'")
    val result = CompilationResult.parse(res.output)
    Assert.assertEquals(2, result.getContractKeys.size)
    Assert.assertEquals(result.getContract("test3"), result.getContract(source, "test3"))
    Assert.assertNotNull(result.getContract("test1"))
    val a = result.getContract(source, "test3")
    //val contract = new CallTransaction.Contract(a.abi)
    //System.out.print(contract.functions(0).toString)
  }

  @Test
  @throws[IOException]
  def compileFilesWithImportFromParentStringTest(): Unit = {
    val source = Paths.get("src", "test", "resources", "solidity", "foo", "file3.sol")
    val allowPathsOption = new SolidityCompiler.AllowPaths(Seq(source.getParent.getParent.toAbsolutePath.toString))
    val res: SolidityCompiler.Result = SolidityCompiler.compile(source.toFile, true, Seq(ABI, BIN, INTERFACE, METADATA, allowPathsOption))
    System.out.println("Out: '" + res.output + "'")
    System.out.println("Err: '" + res.errors + "'")
    val result = CompilationResult.parse(res.output)
    val a = result.getContract(source, "test3")
    //val contract = new CallTransaction.Contract(a.abi)
    //System.out.print(contract.functions(0).toString)
  }

  @Test
  @throws[IOException]
  def compileFilesWithImportFromParentPathTest(): Unit = {
    val source = Paths.get("src", "test", "resources", "solidity", "foo", "file3.sol")
    val allowPathsOption = new SolidityCompiler.AllowPaths(Seq(source.getParent.getParent))
    val res: SolidityCompiler.Result = SolidityCompiler.compile(source.toFile, true, Seq(ABI, BIN, INTERFACE, METADATA, allowPathsOption))
    System.out.println("Out: '" + res.output + "'")
    System.out.println("Err: '" + res.errors + "'")
    val result = CompilationResult.parse(res.output)
    val a = result.getContract("test3")
    //val contract = new CallTransaction.Contract(a.abi)
    //System.out.print(contract.functions(0).toString)
  }
}

