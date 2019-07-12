package com.apex.solidity.compiler

import java.io.BufferedOutputStream
import java.io.BufferedReader
import java.io.File
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.io.Serializable
import java.nio.file.Path
import java.util

import com.apex.solidity.compiler.SolidityCompiler._

object SolidityCompiler {

  private var INSTANCE: SolidityCompiler = null

  class Result(var errors: String, var output: String, var success: Boolean) {
    def isFailed: Boolean = !success
  }

  trait CompilerOption extends Serializable {
    def getValue: String
    def getName: String
  }

  final class OutputOption(var name: String) extends CompilerOption {
    override def getValue = ""

    override def getName: String = name
    override def toString: String = name
  }

  final class NameOnlyOption(var name: String) extends CompilerOption {
    override def getValue = ""

    override def getName: String = name
    override def toString: String = name
  }

  class ListOption (var name: String, val values: Seq[Any]) extends CompilerOption {
    override def getValue: String = {
      val result = new StringBuilder
      import scala.collection.JavaConversions._
      for (value <- values) {
        if (classOf[OutputOption].isAssignableFrom(value.getClass))
          result.append(if (result.length == 0) value.asInstanceOf[OutputOption].getName
          else ',' + value.asInstanceOf[OutputOption].getName)
        else if (classOf[Path].isAssignableFrom(value.getClass))
          result.append(if (result.length == 0) value.asInstanceOf[Path].toAbsolutePath.toString
          else ',' + value.asInstanceOf[Path].toAbsolutePath.toString)
        else if (classOf[File].isAssignableFrom(value.getClass))
          result.append(if (result.length == 0) value.asInstanceOf[File].getAbsolutePath
          else ',' + value.asInstanceOf[File].getAbsolutePath)
        else if (classOf[String].isAssignableFrom(value.getClass))
          result.append(if (result.length == 0) value
          else "," + value)
        else
          throw new UnsupportedOperationException("Unexpected type, value '" + value + "' cannot be retrieved.")
      }
      result.toString
    }
    override def getName: String = name
    override def toString: String = name
  }

  class CustomOption(var name: String, var value: String) extends CompilerOption {

    name = if (name.startsWith("--")) name.substring(2) else name

    override def getValue = value
    override def getName: String = name
  }

  class CombinedJson(override val values: Seq[Any]) extends ListOption("combined-json", values) {
  }

  class AllowPaths(override val values: Seq[Any]) extends ListOption("allow-paths", values) {
  }

  @throws[IOException]
  def runGetVersionOutput: String = {
    val commandParts = new util.ArrayList[String]
    commandParts.add(getInstance.solc.getExecutable.getCanonicalPath)
    commandParts.add("--" + Options.VERSION.getName)
    val processBuilder = new ProcessBuilder(commandParts).directory(getInstance.solc.getExecutable.getParentFile)
    processBuilder.environment.put("LD_LIBRARY_PATH", getInstance.solc.getExecutable.getParentFile.getCanonicalPath)
    val process = processBuilder.start
    val error = new ParallelReader(process.getErrorStream)
    val output = new ParallelReader(process.getInputStream)
    error.start()
    output.start()
    try
      process.waitFor
    catch {
      case e: InterruptedException => throw new RuntimeException(e)
    }
    if (process.exitValue == 0)
      return output.getContent
    throw new RuntimeException("Problem getting solc version: " + error.getContent)
  }

  object Options {
    val AST       = new OutputOption("ast")
    val BIN       = new OutputOption("bin")
    val INTERFACE = new OutputOption("interface")
    val ABI       = new OutputOption("abi")
    val METADATA  = new OutputOption("metadata")
    val ASTJSON   = new OutputOption("ast-json")

    val OPTIMIZE  = new NameOnlyOption("optimize")
    val VERSION   = new NameOnlyOption("version")
  }

  def getInstance: SolidityCompiler = {
    if (SolidityCompiler.INSTANCE == null)
      SolidityCompiler.INSTANCE = new SolidityCompiler()
    SolidityCompiler.INSTANCE
  }

  class ParallelReader private[compiler](var stream: InputStream) extends Thread {
    private val content = new StringBuilder

    def getContent: String = getContent(true)

    def getContent(waitForComplete: Boolean): String = this.synchronized {
      if (waitForComplete)
        while (stream != null)
          try
            wait()
          catch {
            case e: InterruptedException => throw new RuntimeException(e)
          }
      content.toString
    }

    override def run(): Unit = {

      val reader = new BufferedReader(new InputStreamReader(stream))
      try {
        var line: String = reader.readLine
        while (line != null) {
          content.append(line).append("\n")
          line = reader.readLine
        }
      }
      catch {
        case ioe: IOException => ioe.printStackTrace()
      }
      finally {
        this.synchronized { // synchronized (this)
          stream = null
          notifyAll()
        }
      }
    }
  }

  @throws[IOException]
  def compile(sourceDirectory: File, combinedJson: Boolean, options: Seq[CompilerOption]): Result = {
    getInstance.compileSrc(sourceDirectory, false, combinedJson, options)
  }

  @throws[IOException]
  def compile(source: Array[Byte], combinedJson: Boolean, options: Seq[CompilerOption]): Result = {
    getInstance.compileSrc(source, false, combinedJson, options)
  }

}

class SolidityCompiler {

  private var solc: Solc = new Solc()

  @throws[IOException]
  def compileSrc(source: File, optimize: Boolean, combinedJson: Boolean, options: Seq[CompilerOption]): Result = {
    val commandParts = prepareCommandOptions(optimize, combinedJson, options)
    commandParts.add(source.getAbsolutePath)
    val processBuilder = new ProcessBuilder(commandParts).directory(solc.getExecutable.getParentFile)
    processBuilder.environment.put("LD_LIBRARY_PATH", solc.getExecutable.getParentFile.getCanonicalPath)
    val process = processBuilder.start
    val error = new ParallelReader(process.getErrorStream)
    val output = new ParallelReader(process.getInputStream)
    error.start()
    output.start()
    try
      process.waitFor
    catch {
      case e: InterruptedException => throw new RuntimeException(e)
    }
    val success = process.exitValue == 0
    new Result(error.getContent, output.getContent, success)
  }

  @throws[IOException]
  private def prepareCommandOptions(optimize: Boolean,
                                    combinedJson: Boolean,
                                    options: Seq[CompilerOption]) = {
    val commandParts = new util.ArrayList[String]
    commandParts.add(solc.getExecutable.getCanonicalPath)
    if (optimize)
      commandParts.add("--" + Options.OPTIMIZE.getName)
    if (combinedJson) {
      val combinedJsonOption = new CombinedJson(options.filter(_.isInstanceOf[OutputOption]))
      commandParts.add("--" + combinedJsonOption.getName)
      commandParts.add(combinedJsonOption.getValue)
    }
    else {
      import scala.collection.JavaConversions._
      for (option <- options) {
        if (option.isInstanceOf[OutputOption])
          commandParts.add("--" + option.getName)
      }
      for (option <- options.filter(p => p.isInstanceOf[OutputOption])) {
        commandParts.add("--" + option.getName)
      }
    }
    import scala.collection.JavaConversions._
    for (option <- options.filter(p => p.isInstanceOf[ListOption])) {
      commandParts.add("--" + option.getName)
      commandParts.add(option.getValue)
    }
    import scala.collection.JavaConversions._
    for (option <- options.filter(p => p.isInstanceOf[CustomOption])) {
      commandParts.add("--" + option.getName)
      if (option.getValue != null)
        commandParts.add(option.getValue)
    }
    commandParts
  }

  //  private def getElementsOf[T](options: Seq[CompilerOption]) = {
  //    options.filter(_.isInstanceOf[T])
  //  }

  @throws[IOException]
  def compileSrc(source: Array[Byte],
                 optimize: Boolean,
                 combinedJson: Boolean,
                 options: Seq[CompilerOption]): Result = {
    val commandParts = prepareCommandOptions(optimize, combinedJson, options)

    commandParts.add("-")

    val processBuilder = new ProcessBuilder(commandParts).directory(solc.getExecutable.getParentFile)
    processBuilder.environment.put("LD_LIBRARY_PATH", solc.getExecutable.getParentFile.getCanonicalPath)
    val process = processBuilder.start

    val stream = new BufferedOutputStream(process.getOutputStream)
    try
      stream.write(source)
    finally if (stream != null) stream.close()

    val error = new ParallelReader(process.getErrorStream)
    val output = new ParallelReader(process.getInputStream)
    error.start()
    output.start()
    try
      process.waitFor
    catch {
      case e: InterruptedException => throw new RuntimeException(e)
    }
    val success = process.exitValue == 0
    new Result(error.getContent, output.getContent, success)
  }

}