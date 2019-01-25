package com.apex.solidity.compiler

import java.io.{File, IOException, InputStream}
import java.nio.file.{Files, StandardCopyOption}
import java.util.Scanner

class Solc private[compiler]() {    //val config: SystemProperties
  private var solc: File = null

  try
    init(null)    // SystemProperties.customSolcPath
  catch {
    case e: IOException =>
      throw new RuntimeException("Can't init solc compiler: ", e)
  }

  @throws[IOException]
  private def init(customSolcPath: String): Unit = {
    if (customSolcPath != null) {
      solc = new File(customSolcPath)
      if (!solc.canExecute) throw new RuntimeException(String.format("Solidity compiler from config solc.path: %s is not a valid executable", customSolcPath))
    }
    else initBundled()
  }

  @throws[IOException]
  private def initBundled(): Unit = {
    val tmpDir = new File(System.getProperty("java.io.tmpdir"), "solc")
    tmpDir.mkdirs
    val is = getClass.getResourceAsStream("/native/" + getOS + "/solc/file.list")

    val scanner = new Scanner(is)
    try {
      while (scanner.hasNext) {
        val s = scanner.next
        val targetFile = new File(tmpDir, s)
        val fis = getClass.getResourceAsStream("/native/" + getOS + "/solc/" + s)
        Files.copy(fis, targetFile.toPath, StandardCopyOption.REPLACE_EXISTING)
        if (solc == null) { // first file in the list denotes executable
          solc = targetFile
          solc.setExecutable(true)
        }
        targetFile.deleteOnExit()
      }
    }
    finally if (scanner != null) scanner.close()
  }

  private def getOS = {
    val osName = System.getProperty("os.name").toLowerCase
    if (osName.contains("win")) "win"
    else if (osName.contains("linux")) "linux"
    else if (osName.contains("mac")) "mac"
    else throw new RuntimeException("Can't find solc compiler: unrecognized OS: " + osName)
  }

  def getExecutable: File = solc
}


