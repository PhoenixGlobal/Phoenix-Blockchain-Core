package com.apex.solidity.compiler

import java.io.File
import java.util
import java.util
import java.util.{HashMap, Map}

import ContractException.assembleError

class Sources(val files: Array[File]) {
  for (file <- files) {
    artifacts.put(file.getName, new SourceArtifact(file))
  }
  final private val artifacts = new util.HashMap[String, SourceArtifact]
  private var targetArtifact: String = null

  def resolveDependencies(): Unit = {
    import scala.collection.JavaConversions._
    for (srcName <- artifacts.keySet) {
      val src = artifacts.get(srcName)
      import scala.collection.JavaConversions._
      for (dep <- src.getDependencies) {
        val depArtifact = artifacts.get(dep)
        if (depArtifact == null)
          throw assembleError("can't resolve dependency: dependency '%s' not found.", dep)
        src.injectDependency(depArtifact)
      }
    }
    import scala.collection.JavaConversions._
    for (artifact <- artifacts.values) {
      if (!artifact.hasDependentArtifacts)
        targetArtifact = artifact.getName
    }
  }

  def plainSource: String = artifacts.get(targetArtifact).plainSource
}

