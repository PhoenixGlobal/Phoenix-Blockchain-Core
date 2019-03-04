package com.apex.solidity.compiler

import java.io.File
import java.io.IOException
import java.util._
import java.lang.String.format
import java.util.Arrays.asList
import java.util.Collections.emptyList
import org.apache.commons.collections4.CollectionUtils.disjunction
import org.apache.commons.collections4.CollectionUtils.isNotEmpty
import org.apache.commons.lang3.StringUtils.substringsBetween
import ContractException.assembleError

//import SourceArtifact._

import scala.beans.{BeanProperty, BooleanBeanProperty}

//remove if not needed
import scala.collection.JavaConverters._

object SourceArtifact {

  private def extractDependencies(source: String): List[String] = {
    val deps: Array[String] = substringsBetween(source, "import \"", "\";")
    if (deps == null)
      Collections.emptyList[String]()
    else
      deps.toList.asJava
  }

}

class SourceArtifact(@BeanProperty var name: String, var source: String) {

  @BeanProperty
  var dependencies: List[String] = SourceArtifact.extractDependencies(source)

  source = source.replaceAll("import\\s\"\\.*?\\.sol\";", "")

  private val injectedDependencies: Set[SourceArtifact] = new HashSet()

  private val dependentArtifacts: Set[SourceArtifact] = new HashSet()

  def this(f: File) = this("", "")    // this()

  def injectDependency(srcArtifact: SourceArtifact): Unit = {
    injectedDependencies.add(srcArtifact)
    srcArtifact.addDependentArtifact(this)
  }

  private def addDependentArtifact(srcArtifact: SourceArtifact): Unit = {
    dependentArtifacts.add(srcArtifact)
  }

  def hasDependentArtifacts(): Boolean = !dependentArtifacts.isEmpty

  private def getUnresolvedDependencies(): Collection[String] = {
    val ret: Set[String] = new HashSet[String]()
    for (injectedDependency <- injectedDependencies.asScala) {
      ret.add(injectedDependency.getName)
    }
    disjunction(dependencies, ret)
  }

  def plainSource(): String = {
    val unresolvedDeps: Collection[String] = getUnresolvedDependencies
    if (isNotEmpty(unresolvedDeps)) {
      throw assembleError("Followed dependencies aren't resolved: %s",
        unresolvedDeps)
    }
    var result: String = this.source
    for (dependencyArtifact <- injectedDependencies.asScala) {
      val importDefinition: String = format("import \"%s\";", dependencyArtifact.getName)
      val dependencySrc: String = format("// %s\n%s", importDefinition, dependencyArtifact.plainSource())
      result = result.replace(importDefinition, dependencySrc)
    }
    result
  }

}
