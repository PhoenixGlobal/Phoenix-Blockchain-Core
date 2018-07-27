package com.apex.core.structure.state

import com.apex.core.{NodeViewComponent, VersionTag}

trait StateReader extends NodeViewComponent {

  def version: VersionTag

  def maxRollbackDepth: Int

}
