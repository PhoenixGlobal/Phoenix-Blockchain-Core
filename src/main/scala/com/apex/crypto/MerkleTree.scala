/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: MerkleTree.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.crypto

class MerkleTreeNode(var hash: UInt256 = null, var parent: MerkleTreeNode = null, var left: MerkleTreeNode = null, var right: MerkleTreeNode = null) {
  def isRoot: Boolean = {
    parent == null
  }

  def isLeaf: Boolean = {
    left == null && right == null
  }
}

class MerkleTree(hashes: Seq[UInt256]) {
  var rootNode: MerkleTreeNode = null
  var depth: Int = 0

  if (hashes.length == 0) throw new IllegalArgumentException
  rootNode = MerkleTree.build(hashes.map(new MerkleTreeNode(_)))
  var node = rootNode
  while (node != null) {
    node = node.left
    depth += 1
  }
}

object MerkleTree {
  def build(leaves: Seq[MerkleTreeNode]): MerkleTreeNode = {
    if (leaves.length == 0) throw new IllegalArgumentException("leaves")
    if (leaves.length == 1) return leaves(0)
    val parents = new Array[MerkleTreeNode]((leaves.length + 1) / 2)
    for (i <- 0 to parents.length - 1) {
      parents(i) = new MerkleTreeNode()
      parents(i).left = leaves(i * 2)
      leaves(i * 2).parent = parents(i)
      if (i * 2 + 1 == leaves.length) {
        parents(i).right = parents(i).left
      } else {
        parents(i).right = leaves(i * 2 + 1)
        leaves(i * 2 + 1).parent = parents(i)
      }
      parents(i).hash = new UInt256(Crypto.hash256(parents(i).left.hash.data ++ parents(i).right.hash.data))
    }
    build(parents)
  }

  def root(hashes: Seq[UInt256]): UInt256 = {
    if (hashes == null || hashes.isEmpty) throw new IllegalArgumentException
    if (hashes.length == 1) {
      hashes(0)
    } else {
      new MerkleTree(hashes).rootNode.hash
    }
  }
}
