package org.variantsync.truediffdetective

import org.variantsync.diffdetective.variation.tree.VariationTreeNode
import org.variantsync.diffdetective.variation.{Label, NodeType}

import scala.collection.mutable.ListBuffer

object TrueDiffDetective {
  def wrapVariationTreeNode[L <: Label](variationTreeNode: VariationTreeNode[L]): VariationWrapper = {
    val buf: ListBuffer[VariationWrapper] = ListBuffer[VariationWrapper]()
    variationTreeNode.getChildren.forEach(c => buf += wrapVariationTreeNode(c))
    var newNode: VariationWrapper = null
    if (variationTreeNode.isRoot) {
      newNode = RootNodeWrapper(buf.toSeq, variationTreeNode.getLabel)
    }
    else {
      newNode = variationTreeNode.getNodeType match {
        case NodeType.ARTIFACT => ArtifactNodeWrapper(buf.toSeq, variationTreeNode.getLabel)
        case NodeType.IF => IfNodeWrapper(buf.toSeq, variationTreeNode.getFormula, variationTreeNode.getLabel)
        case NodeType.ELIF => ElifNodeWrapper(buf.toSeq, variationTreeNode.getFormula, variationTreeNode.getLabel)
        case NodeType.ELSE => ElseNodeWrapper(buf.toSeq, variationTreeNode.getLabel)
      }
    }
    newNode
  }
}