package org.variantsync.truediffdetective

import org.variantsync.diffdetective.variation.Label
import org.variantsync.diffdetective.variation.tree.VariationTreeNode
import truechange.SortType
import truediff.DiffableList

import scala.collection.mutable.ListBuffer

object TrueDiffDetective {
  def wrapVariationTreeNode[L <: Label](variationTreeNode: VariationTreeNode[L]): VariationWrapper = {
    val buf: ListBuffer[VariationWrapper] = ListBuffer[VariationWrapper]()
    variationTreeNode.getChildren.forEach(c => buf += wrapVariationTreeNode(c))
    val newNode: VariationWrapper = VariationTreeNodeWrapper(variationTreeNode.isRoot, variationTreeNode.getNodeType,
      DiffableList.from(buf.toSeq, SortType(classOf[VariationWrapper].getCanonicalName)), variationTreeNode.getFormula, variationTreeNode.getLabel)
    newNode
  }
}