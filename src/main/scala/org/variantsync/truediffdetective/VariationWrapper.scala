package org.variantsync.truediffdetective

import org.variantsync.diffdetective.variation.{Label, NodeType}
import truechange._
import truediff.{Diffable, DiffableList, Hashable}

import scala.collection.mutable.ListBuffer

case class JavaLitTypeOrNull(cl: Class[_]) extends LitType {
  override def accepts(value: Any): Boolean = cl.isInstance(value) || value == null
}

trait VariationWrapper extends Diffable

case class VariationTreeNodeWrapper[L <: Label](override val _tag: Tag, children: DiffableList[VariationWrapper], formula: org.prop4j.Node, label: L) extends VariationWrapper {
  override lazy val literalHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    Hashable.hash(this.tag.toString, digest)
    this.label.getLines.forEach(Hashable.hash(_, digest))
    if (formula != null) {
      Hashable.hash(this.formula.toString(), digest)
    }
    this.directSubtrees.foreach(t => digest.update(t.literalHash))
    digest.digest()
  }

  override val treeheight: Int = children.treeheight

  override def treesize: Int = children.treesize

  override def toStringWithURI: String = s"${this.getClass.getSimpleName}_$uri($tag, ${children.toStringWithURI}, ${if (formula != null) formula.toString + ", " else ""}$label)"

  override def sig: Signature = Signature(SortType(classOf[VariationWrapper].getCanonicalName), this.tag,
    Map("children" -> ListType(SortType(classOf[VariationWrapper].getCanonicalName))),
    Map("label" -> JavaLitType(classOf[Label]), "formula" -> JavaLitTypeOrNull(classOf[org.prop4j.Node])))

  override protected def literals: Iterable[Any] = Iterable(formula, label)

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(children)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: VariationTreeNodeWrapper[L] =>
      if (this.tag == that.tag) {
        val children = this.children.computeEditScript(that.children, this.uri, this.tag, NamedLink("children"), edits).asInstanceOf[DiffableList[VariationWrapper]]
        updateOwnLiterals(that, edits)
        VariationTreeNodeWrapper[L](tag, children, that.formula, that.label).withURI(this.uri)
      }
      else null
    case _ => null
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    // case 1: we are assigned -> no need to load, just update
    if (this.assigned != null) {
      return this.assigned.updateLiterals(this, edits)
    }

    // case 2: we are unassigned -> load
    val children = this.children.loadUnassigned(edits).asInstanceOf[DiffableList[VariationWrapper]]
    val childrenInsert = edits.mergeKidInsert(children.uri)
    val newtree = VariationTreeNodeWrapper[L](tag, children, formula, label).withURI(this.uri)
    edits += InsertNode(newtree.uri, this.tag, Seq("children" -> childrenInsert), Seq("formula" -> formula, "label" -> label))
    newtree
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.children.loadInitial(edits)
    val childrenInsert = edits.mergeKidInsert(this.children.uri)
    edits += InsertNode(this.uri, this.tag, Seq("children" -> childrenInsert), Seq("formula" -> formula, "label" -> label))
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    // case 1: we are assigned -> unassign
    if (this.assigned != null) {
      this.assigned = null
    }
    // case 2: we are not assigned -> unload
    else {
      edits += Remove(this.uri, this.tag, Seq("children" -> children.uri), Seq("formula" -> formula, "label" -> label))
      this.children.unloadUnassigned(edits)
      edits.mergeKidRemove(this.children.uri, "children")
    }
  }

  private def updateOwnLiterals(that: VariationTreeNodeWrapper[L], edits: EditScriptBuffer): Unit = {
    val oldbuf: ListBuffer[(String, Any)] = ListBuffer[(String, Any)]()
    val newbuf: ListBuffer[(String, Any)] = ListBuffer[(String, Any)]()

    if (this.formula != that.formula) {
      oldbuf += "formula" -> this.formula
      newbuf += "formula" -> that.formula
    }
    if (!this.label.getLines.equals(that.label.getLines)) {
      oldbuf += "label" -> this.label
      newbuf += "label" -> that.label
    }
    if (newbuf.nonEmpty) {
      edits += Update(this.uri, this.tag, oldbuf, newbuf)
    }
  }

  override def updateLiterals(_that: Diffable, edits: EditScriptBuffer): Diffable = {
    if (this.literalHash sameElements _that.literalHash)
      this
    else {
      val that = _that.asInstanceOf[VariationTreeNodeWrapper[L]]
      updateOwnLiterals(that, edits)
      val newlist = this.children.updateLiterals(that.children, edits).asInstanceOf[DiffableList[VariationWrapper]]
      VariationTreeNodeWrapper[L](tag, newlist, that.formula, that.label).withURI(this.uri)
    }
  }
}

object VariationTreeNodeWrapper {
  def apply[L <: Label](_tag: Tag, children: DiffableList[VariationWrapper], formula: org.prop4j.Node, label: L): VariationTreeNodeWrapper[L] = {
    new VariationTreeNodeWrapper[L](_tag, children, formula, label)
  }

  def apply[L <: Label](isRoot: Boolean, nodeType: NodeType, children: DiffableList[VariationWrapper], formula: org.prop4j.Node, label: L): VariationTreeNodeWrapper[L] = {
    VariationTreeNodeWrapper[L](NamedTag(if (isRoot) "ROOT" else nodeType.toString), children, formula, label)
  }

  def apply[L <: Label](isRoot: Boolean, nodeType: NodeType, children: DiffableList[VariationWrapper], label: L): VariationTreeNodeWrapper[L] = {
    require(!isRoot || nodeType == NodeType.ARTIFACT || nodeType == NodeType.ELSE)
    VariationTreeNodeWrapper[L](isRoot, nodeType, children, null, label)
  }
}