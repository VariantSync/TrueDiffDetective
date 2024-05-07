package org.variantsync.truediffdetective

import org.variantsync.diffdetective.variation.Label
import truechange._
import truediff.{Diffable, DiffableList, Hashable}

import scala.collection.mutable.ListBuffer

trait VariationWrapper extends Diffable

case class ArtifactNodeWrapper[L <: Label](children: DiffableList[VariationWrapper], label: L) extends VariationWrapper {
  override lazy val literalHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    Hashable.hash(this.tag.toString, digest)
    this.label.getLines.forEach(Hashable.hash(_, digest))
    this.directSubtrees.foreach(t => digest.update(t.literalHash))
    digest.digest()
  }

  override val treeheight: Int = children.treeheight

  override def treesize: Int = children.treesize

  override def toStringWithURI: String = s"${this.getClass.getSimpleName}_$uri($tag, ${children.toStringWithURI}, $label)"

  override def sig: Signature = Signature(SortType(classOf[VariationWrapper].getCanonicalName), this.tag,
    Map("children" -> ListType(SortType(classOf[VariationWrapper].getCanonicalName))),
    Map("label" -> JavaLitType(classOf[Label])))

  override protected def literals: Iterable[Any] = Iterable(label)

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(children)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: ArtifactNodeWrapper[L] =>
      val children = this.children.computeEditScript(that.children, this.uri, this.tag, NamedLink("children"), edits).asInstanceOf[DiffableList[VariationWrapper]]
      updateOwnLiterals(that, edits)
      ArtifactNodeWrapper[L](children, that.label).withURI(this.uri)
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
    val newtree = ArtifactNodeWrapper[L](children, label).withURI(this.uri)
    edits += InsertNode(newtree.uri, this.tag, Seq("children" -> childrenInsert), Seq("label" -> label))
    newtree
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.children.loadInitial(edits)
    val childrenInsert = edits.mergeKidInsert(this.children.uri)
    edits += InsertNode(this.uri, this.tag, Seq("children" -> childrenInsert), Seq("label" -> label))
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    // case 1: we are assigned -> unassign
    if (this.assigned != null) {
      this.assigned = null
    }
    // case 2: we are not assigned -> unload
    else {
      edits += Remove(this.uri, this.tag, Seq("children" -> children.uri), Seq("label" -> label))
      this.children.unloadUnassigned(edits)
      edits.mergeKidRemove(this.children.uri, "children")
    }
  }

  private def updateOwnLiterals(that: ArtifactNodeWrapper[L], edits: EditScriptBuffer): Unit = {
    val oldbuf: ListBuffer[(String, Any)] = ListBuffer[(String, Any)]()
    val newbuf: ListBuffer[(String, Any)] = ListBuffer[(String, Any)]()

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
      val that = _that.asInstanceOf[ArtifactNodeWrapper[L]]
      updateOwnLiterals(that, edits)
      val newlist = this.children.updateLiterals(that.children, edits).asInstanceOf[DiffableList[VariationWrapper]]
      ArtifactNodeWrapper[L](newlist, that.label).withURI(this.uri)
    }
  }
}

case class IfNodeWrapper[L <: Label](children: DiffableList[VariationWrapper], formula: org.prop4j.Node, label: L) extends VariationWrapper {
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
    Map("label" -> JavaLitType(classOf[Label]), "formula" -> JavaLitType(classOf[org.prop4j.Node])))

  override protected def literals: Iterable[Any] = Iterable(formula, label)

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(children)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: IfNodeWrapper[L] =>
      val children = this.children.computeEditScript(that.children, this.uri, this.tag, NamedLink("children"), edits).asInstanceOf[DiffableList[VariationWrapper]]
      updateOwnLiterals(that, edits)
      IfNodeWrapper[L](children, that.formula, that.label).withURI(this.uri)
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
    val newtree = IfNodeWrapper[L](children, formula, label).withURI(this.uri)
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

  private def updateOwnLiterals(that: IfNodeWrapper[L], edits: EditScriptBuffer): Unit = {
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
      val that = _that.asInstanceOf[IfNodeWrapper[L]]
      updateOwnLiterals(that, edits)
      val newlist = this.children.updateLiterals(that.children, edits).asInstanceOf[DiffableList[VariationWrapper]]
      IfNodeWrapper[L](newlist, that.formula, that.label).withURI(this.uri)
    }
  }
}

case class ElifNodeWrapper[L <: Label](children: DiffableList[VariationWrapper], formula: org.prop4j.Node, label: L) extends VariationWrapper {
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
    Map("label" -> JavaLitType(classOf[Label]), "formula" -> JavaLitType(classOf[org.prop4j.Node])))

  override protected def literals: Iterable[Any] = Iterable(formula, label)

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(children)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: ElifNodeWrapper[L] =>
      val children = this.children.computeEditScript(that.children, this.uri, this.tag, NamedLink("children"), edits).asInstanceOf[DiffableList[VariationWrapper]]
      updateOwnLiterals(that, edits)
      ElifNodeWrapper[L](children, that.formula, that.label).withURI(this.uri)
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
    val newtree = ElifNodeWrapper[L](children, formula, label).withURI(this.uri)
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

  private def updateOwnLiterals(that: ElifNodeWrapper[L], edits: EditScriptBuffer): Unit = {
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
      val that = _that.asInstanceOf[ElifNodeWrapper[L]]
      updateOwnLiterals(that, edits)
      val newlist = this.children.updateLiterals(that.children, edits).asInstanceOf[DiffableList[VariationWrapper]]
      ElifNodeWrapper[L](newlist, that.formula, that.label).withURI(this.uri)
    }
  }
}

case class ElseNodeWrapper[L <: Label](children: DiffableList[VariationWrapper], label: L) extends VariationWrapper {
  override lazy val literalHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    Hashable.hash(this.tag.toString, digest)
    this.label.getLines.forEach(Hashable.hash(_, digest))
    this.directSubtrees.foreach(t => digest.update(t.literalHash))
    digest.digest()
  }

  override val treeheight: Int = children.treeheight

  override def treesize: Int = children.treesize

  override def toStringWithURI: String = s"${this.getClass.getSimpleName}_$uri($tag, ${children.toStringWithURI}, $label)"

  override def sig: Signature = Signature(SortType(classOf[VariationWrapper].getCanonicalName), this.tag,
    Map("children" -> ListType(SortType(classOf[VariationWrapper].getCanonicalName))),
    Map("label" -> JavaLitType(classOf[Label])))

  override protected def literals: Iterable[Any] = Iterable(label)

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(children)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: ElseNodeWrapper[L] =>
      val children = this.children.computeEditScript(that.children, this.uri, this.tag, NamedLink("children"), edits).asInstanceOf[DiffableList[VariationWrapper]]
      updateOwnLiterals(that, edits)
      ElseNodeWrapper[L](children, that.label).withURI(this.uri)
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
    val newtree = ElseNodeWrapper[L](children, label).withURI(this.uri)
    edits += InsertNode(newtree.uri, this.tag, Seq("children" -> childrenInsert), Seq("label" -> label))
    newtree
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.children.loadInitial(edits)
    val childrenInsert = edits.mergeKidInsert(this.children.uri)
    edits += InsertNode(this.uri, this.tag, Seq("children" -> childrenInsert), Seq("label" -> label))
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    // case 1: we are assigned -> unassign
    if (this.assigned != null) {
      this.assigned = null
    }
    // case 2: we are not assigned -> unload
    else {
      edits += Remove(this.uri, this.tag, Seq("children" -> children.uri), Seq("label" -> label))
      this.children.unloadUnassigned(edits)
      edits.mergeKidRemove(this.children.uri, "children")
    }
  }

  private def updateOwnLiterals(that: ElseNodeWrapper[L], edits: EditScriptBuffer): Unit = {
    val oldbuf: ListBuffer[(String, Any)] = ListBuffer[(String, Any)]()
    val newbuf: ListBuffer[(String, Any)] = ListBuffer[(String, Any)]()

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
      val that = _that.asInstanceOf[ElseNodeWrapper[L]]
      updateOwnLiterals(that, edits)
      val newlist = this.children.updateLiterals(that.children, edits).asInstanceOf[DiffableList[VariationWrapper]]
      ElseNodeWrapper[L](newlist, that.label).withURI(this.uri)
    }
  }
}

case class RootNodeWrapper[L <: Label](children: DiffableList[VariationWrapper], label: L) extends VariationWrapper {
  override lazy val literalHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    Hashable.hash(this.tag.toString, digest)
    this.label.getLines.forEach(Hashable.hash(_, digest))
    this.directSubtrees.foreach(t => digest.update(t.literalHash))
    digest.digest()
  }

  override val treeheight: Int = children.treeheight

  override def treesize: Int = children.treesize

  override def toStringWithURI: String = s"${this.getClass.getSimpleName}_$uri($tag, ${children.toStringWithURI}, $label)"

  override def sig: Signature = Signature(SortType(classOf[VariationWrapper].getCanonicalName), this.tag,
    Map("children" -> ListType(SortType(classOf[VariationWrapper].getCanonicalName))),
    Map("label" -> JavaLitType(classOf[Label])))

  override protected def literals: Iterable[Any] = Iterable(label)

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(children)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: RootNodeWrapper[L] =>
      val children = this.children.computeEditScript(that.children, this.uri, this.tag, NamedLink("children"), edits).asInstanceOf[DiffableList[VariationWrapper]]
      updateOwnLiterals(that, edits)
      RootNodeWrapper[L](children, that.label).withURI(this.uri)
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
    val newtree = RootNodeWrapper[L](children, label).withURI(this.uri)
    edits += InsertNode(newtree.uri, this.tag, Seq("children" -> childrenInsert), Seq("label" -> label))
    newtree
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.children.loadInitial(edits)
    val childrenInsert = edits.mergeKidInsert(this.children.uri)
    edits += InsertNode(this.uri, this.tag, Seq("children" -> childrenInsert), Seq("label" -> label))
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    // case 1: we are assigned -> unassign
    if (this.assigned != null) {
      this.assigned = null
    }
    // case 2: we are not assigned -> unload
    else {
      edits += Remove(this.uri, this.tag, Seq("children" -> children.uri), Seq("label" -> label))
      this.children.unloadUnassigned(edits)
      edits.mergeKidRemove(this.children.uri, "children")
    }
  }

  private def updateOwnLiterals(that: RootNodeWrapper[L], edits: EditScriptBuffer): Unit = {
    val oldbuf: ListBuffer[(String, Any)] = ListBuffer[(String, Any)]()
    val newbuf: ListBuffer[(String, Any)] = ListBuffer[(String, Any)]()

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
      val that = _that.asInstanceOf[RootNodeWrapper[L]]
      updateOwnLiterals(that, edits)
      val newlist = this.children.updateLiterals(that.children, edits).asInstanceOf[DiffableList[VariationWrapper]]
      RootNodeWrapper[L](newlist, that.label).withURI(this.uri)
    }
  }
}

object ArtifactNodeWrapper {
  def apply[L <: Label](es: Seq[VariationWrapper], label: L): ArtifactNodeWrapper[L] = ArtifactNodeWrapper[L](DiffableList.from(es, SortType(classOf[VariationWrapper].getCanonicalName)), label)
}

object IfNodeWrapper {
  def apply[L <: Label](es: Seq[VariationWrapper], formula: org.prop4j.Node, label: L): IfNodeWrapper[L] = IfNodeWrapper[L](DiffableList.from(es, SortType(classOf[VariationWrapper].getCanonicalName)), formula, label)
}

object ElifNodeWrapper {
  def apply[L <: Label](es: Seq[VariationWrapper], formula: org.prop4j.Node, label: L): ElifNodeWrapper[L] = ElifNodeWrapper[L](DiffableList.from(es, SortType(classOf[VariationWrapper].getCanonicalName)), formula, label)
}

object ElseNodeWrapper {
  def apply[L <: Label](es: Seq[VariationWrapper], label: L): ElseNodeWrapper[L] = ElseNodeWrapper[L](DiffableList.from(es, SortType(classOf[VariationWrapper].getCanonicalName)), label)
}

object RootNodeWrapper {
  def apply[L <: Label](es: Seq[VariationWrapper], label: L): RootNodeWrapper[L] = RootNodeWrapper[L](DiffableList.from(es, SortType(classOf[VariationWrapper].getCanonicalName)), label)
}