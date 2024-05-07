// based on: https://gitlab.rlp.net/plmz/truediff/-/raw/master/truediff/src/test/scala/truediff/manual/List.scala
// and: https://gitlab.rlp.net/plmz/truediff/-/raw/master/truediff/src/test/scala/truediff/manual/Exp.scala

package org.variantsync.truedifftest

import truechange._
import truediff._

trait TreeNode extends Diffable

case class StringTreeNode(string: String, es: DiffableList[TreeNode]) extends TreeNode {
  override lazy val literalHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    Hashable.hash(this.tag.toString, digest)
    Hashable.hash(this.string, digest)
    this.directSubtrees.foreach(t => digest.update(t.literalHash))
    digest.digest()
  }

  override val treeheight: Int = es.treeheight

  override def treesize: Int = es.treesize

  override def toStringWithURI: String = s"${tag.toString}_$uri(${es.toStringWithURI}, string:$string)"

  override def sig: Signature = Signature(SortType(classOf[TreeNode].getCanonicalName), this.tag,
    Map("es" -> ListType(SortType(classOf[TreeNode].getCanonicalName))), Map("string" -> JavaLitType(classOf[String])))

  override protected def literals: Iterable[Any] = Iterable(string)

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(es)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: StringTreeNode =>
      updateOwnLiterals(that, edits)
      val es = this.es.computeEditScript(that.es, this.uri, this.tag, NamedLink("es"), edits).asInstanceOf[DiffableList[TreeNode]]
      StringTreeNode(that.string, es).withURI(this.uri)
    case _ => null
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned.updateLiterals(that, edits)
    }

    val es = this.es.loadUnassigned(edits).asInstanceOf[DiffableList[TreeNode]]
    val esInsert = edits.mergeKidInsert(es.uri)
    val newtree = StringTreeNode(string, es)
    edits += InsertNode(newtree.uri, this.tag, Seq(
      "es" -> esInsert
    ), Seq("string" -> string))
    newtree
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.es.loadInitial(edits)
    val esInsert = edits.mergeKidInsert(this.es.uri)
    edits += InsertNode(this.uri, this.tag, Seq(
      "es" -> esInsert
    ), Seq("string" -> string))
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      edits += Remove(this.uri, this.tag, Seq("es" -> es.uri), Seq("string" -> string))
      this.es.unloadUnassigned(edits)
      edits.mergeKidRemove(this.es.uri, "es")
    }
  }

  private def updateOwnLiterals(thatX: StringTreeNode, edits: EditScriptBuffer): Unit = {
    if (this.string != thatX.string) {
      edits += Update(this.uri, this.tag,
        Seq("string" -> this.string), Seq("string" -> thatX.string)
      )
    }
  }

  override def updateLiterals(that: Diffable, edits: EditScriptBuffer): Diffable = {
    val thatX = that.asInstanceOf[StringTreeNode]
    updateOwnLiterals(thatX, edits)

    val newlist = this.es.updateLiterals(that.asInstanceOf[StringTreeNode].es, edits).asInstanceOf[DiffableList[TreeNode]]
    StringTreeNode(thatX.string, newlist).withURI(this.uri)
  }
}

case class IntegerTreeNode(integer: Integer, es: DiffableList[TreeNode]) extends TreeNode {
  override lazy val literalHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    Hashable.hash(this.tag.toString, digest)
    Hashable.hash(this.integer, digest)
    this.directSubtrees.foreach(t => digest.update(t.literalHash))
    digest.digest()
  }

  override val treeheight: Int = es.treeheight

  override def treesize: Int = es.treesize

  override def toStringWithURI: String = s"${tag.toString}_$uri(${es.toStringWithURI}, integer:$integer)"

  override def sig: Signature = Signature(SortType(classOf[TreeNode].getCanonicalName), this.tag,
    Map("es" -> ListType(SortType(classOf[TreeNode].getCanonicalName))), Map("integer" -> JavaLitType(classOf[Integer])))

  override protected def literals: Iterable[Any] = Iterable(integer)

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(es)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: IntegerTreeNode =>
      updateOwnLiteral(that, edits)
      val es = this.es.computeEditScript(that.es, this.uri, this.tag, NamedLink("es"), edits).asInstanceOf[DiffableList[TreeNode]]
      IntegerTreeNode(that.integer, es).withURI(this.uri)
    case _ => null
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned.updateLiterals(that, edits)
    }

    val es = this.es.loadUnassigned(edits).asInstanceOf[DiffableList[TreeNode]]
    val esInsert = edits.mergeKidInsert(es.uri)
    val newtree = IntegerTreeNode(integer, es)
    edits += InsertNode(newtree.uri, this.tag, Seq(
      "es" -> esInsert
    ), Seq("integer" -> integer))
    newtree
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.es.loadInitial(edits)
    val esInsert = edits.mergeKidInsert(this.es.uri)
    edits += InsertNode(this.uri, this.tag, Seq(
      "es" -> esInsert
    ), Seq("integer" -> integer))
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      edits += Remove(this.uri, this.tag, Seq("es" -> es.uri), Seq("integer" -> integer))
      this.es.unloadUnassigned(edits)
      edits.mergeKidRemove(this.es.uri, "es")
    }
  }

  private def updateOwnLiteral(thatX: IntegerTreeNode, edits: EditScriptBuffer): Unit = {
    if (this.integer != thatX.integer) {
      edits += Update(this.uri, this.tag,
        Seq("integer" -> this.integer), Seq("integer" -> thatX.integer)
      )
    }
  }

  override def updateLiterals(that: Diffable, edits: EditScriptBuffer): Diffable = {
    val thatX = that.asInstanceOf[IntegerTreeNode]
    updateOwnLiteral(thatX, edits)

    val newlist = this.es.updateLiterals(that.asInstanceOf[IntegerTreeNode].es, edits).asInstanceOf[DiffableList[TreeNode]]
    IntegerTreeNode(thatX.integer, newlist).withURI(this.uri)
  }
}

object StringTreeNode {
  def apply(string: String, es: Seq[TreeNode]): StringTreeNode = StringTreeNode(string, DiffableList.from(es, SortType(classOf[TreeNode].getCanonicalName)))
}

object IntegerTreeNode {
  def apply(integer: Integer, es: Seq[TreeNode]): IntegerTreeNode = IntegerTreeNode(integer, DiffableList.from(es, SortType(classOf[TreeNode].getCanonicalName)))
}