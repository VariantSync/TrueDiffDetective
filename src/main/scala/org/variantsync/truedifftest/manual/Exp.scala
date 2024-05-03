// source: https://gitlab.rlp.net/plmz/truediff/-/raw/master/truediff/src/test/scala/truediff/manual/Exp.scala

package org.variantsync.truedifftest.manual

import truechange._
import truediff._

trait Exp extends Diffable

object Exp {
  case class Hole() extends Exp {
    override def treeheight: Int = 1

    override def treesize: Int = 1

    override def toStringWithURI: String = s"Hole_$uri()"

    override def sig: Signature = Signature(SortType(classOf[Exp].getCanonicalName), this.tag, Map(), Map())

    override protected def literals: Iterable[Any] = Iterable.empty

    override protected def directSubtrees: Iterable[Diffable] = Iterable.empty

    override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
      case Hole() =>
        Hole().withURI(this.uri)
      case _ => null
    }

    override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
      val that = this
      if (that.assigned != null) {
        return that.assigned.updateLiterals(that, edits)
      }

      val newtree = Hole()
      edits += InsertNode(newtree.uri, this.tag, Seq(), Seq())
      newtree
    }

    override def updateLiterals(that: Diffable, edits: EditScriptBuffer): Diffable =
      this

    override def loadInitial(edits: EditScriptBuffer): Unit = {
      edits += InsertNode(this.uri, this.tag, Seq(), Seq())
    }

    override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
      if (this.assigned != null) {
        //        this.assigned = null
      } else {
        edits += Remove(this.uri, this.tag, Seq(), Seq())
      }
    }
  }
}


case class Num(n: Int) extends Exp {

  override lazy val literalHash: Array[Byte] = Hashable.hash("Num", this.n)

  override val treeheight: Int = 1

  override def treesize: Int = 1

  override def sig: Signature = Signature(SortType(classOf[Exp].getCanonicalName), this.tag, Map(), Map("n" -> JavaLitType(classOf[Integer])))

  override protected def literals: Iterable[Any] = Iterable(n)

  override def toStringWithURI: String = s"Num_$uri($n)"

  override protected def directSubtrees: Iterable[Diffable] = Iterable.empty

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case Num(n) if this.n == n =>
      Num(n).withURI(this.uri)
    case _ => null
  }


  override def updateLiterals(thatX: Diffable, edits: EditScriptBuffer): Num = {
    val that = thatX.asInstanceOf[Num]
    if (this.n != that.n) {
      edits += Update(this.uri, this.tag,
        Seq("n" -> this.n), Seq("n" -> that.n)
      )
    }
    Num(that.n).withURI(this.uri)
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned.updateLiterals(that, edits)
    }

    val newtree = Num(this.n)
    edits += InsertNode(newtree.uri, this.tag, Seq(), Seq(
      "n" -> this.n
    ))
    newtree
  }


  override def loadInitial(edits: EditScriptBuffer): Unit = {
    edits += InsertNode(this.uri, this.tag, Seq(), Seq(
      "n" -> this.n
    ))
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      //      this.assigned = null
    } else {
      edits += Remove(this.uri, this.tag, Seq(), Seq(
        "n" -> this.n
      ))
    }
  }
}

case class Add(e1: Exp, e2: Exp) extends Exp {

  override val treeheight: Int = 1 + Math.max(e1.treeheight, e2.treeheight)

  override def treesize: Int = 1 + e1.treesize + e2.treesize

  override def toStringWithURI: String = s"Add_$uri(${e1.toStringWithURI}, ${e2.toStringWithURI})"

  override def sig: Signature = Signature(SortType(classOf[Exp].getCanonicalName), this.tag, Map("e1" -> SortType(classOf[Exp].getCanonicalName), "e2" -> SortType(classOf[Exp].getCanonicalName)), Map())

  override protected def literals: Iterable[Any] = Iterable.empty

  override protected def directSubtrees: Iterable[Diffable] = Iterable(e1, e2)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: Add =>
      val e1 = this.e1.computeEditScript(that.e1, this.uri, this.tag, NamedLink("e1"), edits).asInstanceOf[Exp]
      val e2 = this.e2.computeEditScript(that.e2, this.uri, this.tag, NamedLink("e2"), edits).asInstanceOf[Exp]
      Add(e1, e2).withURI(this.uri)
    case _ => null
  }


  override def updateLiterals(that: Diffable, edits: EditScriptBuffer): Diffable = that match {
    case that: Add =>
      val e1 = this.e1.updateLiterals(that.e1, edits).asInstanceOf[Exp]
      val e2 = this.e2.updateLiterals(that.e2, edits).asInstanceOf[Exp]
      Add(e1, e2).withURI(this.uri)
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned.updateLiterals(that, edits)
    }

    val e1 = that.e1.loadUnassigned(edits).asInstanceOf[Exp]
    val e1Insert = edits.mergeKidInsert(e1.uri)
    val e2 = that.e2.loadUnassigned(edits).asInstanceOf[Exp]
    val e2Insert = edits.mergeKidInsert(e2.uri)
    val newtree = Add(e1, e2)
    edits += InsertNode(newtree.uri, this.tag, Seq(
      "e1" -> e1Insert,
      "e2" -> e2Insert
    ), Seq())
    newtree
  }


  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.e1.loadInitial(edits)
    val e1 = edits.mergeKidInsert(this.e1.uri)
    this.e2.loadInitial(edits)
    val e2 = edits.mergeKidInsert(this.e2.uri)
    edits += InsertNode(this.uri, this.tag, Seq(
      "e1" -> e1,
      "e2" -> e2
    ), Seq())
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      //      this.assigned = null
    } else {
      edits += Remove(this.uri, this.tag, Seq(
        "e1" -> this.e1.uri,
        "e2" -> this.e2.uri
      ), Seq())
      this.e1.unloadUnassigned(edits)
      edits.mergeKidRemove(this.e1.uri, "e1")
      this.e2.unloadUnassigned(edits)
      edits.mergeKidRemove(this.e2.uri, "e2")
    }
  }
}

case class Var(name: String) extends Exp {
  override lazy val literalHash: Array[Byte] = Hashable.hash("Var", this.name)

  override val treeheight: Int = 1

  override def treesize: Int = 1

  override def sig: Signature = Signature(SortType(classOf[Exp].getCanonicalName), this.tag, Map(), Map("name" -> JavaLitType(classOf[String])))

  override protected def literals: Iterable[Any] = Iterable(name)

  override def toStringWithURI: String = s"Var_$uri($name)"

  override protected def directSubtrees: Iterable[Diffable] = Iterable.empty

  override def updateLiterals(thatX: Diffable, edits: EditScriptBuffer): Var = {
    val that = thatX.asInstanceOf[Var]
    if (this.name != that.name) {
      edits += Update(this.uri, this.tag,
        Seq("n" -> this.name), Seq("n" -> that.name)
      )
    }
    Var(that.name).withURI(this.uri)
  }

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case Var(n) if this.name == name =>
      Var(name).withURI(this.uri)
    case _ => null
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned.updateLiterals(that, edits)
    }

    val newtree = Var(this.name)
    edits += InsertNode(newtree.uri, this.tag, Seq(), Seq(
      "name" -> this.name
    ))
    newtree
  }


  override def loadInitial(edits: EditScriptBuffer): Unit = {
    edits += InsertNode(this.uri, this.tag, Seq(), Seq(
      "name" -> this.name
    ))
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      //      this.assigned = null
    } else {
      edits += Remove(this.uri, this.tag, Seq(), Seq(
        "name" -> this.name
      ))
    }
  }
}

case class Call(f: String, a: Exp) extends Exp {
  override def sig: Signature = ???

  override def treeheight: Int = a.treeheight + 1

  override def treesize: Int = a.treeheight + 1

  override def toStringWithURI: String = s"${tag}_$uri($f, ${a.toStringWithURI})"

  override protected def literals: Iterable[Any] = Iterable(f)

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(a)

  override lazy val literalHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    Hashable.hash(this.tag.toString, digest)
    Hashable.hash(this.f, digest)
    this.directSubtrees.foreach(t => digest.update(t.literalHash))
    digest.digest()
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable =
    if (this.assigned != null)
      this.assigned.updateLiterals(this, edits)
    else {
      val a = this.a.loadUnassigned(edits).asInstanceOf[Exp]
      val aInsert = edits.mergeKidInsert(a.uri)
      val newtree = Call(f, a)
      edits += InsertNode(newtree.uri, tag, Seq("a" -> aInsert), Seq("f" -> f))
      newtree
    }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit =
    if (this.assigned == null) {
      edits += Remove(this.uri, this.tag, Seq(
        "a" -> this.a.uri
      ), Seq())
      this.a.unloadUnassigned(edits)
      edits.mergeKidRemove(this.a.uri, "a")
    }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.a.loadInitial(edits)
    val aInsert = edits.mergeKidInsert(this.a.uri)
    edits += InsertNode(uri, tag, Seq("a" -> aInsert), Seq("f" -> f))
  }

  override def updateLiterals(that: Diffable, edits: EditScriptBuffer): Diffable = that match {
    case Call(f2, a2) =>
      if (f != f2)
        edits += Update(uri, tag, Seq("f" -> f), Seq("f" -> f2))
      val a = this.a.updateLiterals(a2, edits).asInstanceOf[Exp]
      Call(f, a).withURI(this.uri)
  }

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case Call(f2, a2) if f == f2 =>
      val a = this.a.computeEditScript(a2, this.uri, this.tag, NamedLink("a"), edits).asInstanceOf[Exp]
      Call(f, a).withURI(this.uri)
    case _ => null
  }
}