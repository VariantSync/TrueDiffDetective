// source: https://gitlab.rlp.net/plmz/truediff/-/raw/master/truediff/src/test/scala/truediff/macros/Exp.scala

package org.variantsync.truedifftest.macros

import truediff.Diffable
import truediff.macros.diffable

@diffable
trait Exp extends Diffable

object Exp {
  case class Hole() extends Exp
}

@diffable
case class Num(n: Int) extends Exp

@diffable
case class Add(e1: Exp, e2: Exp) extends Exp

@diffable
case class Sub(e1: Exp, e2: Exp) extends Exp

@diffable
case class Mul(e1: Exp, e2: Exp) extends Exp

@diffable
case class Var(name: String) extends Exp

@diffable
case class Let(x: String, e: Exp, body: Exp) extends Exp
