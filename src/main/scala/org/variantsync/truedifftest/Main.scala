package org.variantsync.truedifftest

import truediff.Diffable

object Main {
  private def compareAndPrintEditScript(src: Diffable, dest: Diffable): Unit = {
    println("  Comparing:")
    println(s"    ${src.toStringWithURI}")
    println(s"    ${dest.toStringWithURI}")

    val (editscript, newtree) = src.compareTo(dest)
    println("  EditScript:")
    editscript.foreach(c => println("    " + c))
    println("  Core editScript:")
    editscript.coreEdits.foreach(c => println("    " + c))
    println("  New tree:")
    println("    " + newtree.toStringWithURI)

    println()
  }

  def main(args: Array[String]): Unit = {
    {
      println("Manual: ")
      import manual._
      compareAndPrintEditScript(
        Add(Num(1), Add(Num(2), Num(3))),
        Add(Num(1), Add(Num(2), Add(Num(3), Num(4))))
      )
    }
    {
      println("Macros: ")
      import macros._
      compareAndPrintEditScript(
        Add(Num(1), Add(Num(2), Num(3))),
        Add(Num(1), Add(Num(2), Add(Num(3), Num(4))))
      )
    }
  }
}
