package org.variantsync.truediffdetective

import org.apache.commons.collections4.trie
import org.variantsync.diffdetective.util.LineRange
import org.variantsync.diffdetective.variation.tree.{VariationTree, VariationTreeNode}
import org.variantsync.diffdetective.variation.{Label, NodeType}
import truechange._
import truediff.DiffableList

import scala.collection.mutable.ListBuffer

object TrueDiffDetective {
  def wrapVariationTreeNode[L <: Label](variationTreeNode: VariationTreeNode[L]): VariationWrapper = {
    val tdd = new TrueDiffDetective[L]()
    tdd.wrapVariationTreeNode(variationTreeNode)
  }

  def compare[L <: Label](before: VariationTree[L], after: VariationTree[L]): VariationTree[L] = {
    val tdd = new TrueDiffDetective[L]()
    tdd.compare(before, after)
  }
}

class TrueDiffDetective[L <: Label] {
  private val uriToNode = new trie.PatriciaTrie[VariationTreeNode[L]]()

  def wrapVariationTreeNode(variationTreeNode: VariationTreeNode[L]): VariationWrapper = {
    val buf: ListBuffer[VariationWrapper] = ListBuffer[VariationWrapper]()
    variationTreeNode.getChildren.forEach(c => buf += wrapVariationTreeNode(c))
    val newNode: VariationTreeNodeWrapper[L] = VariationTreeNodeWrapper(variationTreeNode.isRoot, variationTreeNode.getNodeType,
      DiffableList.from(buf.toSeq, SortType(classOf[VariationWrapper].getCanonicalName)), variationTreeNode.getFormula, variationTreeNode.getLabel)

    uriToNode.put(newNode.uri.toString, variationTreeNode)
    uriToNode.put(newNode.children.uri.toString, variationTreeNode)
    newNode
  }

  // first attempt to apply edits from script to a copy of VariationTree before as far as possible
  def compare(before: VariationTree[L], after: VariationTree[L]): VariationTree[L] = {
    val _before: VariationTree[L] = before.deepCopy()

    val wrappedBefore: VariationWrapper = wrapVariationTreeNode(_before.root())
    val wrappedAfter: VariationWrapper = wrapVariationTreeNode(after.root())
    val (script, _) = wrappedBefore.compareTo(wrappedAfter)

    script.foreach(applyEdit)

    _before
  }

  private def applyEdit(edit: Edit): Unit = {
    edit match {
      case Detach(node, tag, _, _, _) =>
        val _node: VariationTreeNode[L] = uriToNode.get(node.toString)
        tag match {
          case NamedTag(_) =>
            _node.getParent.removeChild(_node)
          case ListTag(_) =>
            _node.removeAllChildren()
        }

      case Attach(node, _, link, parent, _) =>
        val _node: VariationTreeNode[L] = uriToNode.get(node.toString)
        val _parent: VariationTreeNode[L] = uriToNode.get(parent.toString)
        link.getRawLink match {
          case ListFirstLink(_) => _parent.insertChild(_node, 0)
          case ListNextLink(_) =>
            // parent parameter is actually previous sibling
            val real_parent = _parent.getParent
            real_parent.insertChild(_node, real_parent.indexOfChild(_parent) + 1)
          case NamedLink(_) => ???
        }

      case remove: Remove =>
        def _remove(remove: Remove): Unit = {
          remove match {
            case RemoveList(_, _, list, _) =>
              list.foreach {
                case Left(value) => _remove(value)
                case Right(value) => _remove(RemoveNode(value._1, null, null, null))
              }
            case RemoveNode(node, _, kids, _) =>
              if (kids != null) {
                kids.foreach {
                  k =>
                    k._2 match {
                      case Left(value) => _remove(value)
                      case Right(_) => ???
                    }
                }
              }
              val _node: VariationTreeNode[L] = uriToNode.get(node.toString)
              if (_node.getParent != null) {
                _node.getParent.removeChild(_node)
              }
          }
        }

        _remove(remove)

      case insert: Insert =>
        def _insert(insert: Insert, listnode: VariationTreeNode[L]): Unit = {
          insert match {
            case InsertList(_, _, list, _) =>
              list.foreach {
                case Left(value) => _insert(value, listnode)
                case Right(value) => listnode.addChild(uriToNode.get(value._1))
              }
            case InsertNode(node, tag, kids, lits) =>
              tag match {
                case NamedTag(c) =>
                  val newNode: VariationTreeNode[L] = new VariationTreeNode[L](NodeType.fromName(c),
                    lits.find(_._1.equals("formula")).getOrElse("formula" -> null)._2.asInstanceOf[org.prop4j.Node],
                    LineRange.Invalid(), // for now: every new inserted node is assigned an invalid LineRange (because we do not take care of LineRange yet)
                    lits.find(_._1.equals("label")).get._2.asInstanceOf[L])
                  uriToNode.put(node.toString, newNode)
                case ListTag(_) => ???
              }
              if (listnode != null) {
                listnode.addChild(uriToNode.get(node.toString))
              }
              kids.foreach(x => x._2 match {
                case Left(value) => _insert(value, uriToNode.get(node.toString))
                case Right(value) => uriToNode.get(node.toString).addChild(uriToNode.get(value.toString))
              })
          }
        }

        _insert(insert, null)

      case Update(node, tag, _, newlits) =>
        tag match {
          case NamedTag(c) => c match {
            case s if s.endsWith("IF") | s.endsWith("ELIF") =>
              // !!! there is no setFormula method on VariationTreeNode available (the changes are currently only visible in the labels)
              // uriToNode.get(node.toString).setFormula(newlits.find(_._1.equals("formula")).get._2.asInstanceOf[L])
              uriToNode.get(node.toString).setLabel(newlits.find(_._1.equals("label")).get._2.asInstanceOf[L])
            case s if s.endsWith("ARTIFACT") | s.endsWith("ELSE") =>
              uriToNode.get(node.toString).setLabel(newlits.find(_._1.equals("label")).get._2.asInstanceOf[L])
            case _ => ;
          }
          case ListTag(_) => ???
        }
    }
  }
}