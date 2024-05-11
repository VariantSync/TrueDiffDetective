package org.variantsync.truediffdetective

import org.apache.commons.collections4.trie
import org.variantsync.diffdetective.util.LineRange
import org.variantsync.diffdetective.variation.diff.{DiffNode, Projection, Time, VariationDiff}
import org.variantsync.diffdetective.variation.tree.{VariationNode, VariationTree, VariationTreeNode}
import org.variantsync.diffdetective.variation.{Label, NodeType}
import truechange._
import truediff.DiffableList

import scala.collection.mutable.ListBuffer

object TrueDiffDetective {
  def wrapVariationNode[T <: VariationNode[T, L], L <: Label](variationNode: VariationNode[T, L]): VariationWrapper = {
    val tdd = new TrueDiffDetective[T, L]()
    tdd.wrapVariationNode(variationNode)
  }

  def compare[T <: VariationNode[T, L], L <: Label](before: VariationTree[L], after: VariationTree[L]): VariationDiff[L] = {
    val tdd = new TrueDiffDetective[T, L]()
    tdd.compare(before, after)
  }
}

class TrueDiffDetective[T <: VariationNode[T, L], L <: Label] {
  private val uriToNode = new trie.PatriciaTrie[VariationNode[T, L]]()

  private def getNodeFromURI(uri: URI): T = {
    uriToNode.get(uri.toString) match {
      case projection: Projection[_] => projection.asInstanceOf[T]
      case node: VariationTreeNode[_] => val projection = DiffNode.unchanged(node.asInstanceOf[VariationTreeNode[L]]).projection(Time.AFTER).asInstanceOf[T]
        uriToNode.put(uri.toString, projection)
        projection
      case _ => null.asInstanceOf[T]
    }
  }

  def wrapVariationNode(variationNode: VariationNode[T, L]): VariationWrapper = {
    val buf: ListBuffer[VariationWrapper] = ListBuffer[VariationWrapper]()
    variationNode.getChildren.forEach(c => buf += wrapVariationNode(c))
    val newNode: VariationTreeNodeWrapper[L] = VariationTreeNodeWrapper(variationNode.isRoot, variationNode.getNodeType,
      DiffableList.from(buf.toSeq, SortType(classOf[VariationWrapper].getCanonicalName)), variationNode.getFormula, variationNode.getLabel)

    uriToNode.put(newNode.uri.toString, variationNode)
    uriToNode.put(newNode.children.uri.toString, variationNode)
    newNode
  }

  // first attempt to apply edits from script to a copy of VariationTree before as far as possible
  def compare(before: VariationTree[L], after: VariationTree[L]): VariationDiff[L] = {
    val diffnode: DiffNode[L] = DiffNode.unchanged(before.root)
    val wrappedBefore: VariationWrapper = wrapVariationNode(diffnode.projection(Time.AFTER).asInstanceOf[VariationNode[T, L]])
    val wrappedAfter: VariationWrapper = wrapVariationNode(after.root.asInstanceOf[VariationNode[T, L]])
    val (script, _) = wrappedBefore.compareTo(wrappedAfter)

    script.foreach(applyEdit)

    new VariationDiff[L](diffnode)
  }

  private def applyEdit(edit: Edit): Unit = {
    edit match {
      case Detach(node, tag, _, _, _) =>
        val _node: T = getNodeFromURI(node)
        tag match {
          case NamedTag(_) =>
            _node.getParent.removeChild(_node)
          case ListTag(_) =>
            _node.removeAllChildren()
        }

      case Attach(node, _, link, parent, _) =>
        val _node: T = getNodeFromURI(node)
        val _parent: T = getNodeFromURI(parent)
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
              val _node: T = getNodeFromURI(node)
              if (_node.getParent != null) {
                _node.getParent.removeChild(_node)
              }
          }
        }

        _remove(remove)

      case insert: Insert =>
        def _insert(insert: Insert, listnode: VariationNode[T, L]): Unit = {
          insert match {
            case InsertList(_, _, list, _) =>
              list.foreach {
                case Left(value) => _insert(value, listnode)
                case Right(value) => listnode.addChild(getNodeFromURI(value._1))
              }
            case InsertNode(node, tag, kids, lits) =>
              tag match {
                case NamedTag(c) =>
                  val newNode: T = new VariationTreeNode[L](NodeType.fromName(c),
                    lits.find(_._1.equals("formula")).getOrElse("formula" -> null)._2.asInstanceOf[org.prop4j.Node],
                    LineRange.Invalid(), // for now: every new inserted node is assigned an invalid LineRange (because we do not take care of LineRange yet)
                    lits.find(_._1.equals("label")).get._2.asInstanceOf[L]).asInstanceOf[T]
                  uriToNode.put(node.toString, newNode)
                case ListTag(_) => ???
              }
              if (listnode != null) {
                listnode.addChild(getNodeFromURI(node))
              }
              kids.foreach(x => x._2 match {
                case Left(value) => _insert(value, getNodeFromURI(node))
                case Right(value) => getNodeFromURI(node).addChild(getNodeFromURI(value))
              })
          }
        }

        _insert(insert, null)

      case Update(node, tag, _, newlits) =>
        tag match {
          case NamedTag(c) => c match {
            case s if s.endsWith("IF") | s.endsWith("ELIF") =>
              // !!! there is no setFormula method on VariationNode available (the changes are currently only visible in the labels or using Projection)
              uriToNode.get(node.toString) match {
                case projection: Projection[_] => projection.asInstanceOf[Projection[L]].getBackingNode.setLabel(newlits.find(_._1.equals("label")).get._2.asInstanceOf[L])
                  projection.getBackingNode.setFormula(newlits.find(_._1.equals("formula")).get._2.asInstanceOf[org.prop4j.Node])
                case node: VariationTreeNode[_] => node.asInstanceOf[VariationTreeNode[L]].setLabel(newlits.find(_._1.equals("label")).get._2.asInstanceOf[L])
                case _ => ???
              }
            case s if s.endsWith("ARTIFACT") | s.endsWith("ELSE") =>
              // !!! there is no setFormula method on VariationNode available (the changes are currently only visible in the labels or using Projection)
              uriToNode.get(node.toString) match {
                case projection: Projection[_] => projection.asInstanceOf[Projection[L]].getBackingNode.setLabel(newlits.find(_._1.equals("label")).get._2.asInstanceOf[L])
                case node: VariationTreeNode[_] => node.asInstanceOf[VariationTreeNode[L]].setLabel(newlits.find(_._1.equals("label")).get._2.asInstanceOf[L])
                case _ => ???
              }
            case _ => ;
          }
          case ListTag(_) => ???
        }
    }
  }
}