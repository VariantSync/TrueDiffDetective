package org.variantsync.truediffdetective

import org.variantsync.diffdetective.diff.text.DiffLineNumber
import org.variantsync.diffdetective.variation.diff._
import org.variantsync.diffdetective.variation.tree.{VariationNode, VariationTree, VariationTreeNode}
import org.variantsync.diffdetective.variation.{Label, NodeType}
import truechange._
import truediff.DiffableList

import scala.collection.mutable.ListBuffer

object TrueDiffDetective {
  private def getNodeFromURI[T <: VariationNode[T, L], L <: Label](uri: URI): T = {
    val variationNodeURI: VariationNodeURI[L, T] = uri match {
      case i: VariationNodeURI[_, _] => i.asInstanceOf[VariationNodeURI[L, T]]
      case _ => ???
    }
    variationNodeURI.variationNode match {
      case projection: Projection[_] => projection.asInstanceOf[T]
      case node: VariationTreeNode[_] => val projection = DiffNode.unchanged(node.asInstanceOf[VariationTreeNode[L]]).projection(Time.AFTER).asInstanceOf[T]
        variationNodeURI.variationNode = projection
        projection
      case _ => null.asInstanceOf[T]
    }
  }

  def wrapVariationNode[T <: VariationNode[T, L], L <: Label](variationNode: VariationNode[T, L]): VariationWrapper = {
    val buf: ListBuffer[VariationWrapper] = ListBuffer[VariationWrapper]()
    variationNode.getChildren.forEach(c => buf += wrapVariationNode(c))
    val newNode: VariationNodeWrapper[L] = VariationNodeWrapper(variationNode.isRoot, variationNode.getNodeType,
      DiffableList.from(buf.toSeq, SortType(classOf[VariationWrapper].getCanonicalName)), variationNode.getFormula, variationNode.getLabel, variationNode)
    newNode
  }

  // first attempt to apply edits from script to a copy of VariationTree before as far as possible
  def compare[T <: VariationNode[T, L], L <: Label](before: VariationTree[L], after: VariationTree[L]): VariationDiff[L] = {
    val diffnode: DiffNode[L] = DiffNode.unchanged(before.root)
    val wrappedBefore: VariationWrapper = wrapVariationNode(diffnode.projection(Time.AFTER).asInstanceOf[VariationNode[T, L]])
    val wrappedAfter: VariationWrapper = wrapVariationNode(after.root.asInstanceOf[VariationNode[T, L]])
    val (script, _) = wrappedBefore.compareTo(wrappedAfter)

    script.foreach(applyEdit)

    new VariationDiff[L](diffnode)
  }

  private def applyEdit[T <: VariationNode[T, L], L <: Label](edit: Edit): Unit = {
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
              _node match {
                case projection: Projection[_] => projection.asInstanceOf[Projection[L]].getBackingNode.diffType = DiffType.REM
                case _ => ???
              }
          }
        }

        _remove(remove)

      case insert: Insert =>
        def _insert(insert: Insert, listnode: T): Unit = {
          insert match {
            case InsertList(_, _, list, _) =>
              list.foreach {
                case Left(value) => _insert(value, listnode)
                case Right(value) => listnode.addChild(getNodeFromURI(value._1))
              }
            case InsertNode(node, tag, kids, lits) =>
              tag match {
                case NamedTag(c) =>
                  val newNode: DiffNode[L] = new DiffNode[L](DiffType.ADD, NodeType.fromName(c), DiffLineNumber.Invalid(), DiffLineNumber.Invalid(),
                    lits.find(_._1.equals("formula")).getOrElse("formula" -> null)._2.asInstanceOf[org.prop4j.Node],
                    lits.find(_._1.equals("label")).get._2.asInstanceOf[L])
                  node match {
                    case value: VariationNodeURI[_, _] =>
                      value.asInstanceOf[VariationNodeURI[L, T]].variationNode = newNode.projection(Time.AFTER).asInstanceOf[VariationNode[T, L]]
                    case _ => ???
                  }
                case ListTag(_) => ???
              }
              if (listnode != null) {
                listnode.addChild(getNodeFromURI(node))
              }
              kids.foreach(x => x._2 match {
                case Left(value) => _insert(value, getNodeFromURI(node))
                case Right(value) => getNodeFromURI[T, L](node).addChild(getNodeFromURI(value))
              })
          }
        }

        _insert(insert, null.asInstanceOf[T])

      case Update(node, tag, _, newlits) =>
        tag match {
          case NamedTag(_) =>
            val _node: DiffNode[L] = getNodeFromURI(node).asInstanceOf[VariationNode[T, L]] match {
              case projection: Projection[_] => projection.asInstanceOf[Projection[L]].getBackingNode
              case _ => ???
            }
            val parent = _node.getParent(Time.AFTER)
            val copy = _node.shallowCopy()
            copy.diffType = DiffType.ADD
            _node.diffType = DiffType.REM
            newlits.find(_._1.equals("formula")) match {
              case Some((_, formula)) => copy.setFormula(formula.asInstanceOf[org.prop4j.Node])
              case None => ;
            }
            newlits.find(_._1.equals("label")) match {
              case Some((_, formula)) => copy.setLabel(formula.asInstanceOf[L])
              case None => ;
            }
            if (parent != null) {
              val index = parent.indexOfChild(_node, Time.AFTER)
              parent.removeChild(_node, Time.AFTER)
              parent.insertChild(copy, index, Time.AFTER)
            }
            node match {
              case value: VariationNodeURI[_, _] =>
                value.asInstanceOf[VariationNodeURI[L, T]].variationNode = copy.projection(Time.AFTER).asInstanceOf[VariationNode[T, L]]
              case _ => ???
            }
            copy.addChildren(_node.getChildOrder(Time.AFTER), Time.AFTER);
          case ListTag(_) => ???
        }
    }
  }
}