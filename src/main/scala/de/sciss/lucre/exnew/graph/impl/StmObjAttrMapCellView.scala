/*
 *  StmObjAttrMapCellView.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.exnew.graph.impl

import de.sciss.lucre.exnew.{CellView, Context}
import de.sciss.lucre.{Disposable, Txn, Obj => LObj}

/** A `CellView[T, Option[LObj[T]]` built from an `LObj.attr`.
 *
 * This is very similar to `StmObjCtxCellView`, which is built for a context
 * attribute map, but the two classes have to be distinguished
 * because `LObj.AttrMap` must be put into a handle, and because this
 * forms a special case when used instead an `LObj` expression.
 *
 * Depending on the kind of `context` passed, this will transparently install event
 * reactors or simply register the events used in the expression program.
 */
final class StmObjAttrMapCellView[T <: Txn[T]](attr0: LObj.AttrMap[T], key: String, tx0: T)
                                              (implicit context: Context[T])
  extends CellView[T, Option[LObj[T]]] {

  private[this] val attrH = tx0.newHandle(attr0)

  private def attr(implicit tx: T) = attrH()

  def apply()(implicit tx: T): Option[LObj[T]] =
    attr.get(key)

  def react(fun: T => Option[LObj[T]] => Unit)(implicit tx: T): Disposable[T] = {
    context.reactTo(attr.changed) { implicit tx => upd =>
      upd.changes.foreach {
        case LObj.AttrAdded(`key`, obj) =>
          fun(tx)(Some(obj))

        case LObj.AttrRemoved(`key`, _) =>
          fun(tx)(None)

        case LObj.AttrReplaced(`key`, _, now) =>
          fun(tx)(Some(now))

        case _ => // ignore
      }
    }
  }
}
