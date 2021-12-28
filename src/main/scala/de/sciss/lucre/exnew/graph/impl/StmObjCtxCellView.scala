/*
 *  StmObjCtxCellView.scala
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

import de.sciss.lucre.exnew.{CellView, Context, graph}
import de.sciss.lucre.{Disposable, ExprLike, Form, MapObjLike, Txn, Obj => LObj}

abstract class AbstractCtxCellView[T <: Txn[T], A](attr: Context.Attr[T], key: String)
  extends CellView/*.Var*/[T, Option[A]] {

  // ---- abstract ----

  protected def tryParseValue(value: Any     )(implicit tx: T): Option[A]
  protected def tryParseObj  (obj  : LObj[T] )(implicit tx: T): Option[A]

  // ---- impl ----

  final def apply()(implicit tx: T): Option[A] =
    attr.get(key).flatMap(formValue)

  private def formValue(f: Form[T])(implicit tx: T): Option[A] = f match {
    case ex: ExprLike[T, _] => tryParseValue(ex.value)
    case obj: LObj[T]    => tryParseObj  (obj)
    case _                  => None
  }

  final def react(fun: T => Option[A] => Unit)(implicit tx: T): Disposable[T] = {
    val r1 = attr.changed.react { implicit tx => upd => // RRR
      upd.changes.foreach {
        case MapObjLike.Added  (`key`, f) =>
          val opt = formValue(f)
          if (opt.isDefined) fun(tx)(opt)

        case MapObjLike.Removed(`key`, f) =>
          val opt = formValue(f)
          if (opt.isDefined) fun(tx)(None)

        case MapObjLike.Replaced(`key`, before, now) =>
          val optB = formValue(before)
          val optN = formValue(now  )
          if (optB != optN) fun(tx)(optN)

        case _ => // ignore
      }
    }
    attr.get(key) match {
      case Some(ex: ExprLike[T, _]) =>
        val r2 = ex.changed.react { implicit tx => upd =>
          val vOpt = tryParseValue(upd.now)
          fun(tx)(vOpt)
        }
        Disposable.seq(r1, r2)

      case _ => r1
    }
  }
}

/** A `CellView[T, Option[LObj[T]]` built from a `Context`.
 *
 * This is very similar to `StmObjAttrMapCellView`, which is built for the
   * attribute map of a `lucre.Obj` instead of a context, but the two classes have to be distinguished
 * because `lucre.Obj.AttrMap` must be put into a handle, and because there
 * a special case exists when it used instead an `LObj` expression.
 */
final class StmObjCtxCellView[T <: Txn[T]](attr: Context.Attr[T], key: String)
  extends AbstractCtxCellView[T, LObj[T]](attr, key) {

  protected def tryParseValue(value: Any)(implicit tx: T): Option[LObj[T]] = value match {
    case obj: graph.Obj => obj.peer
    case _              => None
  }

  protected def tryParseObj(obj: LObj[T])(implicit tx: T): Option[LObj[T]] = Some(obj)
}
