/*
 *  ExpandedObjAttr.scala
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

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.exnew.{CellView, Context}
import de.sciss.lucre.exnew.graph.Obj
import de.sciss.lucre.exnew.graph.Obj.Bridge
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Caching, Disposable, IChangeEvent, IExpr, IPull, IPush, ITargets, Txn}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

final class ExpandedObjAttr[T <: Txn[T], A](obj: IExpr[T, Obj], key: String, tx0: T)
                                           (implicit protected val targets: ITargets[T], bridge: Bridge[A],
                                            context: Context[T])
  extends IExpr[T, Option[A]] with IChangeGeneratorEvent[T, Option[A]] with Caching {

  private[this] val valueRef  = Ref.make[Option[A]]()
  private[this] val obs       = Ref[Disposable[T]](Disposable.empty)

  // ---- init ----
  obj.changed.--->(this)(tx0)
  setObj(obj.value(tx0), isInit = true)(tx0)

  private def updateFromObj(now: Option[A])(implicit tx: T): Unit = {
    val before = valueRef.swap(now)
    if (before != now) {
      fire(Change(before, now))
    }
  }

  private def setObj(newObj: Obj, isInit: Boolean)(implicit tx: T): Option[A] = {
    // println(s"newObj = $newObj, bridge = $bridge, key = $key")
    val newView: Option[CellView.Var[T, Option[A]]] = newObj.peer[T].map(p => bridge.cellView(p, key))
    val obsNew = newView.fold(Disposable.empty[T])(_.react { implicit tx => now => // RRR
      updateFromObj(now)
    })
    val now: Option[A] = newView.flatMap(_.apply())
    if (isInit) {
      obs     () = obsNew
      valueRef() = now
    } else {
      obs.swap(obsNew).dispose()
      updateFromObj(now)
    }
    now
  }

  def value(implicit tx: T): Option[A] =
    IPush.tryPull(this).fold(valueRef())(_.now)

  private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Option[A] =
    if (pull.isOrigin(this)) {
      // println(s"pullChange; isOrigin: this, $phase")
      pull.resolveExpr(this)
    } else {
      if (phase.isBefore) valueRef() else {
        val objV = pull.expr(obj)
        setObj(objV, isInit = false)
      }
    }

  def changed: IChangeEvent[T, Option[A]] = this

  def dispose()(implicit tx: T): Unit = {
    obj.changed -/-> this
    obs.swap(Disposable.empty).dispose()
  }
}
