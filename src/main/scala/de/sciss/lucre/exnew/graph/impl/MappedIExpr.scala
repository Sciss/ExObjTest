/*
 *  MappedIExpr.scala
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

import de.sciss.lucre.Exec
import de.sciss.lucre.exnew.impl.IChangeEventImpl
import de.sciss.lucre.exnew.{IChangeEvent, IExpr, IPull, ITargets}

abstract class MappedIExpr[T <: Exec[T], A1, A](in: IExpr[T, A1])
                                               (implicit protected val targets: ITargets[T])
  extends IExpr[T, A] with IChangeEventImpl[T, A] {

  def connect()(implicit tx: T): this.type = {
    in.changed.--->(this)
    this
  }

  protected def mapValue(inValue: A1)(implicit tx: T): A

  def value(implicit tx: T): A = mapValue(in.value)

  private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
    val v = pull.applyChange(in.changed)
    mapValue(v)
  }

  //  private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Change[A]] =
  //    pull(in.changed).flatMap { ch =>
  //      val before  = mapValue(ch.before )
  //      val now     = mapValue(ch.now    )
  //      if (before == now) None else Some(Change(before, now))
  //    }

  def dispose()(implicit tx: T): Unit =
    in.changed.-/->(this)

  def changed: IChangeEvent[T, A] = this
}
