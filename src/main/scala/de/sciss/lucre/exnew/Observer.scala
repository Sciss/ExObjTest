/*
 *  Observer.scala
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

package de.sciss.lucre.exnew

import de.sciss.lucre.{Exec, Observer, Txn}

object IObserver {
  def apply[T <: Txn[T], A](event: IEvent[T, A], fun: T => A => Unit)
                            (implicit tx: T, targets: ITargets[T]): IObserver[T, A] = {
    new IImpl(event, tx, fun)
  }

  private final class IImpl[T <: Txn[T], A](event: IEvent[T, A], tx0: T, fun: T => A => Unit)
                                            (implicit targets: ITargets[T])
    extends IObserver[T, A] {

    override def toString = s"IObserver<$event>"

    targets.addEventReaction[A](event, this)(tx0)

    def apply(update: A)(implicit tx: T): Unit = fun(tx)(update)

    def dispose()(implicit tx: T): Unit = {
      targets.removeEventReaction(event, this)
    }
  }
}
trait IObserver[T <: Exec[T], -A] extends Observer[T, A]