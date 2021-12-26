/*
 *  IDummyEvent.scala
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
package impl

import de.sciss.lucre.{AnyExec, AnyTxn, Disposable, Exec, Txn}
import de.sciss.model.Change

object IDummyEvent {
  /** This method is cheap. */
  def apply[T <: Txn[T], A](): IEvent[T, A] = anyDummy.asInstanceOf[IEvent[T, A]]

  def change[T <: Txn[T], A]: IChangeEvent[T, A] = anyChangeDummy.asInstanceOf[IChangeEvent[T, A]]

  private val anyDummy        = new Impl       [AnyTxn]
  private val anyChangeDummy  = new ChangeImpl [AnyTxn]

  private final class Impl[T <: Txn[T]] extends IEvent[T, Any] {
    override def toString = "event.IDummy"

    def --->(sink: IEvent[T, Any])(implicit tx: T): Unit = ()
    def -/->(sink: IEvent[T, Any])(implicit tx: T): Unit = ()

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Any] = None

    def react(fun: T => Any => Unit)(implicit tx: T): Disposable[T] = Disposable.empty[T]
  }

  private final class ChangeImpl[T <: Txn[T]] extends IChangeEvent[T, Any] {
    override def toString = "event.IDummy"

    def --->(sink: IEvent[T, Any])(implicit tx: T): Unit = ()
    def -/->(sink: IEvent[T, Any])(implicit tx: T): Unit = ()

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Any = ()

    def react(fun: T => Change[Any] => Unit)(implicit tx: T): Disposable[T] = Disposable.empty[T]
  }
}