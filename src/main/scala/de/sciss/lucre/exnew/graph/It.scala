/*
 *  It.scala
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

package de.sciss.lucre.exnew.graph

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.exnew.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.exnew.{Context, graph}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{IChangeEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object It extends ProductReader[It[_]] {
  trait Expanded[T <: Txn[T], A] extends IExpr[T, A] {
    def setValue(value: A /*, dispatch: Boolean*/)(implicit tx: T): Unit

    def ref: AnyRef
  }

  private final class ExpandedImpl[T <: Txn[T], A](val ref: AnyRef)(implicit protected val targets: ITargets[T])
    extends Expanded[T, A] with IChangeGeneratorEvent[T, A] {

    private[this] val valueRef = Ref.make[A]()

    def setValue(value: A /*, dispatch: Boolean*/)(implicit tx: T): Unit = {
      valueRef() = value
      //      val old =  ref.swap(value)
      //      if (/*dispatch && */ old != value) {
      //        fire(Change(old, value))
      //      }
    }

    override private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Change[A]] =
      throw new IllegalArgumentException("pullUpdate on It.Expanded")

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
      value // pull.resolveChange(isNow = isNow) // throw new AssertionError("Should never be here")
    }

    def value(implicit tx: T): A = valueRef()

    def dispose()(implicit tx: T): Unit = ()

    def changed: IChangeEvent[T, A] = this
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): It[_] = {
    require (arity == 1 && adj == 0)
    val _token = in.readInt()
    new It(_token)
  }
}
/** A glue element to make `map` and `flatMap` work. */
final case class It[A](token: Int) extends Ex[A] {

  type Repr[T <: Txn[T]] = It.Expanded[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    new graph.It.ExpandedImpl[T, A](ref)
  }
}
