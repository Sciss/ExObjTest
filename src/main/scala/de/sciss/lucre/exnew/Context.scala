/*
 *  Context.scala
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

package de.sciss.lucre
package exnew

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.exnew.graph.It
// EEE
//import de.sciss.lucre.exnew.graph.Control
//import de.sciss.lucre.exnew.impl.ContextImpl

object Context {
  type Attr[T <: Txn[T]] = MapObjLike[T, String, Form[T]]

  def emptyAttr[T <: Txn[T]]: Attr[T] = anyEmptyAttr.asInstanceOf[EmptyAttr[T]]

  private val anyEmptyAttr = new EmptyAttr[AnyTxn]

  private final class EmptyAttr[T <: Txn[T]] extends Attr[T] {
    override def toString = "empty"

    type V = Form[T]

    def isEmpty (implicit tx: T): Boolean = true
    def nonEmpty(implicit tx: T): Boolean = false

    def contains(key: String)(implicit tx: T): Boolean = false

    def get(key: String)(implicit tx: T): Option[V] = None

    //    def $[R[~ <: Txn[~]] <: Form[~]](key: String)(implicit tx: T, ct: ClassTag[R[T]]): Option[R[T]] = None

    def changed: Observable[T, MapObjLike.Update[String, Form[T]]] =
      Observable.empty

    def dispose()(implicit tx: T): Unit = ()
  }

  def apply[T <: Txn[T]](selfH: Option[Source[T, Obj[T]]] = None, attr: Attr[T] = emptyAttr[T])
                        (implicit
// EEE
//                         workspace: Workspace[T],
                         cursor: Cursor[T],
                         undoManager: UndoManager[T]): Context[T] =
    ???
// EEE
//    new ContextImpl[T](selfH, attr)

  /** Helper class for "upcasting". Used in SoundProcesses `Runner`, for example. */
  class WithTxn[T <: Txn[T]]()(implicit val ctx: Context[T], val tx: T) {
    def cast[U <: Txn[U]]: WithTxn[U] = this.asInstanceOf[WithTxn[U]]
  }
}
trait Context[T <: Txn[T]] extends Disposable[T] {
  implicit def targets    : ITargets    [T]
  implicit def cursor     : Cursor      [T]
// EEE
//  implicit def workspace  : Workspace   [T]
  implicit def undoManager: UndoManager [T]

  def attr/*(implicit tx: T)*/: Context.Attr[T]

  /** Whether the event dispatch system should be connected or not. */
  def connect: Boolean

  def reactTo[A](event: EventLike[T, A])(fun: T => A => Unit)(implicit tx: T): Disposable[T]

// EEE
//  /** Prepares graph expansion by copying control properties over
//   * for subsequent look-up through `getProperty`.
//   */
//  def initGraph(g: Graph)(implicit tx: T): Unit

  /** Creates a temporary nested context into which all `visit` calls are
   * redirected, thus a compound `Disposable` can be returned.
   */
  def nested[A](it: It.Expanded[T, _])(body: => A)(implicit tx: T): (A, Disposable[T])

// EEE
//  def getProperty[A](control: Control, key: String)(implicit tx: T): Option[A]

  def selfOption(implicit tx: T): Option[Obj[T]]

  def visit[U <: Disposable[T]](ref: AnyRef, init: => U)(implicit tx: T): U
}

