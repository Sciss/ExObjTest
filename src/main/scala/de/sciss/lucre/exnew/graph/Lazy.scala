/*
 *  Lazy.scala
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
package graph

trait Lazy extends Product {
  type Repr[T <: Txn[T]] <: Disposable[T]

  // this acts as a fast unique reference
  @transient final protected val ref = new AnyRef

  final def expand[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
    ctx.visit(ref, mkRepr)

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T]
}

/** An element that participates in data-flow, such as an expression `Ex`, an action `Act`, or a trigger `Trig`. */
trait Flow extends Lazy {
  type Repr[T <: Txn[T]] <: Form[T] with Disposable[T]
}