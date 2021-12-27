/*
 *  IEvent.scala
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

import de.sciss.lucre.exnew.ExElem.RefMapIn
import de.sciss.lucre.{AnyTxn, Disposable, Form, Txn}
import de.sciss.serial.{DataInput, DataOutput, RefMapOut, TFormat, Writable}

object IExpr {
  trait Var[T <: Txn[T], A] extends IExpr[T, A] /*with lucre.Ref[T, IExpr[T, A]]*/ {
    def apply()(implicit context: Context[T], tx: T): IExpr[T, A]
    def update(value: IExpr[T, A])(implicit context: Context[T], tx: T): Unit
  }

  implicit def format[T <: Txn[T], A](implicit context: Context[T]): TFormat[T, IExpr[T, A]] =
    new IExprFormat[T, A] // anyFmt.asInstanceOf[TFormat[T, IExpr[T, A]]]

  def read[T <: Txn[T], A](in: DataInput)(implicit context: Context[T], tx: T): IExpr[T, A] = format[T, A].readT(in)

  def write[T <: Txn[T], A](ex: IExpr[T, A], out: DataOutput): Unit = {
//    format[T, A].write(ex, out)
    if (ex == null) out.writeInt(0) else ex.write(out)
  }

//  private val anyFmt = new IExprFormat[AnyTxn, Any]
//
//  private class Fmt[T <: Txn[T]] extends TFormat[T, IExpr[T, Any]] {
//    def write(v: IExpr[T, Any], out: DataOutput): Unit = {
//      val ref = new RefMapOut(out)
//      ref.writeElem(v)
//    }
//
//    def readT(in: DataInput)(implicit tx: T): IExpr[T, Any] = {
//      val ref = new RefMapIn(in)
//      ref.readProductT()
//    }
//  }
}
trait IExpr[T <: Txn[T], +A] extends Form[T] /*ExprLike[T, A]*/ with IChangePublisher[T, A] with Disposable[T] with Writable {
  def value(implicit context: Context[T], tx: T): A

  protected def typeId: Int

  protected def writeData(out: DataOutput): Unit

  // ---- impl ----

  final def write(out: DataOutput): Unit = {
    out.writeInt(typeId)
    writeData(out)
  }
}