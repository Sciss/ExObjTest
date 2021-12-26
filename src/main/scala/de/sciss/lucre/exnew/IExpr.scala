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

import de.sciss.lucre
import de.sciss.lucre.{Disposable, Exec, ExprLike, Form, Txn}
import de.sciss.serial.{DataOutput, Writable}

object IExpr {
  trait Var[T <: Txn[T], A] extends IExpr[T, A] /*with lucre.Ref[T, IExpr[T, A]]*/ {
    def apply()(implicit context: Context[T], tx: T): IExpr[T, A]
    def update(value: IExpr[T, A])(implicit context: Context[T], tx: T): Unit
  }
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