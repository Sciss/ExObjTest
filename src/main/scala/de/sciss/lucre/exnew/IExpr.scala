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
import de.sciss.lucre.{Disposable, Exec, ExprLike}
import de.sciss.serial.{DataOutput, Writable}

object IExpr {
  trait Var[T <: Exec[T], A] extends IExpr[T, A] with lucre.Ref[T, IExpr[T, A]]
}
trait IExpr[T <: Exec[T], +A] extends ExprLike[T, A] with IChangePublisher[T, A] with Disposable[T] with Writable {
  def value(implicit tx: T): A

  protected def typeId: Int

  protected def writeData(out: DataOutput): Unit

  // ---- impl ----

  final def write(out: DataOutput): Unit = {
    out.writeInt(typeId)
    writeData(out)
  }
}