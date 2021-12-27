package de.sciss.lucre.exnew

import de.sciss.lucre.Txn
import de.sciss.serial.DataInput

trait IExprFactory {
  def typeId: Int

  def readIdentified[T <: Txn[T]](in: DataInput)(implicit ctx: Context[T], tx: T): IExpr[T, Any]
}
