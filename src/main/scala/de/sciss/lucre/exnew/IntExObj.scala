/*
 *  IntExObj.scala
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

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.{IntObj, Txn}

object IntExObj {
  def TEST[T <: Txn[T]](implicit tx: T): IntObj[T] = {
    import ExImport._
    import de.sciss.lucre.exnew.graph._
    val ex: Ex[Int] = "in".attr(0) * 2
    //    val vr = Var(0)
    //    val tr = Trig()
    //    ex.changed --> Act(vr.inc, tr)
    //    val res: Ex[Int] = vr.latch(tr)
    val tgt = Targets[T]()
    ??? // new IntEx[T](ex, tgt, tx)
  }
}
