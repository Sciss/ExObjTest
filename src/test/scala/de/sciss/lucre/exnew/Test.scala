package de.sciss.lucre.exnew

import de.sciss.lucre.expr.LucreExpr
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{DataStore, Durable, InMemory, IntObj}

object Test {
  type S = Durable
  type T = Durable.Txn

//  type S = InMemory
//  type T = InMemory.Txn

  def main(args: Array[String]): Unit = {
    LucreExpr.init()
    IntExObj.init()

//    implicit val system: S = InMemory()
    val store: DataStore.Factory = BerkeleyDB.tmp()
    implicit val system: S = Durable(store)
    val (inH, outH) = system.step { implicit tx =>
      import ExImport._
      import de.sciss.lucre.exnew.graph._
//      val ex: Ex[Int] = "in".attr(0) * 2
      val ex: Ex[Int] = Var(123) * 2
      val input     = IntObj.newVar[T](0)
      val transform = IntExObj[T](ex)
      transform.attr.put("in", input)
      val output    = IntObj.newVar[T](transform)
      (tx.newHandle(input), tx.newHandle(output))
    }

    system.step { implicit tx =>
      val out = outH()
      out.changed.react { implicit tx => upd =>
        println(s"OBSERVED: $upd")
      }
    }

    system.step { implicit tx =>
      val in  = inH()
      val out = outH()
      in() = 1000
      println(s"VALUE: ${out.value}")
    }
  }
}
