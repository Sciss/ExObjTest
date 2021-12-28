package de.sciss.lucre.exnew

import de.sciss.lucre.{InMemory, IntObj}

object Test {
//  type S = Durable
//  type T = Durable.Txn

  type S = InMemory
  type T = InMemory.Txn

  def main(args: Array[String]): Unit = {
    implicit val system: S = InMemory()
    val (inH, outH) = system.step { implicit tx =>
      import ExImport._
      import de.sciss.lucre.exnew.graph._
      val ex: Ex[Int] = "in".attr(0) * 2
      val input     = IntObj.newVar[T](0)
      val transform = IntExObj[T](ex)
      println("--- put 'in'")
      transform.attr.put("in", input)
      val output    = IntObj.newVar[T](transform)
      (tx.newHandle(input), tx.newHandle(output))
    }

    system.step { implicit tx =>
      val out = outH()
      println("--- add react")
      out.changed.react { implicit tx => upd =>
        println(s"OBSERVED: $upd")
      }
    }

    system.step { implicit tx =>
      val in = inH()
      println("--- update 'in'")
      in() = 1000
    }

    val v = system.step { implicit tx =>
      val out = outH()
      println("--- call 'value'")
      out.value
    }
    println(s"OUTPUT now $v")
  }
}
