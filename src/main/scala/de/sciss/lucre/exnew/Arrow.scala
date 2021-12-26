/*
 *  Arrow.scala
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

import de.sciss.lucre.expr.graph.Ex

object Arrow {
  trait Right[A, -F] {
    def patchTo(source: Ex.Source[A], sink: F): Unit
  }

  object Left {
    implicit def ex[A]: Left[A, Ex[A]] = new ExArrowLeft[A]

    private final class ExArrowLeft[A] extends Left[A, Ex[A]] {
      override def patchFrom(source: Ex[A], sink: Ex.Sink[A]): Unit =
        sink.update(source)
    }
  }
  trait Left[A, -F] {
    def patchFrom(source: F, sink: Ex.Sink[A]): Unit
  }
}
trait Arrow[A, F] extends Arrow.Left[A, F] with Arrow.Right[A, F]
