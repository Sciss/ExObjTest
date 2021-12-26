/*
 *  QuaternaryOp.scala
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
package graph

import de.sciss.lucre.exnew.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.exnew.impl.IChangeEventImpl
import de.sciss.lucre.{Adjunct, Exec, Txn}
import de.sciss.serial.DataOutput

object QuaternaryOp extends ProductReader[QuaternaryOp[_, _, _, _, _]] {
  abstract class Op[A, B, C, D, E] extends Product {
    def apply(a: A, b: B, c: C, d: D): E
  }

  abstract class NamedOp[A, B, C, D, E] extends Op[A, B, C, D, E] {
    override def productPrefix = s"QuaternaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  type Adjuncts = scala.List[Adjunct]

  // ---- Seq ----

  object SeqMkString extends ProductReader[SeqMkString[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqMkString[_] = {
      require (arity == 0 && adj == 0)
      new SeqMkString()
    }
  }
  final case class SeqMkString[A]() extends NamedOp[Seq[A], String, String, String, String] {
    def apply(a: Seq[A], b: String, c: String, d: String): String =
      a.mkString(b, c, d)

    def name = "SeqMkString"
  }

  object SeqPatch extends ProductReader[SeqPatch[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqPatch[_, _] = {
      require (arity == 0 && adj == 0)
      new SeqPatch()
    }
  }
  final case class SeqPatch[A, B >: A]() extends NamedOp[Seq[A], Int, Seq[B], Int, Seq[B]] {
    def apply(a: Seq[A], from: Int, other: Seq[B], replaced: Int): Seq[B] =
      a.patch(from, other, replaced)

    def name = "SeqPatch"
  }

  private[lucre] final class Expanded[T <: Exec[T], A1, A2, A3, A4, A](op: QuaternaryOp.Op[A1, A2, A3, A4, A],
                                                                       a: IExpr[T, A1], b: IExpr[T, A2],
                                                                       c: IExpr[T, A3], d: IExpr[T, A4])
                                                                      (implicit protected val targets: ITargets[T])
    extends IExpr[T, A] with IChangeEventImpl[T, A] {

    override protected def typeId: Int = ???

    override protected def writeData(out: DataOutput): Unit = {
      out.writeByte(0)  // serialization version
      ??? // op.write(out)
      a.write(out)
      b.write(out)
      c.write(out)
      d.write(out)
    }

    def connect()(implicit tx: T): this.type = {
      a.changed.--->(this)
      b.changed.--->(this)
      c.changed.--->(this)
      d.changed.--->(this)
      this
    }

    override def toString: String = s"QuaternaryOp($op, $a, $b, $c, $d)"

    def changed: IChangeEvent[T, A] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
      val _1v = pull.expr(a)
      val _2v = pull.expr(b)
      val _3v = pull.expr(c)
      val _4v = pull.expr(d)

      value1(_1v, _2v, _3v, _4v)
    }

    @inline
    private def value1(av: A1, bv: A2, cv: A3, dv: A4): A = {
      op.apply(av, bv, cv, dv)
    }

    override def value(implicit context: Context[T], tx: T): A = {
      val av = a.value
      val bv = b.value
      val cv = c.value
      val dv = d.value
      value1(av, bv, cv, dv)
    }

    def dispose()(implicit tx: T): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
      c.changed -/-> changed
      d.changed -/-> changed
    }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): QuaternaryOp[_, _, _, _, _] = {
    require (arity == 5 && adj == 0)
    val _op = in.readProductT[Op[Any, Any, Any, Any, Any]]()
    val _a  = in.readEx[Any]()
    val _b  = in.readEx[Any]()
    val _c  = in.readEx[Any]()
    val _d  = in.readEx[Any]()
    new QuaternaryOp(_op, _a, _b, _c, _d)
  }
}
final case class QuaternaryOp[A1, A2, A3, A4, A](op: QuaternaryOp.Op[A1, A2, A3, A4, A],
                                                 a: Ex[A1], b: Ex[A2], c: Ex[A3], d: Ex[A4])
  extends Ex[A] {

  type Repr[T <: Txn[T]] = IExpr[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val ax = a.expand[T]
    val bx = b.expand[T]
    val cx = c.expand[T]
    val dx = d.expand[T]
    new QuaternaryOp.Expanded[T, A1, A2, A3, A4, A](op, ax, bx, cx, dx).connect()
  }
}
