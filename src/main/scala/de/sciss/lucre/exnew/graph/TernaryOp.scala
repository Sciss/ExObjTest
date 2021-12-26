/*
 *  TernaryOp.scala
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

import de.sciss.lucre.Adjunct.{Num, Widen2}
import de.sciss.lucre.exnew.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.exnew.impl.IChangeEventImpl
import de.sciss.lucre.{Adjunct, Exec, ProductWithAdjuncts, Txn}

import scala.util.control.NonFatal

object TernaryOp extends ProductReader[TernaryOp[_, _, _, _]] {
  abstract class Op[A, B, C, D] extends Product {
    def apply(a: A, b: B, c: C): D
  }

  abstract class NamedOp[A, B, C, D] extends Op[A, B, C, D] {
    override def productPrefix = s"TernaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  type Adjuncts = scala.List[Adjunct]

  // ---- (Num, Num, Num) -> Num ----

  object Clip extends ProductReader[Clip[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Clip[_, _, _] = {
      require (arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any] = in.readAdjunct()
      new Clip[Any, Any, Any]()(_widen, _num)
    }
  }
  final case class Clip[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B, c: B): C = num.clip(widen.widen1(a), widen.widen2(b), widen.widen2(c))

    def name = "Clip"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Fold extends ProductReader[Fold[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Fold[_, _, _] = {
      require (arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any] = in.readAdjunct()
      new Fold[Any, Any, Any]()(_widen, _num)
    }
  }
  final case class Fold[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B, c: B): C = num.fold(widen.widen1(a), widen.widen2(b), widen.widen2(c))

    def name = "Fold"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Wrap extends ProductReader[Wrap[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Wrap[_, _, _] = {
      require (arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any] = in.readAdjunct()
      new Wrap[Any, Any, Any]()(_widen, _num)
    }
  }
  final case class Wrap[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B, c: B): C = num.wrap(widen.widen1(a), widen.widen2(b), widen.widen2(c))

    def name = "Wrap"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  // ---- String ----

  object StringSlice extends ProductReader[StringSlice] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringSlice = {
      require (arity == 0 && adj == 0)
      new StringSlice()
    }
  }
  final case class StringSlice() extends NamedOp[String, Int, Int, String] {
    def apply(a: String, b: Int, c: Int): String = a.slice(b, c)

    def name = "StringSlice"
  }

  object StringSplit extends ProductReader[StringSplit] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringSplit = {
      require (arity == 0 && adj == 0)
      new StringSplit()
    }
  }
  final case class StringSplit() extends NamedOp[String, String, Int, Seq[String]] {
    def apply(s: String, regex: String, limit: Int): Seq[String] =
      try {
        s.split(regex, limit)
      } catch {
        //        case _: PatternSyntaxException => Nil -- type does not exist on SJS!
        case NonFatal(_) => Nil
      }

    def name = "StringSplit"
  }

  // ---- Seq ----

  object SeqIndexOf extends ProductReader[SeqIndexOf[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqIndexOf[_, _] = {
      require (arity == 0 && adj == 0)
      new SeqIndexOf()
    }
  }
  final case class SeqIndexOf[A, B >: A]() extends NamedOp[Seq[A], B, Int, Int] {
    def apply(a: Seq[A], elem: B, from: Int): Int = a.indexOf(elem, from)

    def name = "SeqIndexOf"
  }

  object SeqIndexOfSlice extends ProductReader[SeqIndexOfSlice[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqIndexOfSlice[_, _] = {
      require (arity == 0 && adj == 0)
      new SeqIndexOfSlice()
    }
  }
  final case class SeqIndexOfSlice[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Int, Int] {
    def apply(a: Seq[A], that: Seq[B], from: Int): Int = a.indexOfSlice(that, from)

    def name = "SeqIndexOfSlice"
  }

  object SeqLastIndexOf extends ProductReader[SeqLastIndexOf[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqLastIndexOf[_, _] = {
      require (arity == 0 && adj == 0)
      new SeqLastIndexOf()
    }
  }
  final case class SeqLastIndexOf[A, B >: A]() extends NamedOp[Seq[A], B, Int, Int] {
    def apply(a: Seq[A], elem: B, from: Int): Int = a.lastIndexOf(elem, from)

    def name = "SeqLastIndexOf"
  }

  object SeqLastIndexOfSlice extends ProductReader[SeqLastIndexOfSlice[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqLastIndexOfSlice[_, _] = {
      require (arity == 0 && adj == 0)
      new SeqLastIndexOfSlice()
    }
  }
  final case class SeqLastIndexOfSlice[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Int, Int] {
    def apply(a: Seq[A], that: Seq[B], from: Int): Int = a.lastIndexOfSlice(that, from)

    def name = "SeqLastIndexOfSlice"
  }

  object SeqPadTo extends ProductReader[SeqPadTo[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqPadTo[_, _] = {
      require (arity == 0 && adj == 0)
      new SeqPadTo()
    }
  }
  final case class SeqPadTo[A, B >: A]() extends NamedOp[Seq[A], Int, B, Seq[B]] {
    def apply(a: Seq[A], len: Int, elem: B): Seq[B] = a.padTo(len, elem)

    def name = "SeqPadTo"
  }

  object SeqSlice extends ProductReader[SeqSlice[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqSlice[_] = {
      require (arity == 0 && adj == 0)
      new SeqSlice()
    }
  }
  final case class SeqSlice[A]() extends NamedOp[Seq[A], Int, Int, Seq[A]] {
    def apply(a: Seq[A], b: Int, c: Int): Seq[A] = a.slice(b, c)

    def name = "SeqSlice"
  }

  object SeqSliding extends ProductReader[SeqSliding[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqSliding[_] = {
      require (arity == 0 && adj == 0)
      new SeqSliding()
    }
  }
  final case class SeqSliding[A]() extends NamedOp[Seq[A], Int, Int, Seq[Seq[A]]] {
    def apply(a: Seq[A], size: Int, step: Int): Seq[Seq[A]] =
      a.sliding(math.max(1, size), math.max(1, step)).toIndexedSeq

    def name = "SeqSliding"
  }

  object SeqStartsWith extends ProductReader[SeqStartsWith[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqStartsWith[_, _] = {
      require (arity == 0 && adj == 0)
      new SeqStartsWith()
    }
  }
  final case class SeqStartsWith[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Int, Boolean] {
    def apply(a: Seq[A], b: Seq[B], offset: Int): Boolean = a.startsWith(b, offset)

    def name = "SeqStartsWith"
  }

  object SeqUpdated extends ProductReader[SeqUpdated[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqUpdated[_, _] = {
      require (arity == 0 && adj == 0)
      new SeqUpdated()
    }
  }
  final case class SeqUpdated[A, B >: A]() extends NamedOp[Seq[A], Int, B, Seq[B]] {
    def apply(a: Seq[A], index: Int, elem: B): Seq[B] =
      if (index >= 0 && index < a.size) a.updated(index, elem) else a

    def name = "SeqUpdated"
  }

  // ----

  private[lucre] final class Expanded[T <: Exec[T], A1, A2, A3, A](op: TernaryOp.Op[A1, A2, A3, A],
                                                                   a: IExpr[T, A1], b: IExpr[T, A2],
                                                                   c: IExpr[T, A3], tx0: T)
                                                                  (implicit protected val targets: ITargets[T])
    extends IExpr[T, A] with IChangeEventImpl[T, A] {

    a.changed.--->(this)(tx0)
    b.changed.--->(this)(tx0)
    c.changed.--->(this)(tx0)

    override def toString: String = s"TernaryOp($op, $a, $b, $c)"

    def changed: IChangeEvent[T, A] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
      val _1v = pull.expr(a)
      val _2v = pull.expr(b)
      val _3v = pull.expr(c)

      value1(_1v, _2v, _3v)
    }

    @inline
    private def value1(av: A1, bv: A2, cv: A3): A = {
      op.apply(av, bv, cv)
    }

    def value(implicit tx: T): A = {
      val av = a.value
      val bv = b.value
      val cv = c.value
      value1(av, bv, cv)
    }

    def dispose()(implicit tx: T): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
      c.changed -/-> changed
    }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): TernaryOp[_, _, _, _] = {
    require (arity == 4 && adj == 0)
    val _op = in.readProductT[Op[Any, Any, Any, Any]]()
    val _a  = in.readEx[Any]()
    val _b  = in.readEx[Any]()
    val _c  = in.readEx[Any]()
    new TernaryOp(_op, _a, _b, _c)
  }
}
final case class TernaryOp[A1, A2, A3, A](op: TernaryOp.Op[A1, A2, A3, A], a: Ex[A1], b: Ex[A2], c: Ex[A3])
  extends Ex[A] {

  type Repr[T <: Txn[T]] = IExpr[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val ax = a.expand[T]
    val bx = b.expand[T]
    val cx = c.expand[T]
    new TernaryOp.Expanded[T, A1, A2, A3, A](op, ax, bx, cx, tx)
  }
}
