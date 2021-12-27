/*
 *  BinaryOp.scala
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

import java.net.{URI => _URI}
import de.sciss.asyncfile.Ops.URIOps
import de.sciss.lucre.Adjunct.{HasDefault, Num, NumDiv, NumDouble, NumInt, NumLogic, Ord, Widen2}
import de.sciss.lucre.exnew.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.exnew.impl.IChangeEventImpl
import de.sciss.lucre.{Adjunct, Exec, ProductWithAdjuncts, Txn}
import de.sciss.serial.{DataInput, DataOutput, Writable}
import de.sciss.span.SpanLike

object BinaryOp extends ProductReader[BinaryOp[_, _, _ , _]] {
  object Op {
    def read[A, B, C](rIn: RefMapIn): Op[A, B, C] = {
      val in = rIn.in
      val serVer = in.readByte()
      require (serVer == 0)
      val name = in.readUTF()
      val (r, adj) = name match {
        case Plus .name => (Plus  , in.readShort())
        case Minus.name => (Minus , in.readShort())
        case Times.name => (Times , in.readShort())
      }
      val p = r.read(rIn, key = "???", arity = 0, adj = adj)
      p.asInstanceOf[Op[A, B, C]]
    }
  }
  abstract class Op[A, B, C] extends Product with Writable {
    def apply(a: A, b: B): C
  }

  abstract class NamedOp[A, B, C] extends Op[A, B, C] {
    override def productPrefix = s"BinaryOp$$$name"

    def name: String

    override def toString: String = name

    override def write(out: DataOutput): Unit = {
      out.writeByte(0)  // serialization version
      out.writeUTF(name)
      this match {
        case p: ProductWithAdjuncts =>
          val adj = p.adjuncts
          out.writeShort(adj.size)
          adj.foreach(_.write(out))
        case _ => ()
      }
    }
  }

  type Adjuncts = scala.List[Adjunct]

  // ---- (Num, Num) -> Num ----

  object Plus extends ProductReader[Plus[_, _, _]] {
    final val name = "Plus"

    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Plus[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Plus()(_widen, _num)
    }
  }
  final case class Plus[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.plus(widen.widen1(a), widen.widen2(b))

    def name: String = Plus.name

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Minus extends ProductReader[Minus[_, _, _]] {
    final val name = "Minus"

    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Minus[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Minus()(_widen, _num)
    }
  }
  final case class Minus[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.minus(widen.widen1(a), widen.widen2(b))

    def name: String = Minus.name

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Times extends ProductReader[Times[_, _, _]] {
    final val name = "Times"

    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Times[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Times()(_widen, _num)
    }
  }
  final case class Times[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.times(widen.widen1(a), widen.widen2(b))

    def name: String = Times.name

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  /** Division */
  object Div extends ProductReader[Div[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Div[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : NumDiv[Any]           = in.readAdjunct()
      new Div()(_widen, _num)
    }
  }
  final case class Div[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDiv[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.div(widen.widen1(a), widen.widen2(b))

    def name = "Div"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object ModJ extends ProductReader[ModJ[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): ModJ[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new ModJ[Any, Any, Any]()(_widen, _num)
    }
  }
  final case class ModJ[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.rem(widen.widen1(a), widen.widen2(b))

    def name = "ModJ"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Mod extends ProductReader[Mod[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Mod[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Mod[Any, Any, Any]()(_widen, _num)
    }
  }
  final case class Mod[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.mod(widen.widen1(a), widen.widen2(b))

    def name = "Mod"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  // ---- (Ord, Ord) -> Boolean ----

  /** Equal */
  object Eq extends ProductReader[Eq[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Eq[_, _] = {
      require(arity == 0 && adj == 1)
      val _eq: Adjunct.Eq[Any] { type Boolean = Any } = in.readAdjunct()
      new Eq[Any, Any]()(_eq)
    }
  }
  final case class Eq[A, B]()(implicit eq: Adjunct.Eq[A] { type Boolean = B })
    extends NamedOp[A, A, B] with ProductWithAdjuncts {

    def apply(a: A, b: A): B = eq.eq(a, b)

    def name = "Eq"

    override def adjuncts: Adjuncts = eq :: Nil
  }

  /** Not equal */
  object Neq extends ProductReader[Neq[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Neq[_, _] = {
      require(arity == 0 && adj == 1)
      val _eq: Adjunct.Eq[Any] { type Boolean = Any } = in.readAdjunct()
      new Neq[Any, Any]()(_eq)
    }
  }
  final case class Neq[A, B]()(implicit eq: Adjunct.Eq[A] { type Boolean = B})
    extends NamedOp[A, A, B] with ProductWithAdjuncts {

    def apply(a: A, b: A): B = eq.neq(a, b)

    def name = "Neq"

    override def adjuncts: Adjuncts = eq :: Nil
  }

  /** Less than */
  object Lt extends ProductReader[Lt[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Lt[_, _] = {
      require(arity == 0 && adj == 1)
      val _ord: Ord[Any] { type Boolean = Any } = in.readAdjunct()
      new Lt[Any, Any]()(_ord)
    }
  }
  final case class Lt[A, B]()(implicit ord: Ord[A] { type Boolean = B })
    extends NamedOp[A, A, B] with ProductWithAdjuncts {

    def apply(a: A, b: A): B = ord.lt(a, b)

    def name = "Lt"

    override def adjuncts: Adjuncts = ord :: Nil
  }

  /** Greater than */
  object Gt extends ProductReader[Gt[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Gt[_, _] = {
      require(arity == 0 && adj == 1)
      val _ord: Ord[Any] { type Boolean = Any } = in.readAdjunct()
      new Gt[Any, Any]()(_ord)
    }
  }
  final case class Gt[A, B]()(implicit ord: Ord[A] { type Boolean = B })
    extends NamedOp[A, A, B] with ProductWithAdjuncts {

    def apply(a: A, b: A): B = ord.gt(a, b)

    def name = "Gt"

    override def adjuncts: Adjuncts = ord :: Nil
  }

  /** Less than or equal */
  object Leq extends ProductReader[Leq[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Leq[_, _] = {
      require(arity == 0 && adj == 1)
      val _ord: Ord[Any] { type Boolean = Any } = in.readAdjunct()
      new Leq[Any, Any]()(_ord)
    }
  }
  final case class Leq[A, B]()(implicit ord: Ord[A] { type Boolean = B })
    extends NamedOp[A, A, B] with ProductWithAdjuncts {

    def apply(a: A, b: A): B = ord.lteq(a, b)

    def name = "Leq"

    override def adjuncts: Adjuncts = ord :: Nil
  }

  /** Greater than or equal */
  object Geq extends ProductReader[Geq[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Geq[_, _] = {
      require(arity == 0 && adj == 1)
      val _ord: Ord[Any] { type Boolean = Any } = in.readAdjunct()
      new Geq[Any, Any]()(_ord)
    }
  }
  final case class Geq[A, B]()(implicit ord: Ord[A] { type Boolean = B })
    extends NamedOp[A, A, B] with ProductWithAdjuncts {

    def apply(a: A, b: A): B = ord.gteq(a, b)

    def name = "Geq"

    override def adjuncts: Adjuncts = ord :: Nil
  }

  // ---- (Num, Num) -> Num ----

  object Min extends ProductReader[Min[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Min[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Min[Any, Any, Any]()(_widen, _num)
    }
  }
  final case class Min[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.min(widen.widen1(a), widen.widen2(b))

    def name = "Min"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Max extends ProductReader[Max[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Max[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Max[Any, Any, Any]()(_widen, _num)
    }
  }
  final case class Max[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.max(widen.widen1(a), widen.widen2(b))

    def name = "Max"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object And extends ProductReader[And[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): And[_] = {
      require(arity == 0 && adj == 1)
      val _num: NumLogic[Any] = in.readAdjunct()
      new And()(_num)
    }
  }
  final case class And[A]()(implicit num: NumLogic[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.and(a, b)

    def name = "And"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object Or extends ProductReader[Or[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Or[_] = {
      require(arity == 0 && adj == 1)
      val _num: NumLogic[Any] = in.readAdjunct()
      new Or()(_num)
    }
  }
  final case class Or[A]()(implicit num: NumLogic[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.or(a, b)

    def name = "Or"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object Xor extends ProductReader[Xor[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Xor[_] = {
      require(arity == 0 && adj == 1)
      val _num: NumLogic[Any] = in.readAdjunct()
      new Xor()(_num)
    }
  }
  final case class Xor[A]()(implicit num: NumLogic[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.xor(a, b)

    def name = "Xor"

    override def adjuncts: Adjuncts = num :: Nil
  }

  // NOTE: keep this class for backward serialization compatibility!
  // But in general `Div` kicks in now.
  object IDiv extends ProductReader[IDiv[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): IDiv[_] = {
      require(arity == 0 && adj == 1)
      val _num: NumInt[Any] = in.readAdjunct()
      new IDiv()(_num)
    }
  }
  final case class IDiv[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.div(a, b)

    def name = "IDiv"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object Lcm extends ProductReader[Lcm[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Lcm[_] = {
      require(arity == 0 && adj == 1)
      val _num: NumInt[Any] = in.readAdjunct()
      new Lcm()(_num)
    }
  }
  final case class Lcm[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.lcm(a, b)

    def name = "Lcm"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object Gcd extends ProductReader[Gcd[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Gcd[_] = {
      require(arity == 0 && adj == 1)
      val _num: NumInt[Any] = in.readAdjunct()
      new Gcd()(_num)
    }
  }
  final case class Gcd[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.gcd(a, b)

    def name = "Gcd"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object RoundTo extends ProductReader[RoundTo[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): RoundTo[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new RoundTo()(_widen, _num)
    }
  }
  final case class RoundTo[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.roundTo(widen.widen1(a), widen.widen2(b))

    def name = "RoundTo"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object RoundUpTo extends ProductReader[RoundUpTo[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): RoundUpTo[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new RoundUpTo()(_widen, _num)
    }
  }
  final case class RoundUpTo[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.roundUpTo(widen.widen1(a), widen.widen2(b))

    def name = "RoundUpTo"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Trunc extends ProductReader[Trunc[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Trunc[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Trunc()(_widen, _num)
    }
  }
  final case class Trunc[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.trunc(widen.widen1(a), widen.widen2(b))

    def name = "Trunc"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Atan2 extends ProductReader[Atan2[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Atan2[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : NumDouble[Any]        = in.readAdjunct()
      new Atan2()(_widen, _num)
    }
  }
  final case class Atan2[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.atan2(widen.widen1(a), widen.widen2(b))

    def name = "Atan2"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Hypot extends ProductReader[Hypot[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Hypot[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : NumDouble[Any]        = in.readAdjunct()
      new Hypot()(_widen, _num)
    }
  }
  final case class Hypot[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.hypot(widen.widen1(a), widen.widen2(b))

    def name = "Hypot"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Hypotx extends ProductReader[Hypotx[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Hypotx[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : NumDouble[Any]        = in.readAdjunct()
      new Hypotx()(_widen, _num)
    }
  }
  final case class Hypotx[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.hypotApx(widen.widen1(a), widen.widen2(b))

    def name = "Hypotx"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Pow extends ProductReader[Pow[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Pow[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : NumDouble[Any]        = in.readAdjunct()
      new Pow()(_widen, _num)
    }
  }
  final case class Pow[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.pow(widen.widen1(a), widen.widen2(b))

    def name = "Pow"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object LeftShift extends ProductReader[LeftShift[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): LeftShift[_] = {
      require(arity == 0 && adj == 1)
      val _num: NumInt[Any] = in.readAdjunct()
      new LeftShift()(_num)
    }
  }
  final case class LeftShift[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.shiftLeft(a, b)

    def name = "LeftShift"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object RightShift extends ProductReader[RightShift[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): RightShift[_] = {
      require(arity == 0 && adj == 1)
      val _num: NumInt[Any] = in.readAdjunct()
      new RightShift()(_num)
    }
  }
  final case class RightShift[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.shiftRight(a, b)

    def name = "RightShift"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object UnsignedRightShift extends ProductReader[UnsignedRightShift[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): UnsignedRightShift[_] = {
      require(arity == 0 && adj == 1)
      val _num: NumInt[Any] = in.readAdjunct()
      new UnsignedRightShift()(_num)
    }
  }
  final case class UnsignedRightShift[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.unsignedShiftRight(a, b)

    def name = "UnsignedRightShift"

    override def adjuncts: Adjuncts = num :: Nil
  }

  //  Ring1
  //  Ring2
  //  Ring3
  //  Ring4

  object Difsqr extends ProductReader[Difsqr[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Difsqr[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Difsqr()(_widen, _num)
    }
  }
  final case class Difsqr[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.difSqr(widen.widen1(a), widen.widen2(b))

    def name = "Difsqr"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Sumsqr extends ProductReader[Sumsqr[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Sumsqr[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Sumsqr()(_widen, _num)
    }
  }
  final case class Sumsqr[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.sumSqr(widen.widen1(a), widen.widen2(b))

    def name = "Sumsqr"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Sqrsum extends ProductReader[Sqrsum[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Sqrsum[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Sqrsum()(_widen, _num)
    }
  }
  final case class Sqrsum[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.sqrSum(widen.widen1(a), widen.widen2(b))

    def name = "Sqrsum"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Sqrdif extends ProductReader[Sqrdif[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Sqrdif[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Sqrdif()(_widen, _num)
    }
  }
  final case class Sqrdif[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.sqrDif(widen.widen1(a), widen.widen2(b))

    def name = "Sqrdif"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Absdif extends ProductReader[Absdif[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Absdif[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Absdif()(_widen, _num)
    }
  }
  final case class Absdif[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.absDif(widen.widen1(a), widen.widen2(b))

    def name = "Absdif"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  //  Thresh
  //  Amclip
  //  Scaleneg

  object Clip2 extends ProductReader[Clip2[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Clip2[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Clip2()(_widen, _num)
    }
  }
  final case class Clip2[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.clip2(widen.widen1(a), widen.widen2(b))

    def name = "Clip2"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Excess extends ProductReader[Excess[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Excess[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Excess()(_widen, _num)
    }
  }
  final case class Excess[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.excess(widen.widen1(a), widen.widen2(b))

    def name = "Excess"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Fold2 extends ProductReader[Fold2[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Fold2[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Fold2()(_widen, _num)
    }
  }
  final case class Fold2[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.fold2(widen.widen1(a), widen.widen2(b))

    def name = "Fold2"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object Wrap2 extends ProductReader[Wrap2[_, _, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Wrap2[_, _, _] = {
      require(arity == 0 && adj == 2)
      val _widen: Widen2[Any, Any, Any] = in.readAdjunct()
      val _num  : Num[Any]              = in.readAdjunct()
      new Wrap2()(_widen, _num)
    }
  }
  final case class Wrap2[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.wrap2(widen.widen1(a), widen.widen2(b))

    def name = "Wrap2"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  object RangeExclusive extends ProductReader[RangeExclusive] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): RangeExclusive = {
      require(arity == 0 && adj == 0)
      new RangeExclusive()
    }
  }
  final case class RangeExclusive()
    extends NamedOp[Int, Int, Seq[Int]] {

    def apply(a: Int, b: Int): Seq[Int] = Range(a, b)

    def name = "RangeExclusive"
  }

  object RangeInclusive extends ProductReader[RangeInclusive] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): RangeInclusive = {
      require(arity == 0 && adj == 0)
      new RangeInclusive()
    }
  }
  final case class RangeInclusive()
    extends NamedOp[Int, Int, Seq[Int]] {

    def apply(a: Int, b: Int): Seq[Int] = Range.inclusive(a, b)

    def name = "RangeInclusive"
  }

  // ---- Option ----

  object OptionContains extends ProductReader[OptionContains[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): OptionContains[_] = {
      require(arity == 0 && adj == 0)
      new OptionContains()
    }
  }
  final case class OptionContains[A]() extends NamedOp[Option[A], A, Boolean] {
    def apply(a: Option[A], b: A): Boolean = a.contains(b)

    def name = "OptionContains"
  }

  object OptionGetOrElse extends ProductReader[OptionGetOrElse[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): OptionGetOrElse[_] = {
      require(arity == 0 && adj == 0)
      new OptionGetOrElse()
    }
  }
  final case class OptionGetOrElse[A]() extends NamedOp[Option[A], A, A] {
    def apply(a: Option[A], b: A): A = a.getOrElse(b)

    def name = "OptionGetOrElse"
  }

  object OptionOrElse extends ProductReader[OptionOrElse[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): OptionOrElse[_] = {
      require(arity == 0 && adj == 0)
      new OptionOrElse()
    }
  }
  final case class OptionOrElse[A]() extends NamedOp[Option[A], Option[A], Option[A]] {
    def apply(a: Option[A], b: Option[A]): Option[A] = a.orElse(b)

    def name = "OptionGetOrElse"
  }

  // ---- Seq ----

  object SeqAppended extends ProductReader[SeqAppended[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqAppended[_, _] = {
      require(arity == 0 && adj == 0)
      new SeqAppended()
    }
  }
  final case class SeqAppended[A, B >: A]() extends NamedOp[Seq[A], B, Seq[B]] {
    def apply(a: Seq[A], b: B): Seq[B] = a :+ b

    def name = "SeqAppended"
  }

  object SeqApply extends ProductReader[SeqApply[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqApply[_] = {
      require(arity == 0 && adj == 1)
      val _d: HasDefault[Any] = in.readAdjunct()
      new SeqApply[Any]()(_d)
    }
  }
  final case class SeqApply[A]()(implicit d: HasDefault[A])
    extends NamedOp[Seq[A], Int, A] with ProductWithAdjuncts {

    def apply(a: Seq[A], b: Int): A = if (b < 0 || a.lengthCompare(b) <= 0) {
      d.defaultValue
    } else {
      a.apply(b)
    }

    def name = "SeqApply"

    override def adjuncts: Adjuncts = d :: Nil
  }

  object SeqApplyOption extends ProductReader[SeqApplyOption[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqApplyOption[_] = {
      require(arity == 0 && adj == 0)
      new SeqApplyOption()
    }
  }
  final case class SeqApplyOption[A]() extends NamedOp[Seq[A], Int, Option[A]] {
    def apply(a: Seq[A], b: Int): Option[A] = if (b < 0) None else {
      if (a.lengthCompare(b) <= 0) None else Some(a.apply(b))
    }

    def name = "SeqApplyOption"
  }

  object SeqConcat extends ProductReader[SeqConcat[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqConcat[_] = {
      require(arity == 0 && adj == 0)
      new SeqConcat()
    }
  }
  final case class SeqConcat[A]() extends NamedOp[Seq[A], Seq[A], Seq[A]] {
    def apply(a: Seq[A], b: Seq[A]): Seq[A] = a ++ b

    def name = "SeqConcat"
  }

  object SeqContains extends ProductReader[SeqContains[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqContains[_, _] = {
      require(arity == 0 && adj == 0)
      new SeqContains()
    }
  }
  final case class SeqContains[A, B >: A]() extends NamedOp[Seq[A], B, Boolean] {
    def apply(a: Seq[A], b: B): Boolean = a.contains(b)

    def name = "SeqContains"
  }

  object SeqDiff extends ProductReader[SeqDiff[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqDiff[_, _] = {
      require(arity == 0 && adj == 0)
      new SeqDiff()
    }
  }
  final case class SeqDiff[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Seq[A]] {
    def apply(a: Seq[A], b: Seq[B]): Seq[A] = a.diff(b)

    def name = "SeqDiff"
  }

  object SeqDrop extends ProductReader[SeqDrop[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqDrop[_] = {
      require(arity == 0 && adj == 0)
      new SeqDrop[Any]()
    }
  }
  final case class SeqDrop[A]() extends NamedOp[Seq[A], Int, Seq[A]] {
    def apply(a: Seq[A], b: Int): Seq[A] = a.drop(b)

    def name = "SeqDrop"
  }

  object SeqDropRight extends ProductReader[SeqDropRight[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqDropRight[_] = {
      require(arity == 0 && adj == 0)
      new SeqDropRight[Any]()
    }
  }
  final case class SeqDropRight[A]() extends NamedOp[Seq[A], Int, Seq[A]] {
    def apply(a: Seq[A], b: Int): Seq[A] = a.dropRight(b)

    def name = "SeqDropRight"
  }

  object SeqEndsWith extends ProductReader[SeqEndsWith[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqEndsWith[_, _] = {
      require(arity == 0 && adj == 0)
      new SeqEndsWith()
    }
  }
  final case class SeqEndsWith[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Boolean] {
    def apply(a: Seq[A], b: Seq[B]): Boolean = a.endsWith(b)

    def name = "SeqEndsWith"
  }

  object SeqGrouped extends ProductReader[SeqGrouped[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqGrouped[_] = {
      require(arity == 0 && adj == 0)
      new SeqGrouped[Any]()
    }
  }
  final case class SeqGrouped[A]() extends NamedOp[Seq[A], Int, Seq[Seq[A]]] {
    def apply(a: Seq[A], b: Int): Seq[Seq[A]] = a.grouped(b).toIndexedSeq

    def name = "SeqGrouped"
  }

  object SeqIndexOf extends ProductReader[SeqIndexOf[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqIndexOf[_, _] = {
      require(arity == 0 && adj == 0)
      new SeqIndexOf()
    }
  }
  final case class SeqIndexOf[A, B >: A]() extends NamedOp[Seq[A], B, Int] {
    def apply(a: Seq[A], b: B): Int = a.indexOf(b)

    def name = "SeqIndexOf"
  }

  object SeqIndexOfSlice extends ProductReader[SeqIndexOfSlice[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqIndexOfSlice[_, _] = {
      require(arity == 0 && adj == 0)
      new SeqIndexOfSlice()
    }
  }
  final case class SeqIndexOfSlice[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Int] {
    def apply(a: Seq[A], that: Seq[B]): Int = a.indexOfSlice(that)

    def name = "SeqIndexOfSlice"
  }

  object SeqIntersect extends ProductReader[SeqIntersect[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqIntersect[_, _] = {
      require(arity == 0 && adj == 0)
      new SeqIntersect()
    }
  }
  final case class SeqIntersect[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Seq[A]] {
    def apply(a: Seq[A], b: Seq[B]): Seq[A] = a.intersect(b)

    def name = "SeqIntersect"
  }

  object SeqIsDefinedAt extends ProductReader[SeqIsDefinedAt[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqIsDefinedAt[_] = {
      require(arity == 0 && adj == 0)
      new SeqIsDefinedAt[Any]()
    }
  }
  final case class SeqIsDefinedAt[A]() extends NamedOp[Seq[A], Int, Boolean] {
    def apply(a: Seq[A], b: Int): Boolean = a.isDefinedAt(b)

    def name = "SeqIsDefinedAt"
  }

  object SeqLastIndexOf extends ProductReader[SeqLastIndexOf[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqLastIndexOf[_, _] = {
      require(arity == 0 && adj == 0)
      new SeqLastIndexOf()
    }
  }
  final case class SeqLastIndexOf[A, B >: A]() extends NamedOp[Seq[A], B, Int] {
    def apply(a: Seq[A], b: B): Int = a.lastIndexOf(b)

    def name = "SeqLastIndexOf"
  }

  object SeqLastIndexOfSlice extends ProductReader[SeqLastIndexOfSlice[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqLastIndexOfSlice[_, _] = {
      require(arity == 0 && adj == 0)
      new SeqLastIndexOfSlice()
    }
  }
  final case class SeqLastIndexOfSlice[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Int] {
    def apply(a: Seq[A], that: Seq[B]): Int = a.lastIndexOfSlice(that)

    def name = "SeqLastIndexOfSlice"
  }

  object SeqPrepended extends ProductReader[SeqPrepended[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqPrepended[_, _] = {
      require(arity == 0 && adj == 0)
      new SeqPrepended()
    }
  }
  final case class SeqPrepended[A, B >: A]() extends NamedOp[Seq[A], B, Seq[B]] {
    def apply(a: Seq[A], b: B): Seq[B] = b +: a

    def name = "SeqPrepended"
  }

  object SeqSameElements extends ProductReader[SeqSameElements[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqSameElements[_, _] = {
      require(arity == 0 && adj == 0)
      new SeqSameElements()
    }
  }
  final case class SeqSameElements[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Boolean] {
    def apply(a: Seq[A], b: Seq[B]): Boolean = a.sameElements(b)

    def name = "SeqSameElements"
  }

  object SeqSplitAt extends ProductReader[SeqSplitAt[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqSplitAt[_] = {
      require(arity == 0 && adj == 0)
      new SeqSplitAt[Any]()
    }
  }
  final case class SeqSplitAt[A]() extends NamedOp[Seq[A], Int, (Seq[A], Seq[A])] {
    def apply(a: Seq[A], b: Int): (Seq[A], Seq[A]) = a.splitAt(b)

    def name = "SeqSplitAt"
  }

  object SeqTake extends ProductReader[SeqTake[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqTake[_] = {
      require(arity == 0 && adj == 0)
      new SeqTake[Any]()
    }
  }
  final case class SeqTake[A]() extends NamedOp[Seq[A], Int, Seq[A]] {
    def apply(a: Seq[A], b: Int): Seq[A] = a.take(b)

    def name = "SeqTake"
  }

  object SeqTakeRight extends ProductReader[SeqTakeRight[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqTakeRight[_] = {
      require(arity == 0 && adj == 0)
      new SeqTakeRight[Any]()
    }
  }
  final case class SeqTakeRight[A]() extends NamedOp[Seq[A], Int, Seq[A]] {
    def apply(a: Seq[A], b: Int): Seq[A] = a.takeRight(b)

    def name = "SeqTakeRight"
  }

  object SeqZip extends ProductReader[SeqZip[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqZip[_, _] = {
      require(arity == 0 && adj == 0)
      new SeqZip()
    }
  }
  final case class SeqZip[A, B]() extends NamedOp[Seq[A], Seq[B], Seq[(A, B)]] {
    def apply(a: Seq[A], b: Seq[B]): Seq[(A, B)] = a zip b

    def name = "SeqZip"
  }

  // ---- String ----

  object StringConcat extends ProductReader[StringConcat] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringConcat = {
      require(arity == 0 && adj == 0)
      new StringConcat()
    }
  }
  final case class StringConcat() extends NamedOp[String, String, String] {
    def apply(a: String, b: String): String = a + b

    def name = "StringConcat"
  }

  object StringContains extends ProductReader[StringContains] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringContains = {
      require(arity == 0 && adj == 0)
      new StringContains()
    }
  }
  final case class StringContains() extends NamedOp[String, String, Boolean] {
    def apply(a: String, b: String): Boolean = a.contains(b)

    def name = "StringContains"
  }

  object StringStartsWith extends ProductReader[StringStartsWith] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringStartsWith = {
      require(arity == 0 && adj == 0)
      new StringStartsWith()
    }
  }
  final case class StringStartsWith() extends NamedOp[String, String, Boolean] {
    def apply(a: String, b: String): Boolean = a.startsWith(b)

    def name = "StringStartsWith"
  }

  object StringEndsWith extends ProductReader[StringEndsWith] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringEndsWith = {
      require(arity == 0 && adj == 0)
      new StringEndsWith()
    }
  }
  final case class StringEndsWith() extends NamedOp[String, String, Boolean] {
    def apply(a: String, b: String): Boolean = a.endsWith(b)

    def name = "StringEndsWith"
  }

  object StringIndexOf extends ProductReader[StringIndexOf] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringIndexOf = {
      require(arity == 0 && adj == 0)
      new StringIndexOf()
    }
  }
  final case class StringIndexOf() extends NamedOp[String, String, Int] {
    def apply(a: String, b: String): Int = a.indexOf(b)

    def name = "StringIndexOf"
  }

  object StringLastIndexOf extends ProductReader[StringLastIndexOf] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringLastIndexOf = {
      require(arity == 0 && adj == 0)
      new StringLastIndexOf()
    }
  }
  final case class StringLastIndexOf() extends NamedOp[String, String, Int] {
    def apply(a: String, b: String): Int = a.lastIndexOf(b)

    def name = "StringLastIndexOf"
  }

  object StringTake extends ProductReader[StringTake] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringTake = {
      require(arity == 0 && adj == 0)
      new StringTake()
    }
  }
  final case class StringTake() extends NamedOp[String, Int, String] {
    def apply(a: String, b: Int): String = a.take(b)

    def name = "StringTake"
  }

  object StringDrop extends ProductReader[StringDrop] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringDrop = {
      require(arity == 0 && adj == 0)
      new StringDrop()
    }
  }
  final case class StringDrop() extends NamedOp[String, Int, String] {
    def apply(a: String, b: Int): String = a.drop(b)

    def name = "StringDrop"
  }

  // ---- SpanLike ----

  object SpanLikeClip extends ProductReader[SpanLikeClip] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeClip = {
      require(arity == 0 && adj == 0)
      new SpanLikeClip()
    }
  }
  final case class SpanLikeClip() extends NamedOp[SpanLike, Long, Long] {
    def apply(a: SpanLike, b: Long): Long = a.clip(b)

    def name = "SpanLikeClip"
  }

  object SpanLikeShift extends ProductReader[SpanLikeShift] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeShift = {
      require(arity == 0 && adj == 0)
      new SpanLikeShift()
    }
  }
  final case class SpanLikeShift() extends NamedOp[SpanLike, Long, SpanLike] {
    def apply(a: SpanLike, b: Long): SpanLike = a.shift(b)

    def name = "SpanLikeShift"
  }

  object SpanLikeContains extends ProductReader[SpanLikeContains] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeContains = {
      require(arity == 0 && adj == 0)
      new SpanLikeContains()
    }
  }
  final case class SpanLikeContains() extends NamedOp[SpanLike, Long, Boolean] {
    def apply(a: SpanLike, b: Long): Boolean = a.contains(b)

    def name = "SpanLikeContains"
  }

  object SpanLikeOverlaps extends ProductReader[SpanLikeOverlaps] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeOverlaps = {
      require(arity == 0 && adj == 0)
      new SpanLikeOverlaps()
    }
  }
  final case class SpanLikeOverlaps() extends NamedOp[SpanLike, SpanLike, Boolean] {
    def apply(a: SpanLike, b: SpanLike): Boolean = a.overlaps(b)

    def name = "SpanLikeOverlaps"
  }

  object SpanLikeTouches extends ProductReader[SpanLikeTouches] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeTouches = {
      require(arity == 0 && adj == 0)
      new SpanLikeTouches()
    }
  }
  final case class SpanLikeTouches() extends NamedOp[SpanLike, SpanLike, Boolean] {
    def apply(a: SpanLike, b: SpanLike): Boolean = a.touches(b)

    def name = "SpanLikeTouches"
  }

  object SpanLikeUnion extends ProductReader[SpanLikeUnion] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeUnion = {
      require(arity == 0 && adj == 0)
      new SpanLikeUnion()
    }
  }
  final case class SpanLikeUnion() extends NamedOp[SpanLike, SpanLike, SpanLike] {
    def apply(a: SpanLike, b: SpanLike): SpanLike = a.union(b)

    def name = "SpanLikeUnion"
  }

  object SpanLikeIntersect extends ProductReader[SpanLikeIntersect] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeIntersect = {
      require(arity == 0 && adj == 0)
      new SpanLikeIntersect()
    }
  }
  final case class SpanLikeIntersect() extends NamedOp[SpanLike, SpanLike, SpanLike] {
    def apply(a: SpanLike, b: SpanLike): SpanLike = a.intersect(b)

    def name = "SpanLikeIntersect"
  }

  // ---- URI (File) ----

  object FileReplaceExt extends ProductReader[FileReplaceExt] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FileReplaceExt = {
      require(arity == 0 && adj == 0)
      new FileReplaceExt()
    }
  }
  final case class FileReplaceExt() extends NamedOp[_URI, String, _URI] {
    def apply(a: _URI, s: String): _URI = new URIOps(a).replaceExt(s)

    def name = "FileReplaceExt"
  }

  object FileReplaceName extends ProductReader[FileReplaceName] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FileReplaceName = {
      require(arity == 0 && adj == 0)
      new FileReplaceName()
    }
  }
  final case class FileReplaceName() extends NamedOp[_URI, String, _URI] {
    def apply(a: _URI, s: String): _URI = new URIOps(a).replaceName(s)

    def name: String = "FileReplaceName"
  }

  object FileChild extends ProductReader[FileChild] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FileChild = {
      require(arity == 0 && adj == 0)
      new FileChild()
    }
  }
  final case class FileChild() extends NamedOp[_URI, String, _URI] {
    def apply(a: _URI, s: String): _URI = new URIOps(a)./(s)

    def name: String = "FileChild"
  }

  // ---- Impl ----

  private[lucre] object Expanded extends IExprFactory {
    final val typeId = 0x42696E4F // "BinO"

    override def readIdentified[T <: Txn[T]](in: DataInput)(implicit ctx: Context[T], tx: T): IExpr[T, Any] = {
      val serVer = in.readByte()
      require (serVer == 0)
      val _op = Op.read[Any, Any, Any](new RefMapIn(in))
      val _a  = IExpr.read[T, Any](in)
      val _b  = IExpr.read[T, Any](in)
      implicit val tgt: ITargets[T] = ITargets()
      new Expanded[T, Any, Any, Any, Any](_op, _a, _b)
    }
  }
  private[lucre] final class Expanded[T <: Txn[T], A1, A2, A3, A](op: BinaryOp.Op[A1, A2, A],
                                                                   a: IExpr[T, A1], b: IExpr[T, A2])
                                                                  (implicit protected val targets: ITargets[T])
    extends IExpr[T, A] with IChangeEventImpl[T, A] {

    override protected def typeId: Int = Expanded.typeId

    override protected def writeData(out: DataOutput): Unit = {
      out.writeByte(0)  // serialization version
      op.write(out)
      a.write(out)
      b.write(out)
    }

    def connect()(implicit tx: T): this.type = {
      a.changed.--->(this)
      b.changed.--->(this)
      this
    }

    override def toString: String = s"BinaryOp($op, $a, $b)"

    def changed: IChangeEvent[T, A] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
      val aV  = pull.expr(a)
      val bV  = pull.expr(b)
      value1(aV, bV)
    }

    @inline
    private def value1(av: A1, bv: A2): A = {
      op.apply(av, bv)
    }

    override def value(implicit context: Context[T], tx: T): A = {
      val av = a.value
      val bv = b.value
      value1(av, bv)
    }

    def dispose()(implicit tx: T): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
    }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): BinaryOp[_, _, _ , _] = {
    require (arity == 3 && adj == 0)
    val _op = in.readProductT[Op[Any, Any, _]]()
    val _a  = in.readEx[Any]()
    val _b  = in.readEx[Any]()
    new BinaryOp(_op, _a, _b)
  }
}
final case class BinaryOp[A1, A2, A3, A](op: BinaryOp.Op[A1, A2, A], a: Ex[A1], b: Ex[A2])
  extends Ex[A] {

  type Repr[T <: Txn[T]] = IExpr[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val ax = a.expand[T]
    val bx = b.expand[T]
    new BinaryOp.Expanded[T, A1, A2, A3, A](op, ax, bx).connect()
  }
}
