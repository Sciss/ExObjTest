/*
 *  UnaryOp.scala
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
import de.sciss.lucre.Adjunct.{HasDefault, Num, NumBool, NumFrac, NumInt, ScalarOrd, ToNum, Widen, WidenToDouble}
import de.sciss.lucre.exnew.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.exnew.graph.UnaryOp.Op
import de.sciss.lucre.exnew.graph.impl.MappedIExpr
import de.sciss.lucre.exnew.impl.IEventImpl
import de.sciss.lucre.{Adjunct, Exec, ProductWithAdjuncts, Txn}
import de.sciss.model.Change
import de.sciss.serial.DataOutput
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

object UnaryOp extends ProductReader[UnaryOp[_, _]] {
  abstract class Op[A1, A2] extends Product {
    def apply(a: A1): A2
  }

  abstract class NamedOp[A1, A2] extends Op[A1, A2] {
    override def productPrefix = s"UnaryOp$$$name"

    override def toString: String = name

    def name: String
  }

  type Adjuncts = scala.List[Adjunct]

  // ---- analogous to UGens ----

  object Neg extends ProductReader[Neg[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Neg[_] = {
      require (arity == 0 && adj == 1)
      val _num: Num[Any] = in.readAdjunct()
      new Neg[Any]()(_num)
    }
  }
  final case class Neg[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.negate(a)

    override def name = "Neg"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object Not extends ProductReader[Not[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Not[_] = {
      require (arity == 0 && adj == 1)
      val _num: NumBool[Any] = in.readAdjunct()
      new Not[Any]()(_num)
    }
  }
  final case class Not[A]()(implicit num: NumBool[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.negate(a)

    override def name = "Not"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object BitNot extends ProductReader[BitNot[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): BitNot[_] = {
      require (arity == 0 && adj == 1)
      val _num: NumInt[Any] = in.readAdjunct()
      new BitNot[Any]()(_num)
    }
  }
  final case class BitNot[A]()(implicit num: NumInt[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.not(a)

    override def name = "BitNot"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object Abs extends ProductReader[Abs[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Abs[_] = {
      require (arity == 0 && adj == 1)
      val _num: Num[Any] = in.readAdjunct()
      new Abs[Any]()(_num)
    }
  }
  final case class Abs[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.abs(a)

    override def name = "Abs"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object ToDouble extends ProductReader[ToDouble[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): ToDouble[_, _] = {
      require (arity == 0 && adj == 1)
      val _num: ToNum[Any] { type Double = Any } = in.readAdjunct()
      new ToDouble[Any, Any]()(_num)
    }
  }
  final case class ToDouble[A, B]()(implicit to: ToNum[A] { type Double = B }) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = to.toDouble(a)

    override def name = "ToDouble"

    override def adjuncts: Adjuncts = to :: Nil
  }

  object ToInt extends ProductReader[ToInt[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): ToInt[_, _] = {
      require (arity == 0 && adj == 1)
      val _num: ToNum[Any] { type Int = Any } = in.readAdjunct()
      new ToInt[Any, Any]()(_num)
    }
  }
  final case class ToInt[A, B]()(implicit to: ToNum[A] { type Int = B }) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = to.toInt(a)

    override def name = "ToInt"

    override def adjuncts: Adjuncts = to :: Nil
  }

  object ToLong extends ProductReader[ToLong[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): ToLong[_, _] = {
      require (arity == 0 && adj == 1)
      val _num: ToNum[Any] { type Long = Any } = in.readAdjunct()
      new ToLong[Any, Any]()(_num)
    }
  }
  final case class ToLong[A, B]()(implicit to: ToNum[A] { type Long = B }) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = to.toLong(a)

    override def name = "ToLong"

    override def adjuncts: Adjuncts = to :: Nil
  }

  object Ceil extends ProductReader[Ceil[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ceil[_] = {
      require (arity == 0 && adj == 1)
      val _num: NumFrac[Any] = in.readAdjunct()
      new Ceil[Any]()(_num)
    }
  }
  final case class Ceil[A]()(implicit num: NumFrac[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.ceil(a)

    override def name = "Ceil"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object Floor extends ProductReader[Floor[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Floor[_] = {
      require (arity == 0 && adj == 1)
      val _num: NumFrac[Any] = in.readAdjunct()
      new Floor[Any]()(_num)
    }
  }
  final case class Floor[A]()(implicit num: NumFrac[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.floor(a)

    override def name = "Floor"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object Frac extends ProductReader[Frac[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Frac[_] = {
      require (arity == 0 && adj == 1)
      val _num: NumFrac[Any] = in.readAdjunct()
      new Frac[Any]()(_num)
    }
  }
  final case class Frac[A]()(implicit num: NumFrac[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.frac(a)

    override def name = "Frac"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object Signum extends ProductReader[Signum[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Signum[_] = {
      require (arity == 0 && adj == 1)
      val _num: Num[Any] = in.readAdjunct()
      new Signum[Any]()(_num)
    }
  }
  final case class Signum[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.signum(a)

    override def name = "Signum"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object Squared extends ProductReader[Squared[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Squared[_] = {
      require (arity == 0 && adj == 1)
      val _num: Num[Any] = in.readAdjunct()
      new Squared[Any]()(_num)
    }
  }
  final case class Squared[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.squared(a)

    override def name = "Squared"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object Cubed extends ProductReader[Cubed[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Cubed[_] = {
      require (arity == 0 && adj == 1)
      val _num: Num[Any] = in.readAdjunct()
      new Cubed[Any]()(_num)
    }
  }
  final case class Cubed[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.cubed(a)

    override def name = "Cubed"

    override def adjuncts: Adjuncts = num :: Nil
  }

  object Sqrt extends ProductReader[Sqrt[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Sqrt[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Sqrt[Any, Any]()(_wd)
    }
  }
  final case class Sqrt[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.sqrt(wd.widen1(a))

    override def name = "Sqrt"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Exp extends ProductReader[Exp[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Exp[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Exp[Any, Any]()(_wd)
    }
  }
  final case class Exp[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.exp(wd.widen1(a))

    override def name = "Exp"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Reciprocal extends ProductReader[Reciprocal[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Reciprocal[_, _] = {
      require (arity == 0 && adj == 2)
      val _w  : Widen[Any, Any] = in.readAdjunct()
      val _num: NumFrac[Any]    = in.readAdjunct()
      new Reciprocal[Any, Any]()(_w, _num)
    }
  }
  final case class Reciprocal[A, B]()(implicit w: Widen[A, B], num: NumFrac[B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = num.reciprocal(w.widen1(a))

    override def name = "Reciprocal"

    override def adjuncts: Adjuncts = w :: num :: Nil
  }

  object Midicps extends ProductReader[Midicps[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Midicps[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Midicps[Any, Any]()(_wd)
    }
  }
  final case class Midicps[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.midiCps(wd.widen1(a))

    override def name = "Midicps"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Cpsmidi extends ProductReader[Cpsmidi[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Cpsmidi[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Cpsmidi[Any, Any]()(_wd)
    }
  }
  final case class Cpsmidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.cpsMidi(wd.widen1(a))

    override def name = "Cpsmidi"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Midiratio extends ProductReader[Midiratio[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Midiratio[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Midiratio[Any, Any]()(_wd)
    }
  }
  final case class Midiratio[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.midiRatio(wd.widen1(a))

    override def name = "Midiratio"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Ratiomidi extends ProductReader[Ratiomidi[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ratiomidi[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Ratiomidi[Any, Any]()(_wd)
    }
  }
  final case class Ratiomidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.ratioMidi(wd.widen1(a))

    override def name = "Ratiomidi"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Dbamp extends ProductReader[Dbamp[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Dbamp[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Dbamp[Any, Any]()(_wd)
    }
  }
  final case class Dbamp[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.dbAmp(wd.widen1(a))

    override def name = "Dbamp"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Ampdb extends ProductReader[Ampdb[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ampdb[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Ampdb[Any, Any]()(_wd)
    }
  }
  final case class Ampdb[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.ampDb(wd.widen1(a))

    override def name = "Ampdb"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Octcps extends ProductReader[Octcps[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Octcps[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Octcps[Any, Any]()(_wd)
    }
  }
  final case class Octcps[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.octCps(wd.widen1(a))

    override def name = "Octcps"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Cpsoct extends ProductReader[Cpsoct[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Cpsoct[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Cpsoct[Any, Any]()(_wd)
    }
  }
  final case class Cpsoct[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.cpsOct(wd.widen1(a))

    override def name = "Cpsoct"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Log extends ProductReader[Log[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Log[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Log[Any, Any]()(_wd)
    }
  }
  final case class Log[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.log(wd.widen1(a))

    override def name = "Log"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Log2 extends ProductReader[Log2[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Log2[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Log2[Any, Any]()(_wd)
    }
  }
  final case class Log2[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.log2(wd.widen1(a))

    override def name = "Log2"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Log10 extends ProductReader[Log10[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Log10[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Log10[Any, Any]()(_wd)
    }
  }
  final case class Log10[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.log10(wd.widen1(a))

    override def name = "Log10"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Sin extends ProductReader[Sin[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Sin[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Sin[Any, Any]()(_wd)
    }
  }
  final case class Sin[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.sin(wd.widen1(a))

    override def name = "Sin"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Cos extends ProductReader[Cos[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Cos[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Cos[Any, Any]()(_wd)
    }
  }
  final case class Cos[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.cos(wd.widen1(a))

    override def name = "Cos"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Tan extends ProductReader[Tan[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Tan[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Tan[Any, Any]()(_wd)
    }
  }
  final case class Tan[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.tan(wd.widen1(a))

    override def name = "Tan"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Asin extends ProductReader[Asin[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Asin[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Asin[Any, Any]()(_wd)
    }
  }
  final case class Asin[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.asin(wd.widen1(a))

    override def name = "Asin"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Acos extends ProductReader[Acos[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Acos[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Acos[Any, Any]()(_wd)
    }
  }
  final case class Acos[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.acos(wd.widen1(a))

    override def name = "Acos"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Atan extends ProductReader[Atan[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Atan[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Atan[Any, Any]()(_wd)
    }
  }
  final case class Atan[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.atan(wd.widen1(a))

    override def name = "Atan"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Sinh extends ProductReader[Sinh[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Sinh[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Sinh[Any, Any]()(_wd)
    }
  }
  final case class Sinh[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.sinh(wd.widen1(a))

    override def name = "Sinh"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Cosh extends ProductReader[Cosh[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Cosh[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Cosh[Any, Any]()(_wd)
    }
  }
  final case class Cosh[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.cosh(wd.widen1(a))

    override def name = "Cosh"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  object Tanh extends ProductReader[Tanh[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Tanh[_, _] = {
      require (arity == 0 && adj == 1)
      val _wd: WidenToDouble[Any, Any] = in.readAdjunct()
      new Tanh[Any, Any]()(_wd)
    }
  }
  final case class Tanh[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.tanh(wd.widen1(a))

    override def name = "Tanh"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  //  final case class Rand[A]()(implicit num: Num[A])

  //  final case class Rand2

  // XXX TODO:
  // Linrand
  // Bilinrand
  // Sum3rand

  // Distort
  // Softclip

  //  final case class Coin[A, B]()(implicit num: NumDouble[A] { type Boolean = B })

  // RectWindow
  // HanWindow
  // WelWindow
  // TriWindow

  // Ramp
  // Scurve

  // ---- general ----

  object ToStr extends ProductReader[ToStr[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): ToStr[_] = {
      require (arity == 0 && adj == 0)
      new ToStr()
    }
  }
  final case class ToStr[A]() extends NamedOp[A, String] {
    def apply(a: A) : String = a.toString

    override def name = "ToStr"
  }

  // ---- Option ----

  object OptionSome extends ProductReader[OptionSome[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): OptionSome[_] = {
      require (arity == 0 && adj == 0)
      new OptionSome()
    }
  }
  final case class OptionSome[A]() extends NamedOp[A, Option[A]] {
    def apply(a: A): Option[A] = Some(a)

    override def name = "OptionSome"
  }

  object OptionIsEmpty extends ProductReader[OptionIsEmpty[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): OptionIsEmpty[_] = {
      require (arity == 0 && adj == 0)
      new OptionIsEmpty()
    }
  }
  final case class OptionIsEmpty[A]() extends NamedOp[Option[A], Boolean] {
    def apply(a: Option[A]): Boolean = a.isEmpty

    override def name = "OptionIsEmpty"
  }

  object OptionIsDefined extends ProductReader[OptionIsDefined[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): OptionIsDefined[_] = {
      require (arity == 0 && adj == 0)
      new OptionIsDefined()
    }
  }
  final case class OptionIsDefined[A]() extends NamedOp[Option[A], Boolean] {
    def apply(a: Option[A]): Boolean = a.isDefined

    override def name = "OptionIsDefined"
  }

  object OptionToList extends ProductReader[OptionToList[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): OptionToList[_] = {
      require (arity == 0 && adj == 0)
      new OptionToList()
    }
  }

  // XXX TODO why did we decide to have this specific type?
  final case class OptionToList[A]() extends NamedOp[Option[A], scala.List[A]] {
    def apply(a: Option[A]): scala.List[A] = a.toList

    override def name = "OptionToList"
  }

  // XXX TODO are we sure that this is the way to go
  // and that we do not want to add an exception throwing mechanism to Ex?
  object OptionGet extends ProductReader[OptionGet[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): OptionGet[_] = {
      require (arity == 0 && adj == 1)
      val _d: HasDefault[Any] = in.readAdjunct()
      new OptionGet[Any]()(_d)
    }
  }
  final case class OptionGet[A]()(implicit d: HasDefault[A])
    extends NamedOp[Option[A], A] with ProductWithAdjuncts {

    def apply(a: Option[A]): A = a.getOrElse(d.defaultValue)

    override def name = "OptionGet"

    override def adjuncts: Adjuncts = d :: Nil
  }

  // ---- Tuple2 ----

  object Tuple2_1 extends ProductReader[Tuple2_1[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Tuple2_1[_, _] = {
      require (arity == 0 && adj == 0)
      new Tuple2_1[Any, Any]()
    }
  }
  final case class Tuple2_1[A, B]() extends NamedOp[(A, B), A] {
    def apply(a: (A, B)): A = a._1

    override def name = "Tuple2_1"
  }

  object Tuple2_2 extends ProductReader[Tuple2_2[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Tuple2_2[_, _] = {
      require (arity == 0 && adj == 0)
      new Tuple2_2[Any, Any]()
    }
  }
  final case class Tuple2_2[A, B]() extends NamedOp[(A, B), B] {
    def apply(a: (A, B)): B = a._2

    override def name = "Tuple2_2"
  }

  object Tuple2Swap extends ProductReader[Tuple2Swap[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Tuple2Swap[_, _] = {
      require (arity == 0 && adj == 0)
      new Tuple2Swap[Any, Any]()
    }
  }
  final case class Tuple2Swap[A, B]() extends NamedOp[(A, B), (B, A)] {
    def apply(a: (A, B)): (B, A) = a.swap

    override def name = "Tuple2Swap"
  }

  // ---- Seq ----

  object SeqDistinct extends ProductReader[SeqDistinct[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqDistinct[_] = {
      require (arity == 0 && adj == 0)
      new SeqDistinct[Any]()
    }
  }
  final case class SeqDistinct[A]() extends NamedOp[Seq[A], Seq[A]] {
    def apply(a: Seq[A]): Seq[A] = a.distinct

    override def name = "SeqDistinct"
  }

  object SeqHeadOption extends ProductReader[SeqHeadOption[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqHeadOption[_] = {
      require (arity == 0 && adj == 0)
      new SeqHeadOption[Any]()
    }
  }
  final case class SeqHeadOption[A]() extends NamedOp[Seq[A], Option[A]] {
    def apply(a: Seq[A]): Option[A] = a.headOption

    override def name = "SeqHeadOption"
  }

  object SeqIndices extends ProductReader[SeqIndices[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqIndices[_] = {
      require (arity == 0 && adj == 0)
      new SeqIndices[Any]()
    }
  }
  final case class SeqIndices[A]() extends NamedOp[Seq[A], Seq[Int]] {
    def apply(a: Seq[A]): Seq[Int] = a.indices

    override def name = "SeqIndices"
  }

  object SeqIsEmpty extends ProductReader[SeqIsEmpty[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqIsEmpty[_] = {
      require (arity == 0 && adj == 0)
      new SeqIsEmpty[Any]()
    }
  }
  final case class SeqIsEmpty[A]() extends NamedOp[Seq[A], Boolean] {
    def apply(a: Seq[A]): Boolean = a.isEmpty

    override def name = "SeqIsEmpty"
  }

  object SeqLastOption extends ProductReader[SeqLastOption[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqLastOption[_] = {
      require (arity == 0 && adj == 0)
      new SeqLastOption[Any]()
    }
  }
  final case class SeqLastOption[A]() extends NamedOp[Seq[A], Option[A]] {
    def apply(a: Seq[A]): Option[A] = a.lastOption

    override def name = "SeqLastOption"
  }

  object SeqMaxOption extends ProductReader[SeqMaxOption[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqMaxOption[_] = {
      require (arity == 0 && adj == 1)
      val _ord: ScalarOrd[Any] = in.readAdjunct()
      new SeqMaxOption[Any]()(_ord)
    }
  }
  final case class SeqMaxOption[A]()(implicit ord: ScalarOrd[A])
    extends NamedOp[Seq[A], Option[A]] with ProductWithAdjuncts {

    def apply(a: Seq[A]): Option[A] = if (a.isEmpty) None else Some(a.max(Ordering.fromLessThan[A](ord.lt)))

    override def name = "SeqMaxOption"

    def adjuncts: List[Adjunct] = ord :: Nil
  }

  object SeqMinOption extends ProductReader[SeqMinOption[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqMinOption[_] = {
      require (arity == 0 && adj == 1)
      val _ord: ScalarOrd[Any] = in.readAdjunct()
      new SeqMinOption[Any]()(_ord)
    }
  }
  final case class SeqMinOption[A]()(implicit ord: ScalarOrd[A])
    extends NamedOp[Seq[A], Option[A]] with ProductWithAdjuncts {

    def apply(a: Seq[A]): Option[A] = if (a.isEmpty) None else Some(a.min(Ordering.fromLessThan[A](ord.lt)))

    override def name = "SeqMinOption"

    def adjuncts: List[Adjunct] = ord :: Nil
  }

  object SeqNonEmpty extends ProductReader[SeqNonEmpty[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqNonEmpty[_] = {
      require (arity == 0 && adj == 0)
      new SeqNonEmpty[Any]()
    }
  }
  final case class SeqNonEmpty[A]() extends NamedOp[Seq[A], Boolean] {
    def apply(a: Seq[A]): Boolean = a.nonEmpty

    override def name = "SeqNonEmpty"
  }

  object SeqPermutations extends ProductReader[SeqPermutations[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqPermutations[_] = {
      require (arity == 0 && adj == 0)
      new SeqPermutations[Any]()
    }
  }
  final case class SeqPermutations[A]() extends NamedOp[Seq[A], Seq[Seq[A]]] {
    def apply(a: Seq[A]): Seq[Seq[A]] = a.permutations.toIndexedSeq

    override def name = "SeqPermutations"
  }

  object SeqProduct extends ProductReader[SeqProduct[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqProduct[_] = {
      require (arity == 0 && adj == 1)
      val _num: Num[Any] = in.readAdjunct()
      new SeqProduct[Any]()(_num)
    }
  }
  final case class SeqProduct[A]()(implicit num: Num[A]) extends NamedOp[Seq[A], A] with ProductWithAdjuncts {
    def apply(a: Seq[A]): A = a.foldLeft(num.one)(num.times)

    override def name = "SeqProduct"

    def adjuncts: List[Adjunct] = num :: Nil
  }

  object SeqReverse extends ProductReader[SeqReverse[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqReverse[_] = {
      require (arity == 0 && adj == 0)
      new SeqReverse[Any]()
    }
  }
  final case class SeqReverse[A]() extends NamedOp[Seq[A], Seq[A]] {
    def apply(a: Seq[A]): Seq[A] = a.reverse

    override def name = "SeqReverse"
  }

  object SeqSize extends ProductReader[SeqSize[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqSize[_] = {
      require (arity == 0 && adj == 0)
      new SeqSize[Any]()
    }
  }
  final case class SeqSize[A]() extends NamedOp[Seq[A], Int] {
    def apply(a: Seq[A]): Int = a.size

    override def name = "SeqSize"
  }

  object SeqSorted extends ProductReader[SeqSorted[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqSorted[_] = {
      require (arity == 0 && adj == 1)
      val _ord: ScalarOrd[Any] = in.readAdjunct()
      new SeqSorted[Any]()(_ord)
    }
  }
  final case class SeqSorted[A]()(implicit ord: ScalarOrd[A])
    extends NamedOp[Seq[A], Seq[A]] with ProductWithAdjuncts {

    def apply(a: Seq[A]): Seq[A] = a.sorted(Ordering.fromLessThan(ord.lt))

    override def name = "SeqSorted"

    def adjuncts: List[Adjunct] = ord :: Nil
  }

  object SeqSum extends ProductReader[SeqSum[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqSum[_] = {
      require (arity == 0 && adj == 1)
      val _num: Num[Any] = in.readAdjunct()
      new SeqSum[Any]()(_num)
    }
  }
  final case class SeqSum[A]()(implicit num: Num[A]) extends NamedOp[Seq[A], A] with ProductWithAdjuncts {
    def apply(a: Seq[A]): A = a.foldLeft(num.zero)(num.plus)

    override def name = "SeqSum"

    def adjuncts: List[Adjunct] = num :: Nil
  }

  object SeqZipWithIndex extends ProductReader[SeqZipWithIndex[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqZipWithIndex[_] = {
      require (arity == 0 && adj == 0)
      new SeqZipWithIndex[Any]()
    }
  }
  final case class SeqZipWithIndex[A]() extends NamedOp[Seq[A], Seq[(A, Int)]] {
    def apply(a: Seq[A]): Seq[(A, Int)] = a.zipWithIndex

    override def name = "SeqZipWithIndex"
  }

  object SeqIntegrate extends ProductReader[SeqIntegrate[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqIntegrate[_] = {
      require (arity == 0 && adj == 1)
      val _num: Num[Any] = in.readAdjunct()
      new SeqIntegrate[Any]()(_num)
    }
  }
  final case class SeqIntegrate[A]()(implicit num: Num[A]) extends NamedOp[Seq[A], Seq[A]] with ProductWithAdjuncts {
    def apply(a: Seq[A]): Seq[A] =
      a.iterator.scanLeft(num.zero)(num.plus).drop(1).toSeq

    override def name = "SeqIntegrate"

    def adjuncts: List[Adjunct] = num :: Nil
  }

  object SeqDifferentiate extends ProductReader[SeqDifferentiate[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SeqDifferentiate[_] = {
      require (arity == 0 && adj == 1)
      val _num: Num[Any] = in.readAdjunct()
      new SeqDifferentiate[Any]()(_num)
    }
  }
  final case class SeqDifferentiate[A]()(implicit num: Num[A]) extends NamedOp[Seq[A], Seq[A]] with ProductWithAdjuncts {
    def apply(a: Seq[A]): Seq[A] =
      a.sliding(2, 1).collect { case Seq(a, b) => num.minus(b, a) } .toSeq

    override def name = "SeqDifferentiate"

    def adjuncts: List[Adjunct] = num :: Nil
  }

  // ---- String ----

  object StringIsEmpty extends ProductReader[StringIsEmpty] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringIsEmpty = {
      require (arity == 0 && adj == 0)
      new StringIsEmpty()
    }
  }
  final case class StringIsEmpty() extends NamedOp[String, Boolean] {
    def apply(a: String): Boolean = a.isEmpty

    override def name = "StringIsEmpty"
  }

  object StringNonEmpty extends ProductReader[StringNonEmpty] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringNonEmpty = {
      require (arity == 0 && adj == 0)
      new StringNonEmpty()
    }
  }
  final case class StringNonEmpty() extends NamedOp[String, Boolean] {
    def apply(a: String): Boolean = a.nonEmpty
    override def name = "StringNonEmpty"
  }

  object StringLength extends ProductReader[StringLength] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringLength = {
      require (arity == 0 && adj == 0)
      new StringLength()
    }
  }
  final case class StringLength() extends NamedOp[String, Int] {
    def apply(a: String): Int = a.length

    override def name = "StringLength"
  }

  object StringToIntOption extends ProductReader[StringToIntOption] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringToIntOption = {
      require (arity == 0 && adj == 0)
      new StringToIntOption()
    }
  }
  final case class StringToIntOption() extends NamedOp[String, Option[Int]] {
    def apply(a: String): Option[Int] = {
      // Scala 2.13+ only: a.toIntOption
      try {
        val v = java.lang.Integer.parseInt(a)
        Some(v)
      } catch {
        case _: NumberFormatException => None
      }
    }

    override def name = "StringToIntOption"
  }

  object StringToDoubleOption extends ProductReader[StringToDoubleOption] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringToDoubleOption = {
      require (arity == 0 && adj == 0)
      new StringToDoubleOption()
    }
  }
  final case class StringToDoubleOption() extends NamedOp[String, Option[Double]] {
    def apply(a: String): Option[Double] = {
      // Scala 2.13+ only: a.toDoubleOption
      try {
        val v = java.lang.Double.parseDouble(a)
        Some(v)
      } catch {
        case _: NumberFormatException => None
      }
    }

    override def name = "StringToDoubleOption"
  }

  object StringToBooleanOption extends ProductReader[StringToBooleanOption] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): StringToBooleanOption = {
      require (arity == 0 && adj == 0)
      new StringToBooleanOption()
    }
  }
  final case class StringToBooleanOption() extends NamedOp[String, Option[Boolean]] {
    def apply(a: String): Option[Boolean] = {
      // Scala 2.13+ only: a.toBooleanOption
      if      (a.equalsIgnoreCase("true"  )) Some(true  )
      else if (a.equalsIgnoreCase("false" )) Some(false )
      else None
    }

    override def name = "StringToBooleanOption"
  }

  // ---- SpanLike ----

  object SpanLikeIsEmpty extends ProductReader[SpanLikeIsEmpty] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeIsEmpty = {
      require (arity == 0 && adj == 0)
      new SpanLikeIsEmpty()
    }
  }
  final case class SpanLikeIsEmpty() extends NamedOp[_SpanLike, Boolean] {
    def apply(a: _SpanLike): Boolean = a.isEmpty

    override def name = "SpanLikeIsEmpty"
  }

  object SpanLikeNonEmpty extends ProductReader[SpanLikeNonEmpty] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeNonEmpty = {
      require (arity == 0 && adj == 0)
      new SpanLikeNonEmpty()
    }
  }
  final case class SpanLikeNonEmpty() extends NamedOp[_SpanLike, Boolean] {
    def apply(a: _SpanLike): Boolean = a.nonEmpty

    override def name = "SpanLikeNonEmpty"
  }

  object SpanLikeClosedOption extends ProductReader[SpanLikeClosedOption] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeClosedOption = {
      require (arity == 0 && adj == 0)
      new SpanLikeClosedOption()
    }
  }
  final case class SpanLikeClosedOption() extends NamedOp[_SpanLike, Option[_Span]] {
    def apply(a: _SpanLike): Option[_Span] = a match {
      case sp: _Span  => Some(sp)
      case _          => None
    }

    override def name = "SpanLikeClosedOption"
  }

  object SpanLikeStartOption extends ProductReader[SpanLikeStartOption] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeStartOption = {
      require (arity == 0 && adj == 0)
      new SpanLikeStartOption()
    }
  }
  final case class SpanLikeStartOption() extends NamedOp[_SpanLike, Option[Long]] {
    def apply(a: _SpanLike): Option[Long] = a.startOption

    override def name = "SpanLikeStartOption"
  }

  object SpanLikeStopOption extends ProductReader[SpanLikeStopOption] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeStopOption = {
      require (arity == 0 && adj == 0)
      new SpanLikeStopOption()
    }
  }
  final case class SpanLikeStopOption() extends NamedOp[_SpanLike, Option[Long]] {
    def apply(a: _SpanLike): Option[Long] = a.stopOption

    override def name = "SpanLikeStopOption"
  }

  object SpanLikeLengthOption extends ProductReader[SpanLikeLengthOption] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLikeLengthOption = {
      require (arity == 0 && adj == 0)
      new SpanLikeLengthOption()
    }
  }
  final case class SpanLikeLengthOption() extends NamedOp[_SpanLike, Option[Long]] {
    def apply(a: _SpanLike): Option[Long] = a match {
      case sp: _Span.SpanOrVoid => Some(sp.length)
      case _                    => None
    }

    override def name = "SpanLikeLengthOption"
  }

  // ---- Span ----

  object SpanStart extends ProductReader[SpanStart] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanStart = {
      require (arity == 0 && adj == 0)
      new SpanStart()
    }
  }
  final case class SpanStart() extends NamedOp[_Span, Long] {
    def apply(a: _Span): Long = a.start

    override def name = "SpanStart"
  }

  object SpanStop extends ProductReader[SpanStop] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanStop = {
      require (arity == 0 && adj == 0)
      new SpanStop()
    }
  }
  final case class SpanStop() extends NamedOp[_Span, Long] {
    def apply(a: _Span): Long = a.stop

    override def name = "SpanStop"
  }

  object SpanLength extends ProductReader[SpanLength] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SpanLength = {
      require (arity == 0 && adj == 0)
      new SpanLength()
    }
  }
  final case class SpanLength() extends NamedOp[_Span, Long] {
    def apply(a: _Span): Long = a.length

    override def name = "SpanLength"
  }

  // ---- URI (File) ----

  object FileParentOption extends ProductReader[FileParentOption] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FileParentOption = {
      require (arity == 0 && adj == 0)
      new FileParentOption()
    }
  }
  final case class FileParentOption() extends NamedOp[_URI, Option[_URI]] {
    def apply(a: _URI): Option[_URI] = new URIOps(a).parentOption

    def name = "FileParentOption"
  }

  object FilePath extends ProductReader[FilePath] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FilePath = {
      require (arity == 0 && adj == 0)
      new FilePath()
    }
  }
  final case class FilePath() extends NamedOp[_URI, String] {
    def apply(a: _URI): String = new URIOps(a).path

    def name = "FilePath"
  }

  object FileName extends ProductReader[FileName] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FileName = {
      require (arity == 0 && adj == 0)
      new FileName()
    }
  }
  final case class FileName() extends NamedOp[_URI, String] {
    def apply(a: _URI): String = new URIOps(a).name

    def name = "FileName"
  }

  object FileBase extends ProductReader[FileBase] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FileBase = {
      require (arity == 0 && adj == 0)
      new FileBase()
    }
  }
  final case class FileBase() extends NamedOp[_URI, String] {
    def apply(a: _URI): String = new URIOps(a).base

    def name = "FileBase"
  }

  object FileExtL extends ProductReader[FileExtL] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FileExtL = {
      require (arity == 0 && adj == 0)
      new FileExtL()
    }
  }
  final case class FileExtL() extends NamedOp[_URI, String] {
    def apply(a: _URI): String = new URIOps(a).extL

    def name = "FileExtL"
  }

  // ---- Impl ----

  private[lucre] final class Expanded[T <: Exec[T], A1, A](op: Op[A1, A], a: IExpr[T, A1])
                                                          (implicit targets: ITargets[T])
    extends MappedIExpr[T, A1, A](a) with IEventImpl[T, Change[A]] {

    override protected def typeId: Int = ???

    override protected def writeData(out: DataOutput): Unit = {
      out.writeByte(0)  // serialization version
      ??? // op.write(out)
      a.write(out)
    }

    override def toString: String = s"UnaryOp($op, $a)"

    protected def mapValue(av: A1)(implicit tx: T): A = op(av)
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): UnaryOp[_ , _] = {
    require (arity == 2 && adj == 0)
    val _op = in.readProductT[Op[Any, _]]()
    val _a  = in.readEx[Any]()
    new UnaryOp(_op, _a)
  }

  // XXX TODO: let's do this at another point when `Const` is no longer `Lazy`
  //  def apply1[A1, A](op: Op[A1, A], a: Ex[A1]): Ex[A] = a match {
  //    case Const(av)  => Const(op.apply(av))
  //    case _          => UnaryOp[A1, A](op, a)
  //  }
}

final case class UnaryOp[A1, A](op: Op[A1, A], a: Ex[A1])
  extends Ex[A] {

  type Repr[T <: Txn[T]] = IExpr[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val ax = a.expand[T]
    new UnaryOp.Expanded[T, A1, A](op, ax).connect()
  }
}
