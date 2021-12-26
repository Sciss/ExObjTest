/*
 *  Ex.scala
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

package de.sciss.lucre.exnew.graph

import de.sciss.equal.Implicits._
import de.sciss.lucre.Adjunct.{FromAny, HasDefault, Ord, Scalar, ScalarOrd}
import de.sciss.lucre.exnew.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.exnew.graph.impl.{ExpandedFlatMapOption, ExpandedFlatMapSeq, ExpandedFlatMapSeqOption, ExpandedMapOption, ExpandedMapSeq}
import de.sciss.lucre.exnew.{Context, ExOps, ExOptionOps, ExSeq, ExSeqOps, ExStringOps, Graph, IExpr}
import de.sciss.lucre.{Adjunct, Txn}
import de.sciss.lucre.{Artifact => _Artifact}
import de.sciss.serial.DataInput
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

import java.net.{URI => _URI}
import scala.language.implicitConversions

object Ex /*extends ExPlatform*/ {
  // ---- implicits ----

  implicit def const[A: Value](x: A): Ex[A] = Const(x)

  // ---- extractors ----

  // This doesn't work, you can only get
  // `zipped.map { case Ex(a, b) => ... }`
  // So let's leave that until we know this can be
  // expanded to multiple arities

  //  def unapply[A, B](in: Ex[(A, B)]): Option[(Ex[A], Ex[B])] = Some((in._1, in._2))

  // ----

  object Value {
    implicit object anyVal      extends Value[AnyVal    ]
    implicit object string      extends Value[String    ]
    implicit object spanLike    extends Value[_SpanLike ]
    //    implicit object act       extends Value[Act       ]
    implicit object fileIsValue extends Value[_URI      ]

    implicit def tuple2 [A: Value, B: Value]: Value[(A, B)] = null

    implicit def option [A: Value]: Value[Option[A]] = null
    implicit def seq    [A: Value]: Value[Seq   [A]] = null
  }
  trait Value[-A]

  // ---- implicit lifting and ops ----

  implicit def liftOptionEx[A](x: Option[Ex[A]]): Ex[Option[A]] = x match {
    case Some(ex) => UnaryOp(UnaryOp.OptionSome[A](), ex)
    case None     => Const(Option.empty[A])
  }

  implicit def liftSeqEx[A](x: Seq[Ex[A]]): Ex[Seq[A]] =
    if (x.isEmpty) Const(Nil) else ExSeq(x: _*) // immutable(x): _*)

  implicit def ops      [A]   (x: Ex[A])        : ExOps       [A]     = new ExOps       (x)
  implicit def seqOps   [A]   (x: Ex[Seq  [A]]) : ExSeqOps    [A]     = new ExSeqOps    (x)
  implicit def optionOps[A]   (x: Ex[Option[A]]): ExOptionOps [A]     = new ExOptionOps (x)
// EEE
//  implicit def booleanOps     (x: Ex[Boolean])  : ExBooleanOps        = new ExBooleanOps(x)
  implicit def stringOps      (x: Ex[String])   : ExStringOps         = new ExStringOps (x)

  //  implicit def fileOps(x: Ex[_URI]): ExFileOps = new ExFileOps(x)

  //////////////////////////////

  private val anyCanMapOption         = new CanMapOption        [Any]
  private val anyCanMapSeq            = new CanMapSeq           [Any]
  private val anyCanFlatMapOption     = new CanFlatMapOption    [Any]
  private val anyCanFlatMapSeq        = new CanFlatMapSeq       [Any]
  private val anyCanFlatMapSeqOption  = new CanFlatMapSeqOption [Any]

  private lazy val _init: Unit = {
    Adjunct.addFactory(anyCanMapOption        )
    Adjunct.addFactory(anyCanMapSeq           )
    Adjunct.addFactory(anyCanFlatMapOption    )
    Adjunct.addFactory(anyCanFlatMapSeq       )
    Adjunct.addFactory(anyCanFlatMapSeqOption )
  }

  def init(): Unit = {
    _init
    //    _initPlatform
  }

  object MapExOption extends ProductReader[MapExOption[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): MapExOption[_, _] = {
      require (arity == 3 && adj == 0)
      val _in   = in.readEx[Option[Any]]()
      val _it   = in.readProductT[It[Any]]()
      val _fun  = in.readEx[Any]()
      new MapExOption(_in, _it, _fun)
    }
  }
  final case class MapExOption[A, B] private[Ex] (in: Ex[Option[A]], it: It[A], fun: Ex[B])
    extends Ex[Option[B]] {

    type Repr[T <: Txn[T]] = IExpr[T, Option[B]]

    override def productPrefix: String = s"Ex$$MapExOption" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx  = in.expand[T]
      val itEx  = it.expand[T]
      import ctx.targets
      new ExpandedMapOption[T, A, B](inEx, itEx, fun, tx)
    }
  }

  object MapExSeq extends ProductReader[MapExSeq[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): MapExSeq[_, _] = {
      require (arity == 3 && adj == 0)
      val _in   = in.readEx[Seq[Any]]()
      val _it   = in.readProductT[It[Any]]()
      val _fun  = in.readEx[Any]()
      new MapExSeq(_in, _it, _fun)
    }
  }
  final case class MapExSeq[A, B] private[Ex] (in: Ex[Seq[A]], it: It[A], fun: Ex[B])
    extends Ex[Seq[B]] {

    type Repr[T <: Txn[T]] = IExpr[T, Seq[B]]

    override def productPrefix: String = s"Ex$$MapExSeq" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx  = in.expand[T]
      val itEx  = it.expand[T]
      import ctx.targets
      new ExpandedMapSeq[T, A, B](inEx, itEx, fun, tx)
    }
  }

  object FlatMapExOption extends ProductReader[FlatMapExOption[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FlatMapExOption[_, _] = {
      require (arity == 3 && adj == 0)
      val _in   = in.readEx[Option[Any]]()
      val _it   = in.readProductT[It[Any]]()
      val _fun  = in.readEx[Option[Any]]()
      new FlatMapExOption(_in, _it, _fun)
    }
  }
  final case class FlatMapExOption[A, B] private[Ex] (in: Ex[Option[A]], it: It[A], fun: Ex[Option[B]])
    extends Ex[Option[B]] {

    type Repr[T <: Txn[T]] = IExpr[T, Option[B]]

    override def productPrefix: String = s"Ex$$FlatMapExOption" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new ExpandedFlatMapOption[T, A, B](inEx, itEx, fun, tx)
    }
  }

  object FlatMapExSeq extends ProductReader[FlatMapExSeq[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FlatMapExSeq[_, _] = {
      require (arity == 3 && adj == 0)
      val _in   = in.readEx[Seq[Any]]()
      val _it   = in.readProductT[It[Any]]()
      val _fun  = in.readEx[Seq[Any]]()
      new FlatMapExSeq(_in, _it, _fun)
    }
  }
  final case class FlatMapExSeq[A, B] private[Ex] (in: Ex[Seq[A]], it: It[A], fun: Ex[Seq[B]])
    extends Ex[Seq[B]] {

    type Repr[T <: Txn[T]] = IExpr[T, Seq[B]]

    override def productPrefix: String = s"Ex$$FlatMapExSeq" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx  = in  .expand[T]
      val itEx  = it  .expand[T]
      import ctx.targets
      new ExpandedFlatMapSeq[T, A, B](inEx, itEx, fun, tx)
    }
  }

  object FlatMapExSeqOption extends ProductReader[FlatMapExSeqOption[_, _]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FlatMapExSeqOption[_, _] = {
      require (arity == 3 && adj == 0)
      val _in   = in.readEx[Seq[Any]]()
      val _it   = in.readProductT[It[Any]]()
      val _fun  = in.readEx[Option[Any]]()
      new FlatMapExSeqOption(_in, _it, _fun)
    }
  }
  final case class FlatMapExSeqOption[A, B] private[Ex] (in: Ex[Seq[A]], it: It[A], fun: Ex[Option[B]])
    extends Ex[Seq[B]] {

    type Repr[T <: Txn[T]] = IExpr[T, Seq[B]]

    override def productPrefix: String = s"Ex$$FlatMapExSeqOption" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx  = in.expand[T]
      val itEx  = it.expand[T]
      import ctx.targets
      new ExpandedFlatMapSeqOption[T, A, B](inEx, itEx, fun, tx)
    }
  }

  object CanMap {
    /** One can map over an option expression */
    implicit def option[B]  : CanMap[Option , Ex[B] , Ex[Option[B]]]  = anyCanMapOption .asInstanceOf[CanMapOption  [B]]
    /** One can map over a sequence expression */
    implicit def seq   [B]  : CanMap[Seq    , Ex[B] , Ex[Seq   [B]]]  = anyCanMapSeq    .asInstanceOf[CanMapSeq     [B]]
  }
  trait CanMap[-From[_], -B, +To] extends Adjunct {
    def map[A](from: Ex[From[A]], fun: Ex[A] => B): To
  }

  object CanFlatMap {
    implicit def option   [B] : CanFlatMap[Option , Ex[Option [B]], Ex[Option [B]]] = anyCanFlatMapOption.asInstanceOf[CanFlatMapOption [B]]
    implicit def seq      [B] : CanFlatMap[Seq    , Ex[Seq    [B]], Ex[Seq    [B]]] = anyCanFlatMapSeq   .asInstanceOf[CanFlatMapSeq    [B]]
    implicit def seqOption[B] : CanFlatMap[Seq    , Ex[Option [B]], Ex[Seq    [B]]] = anyCanFlatMapSeqOption.asInstanceOf[CanFlatMapSeqOption[B]]
  }
  trait CanFlatMap[-From[_], -B, +To] {
    def flatMap[A](from: Ex[From[A]], fun: Ex[A] => B): To
  }

  // --------------------- impl ---------------------

  private[exnew] def mkClosure[A, B](fun: Ex[A] => B): (It[A], B) = {
    val b     = Graph.builder
    val it    = b.allocToken[A]()
    //    val (c, r) = Graph.withResult {
    //      fun(it)
    //    }
    //    (it, c, r)
    val r = fun(it)
    (it, r)
  }

  // common to all type classes
  private abstract class MapSupport extends Adjunct with Adjunct.Factory {
    def readIdentifiedAdjunct(in: DataInput): Adjunct = this
  }

  private final class CanMapOption[B] extends MapSupport
    with CanMap[Option, Ex[B], Ex[Option[B]]] {

    final val id = 1001

    override def toString = "CanMapOption"

    def map[A](from: Ex[Option[A]], fun: Ex[A] => Ex[B]): Ex[Option[B]] = {
      val (it, res) = mkClosure(fun)
      MapExOption[A, B](in = from, it = it, fun = res)
    }
  }

  private final class CanMapSeq[B] extends MapSupport
    with CanMap[Seq, Ex[B], Ex[Seq[B]]] {

    final val id = 1002

    override def toString = "CanMapSeq"

    def map[A](from: Ex[Seq[A]], fun: Ex[A] => Ex[B]): Ex[Seq[B]] = {
      val (it, res) = mkClosure(fun)
      MapExSeq[A, B](in = from, it = it, fun = res)
    }
  }

  private final class CanFlatMapOption[B] extends MapSupport
    with CanFlatMap[Option, Ex[Option[B]], Ex[Option[B]]] {

    final val id = 1004

    override def toString = "CanFlatMapOption"

    def flatMap[A](from: Ex[Option[A]], fun: Ex[A] => Ex[Option[B]]): Ex[Option[B]] = {
      val (it, res) = mkClosure(fun)
      FlatMapExOption(in = from, it = it, fun = res)
    }
  }

  private final class CanFlatMapSeq[B] extends MapSupport
    with CanFlatMap[Seq, Ex[Seq[B]], Ex[Seq[B]]] {

    final val id = 1012

    override def toString = "CanFlatMapSeq"

    def flatMap[A](from: Ex[Seq[A]], fun: Ex[A] => Ex[Seq[B]]): Ex[Seq[B]] = {
      val (it, res) = mkClosure(fun)
      FlatMapExSeq[A, B](in = from, it = it, fun = res)
    }
  }

  private final class CanFlatMapSeqOption[B] extends MapSupport
    with CanFlatMap[Seq, Ex[Option[B]], Ex[Seq[B]]] {

    final val id = 1014

    override def toString = "CanFlatMapSeqOption"

    def flatMap[A](from: Ex[Seq[A]], fun: Ex[A] => Ex[Option[B]]): Ex[Seq[B]] = {
      val (it, res) = mkClosure(fun)
      FlatMapExSeqOption[A, B](in = from, it = it, fun = res)
    }
  }

  trait Sink[-A] {
// EEE
//    def update(value: Ex[A]): Unit
  }

  trait Source[+A] {
    def apply(): Ex[A]
  }
}
trait Ex[+A] extends Flow {
  type Repr[T <: Txn[T]] <: IExpr[T, A]
}
