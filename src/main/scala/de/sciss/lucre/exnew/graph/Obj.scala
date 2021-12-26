/*
 *  Obj.scala
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

import de.sciss.lucre.Adjunct.HasDefault
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.exnew.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.exnew.graph.impl.{AbstractCtxCellView, ExpandedObjAttr, MappedIExpr, ObjCellViewVarImpl, ObjImplBase}
import de.sciss.lucre.exnew.graph.{Attr => _Attr}
import de.sciss.lucre.exnew.impl.{ExObjBridgeImpl, ExSeqObjBridgeImpl}
import de.sciss.lucre.exnew.{CellView, Context, IExpr, ITargets}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Adjunct, BooleanObj, Caching, DoubleObj, DoubleVector, IntObj, IntVector, LongObj, ProductWithAdjuncts, SpanLikeObj, SpanObj, StringObj, Sys, Txn, Obj => LObj, Source => LSource}
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

import scala.concurrent.stm.Ref

object Obj {
  private lazy val _init: Unit = {
    Adjunct.addFactory(Source.obj)
    Adjunct.addFactory(Bridge.obj)
  }

  def init(): Unit = _init

  implicit final class ExOps(private val obj: Ex[Obj]) extends AnyVal {
    def attr[A: Bridge](key: String): Obj.Attr[A] = Obj.Attr(obj, key)

    // def attr[A: Bridge](key: String, default: Ex[A]): _Attr.WithDefault[A] = ...

// EEE
//    def copy: Copy = Obj.Copy(obj)

    /** Tries to parse this object as a more specific type.
     * Produces `Some` if the type matches, otherwise `None`.
     */
    def as[A: Bridge]: Ex[Option[A]] = As[A](obj)
  }

  // used by Mellite (no transaction available)
  private[lucre] def wrapH[T <: Txn[T]](peer: LSource[T, LObj[T]], system: Sys): Obj =
    new Impl[T](peer, system)

  private[lucre] def wrap[T <: Txn[T]](peer: LObj[T])(implicit tx: T): Obj =
    new Impl[T](tx.newHandle(peer), tx.system)

  def empty: Ex[Obj] = Const(Empty)

  // scalac: illegal cyclic...
  private[lucre] case object Empty extends Obj with ProductReader[Product /*Empty.type*/] {
    override def productPrefix: String = s"Obj$$Empty$$"  // serialization
    override def toString     : String = "Obj<empty>"

    type Peer[~ <: Txn[~]] = LObj[~]

    def peer[T <: Txn[T]](implicit tx: T): Option[Peer[T]] =
      None // throw new IllegalStateException("Object has not been created yet")

    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Empty.type = {
      require (arity == 0 && adj == 0)
      this
    }
  }

  private final class Impl[In <: Txn[In]](in: LSource[In, LObj[In]], system: Sys)
    extends ObjImplBase[In, LObj](in, system) {

    override def toString: String = s"Obj($in)"
  }

  implicit def hasDefault: HasDefault[Obj] = Bridge.obj

  object Bridge {
    implicit val int      : Bridge[Int        ] = new ExObjBridgeImpl(IntObj       )
    implicit val long     : Bridge[Long       ] = new ExObjBridgeImpl(LongObj      )
    implicit val double   : Bridge[Double     ] = new ExObjBridgeImpl(DoubleObj    )
    implicit val boolean  : Bridge[Boolean    ] = new ExObjBridgeImpl(BooleanObj   )
    implicit val string   : Bridge[String     ] = new ExObjBridgeImpl(StringObj    )
    implicit val spanLike : Bridge[_SpanLike  ] = new ExObjBridgeImpl(SpanLikeObj  )
    implicit val span     : Bridge[_Span      ] = new ExObjBridgeImpl(SpanObj      )
    implicit val intVec   : Bridge[Seq[Int   ]] = new ExSeqObjBridgeImpl(IntVector    )
    implicit val doubleVec: Bridge[Seq[Double]] = new ExSeqObjBridgeImpl(DoubleVector )

    implicit object obj extends Bridge[Obj] with HasDefault[Obj] with Adjunct.Factory {
      final val id = 1005

      type Repr[T <: Txn[T]] = LObj[T]

      def defaultValue: Obj = Empty

      def readIdentifiedAdjunct(in: DataInput): Adjunct = this

      def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[Obj]] =
        new ObjCellViewVarImpl[T, LObj, Obj](tx.newHandle(obj), key) {
          protected def lower(peer: LObj[T])(implicit tx: T): Obj =
            wrap(peer)
        }

      def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[Obj]] = {
        new AbstractCtxCellView[T, Obj](context.attr, key) {
          protected def tryParseValue(value: Any)(implicit tx: T): Option[Obj] = value match {
            case obj: Obj => Some(obj)
            case _        => None
          }

          protected def tryParseObj(peer: LObj[T])(implicit tx: T): Option[Obj] =
            Some(wrap(peer))
        }
      }

      def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[Obj] =
        obj.attr.get(key).map(wrap(_))

      def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[Obj] =
        Some(wrap(obj))
    }
  }
  trait Bridge[A] extends Adjunct {
    /** Creates a bidirectional view between `LObj` and the expression side representation type `A`.
     * If possible, implementations should look at `UndoManager.find` when updating values.
     */
    def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[A]]

    /** Creates a unidirectional view between a context's attribute or self object and the expression side
     * representation type `A`.
     */
    def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[A]]

    def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[A]

    def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[A]
  }

  object Source {
    implicit object obj extends Source[Obj] with Adjunct.Factory {
      final val id = 1006

      type Repr[T <: Txn[T]] = LObj[T]

      def toObj[T <: Txn[T]](value: Obj)(implicit tx: T): LObj[T] =
        value.peer.getOrElse(throw new IllegalStateException("Object has not yet been instantiated"))

      implicit def reprTFormat[T <: Txn[T]]: TFormat[T, LObj[T]] = LObj.format

      def readIdentifiedAdjunct(in: DataInput): Adjunct = this
    }

    implicit def canMake[A](implicit peer: CanMake[A]): Source[A] = peer
  }

  /** An `Obj.Source` either has an `LObj` peer, or it can make one.
   * The latter is represented by sub-trait `CanMake`.
   */
  trait Source[-A] extends Adjunct {
    type Repr[T <: Txn[T]] <: LObj[T]

    def toObj[T <: Txn[T]](value: A)(implicit tx: T): Repr[T]

    implicit def reprTFormat[T <: Txn[T]]: TFormat[T, Repr[T]]
  }

  object CanMake {
    implicit val int      : CanMake[Int        ] = new ExObjBridgeImpl(IntObj       )
    implicit val long     : CanMake[Long       ] = new ExObjBridgeImpl(LongObj      )
    implicit val double   : CanMake[Double     ] = new ExObjBridgeImpl(DoubleObj    )
    implicit val boolean  : CanMake[Boolean    ] = new ExObjBridgeImpl(BooleanObj   )
    implicit val string   : CanMake[String     ] = new ExObjBridgeImpl(StringObj    )
    implicit val intVec   : CanMake[Seq[Int   ]] = new ExSeqObjBridgeImpl(IntVector    )
    implicit val doubleVec: CanMake[Seq[Double]] = new ExSeqObjBridgeImpl(DoubleVector )
  }
  trait CanMake[A] extends Source[A]

  object Attr extends ProductReader[Attr[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Attr[_] = {
      require (arity == 2 && adj == 1)
      val _obj  = in.readEx[Obj]()
      val _key  = in.readString()
      val _bridge: Bridge[Any] = in.readAdjunct()
      new Attr(_obj, _key)(_bridge)
    }
  }
  // XXX TODO --- this should be merged with graph.Attr ?
  final case class Attr[A](obj: Ex[Obj], key: String)(implicit val bridge: Bridge[A])
    extends Ex[Option[A]] with _Attr.Like[A] with ProductWithAdjuncts {

    type Repr[T <: Txn[T]] = IExpr[T, Option[A]]

    override def productPrefix: String = s"Obj$$Attr" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ExpandedObjAttr(obj.expand[T], key, tx)
    }

// EEE
//    def update      (in: Ex[A])         : Unit = Obj.Attr.Update       (obj, key, in)
//    def updateOption(in: Ex[Option[A]]) : Unit = Obj.Attr.UpdateOption (obj, key, in)
//    def set         (in: Ex[A])         : Act  = Obj.Attr.Set          (obj, key, in)

    def adjuncts: List[Adjunct] = bridge :: Nil
  }

  object As extends ProductReader[Ex[Option[_]]] {
    def apply[A](obj: Ex[Obj])(implicit bridge: Bridge[A]): Ex[Option[A]] = Impl(obj)

    // XXX TODO --- we should use cell-views instead, because this way we won't notice
    // changes to the value representation (e.g. a `StringObj.Var` contents change)
    private final class Expanded[T <: Txn[T], A](in: IExpr[T, Obj], tx0: T)
                                                (implicit targets: ITargets[T], bridge: Obj.Bridge[A])
      extends MappedIExpr[T, Obj, Option[A]](in, tx0) {

      protected def mapValue(inValue: Obj)(implicit tx: T): Option[A] =
        inValue.peer[T].flatMap(bridge.tryParseObj(_))
    }

    private final case class Impl[A](obj: Ex[Obj])(implicit val bridge: Bridge[A])
      extends Ex[Option[A]] with ProductWithAdjuncts {

      override def toString: String = s"$obj.as[$bridge]"

      type Repr[T <: Txn[T]] = IExpr[T, Option[A]]

      def adjuncts: List[Adjunct] = bridge :: Nil

      override def productPrefix: String = s"Obj$$As" // serialization

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        import ctx.targets
        new Expanded[T, A](obj.expand[T], tx)
      }
    }

    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ex[Option[_]] = {
      require (arity == 1 && adj == 1)
      val _obj = in.readEx[Obj]()
      val _bridge: Bridge[Any] = in.readAdjunct()
      As(_obj)(_bridge)
    }
  }
}
trait Obj {
  type Peer[~ <: Txn[~]] <: LObj[~]

  def peer[T <: Txn[T]](implicit tx: T): Option[Peer[T]]
}
