/*
 *  Folder.scala
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
import de.sciss.lucre.edit.EditFolder
import de.sciss.lucre.exnew.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.exnew.graph.impl.{AbstractCtxCellView, ObjCellViewVarImpl, ObjImplBase}
import de.sciss.lucre.impl.IDummyEvent
//import de.sciss.lucre.exnew.impl.IActionImpl
import de.sciss.lucre.exnew.{CellView, Context /*, IAction*/}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Adjunct, Caching, Disposable, IChangeEvent, IExpr, IPull, IPush, ITargets, ListObj, ProductWithAdjuncts, Source, Sys, Txn, Folder => LFolder, Obj => LObj}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, TFormat}

import scala.concurrent.stm.Ref

object Folder extends ProductReader[Ex[Folder]] {
  private lazy val _init: Unit =
    Adjunct.addFactory(Bridge)

  def init(): Unit = _init

  def apply(): Ex[Folder] /*with Obj.Make*/ = Apply()

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ex[Folder] = {
    require (arity == 0 && adj == 0)
    Folder()
  }

  private[lucre] case object Empty extends Folder {
    def peer[T <: Txn[T]](implicit tx: T): Option[Peer[T]] = None

    override def toString: String = "Folder<empty>"
  }

  private final class ApplyExpanded[T <: Txn[T]](implicit targets: ITargets[T])
    extends IExpr[T, Folder] /*ExpandedObjMakeImpl[T, Folder]*/ {

    override def toString: String = "Folder()"

    protected def empty: Folder = Empty

    override def value(implicit tx: T): Folder = empty

    override def changed: IChangeEvent[T, Folder] = IDummyEvent.change

    override def dispose()(implicit tx: T): Unit = ()

    protected def make()(implicit tx: T): Folder = {
      val peer = LFolder[T]()
      new Impl[T](tx.newHandle(peer), tx.system)
    }
  }

  private final case class Apply() extends Ex[Folder] /*with Act with Obj.Make*/ {
    override def productPrefix: String = "Folder" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Folder] /*with IAction[T]*/

//    def make: Act = this

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ApplyExpanded[T]
    }
  }

  private[lucre] def wrapH[T <: Txn[T]](peer: Source[T, LFolder[T]], system: Sys): Folder =
    new Impl[T](peer, system)

  private[lucre] def wrap[T <: Txn[T]](peer: LFolder[T])(implicit tx: T): Folder =
    new Impl[T](tx.newHandle(peer), tx.system)

  private final class Impl[T <: Txn[T]](in: Source[T, LFolder[T]], system: Sys)
    extends ObjImplBase[T, LFolder](in, system) with Folder {

    override type Peer[~ <: Txn[~]] = LFolder[~]

    override def toString: String = s"Folder($in)"
  }

  private final class CellViewImpl[T <: Txn[T]](h: Source[T, LObj[T]], key: String)(implicit context: Context[T])
    extends ObjCellViewVarImpl[T, LFolder, Folder](h, key) {

    protected def lower(peer: LFolder[T])(implicit tx: T): Folder =
      wrap(peer)

    implicit def format: TFormat[T, Option[LFolder[T]]] =
      TFormat.option
  }

  implicit object Bridge extends Obj.Bridge[Folder] with HasDefault[Folder] with Adjunct.Factory {
    final val id = 2001

    type Repr[T <: Txn[T]] = LFolder[T]

    def defaultValue: Folder = Empty

    def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    override def cellView[T <: Txn[T]](obj: LObj[T], key: String)
                                      (implicit tx: T, context: Context[T]): CellView.Var[T, Option[Folder]] =
      new CellViewImpl(tx.newHandle(obj), key)

    def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[Folder]] =
      new AbstractCtxCellView[T, Folder](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: T): Option[Folder] = value match {
          case f: Folder  => Some(f)
          case _          => None
        }

        protected def tryParseObj(obj: LObj[T])(implicit tx: T): Option[Folder] = obj match {
          case f: LFolder[T] => Some(wrap(f))
          case _                => None
        }
      }

    def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[Folder] =
      obj.attr.$[LFolder](key).map(wrap(_))

    def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[Folder] = obj match {
      case a: LFolder[T] => Some(wrap(a))
      case _                => None
    }
  }

  private abstract class ExpandedImpl[T <: Txn[T], A](in: IExpr[T, Folder], init: A, tx0: T)
                                                     (implicit protected val targets: ITargets[T], context: Context[T])
    extends IExpr[T, A] with IChangeGeneratorEvent[T, A] with Caching {

    private[this] val obs   = Ref[Disposable[T]](Disposable.empty)
    private[this] val ref   = Ref(init)

    protected def mapValue(f: ListObj[T, LObj[T]])(implicit tx: T): A

    private def setObj(v: Folder)(implicit tx: T): Unit /*Option[Change[A]]*/ = {
      obs.swap(Disposable.empty).dispose()
      // XXX TODO --- should we also fire when size has been non-zero and v.peer is empty?
      v.peer[T].foreach { f =>
        val newObs = context.reactTo(f.changed) { implicit tx => upd =>
          val now     = mapValue(upd.list)
          val before  = ref.swap(now)
          if (before != now) fire(Change(before, now))
        }
        obs()   = newObs
        val now = mapValue(f)
        ref()   = now
        //        val before  = ref.swap(now)
        //        if (before != now) Some(Change(before, now)) else None
      }
    }

    in.changed.--->(this)(tx0)
    setObj(in.value(tx0))(tx0)

    def value(implicit tx: T): A =
      IPush.tryPull(this).fold(ref())(_.now)

    def changed: IChangeEvent[T, A] = this

    def dispose()(implicit tx: T): Unit = {
      in.changed.-/->(this)
      obs.swap(Disposable.empty).dispose()
    }

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A =
      if (pull.isOrigin(this)) pull.resolveExpr(this)
      else {
        if (phase.isBefore) ref() else {
          val res = pull.expr(in)
          setObj(res)
          ref() // res.peer.fold(ref()) { f => mapValue(f) }
        }
      }

    //    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Change[A]] =
    //      if (pull.isOrigin(this)) Some(pull.resolve)
    //      else {
    //        pull(in.changed).flatMap { ch =>
    //          setObj(ch.now)
    //        }
    //      }
  }

  private final class SizeExpanded[T <: Txn[T]](in: IExpr[T, Folder], tx0: T)
                                               (implicit targets: ITargets[T], context: Context[T])
    extends ExpandedImpl[T, Int](in, 0, tx0) {

    protected def mapValue(f: ListObj[T, LObj[T]])(implicit tx: T): Int = f.size
  }

  object Size extends ProductReader[Size] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Size = {
      require (arity == 1 && adj == 0)
      val _in = in.readEx[Folder]()
      new Size(_in)
    }
  }
  final case class Size(in: Ex[Folder]) extends Ex[Int] {
    override def productPrefix: String = s"Folder$$Size" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Int]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new SizeExpanded(in.expand[T], tx)
    }
  }

  private final class IsEmptyExpanded[T <: Txn[T]](in: IExpr[T, Folder], tx0: T)
                                                  (implicit targets: ITargets[T], context: Context[T])
    extends ExpandedImpl[T, Boolean](in, true, tx0) {

    protected def mapValue(f: ListObj[T, LObj[T]])(implicit tx: T): Boolean = f.isEmpty
  }

  object IsEmpty extends ProductReader[IsEmpty] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): IsEmpty = {
      require (arity == 1 && adj == 0)
      val _in = in.readEx[Folder]()
      new IsEmpty(_in)
    }
  }
  final case class IsEmpty(in: Ex[Folder]) extends Ex[Boolean] {
    override def productPrefix: String = s"Folder$$IsEmpty" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Boolean]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new IsEmptyExpanded(in.expand[T], tx)
    }
  }

  private final class NonEmptyExpanded[T <: Txn[T]](in: IExpr[T, Folder], tx0: T)
                                                   (implicit targets: ITargets[T], context: Context[T])
    extends ExpandedImpl[T, Boolean](in, false, tx0) {

    protected def mapValue(f: ListObj[T, LObj[T]])(implicit tx: T): Boolean = f.nonEmpty
  }

  object NonEmpty extends ProductReader[NonEmpty] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): NonEmpty = {
      require (arity == 1 && adj == 0)
      val _in = in.readEx[Folder]()
      new NonEmpty(_in)
    }
  }
  final case class NonEmpty(in: Ex[Folder]) extends Ex[Boolean] {
    override def productPrefix: String = s"Folder$$NonEmpty" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Boolean]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new NonEmptyExpanded(in.expand[T], tx)
    }
  }

  private final class ChildrenExpanded[T <: Txn[T]](in: IExpr[T, Folder], tx0: T)
                                                   (implicit targets: ITargets[T], context: Context[T])
    extends ExpandedImpl[T, Seq[Obj]](in, Nil, tx0) {

    override def toString: String = s"$in.children"

    protected def mapValue(f: ListObj[T, LObj[T]])(implicit tx: T): Seq[Obj] = {
      val b = List.newBuilder[Obj]
      b.sizeHint(f.size)
      f.iterator.foreach { peer =>
        b += Obj.wrap(peer)
      }
      b.result()
    }
  }

  object Children extends ProductReader[Children] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Children = {
      require (arity == 1 && adj == 0)
      val _in = in.readEx[Folder]()
      new Children(_in)
    }
  }
  final case class Children(in: Ex[Folder]) extends Ex[Seq[Obj]] {
    override def productPrefix: String = s"Folder$$Children" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Seq[Obj]]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ChildrenExpanded(in.expand[T], tx)
    }
  }

  implicit final class Ops(private val f: Ex[Folder]) extends AnyVal {
//    /** Prepends an element to the folder */
//    def prepend[A](elem: Ex[A])(implicit source: Obj.Source[A]): Act = Prepend(f, elem)
//
//    /** Appends an element to the folder */
//    def append [A](elem: Ex[A])(implicit source: Obj.Source[A]): Act = Append (f, elem)
//
//    /** Drops the `n` first elements from the folder.
//     * If the folder contains less elements than `n`, the folder will become empty.
//     */
//    def drop(n: Ex[Int]): Act = Drop(f, n)
//
//    /** Drops the `n` last elements from the folder.
//     * If the folder contains less elements than `n`, the folder will become empty.
//     */
//    def dropRight(n: Ex[Int]): Act = DropRight(f, n)
//
//    def clear: Act = Clear(f)

    def size    : Ex[Int    ]   = Size    (f)
    def isEmpty : Ex[Boolean]   = IsEmpty (f)
    def nonEmpty: Ex[Boolean]   = NonEmpty(f)

    def children: Ex[Seq[Obj]]  = Children(f)

//    def remove(obj: Ex[Obj]): Act = Remove(f, obj)
  }
}
/** The representation of a folder within an expression program.
 * It allows to refer to existing folders through `"key".attr[Folder]`
 * or to create a prototype `Folder()` which has a `make` action.
 *
 * '''Note:''' passing a folder with `runWith` is not yet implemented.
 */
trait Folder extends Obj {
  type Peer[~ <: Txn[~]] = LFolder[~]
}