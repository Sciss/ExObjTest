/*
 *  Attr.scala
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

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.exnew.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.exnew.graph.impl.{StmObjAttrMapCellView, StmObjCtxCellView}
import de.sciss.lucre.exnew.impl.CellViewImpl.CatVarImpl
import de.sciss.lucre.exnew.impl.IChangeGeneratorEvent
import de.sciss.lucre.exnew.{Arrow, CellView, Context, IChangeEvent, IExpr, IPull, ITargets}
import de.sciss.lucre.{Adjunct, Disposable, Form, ProductWithAdjuncts, Txn, Obj => LObj}
import de.sciss.model.Change
import de.sciss.serial.DataOutput

import scala.annotation.tailrec
import scala.concurrent.stm.Ref

object Attr extends ProductReader[Attr[_]] {
  object Like {
// EEE
//    implicit def arrowRight   [A]: Arrow.Right[A, Like[A]]          = new LikeArrowRight[A]
//    implicit def arrowRightOpt[A]: Arrow.Right[Option[A], Like[A]]  = new LikeArrowRightOpt[A]
//
//    private final class LikeArrowRight[A] extends Arrow.Right[A, Like[A]] {
//      override def patchTo(source: Ex.Source[A], sink: Like[A]): Unit =
//        sink.update(source())
//    }
//
//    private final class LikeArrowRightOpt[A] extends Arrow.Right[Option[A], Like[A]] {
//      override def patchTo(source: Ex.Source[Option[A]], sink: Like[A]): Unit =
//        sink.updateOption(source())
//    }
  }
  trait Like[A] extends Ex.Sink[A] { self =>
// EEE
//    def updateOption(in: Ex[Option[A]]): Unit
//    def set   (in: Ex[A]): Act
  }

  private[lucre] def resolveNestedIn[T <: Txn[T], A](objOpt: Option[LObj[T]], key: String)
                                                    (implicit tx: T,
                                                     bridge: Obj.Bridge[A]): Option[CellView.Var[T, Option[A]]] = {
    @tailrec
    def loop(prev: Option[LObj[T]], sub: String): Option[CellView.Var[T, Option[A]]] =
      prev match {
        case Some(obj) =>
          val i = sub.indexOf(':')
          if (i < 0) {
            val attrView = bridge.cellView[T](obj, sub)
            Some(attrView)
          } else {
            val head = sub.substring(0, i)
            val tail = sub.substring(i + 1)
            val next = obj.attr.get(head)
            loop(next, tail)
          }

        case _ => None
      }

    loop(objOpt, key)
  }

  private[lucre] def resolveNested[T <: Txn[T], A](key: String)(implicit ctx: Context[T], tx: T,
                                                                bridge: Obj.Bridge[A]): CellView[T, Option[A]] = {
    val isNested = key.contains(":")

    if (isNested) {
      val head :: firstSub :: tail = key.split(":").toList: @unchecked

      @tailrec
      def loop(parent: CellView[T, Option[LObj[T]]], sub: String, rem: List[String]): CellView[T, Option[A]] =
        rem match {
          case Nil =>
            parent.flatMapTx { implicit tx => child => bridge.cellValue(child, sub) }

          case next :: tail =>
            val childView = parent.flatMapTx { implicit tx => child => child.attr.get(key) }
            loop(childView, next, tail)
        }

      val ctxHead   = new StmObjCtxCellView[T](ctx.attr, head)
      val ctxFull   = loop(ctxHead, firstSub, tail)
      ctx.selfOption match {
        case Some(self) =>
          val objHead   = new StmObjAttrMapCellView[T](self.attr, head, tx)
          val objFull   = loop(objHead, firstSub, tail)
          ctxFull orElse objFull
        case None =>
          ctxFull
      }

    } else {
      val ctxFull = bridge.contextCellView[T](key)
      ctx.selfOption match {
        case Some(self) =>
          val objFull = bridge.cellView(self, key)
          ctxFull orElse objFull
        case None =>
          ctxFull
      }
    }
  }

  // similar to `CellViewImpl.OptionOrElseImpl` but adding `.Var` support
  private final class NestedVarCellView[T <: Txn[T], A](firstP  : CellView[T, Option[LObj[T]]],
                                                        secondP : CellView[T, Option[LObj[T]]],
                                                        lastSub : String)(implicit bridge: Obj.Bridge[A])
    extends CellView.Var[T, Option[A]] {

    def apply()(implicit tx: T): Option[A] = {
      val parOpt = firstP() orElse secondP()
      parOpt match {
        case Some(par)  => bridge.cellValue(par, lastSub)
        case None       => None
      }
    }

    def react(fun: T => Option[A] => Unit)(implicit tx: T): Disposable[T] = {
      val f: T => Option[LObj[T]] => Unit = { implicit tx => _ => fun(tx)(apply()) }
      val r1 = firstP .react(f)
      val r2 = secondP.react(f)
      Disposable.seq(r1, r2)
    }

    def update(v: Option[A])(implicit tx: T): Unit = {
      val parOpt = firstP() orElse secondP()
      parOpt match {
        case Some(par)  => bridge.cellView(par, lastSub).update(v)
        case None       =>
      }
    }
  }

  // XXX TODO --- all pretty hack'ish
  private final class FlatVarCellView[T <: Txn[T], A, B](firstP   : CellView[T, Option[A]],
                                                         firstVr  : Option[Var.Expanded[T, B]],
                                                         secondP  : CellView.Var[T, Option[A]],
                                                        )(implicit bridge: Obj.Bridge[A])
    extends CellView.Var[T, Option[A]] {

    def apply()(implicit tx: T): Option[A] =
      firstP() orElse secondP()

    def react(fun: T => Option[A] => Unit)(implicit tx: T): Disposable[T] = {
      val f: T => Option[A] => Unit = { implicit tx => _ => fun(tx)(apply()) }
      val r1 = firstP .react(f)
      val r2 = secondP.react(f)
      Disposable.seq(r1, r2)
    }

    def update(v: Option[A])(implicit tx: T): Unit = {
      firstVr match {
        case Some(vr) =>
          vr.fromAny.fromAny(v.get).foreach { vT =>
            vr.update(new Const.Expanded(vT))
          }
        case None =>
          secondP.update(v)
      }
    }
  }

  private[lucre] def resolveNestedVar[T <: Txn[T], A](key: String)(implicit ctx: Context[T], tx: T,
                                                                   bridge: Obj.Bridge[A]): CellView.Var[T, Option[A]] = {
    val isNested = key.contains(":")

    if (isNested) {
      val head :: firstSub :: tail = key.split(":").toList: @unchecked

      @tailrec
      def loop(parent: CellView[T, Option[LObj[T]]], sub: String, rem: List[String]): (CellView[T, Option[LObj[T]]], String) =
        rem match {
          case Nil =>
            // new CatVarImpl[T, LObj[T], A](parent, child => bridge.cellView(child, sub))
            (parent, sub)

          case next :: tail =>
            val childView = parent.flatMapTx { implicit tx =>  child =>
              child.attr.get(key)
            }
            loop(childView, next, tail)
        }

      val ctxHead             = new StmObjCtxCellView[T](ctx.attr, head)
      val (ctxFullP, lastSub) = loop(ctxHead, firstSub, tail)
      ctx.selfOption match {
        case Some(self) =>
          val objHead       = new StmObjAttrMapCellView[T](self.attr, head, tx)
          val (objFullP, _) = loop(objHead, firstSub, tail)
          new NestedVarCellView(ctxFullP, objFullP, lastSub)

        case None =>
          new CatVarImpl[T, LObj[T], A](ctxFullP)({ implicit tx => child =>
            bridge.cellView(child, lastSub)
          })
      }

    } else {
      ctx.selfOption match {
        case Some(self) =>
          val firstP  = bridge.contextCellView[T](key)
          val secondP = bridge.cellView(self, key)
          val opt: Option[Form[T]] = ctx.attr.get(key)
          /*val firstVr =*/ opt match {
          case Some(ex: Var.Expanded[T, _]) =>
            // work-around for Scala 3.0.2:
            new FlatVarCellView(firstP, Some(ex), secondP)
          // Some(ex)
          case _                            =>
            new FlatVarCellView(firstP, None, secondP)
          // None
        }
        // new FlatVarCellView(firstP, firstVr, secondP)

        case None =>  // if there is no 'self', simply give up on the idea of attributes
          CellView.Var.empty
      }
    }
  }

  object WithDefault extends ProductReader[WithDefault[_]] {
    def apply[A](key: String, default: Ex[A])(implicit bridge: Obj.Bridge[A]): WithDefault[A] =
      Impl(key, default)

    def unapply[A](attr: WithDefault[A]): Option[(String, Ex[A])] =
      Some((attr.key, attr.default))

    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): WithDefault[_] = {
      require (arity == 2 && adj == 1)
      val _key      = in.readString()
      val _default  = in.readEx[Any]()
      val _bridge: Obj.Bridge[Any] = in.readAdjunct()
      WithDefault(_key, _default)(_bridge)
    }

    private final case class Impl[A](key: String, default: Ex[A])(implicit val bridge: Obj.Bridge[A])
      extends WithDefault[A] with ProductWithAdjuncts {

      type Repr[T <: Txn[T]] = IExpr[T, A]

      override def productPrefix: String = s"Attr$$WithDefault" // serialization

// EEE
//      def update      (in: Ex[A])         : Unit  = Attr.Update       (in, key)
//      def updateOption(in: Ex[Option[A]]) : Unit  = Attr.UpdateOption (in, key)
//      def set         (in: Ex[A])         : Act   = Attr.Set          (in, key)

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        val defaultEx: Repr[T] = default.expand[T]
        import ctx.targets
        val attrView = resolveNested(key)
        new WithDefault.Expanded[T, A](key, attrView, defaultEx).connect()
      }

      override def adjuncts: scala.List[Adjunct] = bridge :: Nil
    }

    private[lucre] final class Expanded[T <: Txn[T], A](key: String, attrView: CellView[T, Option[A]], default: IExpr[T, A])
                                                       (implicit protected val targets: ITargets[T])
      extends IExpr[T, A] with IChangeGeneratorEvent[T, A] {

      override protected def typeId: Int = ???

      override protected def writeData(out: DataOutput): Unit = {
        out.writeByte(0)  // serialization version
        out.writeUTF(key)
        ??? // attrView.write(out)
        default.write(out)
      }

      override def toString: String = s"Attr.WithDefault($key, $default)"

      private[this] val ref = Ref(Option.empty[A]) // (attrView()(tx0))

      private[this] val obsAttr = Ref(Disposable.empty[T])

      private def mkObs()(implicit tx: T): Disposable[T] = attrView.react { implicit tx => now =>
        val before  = ref.swap(now)(tx.peer)
        if (before != now) {
          val before1   = before.getOrElse(default.value)
          val now1      = now   .getOrElse(default.value)
          val ch        = Change(before1, now1)
          if (ch.isSignificant) fire(ch)
        }
      }

      def connect()(implicit tx: T): this.type = {
        ref() = attrView()
        obsAttr() = mkObs()
        default.changed.--->(this)
        this
      }

      def value(implicit tx: T): A = {
        val opt = attrView()
        opt.getOrElse(default.value)
      }

      private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
        val dch = default.changed
        if (pull.contains(dch) && ref.get(tx.peer).isEmpty) {
          pull.applyChange(dch)
        } else if (pull.isOrigin(this)) {
          pull.resolveExpr(this)
        } else {
          value
        }
      }

      def dispose()(implicit tx: T): Unit = {
        default.changed -/-> this
        obsAttr.swap(Disposable.empty).dispose()
      }

      def changed: IChangeEvent[T, A] = this
    }
  }
  // N.B. we use a trait here not a case class, because
  // we reuse the interface elsewhere (SP -> Artifact)
  trait WithDefault[A] extends Ex[A] with Like[A] { self =>
    def key: String   // XXX TODO ugh, not binary compatible; we take the risk as we know the three implementations

    def default: Ex[A]

// EEEE
//    def transform(f: Ex[A] => Ex[A]): Act = set(f(self))
  }

  private[lucre] final class Expanded[T <: Txn[T], A](key: String, attrView: CellView[T, Option[A]])
                                                     (implicit protected val targets: ITargets[T])
    extends IExpr[T, Option[A]] with IChangeGeneratorEvent[T, Option[A]] {

    override protected def typeId: Int = ???

    override protected def writeData(out: DataOutput): Unit = {
      out.writeByte(0)  // serialization version
      out.writeUTF(key)
      ??? // attrView.write(out)
    }

    override def toString: String = s"Attr($key)"

    private[this] val ref = Ref(Option.empty[A]) // (value(tx0))

    private[this] val obsAttr = Ref(Disposable.empty[T])

    private def mkObs()(implicit tx: T): Disposable[T] = attrView.react { implicit tx => now =>
      val before = ref.swap(now)(tx.peer)
      val ch = Change(before, now)
      // println(s"Attr.Expanded change $ch")
      if (ch.isSignificant) fire(ch)
    }

    def connect()(implicit tx: T): this.type = {
      ref() = value
      obsAttr() = mkObs()
      this
    }

    def value(implicit tx: T): Option[A] = attrView()

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Option[A] =
      pull.resolveExpr(this)

    def changed: IChangeEvent[T, Option[A]] = this

    def dispose()(implicit tx: T): Unit = {
      // println("Attr.Expanded - dispose")
      obsAttr.swap(Disposable.empty).dispose()
    }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Attr[_] = {
    require (arity == 1 && adj == 1)
    val _key    = in.readString()
    val _bridge: Obj.Bridge[Any] = in.readAdjunct()
    new Attr(_key)(_bridge)
  }
}
final case class Attr[A](key: String)(implicit val bridge: Obj.Bridge[A])
  extends Ex[Option[A]] with Attr.Like[A] with ProductWithAdjuncts {

  type Repr[T <: Txn[T]] = IExpr[T, Option[A]]

// EEE
//  def update      (in: Ex[A])         : Unit  = Attr.Update       (in, key)
//  def updateOption(in: Ex[Option[A]]) : Unit  = Attr.UpdateOption (in, key)
//  def set         (in: Ex[A])         : Act   = Attr.Set          (in, key)

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val attrView = Attr.resolveNested(key)
    new Attr.Expanded[T, A](key, attrView).connect()
  }

  override def adjuncts: scala.List[Adjunct] = bridge :: Nil
}
