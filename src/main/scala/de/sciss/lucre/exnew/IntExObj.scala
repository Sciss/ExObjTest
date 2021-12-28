/*
 *  IntExObj.scala
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

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.exnew.graph.Ex
import de.sciss.lucre.exnew.impl.ContextMixin
import de.sciss.lucre.impl.{ExprNodeImpl, ExprTypeExtension1, GeneratorEvent}
import de.sciss.lucre.{Copy, Cursor, Disposable, Event, EventLike, IntObj, Obj, Pull, Source, Txn}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable

object IntExObj {
  def apply[T <: Txn[T]](ex: Ex[Int])(implicit tx: T): IntObj[T] = {
//    val ex: Ex[Int] = "in".attr(0) * 2
    //    val vr = Var(0)
    //    val tr = Trig()
    //    ex.changed --> Act(vr.inc, tr)
    //    val res: Ex[Int] = vr.latch(tr)
    val tgt = Targets[T]()
    println("--> begin new")
    val res = new IntEx[T](ex, tgt, tx)
    println("<-- end new")
    res
  }

  private[this] final val INT_EX_SER_VERSION = 0x4945

  // XXX TODO is this viable?
  private[this] final class HeadlessContext[T <: Txn[T]](_selfH: Source[T, Obj[T]])
    extends ContextMixin[T] {

    override def connect: Boolean = false

    private[this] val _eventsSet = mutable.Set.empty[Event[T, Any]]
    private[this] var _eventsVec = Vector     .empty[Event[T, Any]]

    def events: Vec[Event[T, Any]] = _eventsVec

    override def reactTo[A](event: EventLike[T, A])(fun: T => A => Unit)(implicit tx: T): Disposable[T] = {
      event match {
        case e: Event[T, A] =>
          val isNew = _eventsSet.add(e)
          println(s"REGISTER EVENT $e (new? $isNew)")
          if (isNew) _eventsVec :+= e
        case _ => ()
      }
      Disposable.empty
    }

    override protected def selfH: Option[Source[T, Obj[T]]] = Some(_selfH)

    private def unsupported(what: String): Nothing =
      throw new UnsupportedOperationException(s"$what of a headless context")

    override implicit def cursor      : Cursor      [T] = unsupported("cursor"      )
// EEE
//    override implicit def workspace   : Workspace   [T] = unsupported("workspace"   )
    override implicit def undoManager : UndoManager [T] = unsupported("undo-manager")

    override def attr: Context.Attr[T] = Context.emptyAttr
  }

  private[this] object IntEx extends ExprTypeExtension1[IntObj] {

    override def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                           (implicit tx: T): IntObj[T] = {
      val cookie = in.readShort()
      require(cookie == INT_EX_SER_VERSION, s"Unexpected cookie $cookie")
      val ref   = new ExElem.RefMapIn(in)
      val ex    = ref.readEx[Int]()
      new IntEx[T](ex, targets, tx)
    }

    override def name: String = "Ex[Int]"

    override final val opLo = -1
    override final val opHi = -1
  }
  private final class IntEx[T <: Txn[T]](ex: Ex[Int], protected val targets: Targets[T], tx0: T)
    extends IntObj[T] with ExprNodeImpl[T, Int] /*with Caching*/ {

    type A = Int

    override def toString = s"Expr$id @${hashCode.toHexString}"

    private[this] val sourcesRef = {
      implicit val tx: T = tx0
      id.newVar(Vec.empty[Event[T, Any]])
    }

    override def value(implicit tx: T): Int = {
      val hc = new HeadlessContext[T](tx0.newHandle[IntObj[T]](this))
      implicit val ctx: Context[T] = hc
      val peer      = ex.expand[T]
      val res       = peer.value
      val eventsOld = sourcesRef()
      val eventsNew = hc.events
      if (eventsOld != eventsNew) {
        sourcesRef()  = eventsNew
        val eventsAdd = eventsNew diff eventsOld
        val eventsRem = eventsOld diff eventsNew
        val c         = changed
        println(s"REMOVE  EVENTS $eventsRem")
        println(s"ADD     EVENTS $eventsAdd")
        eventsRem.foreach(_ -/-> c)
        eventsAdd.foreach(_ ---> c)
      }
      res
    }

    value(tx0)

    override def tpe: Obj.Type = IntObj

    object changed extends Changed with GeneratorEvent[T, Change[A]] {
      private[lucre] def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Change[A]] = {
        ??? // Some(pull.resolve[Change[A]])
      }
    }

    //    final def connect()(implicit tx: T): this.type = {
    //      peer.changed ---> changed
    //      this
    //    }
    //
    //    private[this] def disconnect()(implicit tx: T): Unit = {
    //      peer.changed -/-> changed
    //    }

    override protected def writeData(out: DataOutput): Unit = {
      out.writeByte(1)  // 'node not var'
      out.writeInt(-1)  // opId
      out.writeShort(INT_EX_SER_VERSION)
      val ref = new ExElem.RefMapOut(out)
//      {
//        override protected def writeIdentifiedProduct(p: Product): Unit = {
//          p match {
//            case Attr.WithDefault(key, _) => println(s"EVENT KEY (d): $key")
//            case Attr(key)                => println(s"EVENT KEY    : $key")
//            case _ =>
//          }
//          super.writeIdentifiedProduct(p)
//        }
//      }
      ref.writeElem(ex)
    }

    override protected def disposeData()(implicit tx: T): Unit = {
      //      disconnect()
//      obs .dispose()
//      peer.dispose()
//      ctx .dispose()
    }

    /** Makes a deep copy of an element, possibly translating it to a different system `Out`. */
    override private[lucre] def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out,
                                                        context: Copy[T, Out]): IntObj[Out] = {
      val newTgt = Event.Targets[Out]()
      new IntEx[Out](ex, newTgt, txOut)
    }
  }
}
