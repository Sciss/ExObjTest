/*
 *  ExElem.scala
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

import de.sciss.lucre.exnew.graph.Ex
import de.sciss.lucre.{Adjunct, Artifact, Exec, Ident, ProductWithAdjuncts, Var}
import de.sciss.serial
import de.sciss.serial.{ConstFormat, DataInput, DataOutput}

import java.net.URI
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable

object ExElem {
  final val DefaultPackage = "de.sciss.lucre.exnew.graph"

  /** Derives the `productPrefix` served by the reader by the reader's class name itself.  */
  def addProductReaderSq(xs: Iterable[ProductReader[Product]]): Unit = {
    val m = mapRead
    m.synchronized {
      xs.foreach { value =>
        val cn    = value.getClass.getName
        val nm    = cn.length - 1
        val isObj = cn.charAt(nm) == '$'
        val i     = if (cn.startsWith(DefaultPackage)) DefaultPackage.length + 1 else 0
        val key   = if (isObj) cn.substring(i, nm) else cn.substring(i)
        m += ((key, value))
      }
    }
  }

  trait ProductReader[+A] {
    def read(in: RefMapIn, key: String, arity: Int, adj: Int): A
  }

  private val mapRead = mutable.Map[String, ProductReader[Product]](
  )

  class RefMapOut(out0: DataOutput) extends serial.RefMapOut(out0) {
    override protected def isDefaultPackage(pck: String): Boolean =
      pck == DefaultPackage

    override protected def writeIdentifiedProduct(p: Product): Unit = {
      val adjuncts = p match {
        case hasAdj: ProductWithAdjuncts => hasAdj.adjuncts
        case _ => Nil
      }

      out.writeByte(adjuncts.size)

      p.productIterator.foreach(writeElem)
      adjuncts.foreach(Adjunct.write(out, _))
    }

    override def writeElem(e: Any): Unit = e match {
      case c: graph.Const[_] =>
        out.writeByte('C')
        super.writeElem(c.value)

      case _ => super.writeElem(e)
    }

    override protected def writeCustomElem(e: Any): Any =
      e match {
        //    case f: File =>
        //      out.writeByte('f')
        //      out.writeUTF(f.getPath)
        //      ref0

        case u: URI =>
          out.writeByte('u')
          Artifact.Value.write(u, out)

        case _ => super.writeCustomElem(e)
      }
  }

  class RefMapIn(in0: DataInput) extends serial.RefMapIn[RefMapIn](in0) {
    type Const  = graph.Const[_]
    type U      = Artifact.Value

// EEE
//    def readAct     (): Act     = readProductT()
//    def readControl (): Control = readProductT()
//    def readTrig    (): Trig    = readProductT()
    def readEx   [A](): Ex[A]   = readProductT()

    def readAdjunct[A <: Adjunct](): A = Adjunct.readT(in)

    override protected def readProductWithCookie(cookie: Char): Product = super.readProductWithCookie(cookie)

    override protected def readProductWithKey(key: String, arity: Int): Product = {
      val adj = in.readByte().toInt
      //      val adjuncts  = if (adk == 0) Nil else {
      //        val b = List.newBuilder[Adjunct]
      //        b.sizeHint(adk)
      //        var i = 0
      //        while (i < adk) {
      //          b += Adjunct.read(in)
      //          i += 1
      //        }
      //        b.result()
      //      }
      val r = mapRead.getOrElse(key, throw new NoSuchElementException(s"Unknown element '$key'"))
      r.read(this, key, arity, adj)
    }

    override protected def readIdentifiedConst(): graph.Const[_] = {
      val value = readElem()
      graph.Const(value)
    }

    override protected def readCustomElem(cookie: Char): Any =
      if (cookie == 'f') { // backwards compatibility
        val path = in.readUTF()
        Artifact.fileToURI(path)
      } else {
        super.readCustomElem(cookie)
      }

    override protected def readIdentifiedU(): U = {
      Artifact.Value.read(in)
    }
  }

  def makeVar[T <: Exec[T], A](id: Ident[T])(implicit tx: T): Var[T, A] =
    id.newVar[A](null.asInstanceOf[A])(tx, format)

  def readVar[T <: Exec[T], A](id: Ident[T], in: DataInput): Var[T, A] =
    id.readVar[A](in)(format)

  implicit def format   [A]: ConstFormat[A]       = Fmt   .asInstanceOf[ConstFormat[A]]
  implicit def vecFormat[A]: ConstFormat[Vec[A]]  = VecFmt.asInstanceOf[ConstFormat[Vec[A]]]
  implicit def setFormat[A]: ConstFormat[Set[A]]  = SetFmt.asInstanceOf[ConstFormat[Set[A]]]

  private object Fmt extends ConstFormat[Any] {
    def write(v: Any, out: DataOutput): Unit  = {
      val ref = new RefMapOut(out)
      ref.writeElem(v)
    }

    def read(in: DataInput ): Any = {
      val ref = new RefMapIn(in)
      ref.readElem()
    }
  }

  private abstract class CollectionFmt[That <: Traversable[Any]] extends ConstFormat[That] {
    def newBuilder: mutable.Builder[Any, That]
    def empty: That

    def read(in: DataInput): That = {
      var sz = in.readInt()
      if (sz == 0) empty else {
        val ref = new RefMapIn(in)
        val b = newBuilder
        b.sizeHint(sz)
        while (sz > 0) {
          b += ref.readElem()
          sz -= 1
        }
        b.result()
      }
    }

    def write(coll: That, out: DataOutput): Unit = {
      val sz = coll.size
      out.writeInt(sz)
      if (sz > 0) {
        val ref = new RefMapOut(out)
        coll.foreach { x =>
          ref.writeElem(x)
        }
      }
    }
  }

  private object VecFmt extends CollectionFmt[Vec[Any]] {
    def newBuilder: mutable.Builder[Any, Vec[Any]] = Vector.newBuilder
    def empty: Vec[Any] = Vector.empty
  }

  private object SetFmt extends CollectionFmt[Set[Any]] {
    def newBuilder: mutable.Builder[Any, Set[Any]] = Set.newBuilder
    def empty: Set[Any] = Set.empty
  }
}
