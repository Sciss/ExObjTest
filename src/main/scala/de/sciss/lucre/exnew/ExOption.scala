package de.sciss.lucre.exnew

import de.sciss.lucre.{Adjunct, IExpr, ITargets, ProductWithAdjuncts, Txn}
import de.sciss.lucre.exnew.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.exnew.graph.{Ex, Obj}
import de.sciss.lucre.exnew.graph.impl.MappedIExpr

object ExOption {
  // XXX TODO --- we should use cell-views instead, because this way we won't notice
  // changes to the value representation (e.g. a `StringObj.Var` contents change)
  private final class SelectExpanded[T <: Txn[T], A](in: IExpr[T, Option[Obj]], tx0: T)
                                                    (implicit targets: ITargets[T], bridge: Obj.Bridge[A])
    extends MappedIExpr[T, Option[Obj], Option[A]](in, tx0) {

    protected def mapValue(inValue: Option[Obj])(implicit tx: T): Option[A] =
      inValue.flatMap(_.peer[T].flatMap(bridge.tryParseObj(_)))
  }

  object Select extends ProductReader[Select[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Select[_] = {
      require (arity == 1 && adj == 1)
      val _in = in.readEx[Option[Obj]]()
      val _bridge: Obj.Bridge[Any] = in.readAdjunct()
      new Select[Any](_in)(_bridge)
    }
  }
  final case class Select[A](in: Ex[Option[Obj]])(implicit bridge: Obj.Bridge[A])
    extends Ex[Option[A]] with ProductWithAdjuncts {

    type Repr[T <: Txn[T]] = IExpr[T, Option[A]]

    override def productPrefix: String = s"ExOption$$Select" // serialization

    def adjuncts: List[Adjunct] = bridge :: Nil

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      import ctx.targets
      new SelectExpanded[T, A](inEx, tx)
    }
  }
}
