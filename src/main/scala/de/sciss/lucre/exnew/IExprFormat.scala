package de.sciss.lucre.exnew

import de.sciss.lucre.Txn
import de.sciss.serial.{DataInput, DataOutput, TFormat}

import scala.annotation.switch

object IExprFormat {
  private final val sync = new AnyRef

  @volatile private var factoryMap = Map.empty[Int, IExprFactory]

  def addFactory(f: IExprFactory): Unit = {
    val tpeId = f.typeId
    sync.synchronized {
      if (factoryMap.contains(tpeId))
        throw new IllegalArgumentException(s"IExpr $tpeId was already registered ($f overrides ${factoryMap(tpeId)})")

      factoryMap += tpeId -> f
    }
  }
}
final class IExprFormat[T <: Txn[T], A]()(implicit ctx: Context[T])
  extends TFormat[T, IExpr[T, A]] {

  def readT(in: DataInput)(implicit tx: T): IExpr[T, A] = {
    val typeId = in.readInt()
    if (typeId == 0) null else {
      val f: IExprFactory = (typeId: @switch) match {
        case ExSeq.CountExpanded    .typeId => ExSeq.CountExpanded
        case ExSeq.Expanded         .typeId => ExSeq.Expanded
        case graph.BinaryOp.Expanded.typeId => graph.BinaryOp.Expanded
        case graph.Const.Expanded   .typeId => graph.Const.Expanded
        case graph.Var.ExpandedImpl .typeId => graph.Var.ExpandedImpl
        case _ =>
          IExprFormat.factoryMap.getOrElse(typeId,
            throw new IllegalArgumentException(s"Unknown stream type 0x${typeId.toHexString.toUpperCase}"))
      }
      val any: IExpr[T, Any] = f.readIdentified(in)
      any.asInstanceOf[IExpr[T, A]]
    }
  }

  def write(v: IExpr[T, A], out: DataOutput): Unit =
    if (v == null) out.writeInt(0) else v.write(out)
}
