package de.sciss.lucre.exnew

import de.sciss.log.Level
import de.sciss.lucre.exnew.ExElem.ProductReader
import de.sciss.lucre.exnew.graph.{Ex, Folder}
import de.sciss.lucre.exnew.impl.ExObjBridgeImpl
import de.sciss.lucre.expr.{IntExtensions, LucreExpr}
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Adjunct, BooleanObj, Durable, Expr, InMemory, IntObj, Log, Txn, Folder => LFolder, Obj => LObj}
import de.sciss.serial.DataInput

object Test {
  type S = Durable
  type T = Durable.Txn

//  type S = InMemory
//  type T = InMemory.Txn

  def main(args: Array[String]): Unit = {
//    LucreExpr .init()
    IntObj        .init()
    BooleanObj    .init()
    LFolder       .init()
    IntExtensions .init()
//    Expr.Type     .init()
    Ex            .init()
    graph.Obj     .init()
    IntExObj      .init()

    object ObjBridge extends Adjunct.Factory {
      final val id = 1000

      type ExprAny[T <: Txn[T]] = ({ type R[~ <: Txn[~]] <: Expr[~,     scala.Any ] }) # R[T]

      def readIdentifiedAdjunct(in: DataInput): Adjunct = {
        val typeId  = in.readInt()
        val peer    = LObj.getType(typeId)
        val peerT   = peer.asInstanceOf[Expr.Type[scala.Any, ExprAny]]
        new ExObjBridgeImpl[scala.Any, ExprAny](peerT)
      }
    }
    Adjunct.addFactory(ObjBridge)
    Adjunct.addFactory(Folder.Bridge)

    ExElem.addProductReaderSq({
      import graph.{BinaryOp => BinOp, UnaryOp => UnOp, TernaryOp => TernOp, _}
      Seq[ProductReader[Product]](
        Attr, /*Attr.Set, Attr.Update, Attr.UpdateOption,*/ Attr.WithDefault,
        BinOp,
        BinOp.Plus, BinOp.Minus, BinOp.Times, BinOp.Div, BinOp.ModJ, BinOp.Mod, BinOp.Eq, BinOp.Neq, BinOp.Lt,
        BinOp.Gt, BinOp.Leq, BinOp.Geq, BinOp.Min, BinOp.Max, BinOp.And, BinOp.Or, BinOp.Xor, BinOp.IDiv, BinOp.Lcm,
        BinOp.Gcd, BinOp.RoundTo, BinOp.RoundUpTo, BinOp.Trunc, BinOp.Atan2, BinOp.Hypot, BinOp.Hypotx, BinOp.Pow,
        BinOp.LeftShift, BinOp.RightShift, BinOp.UnsignedRightShift, BinOp.Difsqr, BinOp.Sumsqr, BinOp.Sqrsum,
        BinOp.Sqrdif, BinOp.Absdif, BinOp.Clip2, BinOp.Excess, BinOp.Fold2, BinOp.Wrap2,
        BinOp.RangeExclusive, BinOp.RangeInclusive,
        BinOp.OptionContains, BinOp.OptionGetOrElse, BinOp.OptionOrElse,
        BinOp.SeqAppended, BinOp.SeqApply, BinOp.SeqApplyOption, BinOp.SeqConcat, BinOp.SeqContains, BinOp.SeqDiff,
        BinOp.SeqDrop, BinOp.SeqDropRight, BinOp.SeqEndsWith, BinOp.SeqGrouped, BinOp.SeqIndexOf,
        BinOp.SeqIndexOfSlice, BinOp.SeqIntersect, BinOp.SeqIsDefinedAt, BinOp.SeqLastIndexOf,
        BinOp.SeqLastIndexOfSlice, BinOp.SeqPrepended, BinOp.SeqSameElements, BinOp.SeqSplitAt, BinOp.SeqTake,
        BinOp.SeqTakeRight, BinOp.SeqZip,
        BinOp.StringConcat, BinOp.StringContains, BinOp.StringStartsWith,
        BinOp.StringEndsWith, BinOp.StringIndexOf, BinOp.StringLastIndexOf, BinOp.StringTake, BinOp.StringDrop,
        BinOp.SpanLikeClip, BinOp.SpanLikeShift, BinOp.SpanLikeContains,
        BinOp.SpanLikeOverlaps, BinOp.SpanLikeTouches, BinOp.SpanLikeUnion, BinOp.SpanLikeIntersect,
        BinOp.FileReplaceExt, BinOp.FileReplaceName, BinOp.FileChild,
        Ex.MapExOption, Ex.MapExSeq, /*Ex.MapActOption, Ex.MapSeqAct,*/ Ex.FlatMapExOption, Ex.FlatMapExSeq,
        Ex.FlatMapExSeqOption,
        ExOption.Select,
        ExSeq,
        ExSeq.Count, ExSeq.DropWhile, ExSeq.Exists, ExSeq.Filter, ExSeq.FilterNot, ExSeq.Forall, ExSeq.Find,
        ExSeq.FindLast, ExSeq.IndexWhere, ExSeq.Select, ExSeq.SelectFirst, ExSeq.TakeWhile,
        Folder, Folder.Size, Folder.IsEmpty, Folder.NonEmpty, Folder.Children, /*Folder.Append, Folder.Prepend,*/
        It,
        Obj.Empty, Obj.Attr, /*Obj.Attr.Update, Obj.Attr.UpdateOption, Obj.Attr.Set,*/ Obj.As, /*Obj.Make, Obj.Copy,*/
        QuaternaryOp, QuaternaryOp.SeqMkString, QuaternaryOp.SeqPatch,
        QuinaryOp, QuinaryOp.LinLin, QuinaryOp.LinExp, QuinaryOp.ExpLin, QuinaryOp.ExpExp,
        Quote,
        StringFormat,
        TernOp,
        TernOp.Clip, TernOp.Fold, TernOp.Wrap, TernOp.StringSlice, TernOp.StringSplit, TernOp.SeqIndexOf,
        TernOp.SeqIndexOfSlice, TernOp.SeqLastIndexOf, TernOp.SeqLastIndexOfSlice, TernOp.SeqPadTo, TernOp.SeqSlice,
        TernOp.SeqSliding, TernOp.SeqStartsWith, TernOp.SeqUpdated,
        UnOp,
        UnOp.Neg, UnOp.Not, UnOp.BitNot, UnOp.Abs, UnOp.ToDouble, UnOp.ToInt, UnOp.ToLong, UnOp.Ceil, UnOp.Floor,
        UnOp.Frac, UnOp.Signum, UnOp.Squared, UnOp.Cubed, UnOp.Sqrt, UnOp.Exp, UnOp.Reciprocal, UnOp.Midicps,
        UnOp.Cpsmidi, UnOp.Midiratio, UnOp.Ratiomidi, UnOp.Dbamp, UnOp.Ampdb, UnOp.Octcps, UnOp.Cpsoct, UnOp.Log,
        UnOp.Log2, UnOp.Log10, UnOp.Sin, UnOp.Cos, UnOp.Tan, UnOp.Asin, UnOp.Acos, UnOp.Atan, UnOp.Sinh, UnOp.Cosh,
        UnOp.Tanh, UnOp.ToStr, UnOp.OptionSome, UnOp.OptionIsEmpty, UnOp.OptionIsDefined, UnOp.OptionToList,
        UnOp.OptionGet, UnOp.Tuple2_1, UnOp.Tuple2_2, UnOp.Tuple2Swap, UnOp.SeqDistinct, UnOp.SeqHeadOption,
        UnOp.SeqIndices, UnOp.SeqIsEmpty, UnOp.SeqLastOption, UnOp.SeqMaxOption, UnOp.SeqMinOption, UnOp.SeqNonEmpty,
        UnOp.SeqPermutations, UnOp.SeqProduct, UnOp.SeqReverse, UnOp.SeqSize, UnOp.SeqSorted, UnOp.SeqSum,
        UnOp.SeqZipWithIndex, UnOp.SeqIntegrate, UnOp.SeqDifferentiate, UnOp.StringIsEmpty, UnOp.StringNonEmpty,
        UnOp.StringLength, UnOp.StringToIntOption, UnOp.StringToDoubleOption, UnOp.StringToBooleanOption,
        UnOp.SpanLikeIsEmpty, UnOp.SpanLikeNonEmpty, UnOp.SpanLikeClosedOption, UnOp.SpanLikeStartOption,
        UnOp.SpanLikeStopOption, UnOp.SpanLikeLengthOption, UnOp.SpanStart, UnOp.SpanStop, UnOp.SpanLength,
        UnOp.FileParentOption, UnOp.FilePath, UnOp.FileName, UnOp.FileBase, UnOp.FileExtL,
        Var, /*Var.Set, Var.Update, Var.Inc, Var.Dec,*/
      )
    })

//    Log.event.level = Level.Debug

//    implicit val system: S = InMemory()
    val store = BerkeleyDB.tmp()
    implicit val system: S = Durable(store)

    val (inH, outH) = system.step { implicit tx =>
      import ExImport._
      import de.sciss.lucre.exnew.graph._
//      val ex: Ex[Int] = "in".attr(0) * 2
      val ex: Ex[Int] = "in".attr(Folder()).size
//      val input     = IntObj.newVar[T](0)
      val input     = LFolder[T]()
      val transform = IntExObj[T](ex)
      println("--- put 'in'")
      transform.attr.put("in", input)
      val output    = IntObj.newVar[T](transform)
      (tx.newHandle(input), tx.newHandle(output))
    }

    system.step { implicit tx =>
      val out = outH()
      println("--- add react")
      out.changed.react { implicit tx => upd =>
        println(s"OBSERVED: $upd")
      }
    }

    system.step { implicit tx =>
      val in = inH()
      println("--- update 'in'")
//      in() = 1000
      in.addLast(BooleanObj.newConst(false))
    }

    val v = system.step { implicit tx =>
      val out = outH()
      println("--- call 'value'")
      out.value
    }
    println(s"OUTPUT now $v")

    system.close()
  }
}
