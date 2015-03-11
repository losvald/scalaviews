/*
 * ArrayView2D.scala
 *
 * Copyright (C) 2015 Leo Osvald <leo.osvald@gmail.com>
 *
 * This file is part of ScalaViews.
 *
 * ScalaViews is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ScalaViews is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.scalaviews

import scala.virtualization.lms.common._

private[scalaviews] trait ArrayView2DLike[T, +This <: ArrayView2D[T]] {
    // extends ((Int, Int) => T) {
  val sizes: (Int, Int)
  // def reversed(dim: Int): This
  // def sliced(rFrom: Int, rUntil: Int, cFrom: Int, cUntil: Int): This
}

trait ArrayView2D[@specialized(Int, Double) T]
    extends ArrayView2DLike[T, ArrayView2D[T]] {
  type DimEntry = (Int, T)
  type Entry = (Int, Int, T)
  def append(dim: Int, other: ArrayView2D[T]): ArrayView2D[T]
  def foreach(dim: Int, lateralInd: Int, f: DimEntry => Unit): Unit
  def indexes(dim: Int): Array[Int]
  def values(dim: Int): Array[T]
  def dimEntries(dim: Int): Array[DimEntry]
  val valueCount: Int
  private[scalaviews] def foreachEntryPrint(): Unit
  protected[scalaviews] val depth: Int = 0
  def along(dim: Int) = if (dim == 0) DimOp0 else DimOp1
  case class DimOp(dim: Int) {
    def :+(that: ArrayView2D[T]) = append(dim, that)
  }
  object DimOp0 extends DimOp(0)
  object DimOp1 extends DimOp(1)
}

private[scalaviews]
trait ArrayView2DFactory extends ViewFactory with ScalaOpsPkg
    with LiftNumeric with LiftBoolean
    with StaticData
    with IfThenElse
    with NumericOps with PrimitiveOps with BooleanOps
    with Functions
    with Equal
    with RangeOps with OrderingOps with MiscOps with ArrayOps with StringOps
    with SeqOps with While with Variables with LiftVariables with TupleOps
    with ObjectOps
{

  def empty[T: Manifest] = Empty.asInstanceOf[ViewS[T]]
  def diag[T: Manifest](values: Array[T]): ArrayView2D[T] =
    new Diag[T](values)
  def chain[T: Manifest](
    chainDim: Int,
    subviews: ViewS[T]*
  ): ArrayView2D[T] = new Chain(chainDim, subviews.toIndexedSeq)

  private[scalaviews] trait ViewS[T] extends ArrayView2D[T]
      with ArrayView2DLike[T, ViewS[T]] {
    override final def append(dim: Int, that: ArrayView2D[T]) = that match {
      case that: ViewS[T] => this.append0(dim, that)
      case _ => ??? // TODO: that.prepend0(this)
    }
    override final def foreach(
      dim: Int, lateralInd: Int, f: DimEntry => Unit
    ): Unit = {
      val arg = (lateralInd, f)
      if (dim == 0) foreachC0(arg) else foreachC1(arg)
    }
    override def indexes(dim: Int): Array[Int] =
      if (dim == 0) indexesC0() else indexesC1()
    override def values(dim: Int): Array[T] =
      if (dim == 0) valuesC0() else valuesC1()
    override def dimEntries(dim: Int): Array[DimEntry] =
      if (dim == 0) dimEntriesC0() else dimEntriesC1()

    private[scalaviews] lazy val indexesC0 = compile { u: Rep[Unit] =>
      indexesS(0)
    }
    private[scalaviews] lazy val indexesC1 = compile { u: Rep[Unit] =>
      indexesS(1)
    }
    private[scalaviews] def indexesS(dim: Int) = arrayTabulate2S(dim)(_._1)

    private[scalaviews] lazy val valuesC0 = compile { u: Rep[Unit] =>
      valuesS(0)
    }
    private[scalaviews] lazy val valuesC1 = compile { u: Rep[Unit] =>
      valuesS(1)
    }
    private[scalaviews] def valuesS(dim: Int) = arrayTabulate2S(dim)(_._2)

    private[scalaviews] lazy val dimEntriesC0 = compile { u: Rep[Unit] =>
      dimEntriesS(0)
    }
    private[scalaviews] lazy val dimEntriesC1 = compile { u: Rep[Unit] =>
      dimEntriesS(1)
    }
    // FIXME: causes java.lang.ClassCastException:
    //   [LTuple2IntInt; cannot be cast to [Lscala.Tuple2;
    private[scalaviews] def dimEntriesS(dim: Int): Rep[Array[DimEntry]] =
      arrayTabulate2S(dim) { e: Rep[DimEntry] => e }

    private[scalaviews] final lazy val foreachC0 = compile(foreachS(0))
    private[scalaviews] final lazy val foreachC1 = compile(foreachS(1))
    private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]): Rep[Unit]

    private[scalaviews] def foreach2S(dim: Int)(
      f: Rep[DimEntry] => Rep[Unit]): Rep[Unit]
    private[scalaviews] def foreachEntryS(f: Rep[Entry] => Rep[Unit]): Rep[Unit]
    private[scalaviews] implicit val t: Manifest[T]
    protected def append0(dim: Int, that: ViewS[T]) =
      chain(dim, this, that)

    private def arrayTabulate2S[A: Manifest](dim: Int)(
      f: Rep[DimEntry] => Rep[A]
    ): Rep[Array[A]] = {
      val a = NewArray[A](valueCount)
      var cur: Int = 0
      foreach2S(dim) { e: Rep[DimEntry] =>
        a(cur) = f(e)
        cur += 1
      }
      a
    }

    // TODO: remove (for now, used in the testing code)
    override private[scalaviews] final def foreachEntryPrint() =
      foreachEntryPrintC()
    private[scalaviews] lazy val foreachEntryPrintC = compile {
      u: Rep[Unit] => foreachEntryS(printEntryS)
    }
    private def printEntryS(e: Rep[Entry]): Rep[Unit] =
      print("\n" + e._3 + " @ " + e._1 + "," + e._2)
  }

  private case object Empty extends ViewS[Any] {
    final override val sizes = (0, 0)
    final override val valueCount = 0
    // staged methods will not be called, so they can return anything
    final override private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]): Rep[Unit] = {}
    final override private[scalaviews] def foreach2S(dim: Int)(
      f: Rep[DimEntry] => Rep[Unit]): Rep[Unit] = {}
    final override private[scalaviews] def foreachEntryS(
      f: Rep[Entry] => Rep[Unit]): Rep[Unit] = {}
    final override private[scalaviews] val t = manifest[Any]
    final override protected def append0(dim: Int, that: ViewS[Any]) = that
  }

  private[scalaviews] case class Diag[T: Manifest](a: Array[T])
      extends ViewS[T] {
    override val sizes = (a.length, a.length)
    override val valueCount = a.length
    override private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]
    ): Rep[Unit] = {
      // val (lateralInd, f) = arg // FIXME: type inference doesn't work here
      val lateralInd = arg._1; val f = arg._2
      f((lateralInd, staticData(a).apply(lateralInd)))
    }
    override private[scalaviews] def foreach2S(dim: Int)(
      f: Rep[DimEntry] => Rep[Unit]
    ): Rep[Unit] = {
      val aS = staticData(a)
      for (ind <- Range(0, a.length))
        f((ind: Rep[Int], aS(ind)))
      unit()
    }
    override private[scalaviews] def foreachEntryS(
      f: Rep[Entry] => Rep[Unit]
    ): Rep[Unit] = {
      foreach2S(0) { de: Rep[DimEntry] =>
        f((de._1, de._1, de._2))
      }
    }
    override private[scalaviews] val t = manifest[T]
    override protected def append0(dim: Int, that: ViewS[T]) =
      chain(dim, this, that)
  }

  private[scalaviews] case class Implicit[T: Manifest](
    f: Sizes => T,
    override val sizes: Sizes
  ) extends ViewS[T] {
    override val valueCount = 0
    override private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]
    ): Rep[Unit] = {}
    private[scalaviews] def foreach2S(dim: Int)(
      f: Rep[DimEntry] => Rep[Unit]): Rep[Unit] = {}
    override private[scalaviews] def foreachEntryS(
      f: Rep[Entry] => Rep[Unit]
    ): Rep[Unit] = {}
    override private[scalaviews] val t = manifest[T]
  }

  private[scalaviews] case class Chain[T: Manifest](
    chainDim: Int,
    subviews: IndexedSeq[ViewS[T]]
  ) extends ViewS[T] {
    override val sizes = {
      var sizes: scala.Array[Int] = new scala.Array(2)
      val lateralDim = 1 - chainDim
      val (vFirst, vRest) = (subviews.head, subviews.tail)
      for (dim <- scala.List(0, 1))
        sizes(dim) = vFirst.sizes.productElement(dim).asInstanceOf[Int]
      for (v <- vRest) {
        sizes(chainDim) += v.sizes.productElement(chainDim).asInstanceOf[Int]
        require(v.sizes.productElement(lateralDim) ==
          vFirst.sizes.productElement(lateralDim))
      }
      new Tuple2(sizes(0), sizes(1))
    }
    override val valueCount = (0 /: subviews)(_ + _.valueCount)
    override private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]
    ): Rep[Unit] = {
      if (dim == chainDim) {
        for (v <- subviews)
          v.foreachS(dim)(arg)
      } else {
        val lateralInd = arg._1; val f = arg._2
        val res = binSearchS(0, subviews.size, lateralInd)
        val viewInd = res._1; val localInd = res._2
        val v = subviews(viewInd)
        // TODO: stuck; view is not staged but its selection depends on dynamic
        // behavior (index found by the binary search)
        // v.foreachS(dim)((localInd, f))
        unit()
      }
    }
    override private[scalaviews] def foreach2S(dim: Int)(
      f: Rep[DimEntry] => Rep[Unit]
    ): Rep[Unit] = {
      if (dim == chainDim) {
        var nonLateralInd: Int = 0
        for (v <- subviews) {
          v.foreach2S(dim) { e: Rep[DimEntry] =>
            f((e._1 + nonLateralInd, e._2))
          }
          nonLateralInd += v.sizes.productElement(chainDim).asInstanceOf[Int]
        }
      } else subviews.foreach(_.foreach2S(dim)(f(_)))
    }
    override private[scalaviews] def foreachEntryS(
      f: Rep[Entry] => Rep[Unit]
    ): Rep[Unit] = {
      var nonLateralInd: Int = 0
      for (v <- subviews) {
        v.foreachEntryS { e: Rep[Entry] => f((
          e._1 + (if (chainDim == 0) nonLateralInd else 0),
          e._2 + (if (chainDim == 0) 0 else nonLateralInd),
          e._3))
        }
        nonLateralInd += v.sizes.productElement(chainDim).asInstanceOf[Int]
      }
    }
    override private[scalaviews] val t = manifest[T]
    override protected def append0(dim: Int, that: ViewS[T]) = that match {
      case Chain(chainDim2, subviews2) if chainDim == chainDim2 =>
        new Chain(chainDim2, subviews2 :+ that)
      case _ => super.append0(dim, that)
    }
    private def binSearchS(lo: Int, hi: Int,
      lateralInd: Rep[Int]//, f: Rep[DimEntry => Unit]
    ): Rep[Tuple2[Int, Int]] = {
      // find lowest index s.t. lateralCumSizes(index) > lateralInd
      if (lo >= hi)
        (hi: Rep[Int], lateralInd - (if (hi > 0) lateralCumSizes(hi) else 0))
      else {
        val mid = (lo + hi - 1) >>> 1
        if (lateralCumSizes(mid) > lateralInd)
          binSearchS(lo, mid, lateralInd)
        else
          binSearchS(mid + 1, hi, lateralInd)
      }
    }
    private val lateralCumSizes: IndexedSeq[Int] = {
      val lateralDim = 1 - chainDim
      subviews.map(_.sizes.productElement(lateralDim)).scanLeft(0) {
        _ + _.asInstanceOf[Int]
      }
    }
  }

  private type Sizes = (Int, Int)
}

object ArrayView2D extends ViewFactoryProvider[ArrayView2DFactory] {
  def diag[T: Manifest](values: Array[T]) = Factory.diag(values)

  private[scalaviews] trait Driver extends ScalaViewExp
      with StaticDataExp with IfThenElseExpOpt
      with ExpOpt.BooleanAnd { self =>
    override val codegen = new Codegen
        with ScalaGenStaticData {
      val IR: self.type = self
    }
  }

  override protected def mkFactory = new ArrayView2DFactory with Driver
}
