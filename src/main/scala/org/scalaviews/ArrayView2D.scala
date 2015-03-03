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
  def foreach(dim: Int, lateralInd: Int, f: DimEntry => Unit): Unit
  protected[scalaviews] val depth: Int = 0
}

private[scalaviews]
trait ArrayView2DFactory extends ViewFactory with ScalaOpsPkg
    with LiftNumeric with LiftBoolean
    with StaticData
    with IfThenElse
    with NumericOps with PrimitiveOps with BooleanOps
    with Functions
    // with Equal
    // with RangeOps with OrderingOps with MiscOps with ArrayOps with StringOps
    // with SeqOps with While with Variables// with LiftVariables
    // with ObjectOps
{

  def empty[T: Manifest] = Empty.asInstanceOf[ViewS[T]]
  def diag[T: Manifest](values: Array[T]): ArrayView2D[T] =
    new Diag[T](values)

  private[scalaviews] trait ViewS[T] extends ArrayView2D[T]
      with ArrayView2DLike[T, ViewS[T]] {
    override final def foreach(
      dim: Int, lateralInd: Int, f: DimEntry => Unit
    ): Unit = {
      val arg = (lateralInd, f)
      if (dim == 0) foreachC0(arg) else foreachC1(arg)
    }
    private[scalaviews] final lazy val foreachC0 = compile(foreachS(0))
    private[scalaviews] final lazy val foreachC1 = compile(foreachS(1))
    private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]): Rep[Unit]
    private[scalaviews] implicit val t: Manifest[T]
  }

  private case object Empty extends ViewS[Any] {
    final override val sizes = (0, 0)
    // staged methods will not be called, so they can return anything
    final override private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]): Rep[Unit] = {}
    final override private[scalaviews] val t = manifest[Any]
  }

  private[scalaviews] case class Diag[T: Manifest](a: Array[T])
      extends ViewS[T] {
    override val sizes = (a.length, a.length)
    override private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]
    ): Rep[Unit] = {
      // val (lateralInd, f) = arg // FIXME: type inference doesn't work here
      val lateralInd = arg._1; val f = arg._2
      f((lateralInd, staticData(a).apply(lateralInd)))
    }
    override private[scalaviews] val t = manifest[T]
  }

  private[scalaviews] case class Implicit[T: Manifest](
    f: Sizes => T,
    override val sizes: Sizes
  ) extends ViewS[T] {
    override private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]
    ): Rep[Unit] = {}
    override private[scalaviews] val t = manifest[T]
  }

  private[scalaviews] case class Chain[T: Manifest](
    chainDim: Int,
    subviews: IndexedSeq[ViewS[T]]
  ) extends ViewS[T] {
    override val sizes = {
      var sizes: scala.Array[Int] = scala.Array(2)
      val lateralDim = 1 - chainDim
      val (vFirst, vRest) = (subviews.head, subviews.tail)
      for (v <- vRest) {
        sizes(chainDim) += v.sizes.productElement(chainDim).asInstanceOf[Int]
        require(v.sizes.productElement(lateralDim) ==
          vFirst.sizes.productElement(lateralDim))
      }
      new Tuple2(sizes(0), sizes(1))
    }
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
    override private[scalaviews] val t = manifest[T]
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
