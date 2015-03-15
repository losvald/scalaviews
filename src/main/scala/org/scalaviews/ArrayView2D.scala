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

import scala.collection.SortedMap

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
  def apply(row: Int, col: Int): T
  def foreach(dim: Int, lateralInd: Int, f: DimEntry => Unit): Unit
  def indexes(dim: Int): Array[Int]
  def values(dim: Int): Array[T]
  def dimEntries(dim: Int): Array[DimEntry]
  val valueCount: Int

  // TODO: return a 1D view if dense or SparseVector otherwise
  def multByVector(values: Seq[T])(implicit n: Numeric[T]): Array[T]

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

  def impl[T: Manifest](sizes: Sizes, f: Sizes => T): ArrayView2D[T] =
    new Implicit(f, sizes)
  def impl[T: Manifest](sizes: Sizes, value: T): ArrayView2D[T] = {
    val const = scala.Array(value).apply(0)
    impl(sizes, inds => const)
  }
  def empty[T: Manifest] = Empty.asInstanceOf[ViewS[T]]
  def diag[T: Manifest](
    values: Array[T],
    defaultValue: Option[T] = None
  ): ArrayView2D[T] = new Diag[T](values, mkDefaultValue(defaultValue))
  def blockDiag[T: Manifest](
    blocks: Array[Array[Array[T]]],
    defaultValue: Option[T] = None
  ): ArrayView2D[T] = new BlockDiag[T](blocks, mkDefaultValue(defaultValue))
  def chain[T: Manifest](
    chainDim: Int,
    subviews: ArrayView2D[T]*
  ): ArrayView2D[T] = {
    // XXX: assume all subviews are staged (i.e., they mix-in the ViewS trait)
    new Chain(chainDim,
      subviews.toIndexedSeq.asInstanceOf[IndexedSeq[ViewS[T]]])
  }
  def vector[T: Manifest](
    col: Boolean, length: Int, defaultValue: T, entries: (Int, T)*
  ): ArrayView2D[T] =
    new SparseVector(
      if (col) 1 else 0,
      length,
      defaultValue,
      SortedMap(entries: _*))

  private[scalaviews] trait ViewS[T] extends ArrayView2D[T]
      with ArrayView2DLike[T, ViewS[T]] {
    override final def append(dim: Int, that: ArrayView2D[T]) = that match {
      case that: ViewS[T] => this.append0(dim, that)
      case _ => ??? // TODO: that.prepend0(this)
    }
    override final def apply(row: Int, col: Int): T = applyC(row, col)
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
    override def multByVector(values: Seq[T])(
      implicit n: Numeric[T]
    ): Array[T] = {
      require(sizes._2 == values.size)
      multByVectorC(n)(values.toArray)
    }

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

    // TODO: lazy val: could not find implicit value for parameter n: Numeric[T]
    private type MultByVector = Array[T] => Array[T]
    @volatile private var _multByVectorC: MultByVector = _
    private[scalaviews] def multByVectorC(implicit n: Numeric[T]) = {
      var fC: MultByVector = _multByVectorC
      if (fC == null) {
        synchronized {
          if (_multByVectorC == null) {
            fC = compile(multByVectorS)
            _multByVectorC = fC
          }
        }
      }
      fC
    }
    private[scalaviews] def multByVectorS(values: Rep[Array[T]])(
      implicit n: Numeric[T]
    ) = arraySumMappedS(0) { de: Rep[DimEntry] =>
      de._2 * values(de._1)
    }

    private[scalaviews] final lazy val applyC = compile(applyS)
    private[scalaviews] def applyS(inds: Rep[(Int, Int)]): Rep[T]
    private[scalaviews] final lazy val foreachC0 = compile(foreachS(0))
    private[scalaviews] final lazy val foreachC1 = compile(foreachS(1))
    private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]): Rep[Unit]

    private[scalaviews] def foreach2S(dim: Int)(
      f: Rep[DimEntry] => Rep[Unit]
    ): Rep[Unit] = {
      foreachEntryS { e: Rep[Entry] =>
        f((e.productElement(dim).asInstanceOf[Rep[Int]], e._3: Rep[T]))
      }
    }
    private[scalaviews] def foreachEntryS(f: Rep[Entry] => Rep[Unit]): Rep[Unit]
    private[scalaviews] implicit val t: Manifest[T]
    protected def append0(dim: Int, that: ViewS[T]) =
      chain(dim, this, that)

    private def arraySumMappedS(dim: Int)(f: Rep[DimEntry] => Rep[T])(
      implicit n: Numeric[T]
    ): Rep[Array[T]] = {
      import n.mkNumericOps
      val lateralSize = sizes.productElement(dim).asInstanceOf[Int]
      val a: Rep[Array[T]] = NewArray[T](lateralSize)
      foreachEntryS { e: Rep[Entry] =>
        val (lateralInd, nonLateralInd) = (
          if (dim == 0) (e._1, e._2) else (e._2, e._1))
        // TODO: += gives compilation error: not found: value ev$2
        a(lateralInd) = a(lateralInd) + f((nonLateralInd, e._3))
      }
      a
    }

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
    final override private[scalaviews] def applyS(inds: Rep[(Int, Int)]) = 0
    final override private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]): Rep[Unit] = {}
    final override private[scalaviews] def foreachEntryS(
      f: Rep[Entry] => Rep[Unit]): Rep[Unit] = {}
    final override private[scalaviews] val t = manifest[Any]
    final override protected def append0(dim: Int, that: ViewS[Any]) = that
  }

  private[scalaviews] case class SparseVector[T: Manifest](
    dim: Int,
    length: Int,
    defaultValue: T,
    valueMap: SortedMap[Int, T]
  ) extends ViewS[T] {
    override val sizes = if (dim == 0) (1, length) else (length, 1)
    override val valueCount = valueMap.size

    // TODO: is the following faster than using foreach2S (for gen. Scala code)?
    override def indexes(dim: Int) =
      if (dim == this.dim) indexesNonLateral else indexesLateral
    override def values(dim: Int) = values0
    private[scalaviews] lazy val indexesNonLateral =
      valueMap.keys.toArray
    private[scalaviews] lazy val indexesLateral =
      new scala.Array[Int](valueCount)
    override private[scalaviews] def indexesS(dim: Int) =
      staticData(indexes(dim))
    private[scalaviews] lazy val values0 = valueMap.values.toArray
    override private[scalaviews] def valuesS(dim: Int) = staticData(values(dim))

    override private[scalaviews] def applyS(inds: Rep[(Int, Int)]): Rep[T] = {
      staticData(defaultValue) // TODO: implement
    }
    override private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]): Rep[Unit] = ??? // TODO: implement
    override private[scalaviews] def foreachEntryS(
      f: Rep[Entry] => Rep[Unit]
    ): Rep[Unit] = {
      val (getInd0, getInd1) = {
        def f0(ind: Int): Rep[Int] = 0
        def fInd(ind: Int): Rep[Int] = ind
        if (dim == 0) (f0 _, fInd _) else (fInd _ , f0 _)
      }
      def mkRepEntry(e: (Int, T)) =
        (getInd0(e._1), getInd1(e._1), staticData(e._2))
      // TODO: unroll for-loops only if the map is small
      valueMap.foreach { e =>
        f(mkRepEntry(e))
      }
    }
    override private[scalaviews] val t = manifest[T]
  }

  private[scalaviews] case class Diag[T: Manifest](
    a: Array[T],
    defaultValue: T
  ) extends ViewS[T] {
    override val sizes = (a.length, a.length)
    override val valueCount = a.length
    override private[scalaviews] def applyS(inds: Rep[(Int, Int)]): Rep[T] =
      if (inds._1 == inds._2) staticData(a).apply(inds._1)
      else staticData(defaultValue)
    override private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]
    ): Rep[Unit] = {
      // val (lateralInd, f) = arg // FIXME: type inference doesn't work here
      val lateralInd = arg._1; val f = arg._2
      f((lateralInd, staticData(a).apply(lateralInd)))
    }
    override private[scalaviews] def foreachEntryS(
      f: Rep[Entry] => Rep[Unit]
    ): Rep[Unit] = {
      val aS = staticData(a)
      for (ind <- Range(0, a.length)) {
        val indS: Rep[Int] = ind
        f((indS, indS, aS(ind)))
      }
    }
    override private[scalaviews] val t = manifest[T]
  }

  private[scalaviews] case class BlockDiag[T: Manifest](
    blocks: Array[Array[Array[T]]],
    defaultValue: T // = new scala.Array[T](1)(0) // TODO: cannot find class tag
  ) extends ViewS[T] {
    require(blocks.size > 0)
    val blockSizes: Sizes = {
      // TODO: ensure all blocks match in their dimensions
      (blocks(0).size, blocks(0)(0).size)
    }
    override val sizes = {
      (blockSizes._1 * blocks.size, blockSizes._2 * blocks.size)
    }
    override val valueCount = blocks.size * blockSizes._1 * blockSizes._2
    override private[scalaviews] def applyS(inds: Rep[(Int, Int)]): Rep[T] = {
      val blockRow = inds._1 / blockSizes._1
      val blockCol = inds._2 / blockSizes._2
      if (blockRow == blockCol) {
        staticData(blocks).apply(blockRow).apply(
          inds._1 - blockRow * blockSizes._1).apply(
          inds._2 - blockCol * blockSizes._2)
      } else
        staticData(defaultValue)
    }
    override private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]): Rep[Unit] = ??? // TODO: implement
    override private[scalaviews] def foreachEntryS(
      f: Rep[Entry] => Rep[Unit]
    ): Rep[Unit] = {
      blocks.zipWithIndex.foreach { case (b, bInd) =>
        b.zipWithIndex.foreach { case (bRow, bRowInd) =>
          bRow.zipWithIndex.foreach { case (value, bColInd) =>
            f((
              bRowInd + bInd * blockSizes._1: Rep[Int],
              bColInd + bInd * blockSizes._2: Rep[Int],
              staticData(value))) // TODO: stage the whole block
          }
        }
      }
    }
    override private[scalaviews] val t = manifest[T]
  }

  private[scalaviews] case class Implicit[T: Manifest](
    f: Sizes => T,
    override val sizes: Sizes
  ) extends ViewS[T] {
    override val valueCount = 0
    override private[scalaviews] def applyS(inds: Rep[(Int, Int)]): Rep[T] = {
      staticData(f).apply(inds)
    }
    override private[scalaviews] def foreachS(dim: Int)(
      arg: Rep[(Int, DimEntry => Unit)]
    ): Rep[Unit] = {}
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
    override private[scalaviews] def applyS(inds: Rep[(Int, Int)]): Rep[T] = {
      staticData(new scala.Array(1).apply(0)) // TODO: implement
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

  // helper function to get around the missing class tag in param list, i.e.
  //   def fun[T : Manifest](..., defaultValue: T = new scala.Array[T](1)(0))
  // doesn't work, but we can use this helper to provide similar usage, i.e.
  //   def fun[T : Manifest](..., defaultValue: Option[T] = None)
  private def mkDefaultValue[T : Manifest](o: Option[T]): T = o match {
    case Some(defaultValue) => defaultValue
    case _ => new scala.Array[T](1)(0)
  }
}

object ArrayView2D extends ViewFactoryProvider[ArrayView2DFactory] {
  private[scalaviews] trait FactoryImpl extends ArrayView2DFactory
      with ViewExp
      with ScalaDriver

  override protected def mkFactory = new FactoryImpl {}
}
