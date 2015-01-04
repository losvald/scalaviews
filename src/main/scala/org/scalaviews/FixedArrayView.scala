/*
 * FixedArrayView.scala
 *
 * Copyright (C) 2014 Leo Osvald <leo.osvald@gmail.com>
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

private[scalaviews] trait FixedArrayViewLike[T, +This <: FixedArrayView[T]]
    extends (Int => T) {
  val size: Int
  def reversed: This
  def sliced(from: Int, until: Int = size): This
  def from(ind: Int): This = sliced(ind)
  def until(ind: Int): This = sliced(0, ind)
  def downTo(ind: Int): FixedArrayView[T] = sliced(ind).reversed
  def at(ind: Int): This = sliced(ind, ind + 1)
}

trait FixedArrayView[@specialized(Int, Double) T]
    extends FixedArrayViewLike[T, FixedArrayView[T]] {
  protected def checkSliceArguments(from: Int, until: Int): Unit = {
    require(0 <= from && from <= until)
  }
}

private[scalaviews]
trait FixedArrayViewFactory extends ViewFactory with ScalaOpsPkg
    with LiftNumeric with LiftBoolean
    with StaticData
    with IfThenElse
    with NumericOps with PrimitiveOps with BooleanOps
    // with Equal
    // with RangeOps with OrderingOps with MiscOps with ArrayOps with StringOps
    // with SeqOps with Functions with While with Variables with LiftVariables
    // with ObjectOps
{

  private[scalaviews] trait ApplyS[T] { this: FixedArrayView[T] =>
    override final def apply(i: Int): T = applyC(i)
    private[scalaviews] final lazy val applyC = compile(applyS)

    // public is not allowed when this method is overriden (see below and 3.2.7)
    private[scalaviews] def applyS(i: Rep[Int]): Rep[T]

    private[scalaviews] implicit val t: Manifest[T]
  }

  private[scalaviews] trait ViewS[T] extends FixedArrayView[T] with ApplyS[T]
      with FixedArrayViewLike[T, ViewS[T]]

  private[scalaviews] case class Array1[T: Manifest](
    override val size: Int,
    a: Rep[Array[T]]
  ) extends ViewS[T] {
    override lazy val reversed = new ReversedArray1(this) // cache it
    override def sliced(from: Int, until: Int) = {
      checkSliceArguments(from, until)
      new Array1Slice[T](this, from, until)
    }

    override private[scalaviews] def applyS(i: Rep[Int]) = a(i)
    override private[scalaviews] val t = manifest[T]
  }

  def apply[T: Manifest](len: Int) = {
    require(len >= 0)
    (arr: Array[T]) => {
      require(arr != null)
      new Array1[T](len, staticData(arr))
    }
  }

  private[scalaviews] case class Array1Slice[T: Manifest](
    a: Array1[T],
    from: Int, until: Int
  ) extends ViewS[T] {
    override val size = until - from
    override lazy val reversed = // call ctor directly to avoid check & cache it
      new ReversedArray1Slice(a.reversed, a.size - until, a.size - from)
    override final def sliced(from: Int, until: Int) =
      a.sliced(this.from + from, this.from + until)
    override private[scalaviews] def applyS(i: Rep[Int]) = a.applyS(from + i)
    override private[scalaviews] val t = manifest[T]
  }

  private[scalaviews] case class Array2[T: Manifest](
    override val size: Int,
    private[scalaviews] a1: Rep[Array[T]], len1: Int,
    private[scalaviews] a2: Rep[Array[T]]
  ) extends ViewS[T] {
    // TODO: consider adding len2 and letting len1/len2 be private[scalaviews]
    override lazy val reversed = // cache it
      new ReversedArray2[T](this, size - len1)
    override def sliced(from: Int, until: Int): ViewS[T] = {
      checkSliceArguments(from, until)
      if (from >= len1)
        new Array1[T](size - len1, a2).sliced(from - len1, until - len1)
      else if (until <= len1) new Array1[T](len1, a1).sliced(from, until)
      else new Array2Slice[T](this, from, until)
    }
    override private[scalaviews] def applyS(i: Rep[Int]) = {
      if (len1 > 0 && i < len1) a1(i)
      else a2(i - len1)
    }
    override private[scalaviews] val t = manifest[T]
  }

  def apply[T: Manifest](len1: Int, len2: Int) = {
    require(len1 >= 0 & len2 >= 0)
    (arr1: Array[T], arr2: Array[T]) => { // this compiles for each a1/2
      require(arr1 != null & arr2 != null)
      new Array2(len1 + len2, staticData(arr1), len1, staticData(arr2))
    }
  }

  private[scalaviews] case class Array2Slice[T: Manifest](
    a: Array2[T],
    from: Int, until: Int
  ) extends ViewS[T] {
    override val size = until - from
    override lazy val reversed = // call ctor directly to avoid check & cache it
      new ReversedArray2Slice(a.reversed, a.size - until, a.size - from)
    override def sliced(from: Int, until: Int) =
      a.sliced(this.from + from, this.from + until)
    override private[scalaviews] def applyS(i: Rep[Int]): Rep[T] =
      a.applyS(from + i)
    override private[scalaviews] val t = manifest[T]
  }

  private[scalaviews] trait ReversedApplyS[T] extends ApplyS[T] {
    this: FixedArrayView[T] =>
    private[scalaviews] val rev: ApplyS[T]
    override private[scalaviews] def applyS(i: Rep[Int]): Rep[T] =
       rev.applyS(size - 1 - i)
  }

  private[scalaviews] case class ReversedArray1[T: Manifest](
    private[scalaviews] val rev: Array1[T]
  ) extends ViewS[T] with ReversedApplyS[T] {
    override val size = rev.size
    override val reversed = rev
    override def sliced(from: Int, until: Int) = {
      checkSliceArguments(from, until)
      new ReversedArray1Slice(this, from, until)
    }

    override private[scalaviews] val t = manifest[T]
  }

  private[scalaviews] case class ReversedArray1Slice[T: Manifest](
    a: ReversedArray1[T],
    from: Int, until: Int
  ) extends ViewS[T] {
    override val size = until - from
    override lazy val reversed = // call ctor directly to avoid check & cache it
      new Array1Slice(a.reversed, a.size - until, a.size - from)
    override final def sliced(from: Int, until: Int) =
      a.sliced(this.from + from, this.from + until)
    override private[scalaviews] def applyS(i: Rep[Int]): Rep[T] =
      a.applyS(from + i)
    override private[scalaviews] val t = manifest[T]
  }

  private[scalaviews] case class ReversedArray2[T: Manifest](
    private[scalaviews] val rev: Array2[T],
    len2: Int // cache it to avoid the frequent computation (size - rev.len1)
  ) extends ViewS[T] with ReversedApplyS[T] {
    override val size = rev.size
    override val reversed = rev
    override def sliced(from: Int, until: Int): ViewS[T] = {
      checkSliceArguments(from, until)
      // TODO: consider optimizing by storing Array1 portions inside Array2?
      if (from >= len2)
        new Array1[T](size - len2, rev.a1).reversed.sliced(
          from - len2, until - len2)
      else if (until <= len2)
        new Array1[T](len2, rev.a2).reversed.sliced(from, until)
      else new ReversedArray2Slice[T](this, from, until)
    }
    override private[scalaviews] val t = manifest[T]
  }

  private[scalaviews] case class ReversedArray2Slice[T: Manifest](
    a: ReversedArray2[T],
    from: Int, until: Int
  ) extends ViewS[T] {
    override val size = until - from
    // XXX: this creates a copy of the original Array2Slice
    override lazy val reversed = // call ctor directly to avoid check & cache it
      new Array2Slice(a.reversed, a.size - until, a.size - from)
    override final def sliced(from: Int, until: Int) =
      a.sliced(this.from + from, this.from + until)
    override private[scalaviews] def applyS(i: Rep[Int]): Rep[T] =
      a.applyS(from + i)
    override private[scalaviews] val t = manifest[T]
  }

  private[scalaviews] case class Nested2[T: Manifest](
    v1: ViewS[T],
    v2: ViewS[T]
  ) extends ViewS[T] {
    override val size = v1.size + v2.size
    override lazy val reversed = // cache it
      new Nested2[T](v2.reversed, v1.reversed)
    override def sliced(from: Int, until: Int): ViewS[T] = {
      checkSliceArguments(from, until)
      if (from >= v1.size) v2.sliced(from - v1.size, until - v1.size)
      else if (until <= v1.size) v1.sliced(from, until)
      else new Nested2[T](v1.from(from), v2.until(until - v1.size))
    }
    override private[scalaviews] def applyS(i: Rep[Int]) = {
      if (v1.size > 0 && i < v1.size) v1.applyS(i)
      else v2.applyS(i - v1.size)
    }
    override private[scalaviews] val t = manifest[T]
  }

  // XXX: this belongs to test code, but we apparently cannot:
  // - mix the staged code (Rep etc.) from here with the one from subclass
  // - use Rep from outside the factory
  private[scalaviews] def _doubled[T: Manifest](v0: FixedArrayView[T])(
    implicit n: Numeric[T]
  ): FixedArrayView[T] = {
    val v = v0.asInstanceOf[FixedArrayView[T] with ApplyS[T]]
    class Doubled(from: Int, until: Int) extends ViewS[T] {
      override val size = until - from
      override private[scalaviews] def applyS(i0: Rep[Int]) = {
        import n.mkNumericOps
        val i = from + i0
        v.applyS(i) + v.applyS(i)
      }
      override lazy val reversed: Doubled = new Doubled(from, until)
          with ReversedApplyS[T] {
        override lazy val reversed = rev
        override def sliced(from: Int, until: Int) =
          Doubled.this.sliced(v.size - until, v.size - from).reversed
        override private[scalaviews] val rev = Doubled.this
      }
      override def sliced(from: Int, until: Int) =
        new Doubled(from, until)
      override private[scalaviews] val t = manifest[T]
    }
    new Doubled(0, v.size)
  }

  def nested[T: Manifest](as: Array[T]*): FixedArrayView[T] =
    nest(as.toIndexedSeq, 0, roundUpToPow2(as.size))

  private def roundUpToPow2(n: Int) = {
    var p = 1
    while (p < n) p <<= 1
    p
  }

  private def nest[T : Manifest](
    as: IndexedSeq[Array[T]], from: Int, until: Int
  ): ViewS[T] = {
    val size: Int = (if (as.size < until) as.size else until) - from
    (size: @scala.annotation.switch) match {
      case 1 => {
        val f = apply(as(from).size)
        f(as(from))
      }
      case 2 => {
        val f = apply(as(from).size, as(from + 1).size)
        f(as(from), as(from + 1))
      }
      case _ => {
        val mid = (from + until) / 2
        new Nested2[T](nest(as, from, mid), nest(as, mid, until))
      }
    }
  }
}

object FixedArrayView extends ViewFactoryProvider[FixedArrayViewFactory] {
  def apply[T: Manifest](a: Array[T]): FixedArrayView[T] = {
    val f = Factory(a.size)
    f(a)
  }

  def apply[T: Manifest](a1: Array[T], a2: Array[T]): FixedArrayView[T] = {
    val f = Factory(a1.size, a2.size)
    f(a1, a2)
  }

  def apply[T: Manifest](
    a1: Array[T], a2: Array[T], a3: Array[T],
    aRest: Array[T]*
  ): FixedArrayView[T] = {
    // TODO: optimize for 3 or 4 chunks (avoid ++ and toIndexedSeq overhead)
    Factory.nested(IndexedSeq(a1, a2, a3) ++ aRest.toIndexedSeq: _*)
  }

  trait Implicits {
    import scala.language.implicitConversions

    implicit def array2FixedArrayView[T: Manifest](a: Array[T]):
        FixedArrayView[T] =
      Implicits.arrayCache.get().getOrElseUpdate(a, FixedArrayView(a))
        .asInstanceOf[FixedArrayView[T]]
  }

  object Implicits extends Implicits {
    import scala.collection.mutable.{Map => MutableMap,WeakHashMap}

    private val arrayCache = new ThreadLocal[MutableMap[Array[_], Any]] {
      override def initialValue = new WeakHashMap[Array[_], Any]
    }
  }

  private[scalaviews] trait Driver extends ScalaViewExp
      with StaticDataExp with IfThenElseExpOpt
      with ExpOpt.BooleanAnd { self =>
    override val codegen = new Codegen
        with ScalaGenStaticData {
      val IR: self.type = self
    }
  }

  override protected def mkFactory = new FixedArrayViewFactory with Driver
}
