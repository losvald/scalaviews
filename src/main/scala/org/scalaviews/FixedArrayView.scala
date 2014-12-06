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

// import _root_.scala.lms.tutorial.dslapi

// trait FixedArrayView[T] extends Base {
//   val size: Int
//   // val data: Rep[Array[T]]
//   // def data(): Rep[Array[T]]
// }

trait FixedArrayView[@specialized(Int, Double) T] extends (Int => T) {
  val size: Int
}

private[scalaviews]
trait FixedArrayViewFactory extends ViewFactory with ScalaOpsPkg
    with LiftNumeric with LiftBoolean
    with StaticData
    with IfThenElse
    // with IfThenElse with Equal
    with NumericOps with PrimitiveOps with BooleanOps
    // with RangeOps with OrderingOps with MiscOps with ArrayOps with StringOps
    // with SeqOps with Functions with While with Variables with LiftVariables
    // with ObjectOps
{
  // def apply[T](seq: Rep[Array[T]], len: Int, offset: Int = 0): FixedArrayView[T] = {
  //   // require(offset + len <= seq.size)
  //   new FixedArrayView[T] {
  //     val size = len
  //     // val data = seq // XXX: fixme
  //     // def apply(i: Rep[Int]): T =
  //   }
  // }

  trait ApplyS[T] { this: FixedArrayView[T] =>
    override def apply(i: Int): T = applyC(i)

    // public is not allowed when this method is overriden (see below and 3.2.7)
    private[scalaviews] def applyS(i: Rep[Int]): Rep[T]

    // TODO: We need a Manifest, but [T: Manifest] is not supported for traits,
    // so we cannot provide this boilerplate implementation of the apply method:
    // private[scalaviews] lazy val applyC = compile(applyS)
    private[scalaviews] val applyC: Int => T
  }

  private[scalaviews] abstract case class Array1[T: Manifest](
    override val size: Int,
    a: Rep[Array[T]]
    // TODO: staging might be an overkill here, but it makes other code easier
  ) extends FixedArrayView[T] with ApplyS[T]

  def apply[T: Manifest](len: Int) = {
    require(len >= 0)
    (arr: Array[T], offset: Int) => {
      require(arr != null)
      require(0 <= offset && offset <= len)
      new Array1[T](len, staticData(arr)) {
        override private[scalaviews] def applyS(i: Rep[Int]) = a(i + offset)
        override private[scalaviews] lazy val applyC = compile(applyS)
      }
    }
  }

  // case class remembers len1 and len2, so they can be used for optimizations
  private[scalaviews] case class Array2[T: Manifest](
    override val size: Int,
    a1: Rep[Array[T]], len1: Int,
    a2: Rep[Array[T]]
  ) extends FixedArrayView[T] with ApplyS[T] {
    override private[scalaviews] lazy val applyC = compile(applyS)
    override private[scalaviews] def applyS(i: Rep[Int]) = {
      if (len1 > 0 && i < len1) a1(i)
      else a2(i - len1)
    }
  }

  def apply[T: Manifest](len1: Int, len2: Int) = {
    require(len1 >= 0 & len2 >= 0)
    (arr1: Array[T], arr2: Array[T]) => { // this compiles for each a1/2
      require(arr1 != null & arr2 != null)
      new Array2(len1 + len2, staticData(arr1), len1, staticData(arr2))
    }
  }

  def reversedArray[T: Manifest](arr: Array[T]) = {
    new FixedArrayView.ReversedArray1[T](arr)
      // TODO: Is staging overhead worth avoiding computation of (size - 1)?
      // (instead of using staticData(arr) we could store (size - 1))
        with ApplyS[T] {
      override val size = arr.length
      override def apply(i: Int) = applyC(i)
      override private[scalaviews] lazy val applyC = compile(applyS)
      override private[scalaviews] def applyS(i: Rep[Int]) = a((size - 1) - i)

      private val a = staticData(arr)
    }
  }

  private[scalaviews] abstract case class Reversed[T](rev: FixedArrayView[T])
      extends FixedArrayView[T] {
    override val size = rev.size
  }

  trait ReversedApplyS[T] extends ApplyS[T] { this: FixedArrayView[T] =>
    private[scalaviews] val rev: ApplyS[T]
    override private[scalaviews] def applyS(i: Rep[Int]): Rep[T] =
       rev.applyS(size - 1 - i)
  }

  // TODO: move to companion object as FixedArrayViewFactory.Reversed.apply?
  def reversed[T: Manifest](v: FixedArrayView[T]) = v match {
    // cases which do not require staging
    case Reversed(vOrig) => vOrig
    case FixedArrayView.ReversedArray1(arr) => new FixedArrayView[T] {
      override val size = arr.length
      override def apply(i: Int) = arr(i)
    }
    // cases which require staging to eliminate overhead
    case vRev @ Array1(size, a) =>
      new Array1[T](size, a) with ReversedApplyS[T] {
        override private[scalaviews] lazy val applyC = compile(applyS)
        override private[scalaviews] val rev = vRev
      }
    case vRev @ Array2(size, a1, len1, a2) =>
      // TODO: not ideal because reversed(reversed(Array2)) doesn't give Array2
      new Reversed[T](vRev) with ReversedApplyS[T] {
        private[scalaviews] lazy val applyC = compile(applyS)
        override val rev = vRev // override both fields (so it must public)
      }
    case _ => new Reversed[T](v) {
      override def apply(i: Int) = apply0(i)

      lazy val apply0: (Int => T) = v match { // ensure match is done only once
        case v: ApplyS[_] => (i: Int) => applyC(i)
        case _ => (i: Int) => v.apply(size - 1 - i) // TODO: cache (size - 1)?
      } // TODO: should we match in outer case and mix in if instanceOf[ApplyS]?

      // HACK: use laziness to prevent illegal cast
      lazy val applyC = compile { i: Rep[Int] =>
        v.asInstanceOf[ApplyS[T]].applyS(size - 1 - i)
      }
      // A cleaner way would require staging FixedArrayView:
      // override private[scalaviews] def applyS(i: Rep[Int]) = v match {
      //   case v: ApplyS[_] => v.applyS(size - i)
      //   case _ => ??? // here we would need to stage
      // }
    }
  }

  // XXX: this belongs to test code, but we apparently cannot:
  // - mix the staged code (Rep etc.) from here with the one from subclass
  // - use Rep from outside the factory
  private[scalaviews] def _doubled[T: Manifest](v: FixedArrayView[T])(
    implicit n: Numeric[T]
  ) = new FixedArrayView[T] with ApplyS[T] {
    override val size = v.size
    override private[scalaviews] def applyS(i: Rep[Int]) = {
      import n.mkNumericOps
      v.asInstanceOf[ApplyS[T]].applyS(i) +
      v.asInstanceOf[ApplyS[T]].applyS(i)
    }
    private[scalaviews] lazy val applyC = compile(applyS)
  }

  def sliced[T: Manifest](v: FixedArrayView[T], from: Int, until: Int):
      FixedArrayView[T]  = {
    require(0 <= from && from <= until)
    val _size = until - from
    v match {
      case v @ Array1(_, a) => new Array1[T](_size, a) {
        override private[scalaviews] def applyS(i: Rep[Int]) =
          v.applyS(from + i)
        override private[scalaviews] lazy val applyC = compile(applyS)
      }
      case v @ Array2(_, a1, len1, a2) =>
        if (from >= len1) new Array1[T](_size, a2) {
          override private[scalaviews] def applyS(i: Rep[Int]) =
            a((from - len1) + i)
          override private[scalaviews] lazy val applyC = compile(applyS)
        }
        else if (until <= len1) new Array1[T](_size, a1) {
          override private[scalaviews] def applyS(i: Rep[Int]) =
            a(from + i)
          override private[scalaviews] lazy val applyC = compile(applyS)
        }
        else new Array2[T](_size, a1, len1 - from, a2) {
          // TODO: optimize so we don't nest applyS methods arbitrarily deep
          // For that, we need to either introduce a case class for sliced views
          // or add the slicing info as a parameter/field of existing case class
          override private[scalaviews] def applyS(i: Rep[Int]) =
            v.applyS(i + from)
          override private[scalaviews] lazy val applyC = compile(applyS)
        }
      case _ => new FixedArrayView[T] {
        override val size = _size
        override def apply(i: Int) = apply0(i)

        lazy val apply0: (Int => T) = v match { // ensure match is done once
          case v: ApplyS[T] => (i: Int) => applyC(i)
          case _ => (i: Int) => v.apply(i + from)
        }

        // HACK: use laziness to prevent illegal cast
        lazy val applyC = compile { i: Rep[Int] =>
          v.asInstanceOf[ApplyS[T]].applyS(i + from)
        }
      }
    }
  }
}

object FixedArrayView extends ViewFactoryProvider[FixedArrayViewFactory] {
  private[scalaviews] trait Driver extends ScalaViewExp
      with StaticDataExp with IfThenElseExpOpt
      with ExpOpt.BooleanAnd { self =>
    override val codegen = new Codegen
        with ScalaGenStaticData {
      val IR: self.type = self
    }
  }

  override protected def mkFactory = new FixedArrayViewFactory with Driver

  private[scalaviews] abstract case class ReversedArray1[T](arr: Array[T])
      extends FixedArrayView[T]
}
