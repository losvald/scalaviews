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

trait FixedArrayView extends (Int => Int) { //with BaseExp {
  val size: Int
}

trait FixedArrayViewFactory extends ViewFactory with ScalaOpsPkg
    with LiftNumeric
    // with LiftString  with LiftBoolean
    // with IfThenElse with Equal
    // with NumericOps with PrimitiveOps with BooleanOps
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

  def apply(len1: Int, len2: Int) = {
    // (a1: Array[Int], a2: Array[Int]) => // this compiles for each a1/2
    require(len1 >= 0 & len2 >= 0)
    val v = new FixedArrayView {
      lazy val size = sizeC()
      lazy val sizeC = compile(sizeBody)
      protected def sizeBody(u: Rep[Unit]) = len1 + len2

      override def apply(i: Int): Int = applyC(i)
      lazy val applyC = compile(applyBody)
      protected def applyBody(i: Rep[Int]) = // TODO: real implementation
        if (i < len1) i else i - (len1 + len2)
    } // using a closure, this compiles once only (per apply() call)
    (a1: Array[Int], a2: Array[Int]) => v
  }
}
