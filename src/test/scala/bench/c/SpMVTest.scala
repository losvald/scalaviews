/*
 * SpMVTest.scala
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

package org.scalaviews.bench

import org.scalaviews.ArrayView2D

import org.scalatest.FunSuite

class SpMVTest extends FunSuite {
  val ArrayView2D = CArrayView2D.Factory
  type ArrayView2DS[T] = org.scalaviews.ArrayView2DFactory#ViewS[T]

  import scala.language.implicitConversions
  implicit def view2ViewS[T: Manifest](v: ArrayView2D[T]): ArrayView2DS[T] =
    v.asInstanceOf[ArrayView2DS[T]]

  test("multByVectorC - diag") {
    val d456 = ArrayView2D.diag(Array(4, 5, 6))
    print(d456.multByVectorC.body) // TODO: something is wrong in CDriver?
    // scala.virtualization.lms.internal.GenerationFailedException: CLikeGen:
    //   remap(m) : Type Array[Int] cannot be remapped.
    // print(d456.foreachEntryPrintC.body) // same problem here
  }
}
