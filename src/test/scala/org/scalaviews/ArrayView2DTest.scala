/*
 * ArrayViewTest.scala
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

import org.scalatest.BeforeAndAfterAll
import org.scalatest.{FunSuite,MustMatchers}

class ArrayView2DTest extends FunSuite with MustMatchers {
  import ArrayView2D._

  // bring into scope inner case classes from the factory
  type Chain[T] = ArrayView2DFactory#Chain[T]
  type Diag[T] = ArrayView2DFactory#Diag[T]

  test("foreach2 - chain of diags") {
    val d456 = Factory.diag(Array(4, 5, 6))
    val d789 = Factory.diag(Array(7, 8, 9))
    val c0D456D789 = d456.along(0) :+ d789
    c0D456D789.sizes must be ((6, 3))
    c0D456D789.indexes(0) must contain theSameElementsInOrderAs Array(
      0, 1, 2, 3, 4, 5)
    c0D456D789.values(0) must contain theSameElementsInOrderAs Array(
      4, 5, 6, 7, 8, 9)
    c0D456D789.valueCount must be (6)

    // TODO: uncomment when dimEntries is fixed, and get rid of assert below
    // c0D456D789.dimEntries(0) must contain theSameElementsInOrderAs Array(
    //   (0, 4), (1, 5), (2, 6), (3, 7), (4, 8), (5, 9))
    assert(foreach2Output(c0D456D789, 0) === """
4 @ 0
5 @ 1
6 @ 2
7 @ 3
8 @ 4
9 @ 5""")
  }

  private def foreach2Output[T](v: ArrayView2D[T], dim: Int): String = {
    val ps = new java.io.ByteArrayOutputStream
    Console.withOut(ps) { v.foreach2Print(dim) }
    ps.toString
  }
}
