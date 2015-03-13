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
    assert(foreachEntryOutput(c0D456D789) === """
4 @ 0,0
5 @ 1,1
6 @ 2,2
7 @ 3,0
8 @ 4,1
9 @ 5,2""")

    c0D456D789.indexes(1) must contain theSameElementsInOrderAs Array(
      0, 1, 2, 0, 1, 2)
    c0D456D789.values(1) must contain theSameElementsInOrderAs Array(
      4, 5, 6, 7, 8, 9)
  }

  test("foreach2 - block diag") {
    val A = Array
    val bd3by2 = Factory.blockDiag(A(
      A(
        A(0, 10, 20),
        A(30, 40, 50)),
      A(
        A(66, 77, 88),
        A(99, 100, 101)),
      A(
        A(120, 130, 140),
        A(150, 160, 170))))
    assert(foreachEntryOutput(bd3by2) === """
0 @ 0,0
10 @ 0,1
20 @ 0,2
30 @ 1,0
40 @ 1,1
50 @ 1,2
66 @ 2,3
77 @ 2,4
88 @ 2,5
99 @ 3,3
100 @ 3,4
101 @ 3,5
120 @ 4,6
130 @ 4,7
140 @ 4,8
150 @ 5,6
160 @ 5,7
170 @ 5,8""")
  }

  test("foreach2 - col vector") {
    val v235 = Factory.vector(true, 6, (2, 2000), (3, 30), (5, 500))
    v235.sizes must be ((6, 1))
    assert(foreachEntryOutput(v235) === """
2000 @ 2,0
30 @ 3,0
500 @ 5,0""")
    v235.indexes(0) must contain theSameElementsInOrderAs Array(
      0, 0, 0)
    v235.indexes(1) must contain theSameElementsInOrderAs Array(
      2, 3, 5)
    v235.values(0) must contain theSameElementsInOrderAs Array(
      2000, 30, 500)
    v235.values(1) must contain theSameElementsInOrderAs Array(
      2000, 30, 500)
    v235.valueCount must be (3)
  }

  test("multByVector") {
    val d456 = Factory.diag(Array(4, 5, 6))
    val d789 = Factory.diag(Array(7, 8, 9))
    val spMat = d456.along(1) :+ d789
    val vRes = spMat.multByVector(Seq(0, 0, 2000, 30, 0, 500))
    vRes must contain theSameElementsInOrderAs Array(
      7 * 30,
      0,
      6 * 2000 + 9 * 500)
  }

  // http://www.cise.ufl.edu/research/sparse/matrices/Meszaros/p0040.html
  lazy val p0040_cdd = {
    val A = Array
    val fourOnes = A(1, 1, 1, 1)
    val fourMinusOnes = A(-1, -1, -1, -1)
    val bd2x4 = Factory.blockDiag(A(
      A(fourMinusOnes, fourOnes), A(fourMinusOnes, fourOnes),
      A(fourMinusOnes, fourOnes), A(fourMinusOnes, fourOnes),
      A(fourMinusOnes, fourOnes), A(fourMinusOnes, fourOnes),
      A(fourMinusOnes, fourOnes), A(fourMinusOnes, fourOnes),
      A(fourMinusOnes, fourOnes), A(fourMinusOnes, fourOnes)))
    bd2x4.sizes must be ((20, 40))
    val e1 = Factory.impl((3, 1), 0)
    val cd3x3e1 = Factory.chain(1,
      Factory.diag(A.fill(3)(-1669)), e1,
      Factory.diag(A.fill(3)(-2191)), e1,
      Factory.diag(A.fill(3)(-1553)), e1,
      Factory.diag(A.fill(3)(-1829)), e1,
      Factory.diag(A.fill(3)(-1772)), e1,
      Factory.diag(A.fill(3)(-1665)), e1,
      Factory.diag(A.fill(3)(-2220)), e1,
      Factory.diag(A.fill(3)(-1634)), e1,
      Factory.diag(A.fill(3)(-2211)), e1,
      Factory.diag(A.fill(3)(-2142)), e1)
    cd3x3e1.sizes must be ((3, 40))
    val cbd2x4cd3x3e1 = bd2x4.along(0) :+ cd3x3e1
    cbd2x4cd3x3e1.sizes must be ((23, 40))
    val cdcbd2x4cd3x3e1 = Factory.diag(A.fill(23)(1)).along(1) :+ cbd2x4cd3x3e1
    cdcbd2x4cd3x3e1.sizes must be ((23, 63))
    cdcbd2x4cd3x3e1
  }

  test("multByVector - nested chain of (block) diags") {
    val spMat = p0040_cdd
    val act = spMat.multByVector(Array(60,-42,-54,44,62,23,-23,-15,73,51,50,-15,11,-14,-73,-28,-88,-70,-76,88,-92,84,-79,57,-55,23,43,-93,12,31,-98,-74,-86,-41,94,-28,14,-45,-60,-86,-50,-70,49,-68,-61,88,16,-26,94,-90,70,48,-52,-40,-1,-59,24,97,66,79,-69,52,-97))
    val exp = Array(-8,26,94,-104,169,-84,96,-134,230,-106,75,-40,-37,34,-28,-73,-216,58,-41,53,480803,334726,-43580)
    act must contain theSameElementsInOrderAs exp
  }

  private def foreachEntryOutput[T](v: ArrayView2D[T]): String = {
    val ps = new java.io.ByteArrayOutputStream
    Console.withOut(ps) { v.foreachEntryPrint() }
    ps.toString
  }
}
