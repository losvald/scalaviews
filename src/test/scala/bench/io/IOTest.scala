/*
 * IOTest.scala
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

package bench.io

import org.scalatest.{FunSuite,MustMatchers}

import scala.io.Source

class IOTest extends FunSuite with MustMatchers {
  test("fromMatrixMarket - matrix coordinate int general") {
    val (sizes, entryIt) = fromMatrixMarket[Int](Source.fromString(
      """%%MatrixMarket matrix coordinate integer general
% A 4x5 sparse matrix with 8 nonzeroes
4 5  8
1 1     10
  2  2 105
4 2   2505
4 4 -280
3 3 15
4 5 3332
1 4 6
"""))
    sizes must be ((4, 5))
    entryIt.toTraversable must contain theSameElementsAs List(
      (0, 0, 10),
      (0, 3, 6),
      (1, 1, 105),
      (2, 2, 15),
      (3, 1, 2505),
      (3, 3, -280),
      (3, 4, 3332))
  }

  test("fromMatrixMarket - matrix coordinate real symmetric") {
    val (sizes, entryIt) = fromMatrixMarket[Double](Source.fromString(
      """%%MatrixMarket matrix coordinate real symmetric
3 3 2
1 2 -3.45
2 2 6
"""))
    sizes must be ((3, 3))
    entryIt.toTraversable must contain theSameElementsAs List(
      (0, 1, -3.45),
      (1, 0, -3.45), // verify the off-diagonal element is mirrored
      (1, 1, 6))     // verify the diagonal element is not mirrored
  }

  test("fromMatrixMarket - matrix coordinate integer skew-symmetric") {
    val (sizes, entryIt) = fromMatrixMarket[Int](Source.fromString(
      """%%MatrixMarket matrix coordinate integer skew-symmetric
9 9 3
2 1 5
5 5 88
3 4 -1
"""))
    sizes must be ((9, 9))
    entryIt.toTraversable must contain theSameElementsAs List(
      (2, 3, -1),
      (1, 0, 5),
      (4, 4, 88), // verify the diagonal element is not mirrored
      (0, 1, -5), // verify the off-diagonal elements are mirrored & negated
      (3, 2, 1))
  }
}
