/*
 * ArrayViewTest.scala
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

import org.scalatest.BeforeAndAfterAll
import org.scalatest.{FunSuite,MustMatchers}

object ArrayViewTest {
  type Factory = ArrayViewFactory

  val a1Len5 = Array.range(0, 50, 10)
  val a2Len3 = Array.range(500, 800, 100)
  // val a1Len9 = Array.range(0, 90, 10)
  // val a1Len2 = Array(0, 10)
  // val a2Len1 = Array(500)

  val v4Arrays = Seq(
    Array(100, 101), Array(102, 103, 104),
    Array(105, 106, 107, 108), Array(109))

  val v7Arrays = v4Arrays ++ Seq(
    Array(110, 111, 112), Array(113, 114),
    Array(115, 116, 117))
}

class ArrayViewTest extends FunSuite with ClassMatchers {
  import ArrayView._
  import ArrayViewTest._

  // bring into scope inner case classes from the factory
  type Array1[T] = ArrayViewFactory#Array1[T]
  type Array1Slice[T] = ArrayViewFactory#Array1Slice[T]
  type Array2[T] = ArrayViewFactory#Array2[T]
  type Array2Slice[T] = ArrayViewFactory#Array2Slice[T]
  type ReversedArray1[T] = ArrayViewFactory#ReversedArray1[T]
  type ReversedArray1Slice[T] = ArrayViewFactory#ReversedArray1Slice[T]
  type ReversedArray2[T] = ArrayViewFactory#ReversedArray2[T]
  type ReversedArray2Slice[T] = ArrayViewFactory#ReversedArray2Slice[T]
  type Nested2[T] = ArrayViewFactory#Nested2[T]

  val len5F = Factory[Int](5)
  val len3F = Factory[Int](3)

  lazy val v1Len5 = len5F(a1Len5)
  lazy val v1Len5From2 = len5F(a1Len5).sliced(2)
  lazy val v2Len3 = len3F(a2Len3)

  lazy val v1Len5From2Until4 = v1Len5.sliced(2, 4)
  lazy val v2Len3From1Until2 = v2Len3.sliced(1, 2)

  val len5And3F = Factory[Int](5, 3)

  lazy val len5And3 = len5And3F(a1Len5, a2Len3)

  lazy val len5And3Rev = len5And3.reversed
  lazy val a1Len5Rev = v1Len5.reversed

  lazy val len5And3From1Until5 = len5And3.sliced(1, 5)
  lazy val len5And3From6Until7 = len5And3.sliced(6, 7)
  lazy val len5And3From4Until6 = len5And3.sliced(4, 6)

  lazy val len5And3Dbl = Factory._doubled(len5And3)

  lazy val len5And3And5 = Factory.nested(a1Len5, a2Len3, a1Len5)

  lazy val v4 = Factory.nested(v4Arrays: _*)
  lazy val v7 = Factory.nested(v7Arrays: _*)

  lazy val v4Rev = v4.reversed
  lazy val v7Rev = v7.reversed

  test("empty") {
    val empty = Factory.empty
    empty.size must be (0)
    empty.depth must be (0)
    empty.reversed must be theSameInstanceAs empty
    empty.sliced(0, 0) must be theSameInstanceAs empty
    empty :++ empty must be theSameInstanceAs empty
    empty ++: empty must be theSameInstanceAs empty
    empty.iterator.hasNext must be (false)
  }

  test("1-array - factory") {
    an [IllegalArgumentException] must be thrownBy Factory[Int](0)
    an [IllegalArgumentException] must be thrownBy Factory[Double](-1)
    an [IllegalArgumentException] must be thrownBy len3F(null)
  }

  test("2-array - factory") {
    an [IllegalArgumentException] must be thrownBy Factory[Int](0, 4)
    an [IllegalArgumentException] must be thrownBy Factory[Int](7, 0)
    an [IllegalArgumentException] must be thrownBy Factory[Int](-3, 2)
    an [IllegalArgumentException] must be thrownBy Factory[Int](2, -3)
  }

  test("N-array - factory") {
    val v5 = Factory.nested(Array(0), Array(1), Array(2), Array(3), Array(4))
    val v5Portion1 = v5.sliced(2, 4)
    v5Portion1 must not be (anInstanceOf[Nested2[_]])
    v5Portion1 must be (anInstanceOf[Array2Slice[_]])
    val v5Portion2 = v5.sliced(4, 5)
    v5Portion2 must be (anInstanceOf[Array1Slice[_]])
  }

  test("1-array - size") {
    v2Len3.size must be (3)
    v1Len5From2.size must be (5 - 2)
  }

  test("2-array - size") {
    assert(len5And3F(a1Len5, a2Len3).size === 8)
    val len1And1F = Factory[Int](1, 1) // TODO: why I cannot inline this?
  }

  test("N-array - size") {
    assert(len5And3And5.size === 5 + 3 + 5)
    assert(v4.size === 10)
    assert(v7.size === 18)
  }

  test("generic - apply", Metatest) {
    len5And3Dbl must not be (anInstanceOf[Array2[_]])
    len5And3Dbl(0) must be (0);  len5And3Dbl(1) must be (20)
    len5And3Dbl(4) must be (80); len5And3Dbl(5) must be (1000)
  }

  test("1-array - apply (Int)") {
    v2Len3(1) must be (600)
    v1Len5From2(0) must be (20)
    v1Len5From2(2) must be (40)
  }

  test("2-array - apply (Int)") {
    assert(len5And3(0) === 0)
    assert(len5And3(1) === 10)
    assert(len5And3(4) === 40)
    assert(len5And3(5) === 500)
    assert(len5And3(7) === 700)
  }

  test("2-array - apply (Double)") {
    val piF = Factory[Double](2, 2)
    val pi = piF(Array(3.0, 0.1), Array(0.04, 0.001))
    assert(pi(0) === 3)
    assert(pi(1) === 0.1)
    assert(pi(2) === 0.04)
  }

  test("N-array - apply (Int)") {
    for (i <- 0 until 18)
      assert(v7(i) === 100 + i, "for i=" + i)
  }

  test("1-array - reversed") {
    assert(a1Len5Rev.size === 5)

    val aPiDigit0To4 = Array(3, 1, 4, 1, 5)
    val vPiDigit0To4 = len5F(aPiDigit0To4)
    val vPiDigit4To0 = vPiDigit0To4.reversed
    assert(vPiDigit4To0 eq vPiDigit0To4.reversed) // verify cached
    vPiDigit4To0.size must be (5)
    vPiDigit4To0 must be (anInstanceOf[ReversedArray1[_]])

    // verify update to array is seen by the view
    aPiDigit0To4(2) = 44
    assert(vPiDigit4To0(2) === 44)
    aPiDigit0To4(4) = 50
    assert(vPiDigit4To0(0) === 50)
    aPiDigit0To4(3) = 11
    assert(vPiDigit4To0(1) === 11)
    // aPiDigits0To4 == Array(3, 1, 44, 11, 50)

    // verify slicing
    val v11And44And1 = vPiDigit4To0.sliced(1, 4)
    v11And44And1.size must be (4 - 1)
    v11And44And1(0) must be (11)
    v11And44And1(1) must be (44)
    v11And44And1(2) must be (1)
    v11And44And1 must be (anInstanceOf[ReversedArray1Slice[_]])

    // verify slice of a slice behaves correctly (with implicit until)
    val v44And1 = v11And44And1.sliced(1)
    v44And1.size must be (3 - 1)
    v44And1(0) must be (44)
    v44And1(1) must be (1)
    v44And1 must be (anInstanceOf[ReversedArray1Slice[_]])

    // verify reversal of a slice of slice is not reversed
    val v1And44 = v44And1.reversed
    assert(v1And44 eq v44And1.reversed) // verify cached
    v1And44 must be (anInstanceOf[Array1Slice[_]])
    v1And44.size must be (2)
    v1And44(0) must be (1)
    v1And44(1) must be (44)

    // TODO: verify update through a view affects the array
    // vPiDigit4To0(2) = 444
    // assert(aPiDigit0To4(2) === 444)
  }

  test("1-array - reversed is idempotent") {
    a1Len5Rev must be (anInstanceOf[ReversedArray1[_]]) // sanity check
    val a1Len5RevTwice = a1Len5Rev.reversed
    a1Len5RevTwice must not be (anInstanceOf[ReversedArray1[_]])
    a1Len5RevTwice must be (anInstanceOf[Array1[_]])
    assert(a1Len5RevTwice eq v1Len5) // verify the same instance
    a1Len5RevTwice.size must be (5)
    for (i <- 0 until 5)
      assert(a1Len5(i) === a1Len5RevTwice(i), "for i=" + i)
  }

  test("2-array - reversed") {
    len5And3Rev.size must be (8)
    for (i <- 0 until len5And3Rev.size)
      assert(len5And3Rev(i) === len5And3(7 - i), "for i=" + i)
    len5And3Rev must be (anInstanceOf[ReversedArray2[_]])
    assert(a1Len5Rev.reversed eq v1Len5) // verify idempotent

    // verify correctness after an element is sliced from both ends
    val len5And3From6DownTo1 = len5And3Rev.sliced(1, len5And3Rev.size - 1)
    for (i <- 1 until len5And3Rev.size - 1)
      assert(len5And3From6DownTo1(i - 1) === len5And3Rev(i), "for i=" + i)

    // verify a 2-array slice of slice is equal to the direct 2-array slice
    val len5And3From5DownTo4 = len5And3Rev.sliced(2, 4)
    len5And3From5DownTo4.size must be (4 - 2)
    len5And3From5DownTo4(0) must be (500)
    len5And3From5DownTo4(1) must be (40)
    len5And3From5DownTo4 must be (anInstanceOf[ReversedArray2Slice[_]])
    len5And3From5DownTo4 must be (len5And3From6DownTo1.sliced(1, 3))

    // verify slicing into reversed 1-array view (and equality of slicing twice)
    val len5And3From4DownTo1 = len5And3Rev.sliced(3, 7)
    len5And3From4DownTo1.size must be (7 - 3)
    len5And3From4DownTo1 must be (anInstanceOf[ReversedArray1Slice[_]])
    len5And3From4DownTo1(0) == 40
    len5And3From4DownTo1(2) == 20
    len5And3From4DownTo1(3) == 10
    len5And3From4DownTo1 must be (len5And3From6DownTo1.sliced(2, 6))
    val len5And3From6DownTo6 = len5And3Rev.sliced(1, 2)
    len5And3From6DownTo6.size must be (2 - 1)
    len5And3From6DownTo6(0) must be (600)
    len5And3From6DownTo6 must be (anInstanceOf[ReversedArray1Slice[_]])
    len5And3From6DownTo6 must be (len5And3From6DownTo1.sliced(0, 1))

    // verify reversal of 1-array view is equal to non-reversed 1-array view
    len5And3From4DownTo1.reversed must be (len5And3From1Until5)
    len5And3From6DownTo6.reversed must be (len5And3From6Until7)
  }

  test("N-array - reversed") {
    v4Rev must be (anInstanceOf[Nested2[_]])
    v4Rev(0) must be (109)
    v4Rev(4) must be (105)
    v4Rev(9) must be (100)
    v4Rev.reversed must be (v4) // verify idempotent

    // verify slicing after reversal
    val v4RevFrom2Until7 = v4Rev.sliced(1, 7)
    v4RevFrom2Until7(0) must be (108)
    v4RevFrom2Until7(5) must be (103)
  }

  test("generic - reversed") {
    // Factory is not a stable identifier and ArrayViewFactory is a trait,
    // so it seems like we need to extend the factory if we want to use
    // path-dependent types ApplyS and Rep,
    // class CustomFactory extends ArrayViewFactory with Driver {
    //   val len5And3Dbl = new ArrayView[Int] with ApplyS[Int] {
    //     // but compiler reports type mismatch of Rep
    //     private[scalaviews] override def applyS(i: Rep[Int]) =
    //       len5And3.applyS(i)
    //     private[scalaviews] lazy val applyC = super.compile(applyS)
    //   }
    // }
    val len5And3Dbl = Factory._doubled(len5And3)
    val len5And3DblRev = len5And3Dbl.reversed
    len5And3DblRev.size must be (8)
    len5And3DblRev(2) must be (1000)
    len5And3DblRev(7) must be (0)
    assert(len5And3DblRev.reversed eq len5And3Dbl) // verify idempotent
  }

  test("1-array - sliced") {
    // slice an empty portion out of an Array1 and a ReversedArray1
    v1Len5.sliced(0, 0) must be theSameInstanceAs Factory.empty
    a1Len5Rev.sliced(0, 0) must be theSameInstanceAs Factory.empty

    // slice a portion of size 2 from the middle
    v1Len5From2Until4 must be (anInstanceOf[Array1Slice[_]])
    v1Len5From2Until4.size must be (4 - 2)
    val (fst, snd) = (20, 30)
    v1Len5From2Until4(0) must be (fst)
    v1Len5From2Until4(1) must be (snd)
    v1Len5From2Until4.sliced(0, 0) must be theSameInstanceAs Factory.empty
    val v1Len5From2Until4Rev = v1Len5From2Until4.reversed
    v1Len5From2Until4Rev must be (anInstanceOf[ReversedArray1Slice[_]])
    assert(v1Len5From2Until4Rev.reversed === v1Len5From2Until4)
    v1Len5From2Until4Rev.sliced(0, 0) must be theSameInstanceAs Factory.empty

    // slice the 1st half and verify it's the same as the 2nd slice of reversed
    val v1Len5From2Until3 = v1Len5From2Until4.sliced(0, 1)
    assertResult((fst, fst)) {
      val v1RevThenSlice = v1Len5From2Until4Rev.sliced(1, 2)
      val v1SliceThenRev = v1Len5From2Until3.reversed
      assert(v1RevThenSlice === v1SliceThenRev)
      (v1RevThenSlice(0), v1SliceThenRev(0))
    }

    // slice the 2nd half and verify it's the same as the 1st slice of reversed
    val v1Len5From3Until4 = v1Len5From2Until4.sliced(1, 2)
    assertResult((snd, snd)) {
      val v1RevThenSlice = v1Len5From2Until4Rev.sliced(0, 1)
      val v1SliceThenRev = v1Len5From3Until4.reversed
      assert(v1RevThenSlice === v1SliceThenRev)
      (v1RevThenSlice(0), v1SliceThenRev(0))
    }

    // verify correctness of a slice of slice (size 1)
    v2Len3From1Until2.size must be (2 - 1)
    v2Len3From1Until2 must be (anInstanceOf[Array1Slice[_]])
    val x = 600
    v2Len3From1Until2(0) must be (x)
    val v2Len3From1Until2Rev = v2Len3From1Until2.reversed
    v2Len3From1Until2Rev(0) must be (x)
    v2Len3From1Until2Rev must be (anInstanceOf[ReversedArray1Slice[_]])
    val v2Len3From1DownTo1 = v2Len3From1Until2Rev.sliced(0, 1) // full slice
    v2Len3From1DownTo1(0) must be (x)
    v2Len3From1DownTo1 must be (anInstanceOf[ReversedArray1Slice[_]])
    val v2Len3From1To1 = v2Len3From1DownTo1.reversed
    v2Len3From1To1 must be (anInstanceOf[Array1Slice[_]])
    v2Len3From1To1(0) must be (x)
    v2Len3From1To1.sliced(0, 1)(0) must be (x) // full slice
  }

  test("2-array - sliced") {
    // slice an empty portion out of an Array1 and a ReversedArray1
    len5And3.sliced(0, 0) must be theSameInstanceAs Factory.empty
    len5And3Rev.sliced(0, 0) must be theSameInstanceAs Factory.empty

    // verify slicing into 1-array slices
    len5And3From1Until5.size must be (5 - 1)
    for (i <- 1 until 5)
      assert(len5And3(i) === len5And3From1Until5(i - 1), "for i=" + i)
    len5And3From1Until5 must be (anInstanceOf[Array1Slice[_]])
    len5And3From6Until7.size must be (7 - 6)
    len5And3From6Until7(0) must be (600)
    len5And3From6Until7 must be (anInstanceOf[Array1Slice[_]])
    len5And3From6Until7.sliced(0, 0) must be theSameInstanceAs Factory.empty

    // verify (slices of) 1-array slice are equal to slices of either array
    len5And3From6Until7(0) must be (v2Len3From1Until2(0))
    len5And3From6Until7 must be (v2Len3From1Until2)
    val len5And3From2Until5 = len5And3From1Until5.sliced(1, 4) // slice of slice
    len5And3From2Until5 must be (anInstanceOf[Array1Slice[_]])
    len5And3From2Until5.size must be (5 - 2)
    len5And3From2Until5 must be (v1Len5From2)

    // verify reversal of a 1-array slices
    val len5And3From6DownTo6 = len5And3From6Until7.reversed
    len5And3From6DownTo6 must be (anInstanceOf[ReversedArray1Slice[_]])
    len5And3From6DownTo6(0) must be (600)
    len5And3From6DownTo6.reversed.sliced(0, 1)(0) must be (600)
    len5And3From6DownTo6.reversed must be (len5And3From6Until7) // verify idem
    // assert(len5And3From6DownTo6.reversed eq (len5And3From6Until7)) // FIXME
    len5And3From6DownTo6.sliced(0, 0) must be theSameInstanceAs Factory.empty
    val len5And3From4DownTo2 = len5And3From2Until5.reversed
    for (i <- 0 to 2)
      assert(len5And3From4DownTo2(i) === v1Len5From2(2 - i), "for i=" + i)
    len5And3From4DownTo2.reversed must be (v1Len5From2) // reverse of reversed

    // Verify slicing into 2-array is correct
    len5And3From4Until6.size must be (6 - 4)
    len5And3From4Until6(0) must be (40)
    len5And3From4Until6(1) must be (500)
    len5And3From4Until6 must be (anInstanceOf[Array2Slice[_]])
    val len5And3From5DownTo4 = len5And3From4Until6.reversed
    len5And3From5DownTo4 must be (anInstanceOf[ReversedArray2Slice[_]])
    len5And3From5DownTo4(0) must be (len5And3From4Until6(1))
    len5And3From5DownTo4(1) must be (len5And3From4Until6(0))
    len5And3From5DownTo4.reversed must be (len5And3From4Until6) // verify idem
    // assert(len5And3From5DownTo4.reversed eq len5And3From4Until6) // FIXME
    // and further slicing into 1-array slices is equal to other 1-array slices
    len5And3From5DownTo4.sliced(0, 1)(0) must be (len5And3From4Until6(1))
    len5And3From5DownTo4.sliced(1, 2)(0) must be (len5And3From4Until6(0))
  }

  test("N-array - sliced") {
    // verify slicing that results in (a slice of) Array2
    val v7Portion3And4 = v7.sliced(10, 15)
    v7Portion3And4 must be (anInstanceOf[Array2Slice[_]])
    v7Portion3And4(2) must be (112)
    v7Portion3And4(3) must be (113)

    // verify slicing that results indirectly in (a slice of) Array1
    val v7Portion0 = v7 until 2
    v7Portion0 must be (anInstanceOf[Array1Slice[_]])

    // verify slicing that results directly in (a slice of) Array1
    val v7From16 = v7 from 16
    v7From16 must be (anInstanceOf[Array1Slice[_]])
    v7From16(0) must be (116)
    v7From16(1) must be (117)

    // verify slicing that results in Nested2 at the beginning or in the middle
    val v7Until10 = v7 until 10
    v7Until10 must be (anInstanceOf[Nested2[_]])
    v7Until10 must be (v4.sliced(0, v4.size))
    val v7From7Until11 = v7.sliced(7, 11)
    v7From7Until11 must be (anInstanceOf[Nested2[_]])
    v7From7Until11(0) must be (107)
    v7From7Until11(2) must be (109)
    v7From7Until11(3) must be (110)

    // verify further reversal & slicing on a Nested2 slice
    val v7From9DownTo8 = v7From7Until11.reversed.sliced(1, 3)
    v7From9DownTo8(0) must be (109)
    v7From9DownTo8(1) must be (108)
  }

  test("generic - sliced") {
    val len5And3DblFrom3Until7 = len5And3Dbl.sliced(3, 7)
    len5And3DblFrom3Until7.size must be (7 - 3)
    for (i <- 3 until 7)
      assert(len5And3Dbl(i) === len5And3DblFrom3Until7(i - 3), "for i=" + i)
    len5And3DblFrom3Until7 must not be (anInstanceOf[Array1[_]])
    len5And3DblFrom3Until7 must not be (anInstanceOf[Array2[_]])
  }

  object Nested2Implicits {
    implicit class Nested2WithSubviews[T: Manifest](v: ArrayView[T]) {
      val _1 = v.asInstanceOf[Nested2[T]].v1
      val _2 = v.asInstanceOf[Nested2[T]].v2
    }
  }

  test("N-array - preorder") {
    import Nested2Implicits._
    var exps = List(
      v7,
      v7._1,
      v7._1._1,
      v7._1._2,
      v7._2,
      v7._2._1,
      v7._2._2).zipWithIndex
    v7.preorder { act =>
      val (exp, ord) :: expsRest = exps; exps = expsRest
      assert(act eq exp, "at visit #" + ord)
    }
  }

  test("N-array - inorder") {
    import Nested2Implicits._
    assertTraversal(v7.inorder,
      v7._1._1,
      v7._1,
      v7._1._2,
      v7,
      v7._2._1,
      v7._2,
      v7._2._2)
  }

  private def assertTraversal[T, V <: ArrayView[T]](
    traverse: (V => Unit) => Unit, exps0: ArrayView[T]*) = {
    import org.scalatest.exceptions.{TestFailedException => TFE}
    var exps = exps0.toList.zipWithIndex
    try {
      traverse { act: V =>
        exps = exps match {
          case (exp, ord) :: expsRest =>
            if (exp ne act)
              throw new TFE("Unexpected visit #" + ord +
                ": hashCode %d expected but %d found".format(
                  exp.hashCode, act.hashCode),
                0)
            expsRest
          case _ => throw new TFE("unexpected end of traversal", 0)
        }
      }
    } catch { // stack depth for TFEs thrown above varies, so catch & wrap them
      case tfe: TFE => throw new TFE(tfe, 1);
    }
    if (exps.nonEmpty)
      throw new TFE(
        "unexpected traversal length: " + exps.size + " extra visits",
        1)
  }

  test("1-array - :++") {
    import Nested2Implicits._
    def verifyAppend(a1: ArrayView[Int]): Unit = {
      val nested2 = a1 :++ len5And3
      nested2 mustBe a [Nested2[_]]
      nested2._2 must be theSameInstanceAs len5And3
      nested2._1 must be theSameInstanceAs a1
      nested2.depth must be (1)
    }

    verifyAppend(v1Len5)
    verifyAppend(a1Len5Rev)
    verifyAppend(v1Len5From2)
    verifyAppend(v1Len5From2.reversed)

    def verifyAppendEmpty(a1: ArrayView[Int]): Unit = {
      val same = a1 :++ Factory.empty
      same must not be a [Nested2[_]]
      same must be theSameInstanceAs a1
    }

    verifyAppendEmpty(v1Len5)
    verifyAppendEmpty(a1Len5Rev)
    verifyAppendEmpty(v1Len5From2)
    verifyAppendEmpty(v1Len5From2.reversed)
  }

  test("2-array - :++") {
    import Nested2Implicits._
    def verifyAppend(a2: ArrayView[Int]): Unit = {
      val nested2 = a2 :++ v1Len5
      nested2 mustBe a [Nested2[_]]
      nested2._2 must be theSameInstanceAs v1Len5
      nested2._1 must be theSameInstanceAs a2
      nested2.depth must be (1)
    }

    verifyAppend(len5And3)
    verifyAppend(len5And3Rev)
    verifyAppend(len5And3From1Until5)
    verifyAppend(len5And3Rev until 5 from 1)

    def verifyAppendEmpty(a2: ArrayView[Int]): Unit = {
      val same = a2 :++ Factory.empty
      same must not be a [Nested2[_]]
      same must be theSameInstanceAs a2
    }

    verifyAppendEmpty(len5And3)
    verifyAppendEmpty(len5And3Rev)
    verifyAppendEmpty(len5And3From1Until5)
    verifyAppendEmpty(len5And3Rev until 5 from 1)
  }

  test("N-array - :++") {
    import Nested2Implicits._

    // verify appending Array1 increases the nesting by 1 level
    val v4A1 = v4 :++ v2Len3
    assertTraversal(v4A1.preorder,
      v4A1,
      v4, v4._1, v4._2,
      v2Len3)
    assertTraversal(v4A1.inorder,
      v4._1, v4, v4._2,
      v4A1,
      v2Len3)
    v4A1.depth must be (v4.depth + 1)

    // verify appending another Array1 does not increase the nesting level
    val v4A1A1 = v4A1 :++ v1Len5
    assertTraversal(v4A1A1.preorder,
      v4A1A1,
      v4, v4._1, v4._2,
      v4A1A1._2, v2Len3, v1Len5)
    assertTraversal(v4A1A1.inorder,
      v4._1, v4, v4._2,
      v4A1A1,
      v2Len3, v4A1A1._2, v1Len5)
    v4A1A1.depth must be (v4A1.depth)
    v4A1A1.depth must be (2)

    {
      // verify appending another Array2 increase the nesting level
      val v4A1A1A2 = v4A1A1 :++ len5And3
      assertTraversal(v4A1A1A2.preorder,
        v4A1A1A2,
        v4A1A1, v4, v4._1, v4._2, v4A1A1._2, v2Len3, v1Len5,
        len5And3)
      assertTraversal(v4A1A1A2.inorder,
        v4._1, v4, v4._2, v4A1A1, v2Len3, v4A1A1._2, v1Len5,
        v4A1A1A2,
        len5And3)

      // verify appending nested with 2 leaves does not increase the nesting
      // and pushes down the last leaf, creating a gap of 1 leaf in between
      val v4A1A1A2N2 = v4A1A1A2 :++ v4Rev
      v4A1A1A2N2.depth must be (v4A1A1A2.depth)
      assertTraversal(v4A1A1A2N2.preorder,
        v4A1A1A2N2,
        v4A1A1, v4, v4._1, v4._2, v4A1A1._2, v2Len3, v1Len5,
        v4A1A1A2N2._2, len5And3, v4Rev, v4Rev._1, v4Rev._2)
      assertTraversal(v4A1A1A2N2.inorder,
        v4._1, v4, v4._2, v4A1A1, v2Len3, v4A1A1._2, v1Len5,
        v4A1A1A2N2,
        len5And3, v4A1A1A2N2._2, v4Rev._1, v4Rev, v4Rev._2)

      // verify appending another non-nested view is not fooled by the gap
      val v4A1A1A2N2A1 = v4A1A1A2N2 :++ len5And3From1Until5
      v4A1A1A2N2A1.depth must be (v4A1A1A2N2.depth + 1)

      // verify only appended "deep enough" views actually increase the nesting
      (v4A1A1A2N2A1 :++ v4A1A1).depth must be (v4A1A1A2N2A1.depth);
      (v4A1A1A2N2A1 :++ v4A1A1A2).depth must be (v4A1A1A2N2A1.depth + 1)
    }

    {
      // verify appending a nested view with 2 leaves increase the nesting
      val v4A1A1N2 = v4A1A1 :++ v4Rev
      assertTraversal(v4A1A1N2.preorder,
        v4A1A1N2,
        v4A1A1, v4, v4._1, v4._2, v4A1A1._2, v2Len3, v1Len5,
        v4Rev, v4Rev._1, v4Rev._2)
      assertTraversal(v4A1A1N2.inorder,
        v4._1, v4, v4._2, v4A1A1, v2Len3, v4A1A1._2, v1Len5,
        v4A1A1N2,
        v4Rev._1, v4Rev, v4Rev._2)

      // verify appending a non-nested view does not increase the nesting
      // and pushes down the nested view with 2 leaves, not creating a gap
      val v4A1A1N2A2 = v4A1A1N2 :++ len5And3
      v4A1A1N2A2.depth must be (v4A1A1N2.depth)
      assertTraversal(v4A1A1N2A2.preorder,
        v4A1A1N2A2,
        v4A1A1, v4, v4._1, v4._2, v4A1A1._2, v2Len3, v1Len5,
        v4A1A1N2A2._2, v4Rev, v4Rev._1, v4Rev._2, len5And3)
      assertTraversal(v4A1A1N2A2.inorder,
        v4._1, v4, v4._2, v4A1A1, v2Len3, v4A1A1._2, v1Len5,
        v4A1A1N2A2,
        v4Rev._1, v4Rev, v4Rev._2, v4A1A1N2A2._2, len5And3)
      v4A1A1N2A2.depth must be (3)

      // verify only a non-nested view can be appended without increasing depth
      (v4A1A1N2A2 :++ v1Len5).depth must be (v4A1A1N2A2.depth)
      (v4A1A1N2A2 :++ v4).depth must be (v4A1A1N2A2.depth + 1)
    }
  }

  test("1-array - ++:") { // TLDR: symmetric to "1-array - :++"
    import Nested2Implicits._
    def verifyPrepend(a1: ArrayView[Int]): Unit = {
      val nested2 = len5And3 ++: a1
      nested2 mustBe a [Nested2[_]]
      nested2._1 must be theSameInstanceAs len5And3
      nested2._2 must be theSameInstanceAs a1
      nested2.depth must be (1)
    }

    verifyPrepend(v1Len5)
    verifyPrepend(a1Len5Rev)
    verifyPrepend(v1Len5From2)
    verifyPrepend(v1Len5From2.reversed)

    def verifyPrependEmpty(a2: ArrayView[Int]): Unit = {
      val same = Factory.empty[Int] ++: a2
      same must not be a [Nested2[_]]
      same must be theSameInstanceAs a2
    }

    verifyPrependEmpty(v1Len5)
    verifyPrependEmpty(a1Len5Rev)
    verifyPrependEmpty(v1Len5From2)
    verifyPrependEmpty(v1Len5From2.reversed)
  }

  test("2-array - ++:") { // TLDR: symmetric to "2-array - :++"
    import Nested2Implicits._
    def verifyPrepend(a2: ArrayView[Int]): Unit = {
      val nested2 = v1Len5 ++: a2
      nested2 mustBe a [Nested2[_]]
      nested2._1 must be theSameInstanceAs v1Len5
      nested2._2 must be theSameInstanceAs a2
      nested2.depth must be (1)
    }

    verifyPrepend(len5And3)
    verifyPrepend(len5And3Rev)
    verifyPrepend(len5And3From1Until5)
    verifyPrepend(len5And3Rev until 5 from 1)

    def verifyPrependEmpty(a2: ArrayView[Int]): Unit = {
      val same = Factory.empty[Int] ++: a2
      same must not be a [Nested2[_]]
      same must be theSameInstanceAs a2
    }

    verifyPrependEmpty(len5And3)
    verifyPrependEmpty(len5And3Rev)
    verifyPrependEmpty(len5And3From1Until5)
    verifyPrependEmpty(len5And3Rev until 5 from 1)
  }

  test("N-array - ++:") { // TLDR: symmetric to "N-array - :++"
    import Nested2Implicits._

    // verify prepending Array1 increases the nesting by 1 level
    val v4A1 = v2Len3 ++: v4
    assertTraversal(v4A1.preorder,
      v4A1,
      v2Len3,
      v4, v4._1, v4._2)
    assertTraversal(v4A1.inorder,
      v2Len3,
      v4A1,
      v4._1, v4, v4._2)
    v4A1.depth must be (v4.depth + 1)

    // verify prepending another Array1 does not increase the nesting level
    val v4A1A1 = v1Len5 ++: v4A1
    assertTraversal(v4A1A1.preorder,
      v4A1A1,
      v4A1A1._1, v1Len5, v2Len3,
      v4, v4._1, v4._2)
    assertTraversal(v4A1A1.inorder,
      v1Len5, v4A1A1._1, v2Len3,
      v4A1A1,
      v4._1, v4, v4._2)
    v4A1A1.depth must be (v4A1.depth)
    v4A1A1.depth must be (2)

    {
      // verify prepending another Array2 increases the nesting level
      val v4A1A1A2 = len5And3 ++: v4A1A1
        // v4A1A1A2,
        // v4A1A1, v4, v4._1, v4._2, v4A1A1._2, v2Len3, v1Len5,
        // len5And3)
      assertTraversal(v4A1A1A2.preorder,
        v4A1A1A2,
        len5And3,
        v4A1A1, v4A1A1._1, v1Len5, v2Len3, v4, v4._1, v4._2)
      assertTraversal(v4A1A1A2.inorder,
        len5And3,
        v4A1A1A2,
        v1Len5, v4A1A1._1, v2Len3, v4A1A1, v4._1, v4, v4._2)

      // verify prepending nested with 2 leaves does not increase the nesting
      // and pushes down the last leaf, creating a gap of 1 leaf in between
      val v4A1A1A2N2 = v4Rev ++: v4A1A1A2
      v4A1A1A2N2.depth must be (v4A1A1A2.depth)
      assertTraversal(v4A1A1A2N2.preorder,
        v4A1A1A2N2,
        v4A1A1A2N2._1, v4Rev, v4Rev._1, v4Rev._2, len5And3,
        v4A1A1, v4A1A1._1, v1Len5, v2Len3, v4, v4._1, v4._2)
      assertTraversal(v4A1A1A2N2.inorder,
        v4Rev._1, v4Rev, v4Rev._2, v4A1A1A2N2._1, len5And3,
        v4A1A1A2N2,
        v1Len5, v4A1A1._1, v2Len3, v4A1A1, v4._1, v4, v4._2)

      // verify prepending another non-nested view is not fooled by the gap
      val v4A1A1A2N2A1 = len5And3From1Until5 ++: v4A1A1A2N2
      v4A1A1A2N2A1.depth must be (v4A1A1A2N2.depth + 1)

      // verify only prepended "deep enough" views actually increase the nesting
      (v4A1A1 ++: v4A1A1A2N2A1).depth must be (v4A1A1A2N2A1.depth);
      (v4A1A1A2 ++: v4A1A1A2N2A1).depth must be (v4A1A1A2N2A1.depth + 1)
      (v4A1A1A2N2A1 ++: v4A1A1A2N2A1).depth must be (v4A1A1A2N2A1.depth + 1)
    }

    {
      // verify prepending a nested view with 2 leaves increase the nesting
      val v4A1A1N2 = v4Rev ++: v4A1A1
      assertTraversal(v4A1A1N2.preorder,
        v4A1A1N2,
        v4Rev, v4Rev._1, v4Rev._2,
        v4A1A1, v4A1A1._1, v1Len5, v2Len3, v4, v4._1, v4._2)
      assertTraversal(v4A1A1N2.inorder,
        v4Rev._1, v4Rev, v4Rev._2,
        v4A1A1N2,
        v1Len5, v4A1A1._1, v2Len3, v4A1A1, v4._1, v4, v4._2)

      // verify prepending a non-nested view does not increase the nesting
      // and pushes down the nested view with 2 leaves, not creating a gap
      val v4A1A1N2A2 = len5And3 ++: v4A1A1N2
      v4A1A1N2A2.depth must be (v4A1A1N2.depth)
      assertTraversal(v4A1A1N2A2.preorder,
        v4A1A1N2A2,
        v4A1A1N2A2._1, len5And3, v4Rev, v4Rev._1, v4Rev._2,
        v4A1A1, v4A1A1._1, v1Len5, v2Len3, v4, v4._1, v4._2)
      assertTraversal(v4A1A1N2A2.inorder,
        len5And3, v4A1A1N2A2._1, v4Rev._1, v4Rev, v4Rev._2,
        v4A1A1N2A2,
        v1Len5, v4A1A1._1, v2Len3, v4A1A1, v4._1, v4, v4._2)
      v4A1A1N2A2.depth must be (3)

      // verify only a non-nested view can be prepended without increasing depth
      (v1Len5 ++: v4A1A1N2A2).depth must be (v4A1A1N2A2.depth)
      (v4 ++: v4A1A1N2A2).depth must be (v4A1A1N2A2.depth + 1)
    }
  }

  test("N-array - ++: same as ++: when same-depth") {
    import Nested2Implicits._
    val v4v4RevAppended = v4 :++ v4Rev
    val v4v4RevPrepended = v4 ++: v4Rev
    assert(v4v4RevAppended._1 eq v4)
    assert(v4v4RevAppended._2 eq v4Rev)
    assert(v4v4RevPrepended._1 eq v4)
    assert(v4v4RevPrepended._2 eq v4Rev)
    assert(v4v4RevAppended.depth === v4v4RevPrepended.depth)
  }

  test("N-array - :++ falls back to ++: if greater depth") {
    import Nested2Implicits._
    val v4A1 = v2Len3 ++: v4
    v4A1.depth must be (2)
    v4A1._2.depth must be (v4A1._1.depth + 1)

    val v4A1A1 = v1Len5 :++ v4A1
    v4A1A1.depth must be (v4A1.depth)
    assertTraversal(v4A1A1.preorder,
      v4A1A1,
      v4A1A1._1, v1Len5, v2Len3,
      v4, v4._1, v4._2)
    assertTraversal(v4A1A1.inorder,
      v1Len5, v4A1A1._1, v2Len3,
      v4A1A1,
      v4._1, v4, v4._2)
  }

  test("N-array - ++: falls back to :++ if greater depth") {
    import Nested2Implicits._
    val v4A1 = v4 :++ v2Len3
    v4A1.depth must be (2)
    v4A1._1.depth must be (v4A1._2.depth + 1)

    val v4A1A1 = v4A1 ++: v1Len5
    v4A1A1.depth must be (v4A1.depth)
    assertTraversal(v4A1A1.preorder,
      v4A1A1,
      v4, v4._1, v4._2,
      v4A1A1._2, v2Len3, v1Len5)
    assertTraversal(v4A1A1.inorder,
      v4._1, v4, v4._2,
      v4A1A1,
      v2Len3, v4A1A1._2, v1Len5)
  }

  test("1-array - iterator") {
    v1Len5.iterator.toList must contain theSameElementsInOrderAs a1Len5.toList
    a1Len5Rev.iterator.toList must contain theSameElementsInOrderAs (
      a1Len5.toList.reverse)
    v1Len5From2.iterator.toList must contain theSameElementsInOrderAs (
      a1Len5.toList.drop(2))
    v1Len5From2.reversed.iterator.toList must contain theSameElementsInOrderAs (
      a1Len5.toList.drop(2).reverse)
  }

  test("2-array - iterator") {
    val len5And3List = a1Len5.toList ::: a2Len3.toList
    len5And3.iterator.toList must contain theSameElementsInOrderAs len5And3List
    len5And3Rev.iterator.toList must contain theSameElementsInOrderAs (
      len5And3List.reverse)
    val len5And3From4Until6List = len5And3List.slice(4, 6)
    len5And3From4Until6.iterator.toList must contain theSameElementsInOrderAs (
      len5And3From4Until6List)
    (len5And3From4Until6.reversed.iterator.toList must contain
      theSameElementsInOrderAs len5And3From4Until6List.reverse)
  }

  test("N-array - iterator (depth 1)") {
    val vDepth1 = v4
    assume(vDepth1.depth === 1)
    val act = vDepth1.iterator.toArray
    act must contain theSameElementsInOrderAs (v4Arrays.flatten)
    act must contain theSameElementsInOrderAs (
      vDepth1.asInstanceOf[Nested2[Int]].iteratorUnstaged.toArray[Int])
  }

  test("N-array - iterator (depth 2)") {
    val vDepth2 = v7
    assume(vDepth2.depth === 2)
    val act = vDepth2.iterator.toArray
    act must contain theSameElementsInOrderAs (v7Arrays.flatten)
    act must contain theSameElementsInOrderAs (
      vDepth2.asInstanceOf[Nested2[Int]].iteratorUnstaged.toArray[Int])
  }

  test("N-array - iterator (unbalanced)") {
    import Nested2Implicits._
    val v = v2Len3 ++: (v4 :++ v1Len5)
    assume(v.depth === 3)
    assume(v._1.depth === 0)
    assume(v._2.depth === 2)
    assume(v._2._1.depth === 1)
    assume(v._2._2.depth === 0)
    assume(v._2.depth - v._1.depth === 2)
    assume(v._2._1.depth - v._2._2.depth === 1)
    val act = v.iterator.toArray
    act must contain theSameElementsInOrderAs (
      a2Len3 ++ v4Arrays.flatten ++ a1Len5)
    act must contain theSameElementsInOrderAs (
      v.asInstanceOf[Nested2[Int]].iteratorUnstaged.toArray[Int])
  }

  private def foreachList[T](v: ArrayView[T]) = {
    val data = scala.collection.mutable.MutableList.empty[T]
    v.foreach { datum => data += datum }
    data.toList
  }

  test("1-array - foreach") {
    foreachList(v1Len5) must contain theSameElementsInOrderAs a1Len5.toList
    foreachList(a1Len5Rev) must contain theSameElementsInOrderAs (
      a1Len5.toList.reverse)
    foreachList(v1Len5From2) must contain theSameElementsInOrderAs (
      a1Len5.toList.drop(2))
    foreachList(v1Len5From2.reversed) must contain theSameElementsInOrderAs (
      a1Len5.toList.drop(2).reverse)
  }

  test("2-array - foreach") {
    val len5And3List = a1Len5.toList ::: a2Len3.toList
    foreachList(len5And3) must contain theSameElementsInOrderAs len5And3List
    foreachList(len5And3Rev) must contain theSameElementsInOrderAs (
      len5And3List.reverse)
    val len5And3From4Until6List = len5And3List.slice(4, 6)
    foreachList(len5And3From4Until6) must contain theSameElementsInOrderAs (
      len5And3From4Until6List)
    (foreachList(len5And3From4Until6.reversed) must contain
      theSameElementsInOrderAs len5And3From4Until6List.reverse)
  }

  test("N-array - foreach (unbalanced)") {
    import Nested2Implicits._
    val v = v2Len3 ++: (v4 :++ v1Len5)
    assume(v.depth === 3)
    assume(v._1.depth === 0)
    assume(v._2.depth === 2)
    assume(v._2._1.depth === 1)
    assume(v._2._2.depth === 0)
    assume(v._2.depth - v._1.depth === 2)
    assume(v._2._1.depth - v._2._2.depth === 1)
    foreachList(v) must contain theSameElementsInOrderAs (
      a2Len3.toList ++ (List.empty[Int] /: v4Arrays)(_ ++ _.toList) ++
        a1Len5.toList)
  }

  import Implicits._

  test("implicits - from") {
    a1Len5 from 2 must be (v1Len5From2)
  }

  test("implicits - until from") {
    // TODO: should it be the case that: (from 2 until 4) != (until 4 from 2)?
    a1Len5 until 4 from 2 must be (v1Len5From2Until4)
  }

  test("implicits - downTo") {
    a2Len3 downTo 1 must be (v2Len3.sliced(1).reversed)
  }

  test("implicits - at") {
    a2Len3 at 1 must be (v2Len3.sliced(1, 2))
  }
}

class ArrayViewScalaCodegenTest extends FunSuite with TypeMatchers
    with ClassMatchers
    with BeforeAndAfterAll
    with ViewFactoryProvider[ArrayViewFactory] {
  import ArrayViewTest._
  import ArrayView._
  def mkFactory = new FactoryImpl with CompileMock

  override def beforeAll() {
    Factory.reset
  }

  type ApplyS[T] = ArrayViewFactory#ApplyS[T]

  import CompileMock._
  import scala.language.reflectiveCalls
  import scala.reflect.runtime.universe._

  val len5F = Factory[Int](5)
  val len3F = Factory[Int](3)
  val len1F = Factory[Int](1)

  lazy val len5 = len5F(a1Len5)
  lazy val len3 = len3F(a2Len3)

  lazy val len5From2Until4 = len5.sliced(2, 4)

  lazy val len19And23F = Factory[Int](19, 23)
  lazy val len19And23 = len19And23F(a1Len5, a2Len3)

  lazy val len19And23Dbl = Factory._doubled(len19And23)

  lazy val len19And23Rev = len19And23.reversed

  lazy val len19And23From13Until18 = len19And23.sliced(13, 18)
  lazy val len19And23From18Until21 = len19And23.sliced(18, 21)
  lazy val len19And23From21Until42 = len19And23.sliced(21, 42)

  lazy val v7 = Factory.nested(v7Arrays: _*)

  def getApplyC[T](v: ArrayView[T]) = v.asInstanceOf[ApplyS[_]].applyC

  test("applyC - Array1") {
    val method = getApplyC(len5)
    method.body must contain noneOf ('-', '+')
    method.body must not include ("if")
    method.body.count(_ == '\n') must be (2)
  }

  test("applyC - Array2") {
    val method = getApplyC(len19And23)
    method.paramType must be (ofType[Int])
    method.resultType must be (ofType[Int])
    method.body must include ("< 19")
    method.body must include ("- 19")
    method.body must not include ("&&")
  }

  test("applyC - Nested2") {
    val method = getApplyC(v7)
    method.body must include ("< 10")
    method.body must include ("< 5")
    method.body must include ("< 2")
  }

  test("reversed - Array2") {
    val method = getApplyC(len19And23Rev)
    method.body must startWith regex "val .* = 41 - .*"
  }

  test("reversed - generic") {
    val len19And23DblRev = len19And23Dbl.reversed
    len19And23DblRev must be (anInstanceOf[ApplyS[_]])
  }

  test("sliced - Array1") {
    val method = getApplyC(len5From2Until4)
    method.body.count(_ == '+') must be (1)
    method.body must include ("2")
    method.body must not include ("if")
  }

  test("sliced - Array2 (both chunks)") {
    val method = getApplyC(len19And23From18Until21)
    method.body.count(_ == '+') must be (1)
    method.body must include ("if")
    method.body must include ("else")
  }

  test("sliced - Array2 (1st chunk only)") {
    val method = getApplyC(len19And23From13Until18)
    method.body.count(_ == '+') must be (1)
    method.body must include ("13")
    method.body must not include ("if")
    method.body must not include ("else")
  }

  test("sliced - Array2 (2nd chunk only)") {
    val method = getApplyC(len19And23From21Until42)
    method.body.count(_ == '+') must be (1)
    method.body must include ("2")
    method.body must not include ("if")
    method.body must not include ("else")
  }

  test("sliced - generic") {
    val len19And23DblFrom18To21 = len19And23Dbl.sliced(18, 21)
    len19And23DblFrom18To21 must be (anInstanceOf[ApplyS[_]])
  }

  test("reversed - Array2 (2nd chunk only)") {
    val len19And23From21Until42Rev = len19And23From21Until42.reversed
    val method = getApplyC(len19And23From21Until42Rev)
    method.body must not include ("if")
    method.body must not include ("else")

    // verify that "2 + (20 - x)" is optimized to "22 - x"
    // TODO: support such an arithmetic optimization in ArrayViewFactory
    method.body must not contain allOf ('-', '+')
    method.body must not include ("20")
    method.body must include ("22")
    method.body.count(_ == '-') must be (1)
  }

  def getForeachC[T](v: ArrayView[T]) =
    v.asInstanceOf[ArrayViewFactory#ForeachS[T]].foreachC

  val foreachCBodyEnd = """
\(\)"""

  private def mkForeachCUnrolledRegex0[T](sizeElems: (Int, Seq[T])) =
    (""".* Array\(""" + sizeElems._2.mkString(",") + """\).*""" + """
val .* = x\d+\((\d+)\)
val x\d+ = x\d+\(x\d+\)""" * sizeElems._1)

  def mkForeachCUnrolledRegex[T](size: Int, arrayElems: T*) =
    mkForeachCUnrolledRegex0((size, arrayElems)) + foreachCBodyEnd

  test("foreach - Array1 (unrolled)") {
    val method = getForeachC(len3)
    method.body must fullyMatch regex (
      mkForeachCUnrolledRegex(3, 500, 600, 700) withGroups ("0", "1", "2"))
  }

  test("foreach - ReversedArray1 (unrolled)") {
    val method = getForeachC(len3.reversed)
    method.body must fullyMatch regex (
      mkForeachCUnrolledRegex(3, 500, 600, 700) withGroups ("2", "1", "0"))
  }

  test("foreach - Array1Slice (unrolled)") {
    val method = getForeachC(len5From2Until4)
    method.body must fullyMatch regex (
      mkForeachCUnrolledRegex(2, 0, 10, 20, 30, 40) withGroups ("2", "3"))
  }

  test("foreach - ReversedArray1Slice (unrolled)") {
    val method = getForeachC(len5From2Until4.reversed)
    method.body must fullyMatch regex (
      mkForeachCUnrolledRegex(2, 0, 10, 20, 30, 40) withGroups ("3", "2"))
  }

  def mkForeachCUnrolledRegex[T](sizeElems: (Int, Seq[T])*) =
    (sizeElems map mkForeachCUnrolledRegex0 mkString "\n") + foreachCBodyEnd

  test("foreach - Array2Slice (unrolled)") {
    val method = getForeachC(len19And23From18Until21)
    method.body must fullyMatch regex (
      mkForeachCUnrolledRegex(
        (1, a1Len5.toSeq),
        (2, a2Len3.toSeq)
      ) withGroups (
        "18",
        "0", "1"))
  }

  test("foreach - ReversedArray2Slice (unrolled)") {
    val method = getForeachC(len19And23From18Until21.reversed)
    method.body must fullyMatch regex (
      mkForeachCUnrolledRegex(
        (2, a2Len3.toSeq),
        (1, a1Len5.toSeq)
      ) withGroups (
        "1", "0",
        "18"))
  }

  test("foreach - Nested2 (unrolled)") {
    val v = len5.sliced(3, 4) :++ len1F(Array(400)) :++ len1F(Array(5000))
    val method = getForeachC(v)
    method.body must fullyMatch regex (
      mkForeachCUnrolledRegex(
        (1, a1Len5.toSeq),
        (1, Seq(400)),
        (1, Seq(5000))
      ) withGroups (
        "3",
        "0",
        "0"))
  }

  type IterS[T] = ArrayViewFactory#IteratorS[T]#Iter
  def getIteratorS[T](v: ArrayView[T]) =
    v.asInstanceOf[ArrayViewFactory#IteratorS[T]].iterator.asInstanceOf[
      IterS[T]]

  // TODO: implement tests that inspect code of compiled iteratorS methods
}
