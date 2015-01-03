/*
 * FixedArrayViewTest.scala
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

object FixedArrayViewTest {
  type Factory = FixedArrayViewFactory

  val a1Len5 = Array.range(0, 50, 10)
  val a2Len3 = Array.range(500, 800, 100)
  val a1Len9 = Array.range(0, 90, 10)
  val a1Len2 = Array(0, 10)
  val a2Len1 = Array(500)
}

class FixedArrayViewTest extends FunSuite with ClassMatchers {
  import FixedArrayView._
  import FixedArrayViewTest._

  // bring into scope inner case classes from the factory
  type Array1[T] = FixedArrayViewFactory#Array1[T]
  type Array1Slice[T] = FixedArrayViewFactory#Array1Slice[T]
  type Array2[T] = FixedArrayViewFactory#Array2[T]
  type Array2Slice[T] = FixedArrayViewFactory#Array2Slice[T]
  type ReversedArray1[T] = FixedArrayViewFactory#ReversedArray1[T]
  type ReversedArray1Slice[T] = FixedArrayViewFactory#ReversedArray1Slice[T]
  type ReversedArray2[T] = FixedArrayViewFactory#ReversedArray2[T]
  type ReversedArray2Slice[T] = FixedArrayViewFactory#ReversedArray2Slice[T]

  val len5F = Factory[Int](5)
  val len3F = Factory[Int](3)
  val len0F = Factory[Double](0)

  lazy val v1Len5 = len5F(a1Len5)
  lazy val v1Len5From2 = len5F(a1Len5).sliced(2)
  lazy val v2Len3 = len3F(a2Len3)

  lazy val v1Len5From2Until4 = v1Len5.sliced(2, 4)
  lazy val v2Len3From1Until2 = v2Len3.sliced(1, 2)

  val len5And3F = Factory[Int](5, 3)
  val len0And3F = Factory[Int](0, 3)

  lazy val len5And3 = len5And3F(a1Len5, a2Len3)

  lazy val emptyDouble = len0F(Array.empty[Double])

  lazy val len5And3Rev = len5And3.reversed
  lazy val a1Len5Rev = v1Len5.reversed
  lazy val emptyDoubleRev = emptyDouble.reversed

  lazy val len5And3From1Until5 = len5And3.sliced(1, 5)
  lazy val len5And3From6Until7 = len5And3.sliced(6, 7)
  lazy val len5And3From4Until6 = len5And3.sliced(4, 6)

  lazy val len5And3Dbl = Factory._doubled(len5And3)

  test("1-array - factory") {
    an [IllegalArgumentException] must be thrownBy Factory[Double](-1)
    an [IllegalArgumentException] must be thrownBy len3F(null)
  }

  test("1-array - size") {
    v2Len3.size must be (3)
    v1Len5From2.size must be (5 - 2)
  }

  test("2-array - size") {
    assert(len5And3F(a1Len5, a2Len3).size === 8)
    assert(len0And3F(Array.empty, a2Len3).size === 3)
    val len0And0F = Factory[Int](0, 0) // TODO: why I cannot inline this?
    assert(len0And0F(Array.empty, Array.empty).size === 0)
    assert(len0And3F(a1Len5, a1Len5).size === 3)
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

  test("1-array - reversed") {
    assert(a1Len5Rev.size === 5)
    assert(emptyDoubleRev.size === 0)

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

  test("2-array - reversed (first empty)") {
    val len0And3 = len0And3F(Array.empty, a2Len3)
    assert(len0And3(0) === 500)
    assert(len0And3(1) === 600)
    assert(len0And3(2) === 700)

    val aPiDigit0To4 = Array(3, 1, 4, 1, 5)
    val vPiDigit0To4F = Factory[Int](aPiDigit0To4.length, 0)
    val vPiDigit0To4 = vPiDigit0To4F(aPiDigit0To4, Array.empty)
    val vPiDigit4To0 = vPiDigit0To4.reversed
    vPiDigit4To0 must not be (anInstanceOf[ReversedArray1[_]])
    vPiDigit4To0 must be (anInstanceOf[ReversedArray2[_]])
    vPiDigit4To0.size must be (5)
    vPiDigit4To0(0) must be (5)
    vPiDigit4To0(4) must be (3)

    // verify the same instance is returned after 2nd reversal, not a copy
    val vPiDigit0To4Orig = vPiDigit4To0.reversed
    assert(vPiDigit0To4Orig eq vPiDigit0To4)

    // verify update to array is seen by the view
    // TODO: implement setters
    // vPiDigit0To4(2) = 44
    // vPiDigit4To0(2) must be (44)
    // vPiDigit0To4(4) = 50
    // vPiDigit4To0(0) must be (50)
  }

  test("generic - reversed") {
    // Factory is not a stable identifier and FixedArrayViewFactory is a trait,
    // so it seems like we need to extend the factory if we want to use
    // path-dependent types ApplyS and Rep,
    // class CustomFactory extends FixedArrayViewFactory with Driver {
    //   val len5And3Dbl = new FixedArrayView[Int] with ApplyS[Int] {
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

  test("1-array - slice") {
    // slice a portion of size 2 from the middle
    v1Len5From2Until4 must be (anInstanceOf[Array1Slice[_]])
    v1Len5From2Until4.size must be (4 - 2)
    val (fst, snd) = (20, 30)
    v1Len5From2Until4(0) must be (fst)
    v1Len5From2Until4(1) must be (snd)
    val v1Len5From2Until4Rev = v1Len5From2Until4.reversed
    v1Len5From2Until4Rev must be (anInstanceOf[ReversedArray1Slice[_]])
    assert(v1Len5From2Until4Rev.reversed === v1Len5From2Until4)

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
    // verify slicing into 1-array slices
    len5And3From1Until5.size must be (5 - 1)
    for (i <- 1 until 5)
      assert(len5And3(i) === len5And3From1Until5(i - 1), "for i=" + i)
    len5And3From1Until5 must be (anInstanceOf[Array1Slice[_]])
    len5And3From6Until7.size must be (7 - 6)
    len5And3From6Until7(0) must be (600)
    len5And3From6Until7 must be (anInstanceOf[Array1Slice[_]])

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

  test("generic - sliced") {
    val len5And3DblFrom3Until7 = len5And3Dbl.sliced(3, 7)
    len5And3DblFrom3Until7.size must be (7 - 3)
    for (i <- 3 until 7)
      assert(len5And3Dbl(i) === len5And3DblFrom3Until7(i - 3), "for i=" + i)
    len5And3DblFrom3Until7 must not be (anInstanceOf[Array1[_]])
    len5And3DblFrom3Until7 must not be (anInstanceOf[Array2[_]])
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

class FixedArrayViewScalaCodegenTest extends FunSuite with TypeMatchers
    with ClassMatchers
    with BeforeAndAfterAll
    with ViewFactoryProvider[FixedArrayViewFactory] {
  import FixedArrayViewTest.{a1Len5, a2Len3}
  import FixedArrayView.{Factory => _, _} // mock the factory
  def mkFactory = new FixedArrayViewFactory with Driver with CompileMock

  override def beforeAll() {
    Factory.reset
  }

  type ApplyS[T] = FixedArrayViewFactory#ApplyS[T]

  import CompileMock._
  import scala.language.reflectiveCalls
  import scala.reflect.runtime.universe._

  val len5F = Factory[Int](5)

  lazy val len5 = len5F(a1Len5)

  lazy val len5From2Until7 = len5.sliced(2, 7)

  lazy val len19And23F = Factory[Int](19, 23)
  lazy val len19And23 = len19And23F(a1Len5, a2Len3)

  lazy val len0And3F = Factory[Int](0, 3)
  lazy val len0And3 = len0And3F(Array.empty, a2Len3)

  lazy val len19And23Dbl = Factory._doubled(len19And23)

  lazy val len0And3Rev = len0And3.reversed
  lazy val len19And23Rev = len19And23.reversed

  lazy val len19And23From13Until18 = len19And23.sliced(13, 18)
  lazy val len19And23From18Until21 = len19And23.sliced(18, 21)
  lazy val len19And23From21Until42 = len19And23.sliced(21, 42)

  def getApplyC[T](v: FixedArrayView[T]) = v.asInstanceOf[ApplyS[_]].applyC

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

  test("applyC - Array2 (first empty)") {
    // verify the branching and boolean and is optimized away, since
    val method = getApplyC(len0And3)
    method.body must not include ("- 0")
    method.body must not include ("&&")
    method.body must not include ("<")
    method.body must not include ("if")
    method.body must fullyMatch regex """val (x[0-9]*).*Array\(500,600,700\).*
val ([^ ]*) = \1\(.*
\2"""
  }

  test("reversed - Array2") {
    val method = getApplyC(len19And23Rev)
    method.body must startWith regex "val .* = 41 - .*"
  }

  test("reversed - Array2 (first empty)") {
    val method = getApplyC(len0And3Rev)
    method.body must include ("2 -")
    method.body must not include ("if")
    method.body.count(_ == '\n') must be (3)
  }

  test("reversed - generic") {
    len19And23Dbl.apply(1) must be (20)
    val len19And23DblRev = len19And23Dbl.reversed
    len19And23DblRev must be (anInstanceOf[ApplyS[_]])
  }

  test("sliced - Array1") {
    val method = getApplyC(len5From2Until7)
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
    // TODO: support such an arithmetic optimization in FixedArrayViewFactory
    // method.body must not contain allOf ('-', '+')
    // method.body must not include ("20")
    // method.body must not include ("2")
    // method.body must include ("22")
    // method.body.count(_ == '-') must be (1)
  }
}
