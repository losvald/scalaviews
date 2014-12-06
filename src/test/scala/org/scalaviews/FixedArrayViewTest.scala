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
  type Array2[T] = FixedArrayViewFactory#Array2[T]
  type Reversed[T] = FixedArrayViewFactory#Reversed[T]

  val len5F = Factory[Int](5)
  val len3F = Factory[Int](3)

  lazy val v1Len5 = len5F(a1Len5, 0)
  lazy val v1Len5From2 = len5F(a1Len9, 2)
  lazy val v2Len3 = len3F(a2Len3, 0)

  lazy val v1Len5From2Until4 = Factory.sliced(v1Len5, 2, 4)
  lazy val v2Len5From1Until2 = Factory.sliced(v2Len3, 1, 2)

  val len5And3F = Factory[Int](5, 3)
  val len0And3F = Factory[Int](0, 3)

  lazy val len5And3 = len5And3F(a1Len5, a2Len3)

  lazy val len5And3Rev = Factory.reversed(len5And3)
  lazy val a1Len5Rev = Factory.reversedArray(a1Len5)
  lazy val emptyDoubleRev = Factory.reversedArray(Array.empty[Double])

  lazy val len5And3From1Until5 = Factory.sliced(len5And3, 1, 5)
  lazy val len5And3From6Until7 = Factory.sliced(len5And3, 6, 7)
  lazy val len5And3From4Until6 = Factory.sliced(len5And3, 4, 6)

  lazy val len5And3Dbl = Factory._doubled(len5And3)

  test("ctor - Array1") {
    an [IllegalArgumentException] must be thrownBy len3F(a1Len5, 4)
    an [IllegalArgumentException] must be thrownBy len3F(a1Len5, -1)
    an [IllegalArgumentException] must be thrownBy Factory[Double](-1)
  }

  test("size - Array1") {
    v2Len3.size must be (3)
    v1Len5From2.size must be (5)
  }

  test("size - 2 chunks") {
    assert(len5And3F(a1Len5, a2Len3).size === 8)
    assert(len0And3F(Array.empty, a2Len3).size === 3)
    val len0And0F = Factory[Int](0, 0) // TODO: why I cannot inline this?
    assert(len0And0F(Array.empty, Array.empty).size === 0)
    assert(len0And3F(a1Len5, a1Len5).size === 3)
  }

  test("size - reversed") {
    assert(a1Len5Rev.size === 5)
    assert(emptyDoubleRev.size === 0)
    assert(emptyDoubleRev.size === 0)
  }

  test("apply - _doubled", Metatest) {
    len5And3Dbl must not be (anInstanceOf[Array2[_]])
    len5And3Dbl(0) must be (0);  len5And3Dbl(1) must be (20)
    len5And3Dbl(4) must be (80); len5And3Dbl(5) must be (1000)
  }

  test("apply - Array1") {
    v2Len3(1) must be (600)
    v1Len5From2(0) must be (20)
    v1Len5From2(2) must be (40)
  }

  test("apply - Array2 (Int)") {
    assert(len5And3(0) === 0)
    assert(len5And3(1) === 10)
    assert(len5And3(4) === 40)
    assert(len5And3(5) === 500)
    assert(len5And3(7) === 700)
  }

  test("apply - Array2 (Double)") {
    val piF = Factory[Double](2, 2)
    val pi = piF(Array(3.0, 0.1), Array(0.04, 0.001))
    assert(pi(0) === 3)
    assert(pi(1) === 0.1)
    assert(pi(2) === 0.04)
  }

  test("apply - Array2 (first empty)") {
    val len0And3 = len0And3F(Array.empty, a2Len3)
    assert(len0And3(0) === 500)
    assert(len0And3(1) === 600)
    assert(len0And3(2) === 700)
  }

  test("reversed - Array1") {
    val aPiDigit0To4 = Array(3, 1, 4, 1, 5)
    val vPiDigit4To0 = Factory.reversedArray(aPiDigit0To4)
    vPiDigit4To0.size must be (5)
    vPiDigit4To0 must be (anInstanceOf[ReversedArray1[_]])

    // verify update to array is seen by the view
    aPiDigit0To4(2) = 44
    assert(vPiDigit4To0(2) === 44)
    aPiDigit0To4(4) = 50
    assert(vPiDigit4To0(0) === 50)

    // TODO: verify update through a view affects the array
    // vPiDigit4To0(2) = 444
    // assert(aPiDigit0To4(2) === 444)
  }

  test("reversed - reversed of Array1") {
    a1Len5Rev must be (anInstanceOf[ReversedArray1[_]]) // sanity check
    val a1Len5RevTwice = Factory.reversed(a1Len5Rev)
    a1Len5RevTwice must not be (anInstanceOf[ReversedArray1[_]])
    a1Len5RevTwice.size must be (5)
    for (i <- 0 until 5)
      assert(a1Len5(i) === a1Len5RevTwice(i), "for i=" + i)
  }

  test("reversed - Array2") {
    for (i <- 0 until 8)
      assert(len5And3Rev(i) === len5And3(7 - i), "for i=" + i)
    len5And3Rev must be (anInstanceOf[Reversed[_]])
  }

  test("reversed - Array2 (first empty)") {
    val aPiDigit0To4 = Array(3, 1, 4, 1, 5)
    val vPiDigit0To4F = Factory[Int](aPiDigit0To4.length, 0)
    val vPiDigit0To4 = vPiDigit0To4F(aPiDigit0To4, Array.empty)
    val vPiDigit4To0 = Factory.reversed(vPiDigit0To4)
    vPiDigit4To0 must not be (anInstanceOf[ReversedArray1[_]])
    vPiDigit4To0 must be (anInstanceOf[Reversed[_]])
    vPiDigit4To0.size must be (5)
    vPiDigit4To0(0) must be (5)
    vPiDigit4To0(4) must be (3)

    // verify update to array is seen by the view
    // TODO: implement setters
    // vPiDigit0To4(2) = 44
    // vPiDigit4To0(2) must be (44)
    // vPiDigit0To4(4) = 50
    // vPiDigit4To0(0) must be (50)
  }

  test("reversed - generic (non-staged)") {
    val vTrueFalse = new FixedArrayView[Boolean] {
      override val size = 2
      override def apply(i: Int) = if (i == 0) true else false
    }
    val vFalseTrue = Factory.reversed(vTrueFalse)
    vFalseTrue(0) must be (false)
    vFalseTrue(1) must be (true)
    vFalseTrue.size must be (2)
  }

  test("reversed - generic (staged)") {
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
    val len5And3DblRev = Factory.reversed(len5And3Dbl)
    len5And3DblRev must be (anInstanceOf[Reversed[_]])
    len5And3DblRev.size must be (8)
    len5And3DblRev(2) must be (1000)
    len5And3DblRev(7) must be (0)
  }

  test("sliced - Array1") {
    v1Len5From2Until4.size must be (4 - 2)
    v1Len5From2Until4(0) must be (20)
    v1Len5From2Until4(1) must be (30)
    v2Len5From1Until2.size must be (2 - 1)
    v2Len5From1Until2(0) must be (600)
  }

  test("sliced - Array2") {
    len5And3From1Until5.size must be (5 - 1)
    for (i <- 1 until 5)
      assert(len5And3(i) === len5And3From1Until5(i - 1), "for i=" + i)
    len5And3From1Until5 must be (anInstanceOf[Array1[_]])

    len5And3From6Until7.size must be (7 - 6)
    len5And3From6Until7(0) must be (600)
    len5And3From6Until7 must be (anInstanceOf[Array1[_]])

    len5And3From4Until6.size must be (6 - 4)
    len5And3From4Until6(0) must be (40)
    len5And3From4Until6(1) must be (500)
    len5And3From4Until6 must be (anInstanceOf[Array2[_]])
  }

  test("sliced - generic") {
    val len5And3DblFrom3Until7 = Factory.sliced(len5And3Dbl, 3, 7)
    len5And3DblFrom3Until7.size must be (7 - 3)
    for (i <- 3 until 7)
      assert(len5And3Dbl(i) === len5And3DblFrom3Until7(i - 3), "for i=" + i)
    len5And3DblFrom3Until7 must not be (anInstanceOf[Array1[_]])
    len5And3DblFrom3Until7 must not be (anInstanceOf[Array2[_]])
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

  lazy val len5 = len5F(a1Len5, 0)

  lazy val len5From2Until7 = Factory.sliced(len5, 2, 7)

  lazy val len19And23F = Factory[Int](19, 23)
  lazy val len19And23 = len19And23F(a1Len5, a2Len3)

  lazy val len0And3F = Factory[Int](0, 3)
  lazy val len0And3 = len0And3F(Array.empty, a2Len3)

  lazy val len19And23Dbl = Factory._doubled(len19And23)

  lazy val len0And3Rev = Factory.reversed(len0And3)
  lazy val len19And23Rev = Factory.reversed(len19And23)

  lazy val len19And23From13Until18 = Factory.sliced(len19And23, 13, 18)
  lazy val len19And23From18Until21 = Factory.sliced(len19And23, 18, 21)
  lazy val len19And23From21Until42 = Factory.sliced(len19And23, 21, 42)

  test("applyC - Array1") {
    val method = len5.applyC
    method.body must contain noneOf ('-', '+')
    method.body must not include ("if")
    method.body.count(_ == '\n') must be (2)
  }

  test("applyC - Array2") {
    val method = len19And23.applyC
    method.paramType must be (ofType[Int])
    method.resultType must be (ofType[Int])
    method.body must include ("< 19")
    method.body must include ("- 19")
    method.body must not include ("&&")
  }

  test("applyC - Array2 (first empty)") {
    // verify the branching and boolean and is optimized away, since
    val method = len0And3.applyC
    method.body must not include ("- 0")
    method.body must not include ("&&")
    method.body must not include ("<")
    method.body must not include ("if")
    method.body must fullyMatch regex """val (x[0-9]*).*Array\(500,600,700\).*
val ([^ ]*) = \1\(.*
\2"""
  }

  def getApplyC[T](v: FixedArrayView[T]) = v.asInstanceOf[ApplyS[_]].applyC

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
    val len19And23DblRev = Factory.reversed(len19And23Dbl)
    an [ClassCastException] must be thrownBy getApplyC(len19And23DblRev)
    len19And23DblRev must not be (anInstanceOf[ApplyS[_]])
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
    val len19And23DblFrom18To21 = Factory.sliced(len19And23Dbl, 18, 21)
    len19And23DblFrom18To21 must not be (anInstanceOf[ApplyS[_]])
  }

  test("reversed - Array2 (2nd chunk only)") {
    val len19And23From21Until42Rev = Factory.reversed(len19And23From21Until42)
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
