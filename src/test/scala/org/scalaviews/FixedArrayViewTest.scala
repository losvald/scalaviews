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
  type Array2[T] = FixedArrayViewFactory#Array2[T]
  type Reversed[T] = FixedArrayViewFactory#Reversed[T]

  val len5And3F = Factory[Int](5, 3)
  val len0And3F = Factory[Int](0, 3)

  lazy val len5And3 = len5And3F(a1Len5, a2Len3)
  lazy val len5And3Rev = Factory.reversed(len5And3)

  lazy val a1Len5Rev = Factory.reversedArray(a1Len5)
  lazy val emptyDoubleRev = Factory.reversedArray(Array.empty[Double])

  lazy val len5And3Dbl = Factory._doubled(len5And3)

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

  test("apply - 2 chunks (Int)") {
    assert(len5And3(0) === 0)
    assert(len5And3(1) === 10)
    assert(len5And3(4) === 40)
    assert(len5And3(5) === 500)
    assert(len5And3(7) === 700)
  }

  test("apply - 2 chunks (Double)") {
    val piF = Factory[Double](2, 2)
    val pi = piF(Array(3.0, 0.1), Array(0.04, 0.001))
    assert(pi(0) === 3)
    assert(pi(1) === 0.1)
    assert(pi(2) === 0.04)
  }

  test("apply - 2 chunks (first empty)") {
    val len0And3 = len0And3F(Array.empty, a2Len3)
    assert(len0And3(0) === 500)
    assert(len0And3(1) === 600)
    assert(len0And3(2) === 700)
  }

  test("apply - reversed of Array1") {
    val aPiDigit0To4 = Array(3, 1, 4, 1, 5)
    val vPiDigit4To0 = Factory.reversedArray(aPiDigit0To4)
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

  test("apply - reversed of reversed of Array1") {
    a1Len5Rev must be (anInstanceOf[ReversedArray1[_]]) // sanity check
    val a1Len5RevTwice = Factory.reversed(a1Len5Rev)
    a1Len5RevTwice must not be (anInstanceOf[ReversedArray1[_]])
    for (i <- 0 until 5)
      assert(a1Len5(i) === a1Len5RevTwice(i), "for i=" + i)
  }

  test("apply - reversed of Array2") {
    for (i <- 0 until 8)
      assert(len5And3Rev(i) === len5And3(7 - i), "for i=" + i)
    len5And3Rev must be (anInstanceOf[Reversed[_]])
  }

  test("apply - reversed of Array2 (first empty)") {
    val aPiDigit0To4 = Array(3, 1, 4, 1, 5)
    val vPiDigit0To4F = Factory[Int](aPiDigit0To4.length, 0)
    val vPiDigit0To4 = vPiDigit0To4F(aPiDigit0To4, Array.empty)
    val vPiDigit4To0 = Factory.reversed(vPiDigit0To4)
    vPiDigit4To0 must not be (anInstanceOf[ReversedArray1[_]])
    vPiDigit4To0 must be (anInstanceOf[Reversed[_]])
    vPiDigit4To0(0) must be (5)
    vPiDigit4To0(4) must be (3)

    // verify update to array is seen by the view
    // TODO: implement setters
    // vPiDigit0To4(2) = 44
    // vPiDigit4To0(2) must be (44)
    // vPiDigit0To4(4) = 50
    // vPiDigit4To0(0) must be (50)
  }

  test("apply - reversed of generic (non-staged)") {
    val vTrueFalse = new FixedArrayView[Boolean] {
      override val size = 2
      override def apply(i: Int) = if (i == 0) true else false
    }
    val vFalseTrue = Factory.reversed(vTrueFalse)
    vFalseTrue(0) must be (false)
    vFalseTrue(1) must be (true)
  }

  test("apply - reversed of generic (staged)") {
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
}

class FixedArrayViewScalaCodegenTest extends FunSuite with TypeMatchers
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

  lazy val len19And23F = Factory[Int](19, 23)
  lazy val len19And23 = len19And23F(a1Len5, a2Len3)

  lazy val len0And3F = Factory[Int](0, 3)
  lazy val len0And3 = len0And3F(Array.empty, a2Len3)

  lazy val len0And3Rev = Factory.reversed(len0And3)
  lazy val len19And23Rev = Factory.reversed(len19And23)

  test("applyC - 2 chunks") {
    val method = len19And23.applyC
    method.paramType must be (ofType[Int])
    method.resultType must be (ofType[Int])
    method.body must include ("< 19")
    method.body must include ("- 19")
    method.body must not include ("&&")
  }

  test("applyC - 2 chunks (first empty)") {
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

  test("applyC - reversed of Array2") {
    val method = getApplyC(len19And23Rev)
    method.body must startWith regex "val .* = 41 - .*"
  }

  test("applyC - reversed of Array2 (first empty)") {
    val method = getApplyC(len0And3Rev)
    method.body must include ("2 -")
    method.body must not include ("if")
    method.body.count(_ == '\n') must be (3)
  }

  test("applyC - reversed (generic)") {
    val len19And23Dbl = Factory._doubled(len19And23)
    len19And23Dbl.apply(1) must be (20)
    val len19And23DblRev = Factory.reversed(len19And23Dbl)
    an [ClassCastException] must be thrownBy getApplyC(len19And23DblRev)
  }
}
