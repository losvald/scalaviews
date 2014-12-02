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

  val len5And3F = Factory[Int](5, 3)
  val len0And3F = Factory[Int](0, 3)

  test("size - 2 chunks") {
    assert(len5And3F(a1Len5, a2Len3).size === 8)
    assert(len0And3F(Array.empty, a2Len3).size === 3)
    val len0And0F = Factory[Int](0, 0) // TODO: why I cannot inline this?
    assert(len0And0F(Array.empty, Array.empty).size === 0)
    assert(len0And3F(a1Len5, a1Len5).size === 3)
  }

  test("apply - 2 chunks (Int)") {
    val len5And3 = len5And3F(a1Len5, a2Len3)
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
}

class FixedArrayViewScalaCodegenTest extends FunSuite with TypeMatchers
    with ViewFactoryProvider[FixedArrayViewFactory] {
  import FixedArrayViewTest.{a1Len5, a2Len3}
  import FixedArrayView.{Factory => _, _} // mock the factory
  def mkFactory = new FixedArrayViewFactory with Driver with CompileMock

  import CompileMock._
  import scala.language.reflectiveCalls
  import scala.reflect.runtime.universe._

  val len19And23F = Factory[Int](19, 23)
  val len19And23 = len19And23F(a1Len5, a2Len3)

  val len0And3F = Factory[Int](0, 3)
  val len0And3 = len0And3F(Array.empty, a2Len3)

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
  }
}
