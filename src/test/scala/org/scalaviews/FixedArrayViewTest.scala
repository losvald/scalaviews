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

import org.scalatest.FunSuite

trait FixedArrayViewTestBase extends FunSuite {
  type Factory = FixedArrayViewFactory

  val a1Len5 = Array.range(0, 50, 10)
  val a2Len3 = Array.range(500, 800, 100)
  val a1Len9 = Array.range(0, 90, 10)
  val a1Len2 = Array(0, 10)
  val a2Len1 = Array(500)
}

class FixedArrayViewTest extends FixedArrayViewTestBase {
  import FixedArrayView._

  val len5And3F = Factory(5, 3)
  val len0And3F = Factory(0, 3)

  test("size - 2 chunks") {
    assert(len5And3F(a1Len5, a2Len3).size === 8)
    assert(len0And3F(Array.empty, a2Len3).size === 3)
    assert(Factory(0, 0)(Array.empty, Array.empty).size === 0)
    assert(len0And3F(a1Len5, a1Len5).size === 3)
  }

  test("apply - 2 chunks (non-empty)") {
    val len5And3 = len5And3F(a1Len5, a2Len3)
    assert(len5And3(0) === 0)
    assert(len5And3(1) === 10)
    assert(len5And3(4) === 40)
    assert(len5And3(5) === 500)
    assert(len5And3(7) === 700)
  }

  test("apply - 2 chunks (first empty)") {
    val len0And3 = len0And3F(Array.empty, a2Len3)
    assert(len0And3(0) === 500)
    assert(len0And3(1) === 600)
    assert(len0And3(2) === 700)
  }
}

class FixedArrayViewScalaCodegenTest extends FixedArrayViewTestBase {
  import FixedArrayView._

  object FactoryMock extends Factory with Driver with CompileMock {
  }

  import CompileMock._
  import scala.language.reflectiveCalls
  import scala.reflect.runtime.universe._

  val len19And23FM = FactoryMock(19, 23)
  val len19And23M = len19And23FM(a1Len5, a2Len3)

  val len0And3FM = FactoryMock(0, 3)
  val len0And3M = len0And3FM(Array.empty, a2Len3)

  test("sizeC - 2 chunks") {
    val method = len19And23M.sizeC
    assert(method.paramType.tpe =:= typeTag[Unit].tpe)
    assert(method.resultType.tpe =:= typeTag[Int].tpe)
    assert(method.body === "42")
  }

  test("applyC - 2 chunks") {
    val method = len19And23M.applyC
    assert(method.paramType.tpe =:= typeTag[Int].tpe)
    assert(method.resultType.tpe =:= typeTag[Int].tpe)
    assert(method.body.contains("< 19"))
    assert(method.body.contains("- 19"))
  }

  ignore("applyC - 2 chunks (first empty)") {
    // FIXME: comparing if index < 0 is pointless and should be optimized away
    val method = len0And3M.applyC
    assert(!method.body.contains("<"))
  }
}
