/*
 * ArrayView2DCodetest.scala
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

import CompileMock._

import scala.virtualization.lms.common._

import org.scalatest.{FunSuite,MustMatchers}

import scala.language.reflectiveCalls
import scala.reflect.runtime.universe._

class ArrayView2DCodetest extends FunSuite with MustMatchers
    with ViewFactoryProvider[ArrayView2DFactory] {
  import ArrayView2D._
  def mkFactory = new FactoryImpl with CompileMock

  import scala.language.implicitConversions
  implicit def view2ViewS[T: Manifest](v: ArrayView2D[T]): Factory#ViewS[T] =
    v.asInstanceOf[Factory#ViewS[T]]

  private def mkIndexesCRegex[T](inds: Int*) =
    ("""val (x\d+) = new Array\[Int\]\(""" + inds.size + """\)""" +
      ("" /: inds.zipWithIndex) { (s, e) => s + """
val .* \1\(""" + e._2 + """\) = """ + e._1
    } + """
\1""")

  test("foreach2 - chain of diags") {
    val d456 = Factory.diag(Array(4, 5, 6))
    val d789 = Factory.diag(Array(7, 8, 9))
    val c0D456D789 = (d456.along(0) :+ d789)
    c0D456D789.indexesC0.body must fullyMatch regex mkIndexesCRegex(
      0, 1, 2, 3, 4, 5)
    // println(c0D456D789.valuesC0.body)
    // println(c0D456D789.dimEntriesC0.body) // TODO: uncomment after fix
    c0D456D789.indexesC1.body must fullyMatch regex mkIndexesCRegex(
      0, 1, 2, 0, 1, 2)
  }

  lazy val p0040_cddLike = {
    val A = Array
    val d2x4Block = A(A(-1, -1, -1, -1), A(1, 1, 1, 1))
    val bd2x4 = Factory.blockDiag(A.fill(3)(d2x4Block))
    val e1 = Factory.impl((3, 1), 0)
    val cd3x3e1 = Factory.chain(1,
      Factory.diag(A.fill(3)(-1634)), e1,
      Factory.diag(A.fill(3)(-2211)), e1,
      Factory.diag(A.fill(3)(-2142)), e1)
    val cbd2x4cd3x3e1 = bd2x4.along(0) :+ cd3x3e1
    val cdcbd2x4cd3x3e1 = Factory.diag(A.fill(9)(1)).along(1) :+ cbd2x4cd3x3e1
    cdcbd2x4cd3x3e1
  }

  test("multByVectorC - nested chain of (block) diags") {
    val body = p0040_cddLike.multByVectorC.body
    val expMultCnt = 9 + 3 * ((2 * 4) + 3 * 1)
    body.count(_ == '*') must be (expMultCnt)
    body.count(_ == '+') must be (expMultCnt +- 1)
  }
}
