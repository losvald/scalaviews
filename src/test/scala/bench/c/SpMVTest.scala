/*
 * SpMVTest.scala
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

package org.scalaviews.bench

import org.scalaviews.ArrayView2D

import org.scalatest.{FunSuite,MustMatchers}

class SpMVTest extends FunSuite with MustMatchers {
  val ArrayView2D = CArrayView2D.Factory
  type ArrayView2DS[T] = org.scalaviews.ArrayView2DFactory#ViewS[T]

  import scala.language.implicitConversions
  implicit def view2ViewS[T: Manifest](v: ArrayView2D[T]): ArrayView2DS[T] =
    v.asInstanceOf[ArrayView2DS[T]]

  def mkIntArrayRegex(values: Int*) =
    """int ?\*.* = \(int32_t\[\]\)\{""" + values.mkString(",") + """\};"""

  test("multByVectorC - static diag") {
    val a = Array(4, 5, 6)
    val body = ArrayView2D.diag(a).multByVectorC.body
    body must include regex mkIntArrayRegex(4, 5, 6)
    body.count(_ == '+') must be (a.length)
    // TODO: more assertions
  }

  test("multByVectorC - dynamic chain of diags") {
    // a2    a3
    //    a2    a3
    //             a3
    // a5
    //    a5
    //       ..
    val a2 = new Array[Int](2)
    val cd2e1 = ArrayView2D.diag(a2).along(0) :+ ArrayView2D.impl((1, 2), 0)
    val a3 = new Array[Int](3)
    val cd2e1d3 = cd2e1.along(1) :+ ArrayView2D.diag(a3)
    val a5 = new Array[Int](5)
    val m = cd2e1d3.along(0) :+ ArrayView2D.diag(a5)

    // change the values on diagonals through the view before generating code
    m(0, 0) = 90
    m(1, 1) = 8
    m(0, 2) = 70000
    m(1, 3) = 600
    m(2, 4) = 5000
    m(3, 0) = 400000
    m(4, 1) = 3000000
    m(5, 2) = 20000000
    m(7, 4) = 100000000

    // verify the updates are correctly reflected in the generated code
    val body = m.multByVectorC.body
    body must include regex """(?s)""" + List(
      mkIntArrayRegex(90, 8),
      mkIntArrayRegex(70000, 600, 5000),
      mkIntArrayRegex(400000,3000000,20000000,0,100000000)).mkString(".*")
  }

  def genSmvHeader[T : Numeric](
    v: ArrayView2DS[T], name: String, dataType: String
  ) = {
    val (rowCnt, colCnt) = v.sizes
    val multByVectorCBodyFixed = v.multByVectorC.body.replaceAll(
      """(?m)^([a-z_ *]*)\*( ?\w+) = ([^{;]*)(\{.*\})""",
      """static $1$2[] = $4""")
    val guard = ("CPPVIEWS_BENCH_SM_SCALAVIEW_" + name.toUpperCase +
      "_HPP_")
    s"""// generated by ScalaViews + LMS
#ifndef $guard
#define $guard

#include "facade.hpp"

$multByVectorCBodyFixed

struct $name
    : public SmvFacade<Scalaview<$dataType, $rowCnt, $colCnt, $name>> {
  template<typename T> T* CVecMult(T* v) const { return snippet(v); }
};

#endif  // $guard
"""
  }

  test("genSmvHeader", CppViewsTest) {
    val (name, dataType) = ("ADiagView", "double")
    val hdr = genSmvHeader(
      ArrayView2D.diag(Array(3, 0.14)),
      name, dataType)

    // verify the header include guard name is derived from the name (unique)
    hdr must fullyMatch regex ("""(?s)//.*
#ifndef CPPVIEWS_BENCH_SM_SCALAVIEW_(\w+)_HPP_
#define CPPVIEWS_BENCH_SM_SCALAVIEW_\1_HPP_
.*
#endif\b.*""" withGroups ("ADIAGVIEW"))

    // verify the class with the right inheritance is generated
    hdr must include regex ("(?sm)" +
      """^(?:struct|class) (\w+)[^:{]*""" +
      """:[^:]*public SmvFacade<\s*""" +
      """(\w+)<\s*(\w+),\s*(\d+),\s*(\d+),\s*\1\s*>\s*> \{"""
      withGroups (name, "Scalaview", dataType, "2", "2"))

    // Verify the method & function wrapper is generated for each staged method
    def mkContainsMethodRegex(method: String) =
      s"(?s)(?:struct|class) $name[^{]+\\{.*$method.*\\}"

    { // multByVectorC
      val (method, function) = ("CVecMult", "snippet")
      hdr must include regex mkContainsMethodRegex(
        s"$method\\([^)]* (\\w+)\\) const \\{ return $function\\(\\w+\\); \\}")
      hdr must include regex ("""(?sm)
^(\w+) ?\* ?(\w+)\(\1 ?\* +(?:\w+)\) \{
\1 ?\* ?\w+ = new \1\[(\d+)\]\(\);
.*^static \1 \w+\[\] = \{([+-.0-9]+),([+-.0-9]+)\};
.*^return \w+;
\}""" withGroups (dataType, function, "2", "3.0", "0.14"))
    }
  }
}
