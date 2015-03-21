/*
 * package.scala
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

package object bench {

  object CArrayView2D extends ViewFactoryProvider[ArrayView2DFactory] {
    override protected def mkFactory =
      new ArrayView2DFactory
          with ViewExp
          with CDriver
          with CCompileMock
  }

  private[bench] trait CDriver { self: ViewExp =>
    trait Codegen extends CCodeGenPkg
        with CGenStaticData {
      val IR: self.type = self

      // XXX: workaround for probable remap bug with Array[Int] / Array[Double]
      override def remap[A](m: Manifest[A]): String = m.toString match {
        case "Array[Int]" => "int*"
        case "Array[Double]" => "double*"
        case _ => super.remap(m)
      }
    }
    val codegen = new Codegen {}
  }

  // Since we cannot compile the generated C code, mock the compile
  // (this should work fine as long as no real function is called on a view)
  abstract class Function1Mock[A, B] extends (A => B) {
    val body: String
    override def apply(a: A) = null.asInstanceOf[B]
  }

  import scala.language.implicitConversions

  implicit def mockFunction1[A, B](f: A => B) =
    f.asInstanceOf[Function1Mock[A, B]]

  // XXX: make org/scalaviews/CompileMock.scala more generic and reuse it here
  private[bench] trait CCompileMock { this: CDriver with ViewExp with Compile =>
    override def compile[A, B](f: Exp[A] => Exp[B])(
      implicit mA: Manifest[A], mB: Manifest[B]
    ) = new Function1Mock[A, B] {
      lazy val body: String = {
        val source = new java.io.StringWriter
        codegen.emitSource(f, "snippet", new java.io.PrintWriter(source))
        source.toString
      }
    }
  }
}
