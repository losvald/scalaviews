/*
 * CompileMock.scala
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

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.reflect.runtime.universe._

import java.io.{ByteArrayOutputStream, PrintWriter}

trait CompileMock extends CompileScala { this: BaseExp =>
  abstract class FunWithSource[A, B] extends Function1[A, B] {
    val body: String // avoid class parameter so that it can be lazy
    val paramType: TypeTag[_]
    val resultType: TypeTag[_]
  }

  // note we cannot extend Compile since ScalaCompile doesn't mix in Compile
  abstract override def compile[A, B](f: Exp[A] => Exp[B])(
    implicit mA: Manifest[A], mB: Manifest[B]
  ): FunWithSource[A, B] = {
    def emitBody() = {
      val baos = new ByteArrayOutputStream
      codegen.emitSource(f, "MockCompiledSnippet", new PrintWriter(baos))
      val source = baos.toString
      val s = source.view(source.indexOf('{') + 1, source.lastIndexOf('}'))
      s.slice(s.indexOf('{') + 1, s.lastIndexOf('}')).force.trim
    }

    // mock the function, but provide the lazily evaluated "source" attribute,
    // as well as the parameter and result types
    new FunWithSource[A, B] {
      lazy val body = emitBody()
      val paramType = typeTag[A]
      val resultType = typeTag[B]
      def apply(a: A) = null.asInstanceOf[B]
    }
  }
}

object CompileMock {
  implicit def function1ToFunWithSource[A, B](f: Function1[A, B]) =
    f.asInstanceOf[CompileMock#FunWithSource[A, B]]
}
