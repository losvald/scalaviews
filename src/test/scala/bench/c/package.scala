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

      // XXX: dirty hack to avoid a bug when generating static array
      override def addRef(): String = " "

      def getMemoryAllocString(count: String, memType: String): String =
        s"new $memType[$count]()"

      // XXX: the rest is copied from dslapi.scala in LMS tutorial
      override def format(s: Exp[Any]): String = {
        remap(s.tp) match {
          case "uint16_t" => "%c"
          case "bool" | "int8_t" | "int16_t" | "int32_t" => "%d"
          case "int64_t" => "%ld"
          case "float" | "double" => "%f"
          case "string" => "%s"
          case "char*" => "%s"
          case "char" => "%c"
          case "void" => "%c"
          case _ =>
            throw new scala.virtualization.lms.internal.
              GenerationFailedException(
                "CGenMiscOps: cannot print type " + remap(s.tp))
        }
      }
      override def quoteRawString(s: Exp[Any]): String = {
        remap(s.tp) match {
          case "string" => quote(s) + ".c_str()"
          case _ => quote(s)
        }
      }
      override def isPrimitiveType(tpe: String) : Boolean = {
        // treat string as a primitive type to prevent memory management
        // (always stack allocated and freed automatically at the scope exit)
        tpe match {
          case "char*" => true
          case "char" => true
          case _ => super.isPrimitiveType(tpe)
        }
      }
      override def quote(x: Exp[Any]) = x match {
        // TODO: more escapes?
        case Const(s: String) => "\""+s.replace("\"", "\\\"")+"\""
        case Const('\n') if x.tp == manifest[Char] => "'\\n'"
        case Const('\t') if x.tp == manifest[Char] => "'\\t'"
        case Const('\0') if x.tp == manifest[Char] => "'\\0'"
        case _ => super.quote(x)
      }
      override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
        case a@ArrayNew(n) =>
          val arrType = remap(a.m)
          stream.println(arrType + "* " + quote(sym) + " = " +
            getMemoryAllocString(quote(n), arrType) + ";")
        case ArrayApply(x,n) => emitValDef(sym, quote(x) + "[" + quote(n) + "]")
        case ArrayUpdate(x,n,y) =>
          stream.println(quote(x) + "[" + quote(n) + "] = " + quote(y) + ";")
        case PrintLn(s) =>
          stream.println("printf(\"" + format(s) + "\\n\"," +
            quoteRawString(s) + ");")
        case StringCharAt(s,i) =>
          emitValDef(sym, "%s[%s]".format(quote(s), quote(i)))
        // case Comment(s, verbose, b) =>
        //   stream.println("//#" + s)
        //   if (verbose) {
        //     stream.println("// generated code for " + s.replace('_', ' '))
        //   } else {
        //     stream.println("// generated code")
        //   }
        //   emitBlock(b)
        //   emitValDef(sym, quote(getBlockResult(b)))
        //   stream.println("//#" + s)
        case _ => super.emitNode(sym,rhs)
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

  abstract class Function2Mock[A1, A2, B] extends ((A1, A2) => B) {
    val body: String
    override def apply(a1: A1, a2: A2) = null.asInstanceOf[B]
  }

  // tags
  import org.scalatest.Tag
  object CppViewsTest extends Tag("org.scalaviews.tags.CppViewsTest")

  import scala.language.implicitConversions

  implicit def mockFunction1[A, B](f: A => B) =
    f.asInstanceOf[Function1Mock[A, B]]

  implicit def mockFunction2[A1, A2, B](f: (A1, A2) => B) =
    f.asInstanceOf[Function2Mock[A1, A2, B]]

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
    override def compile2[A1, A2, B](f: (Exp[A1], Exp[A2]) => Exp[B])(
      implicit mA: Manifest[A1], mA2: Manifest[A2], mB: Manifest[B]
    ) = new Function2Mock[A1, A2, B] {
      lazy val body: String = {
        val source = new java.io.StringWriter
        codegen.emitSource2(f, "snippet", new java.io.PrintWriter(source))
        source.toString
      }
    }
  }
}
