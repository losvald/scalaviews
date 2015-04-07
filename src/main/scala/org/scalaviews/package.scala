/*
 * package.scala
 *
 * Copyright (C) 2014-2015 Leo Osvald <leo.osvald@gmail.com>
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

package org

import scala.virtualization.lms.common.{
  // hide the incomplete implementations and extend them below
  Compile=>Compile1, CompileScala=>CompileScala1,
  _}

package object scalaviews {
  trait View[+T] {
    def size: Int
    def foreach(f: T => Unit): Unit
    def iterator: Iterator[T]
  }

  trait ViewFactory extends Compile {
    // this allows us to call Expressions's reset after it is mixed in
    private[scalaviews] def reset
  }

  // helper for providing reusable thread-safe singleton factories
  private[scalaviews] trait ViewFactoryProvider[F <: ViewFactory] {
    type Factory = F
    def Factory = factory.get() // calling reset would break shared staged code

    protected def mkFactory: F

    private val factory = new ThreadLocal[F] {
      override def initialValue = mkFactory
    }
  }

  private[scalaviews] trait ViewExp extends ScalaOpsPkgExp
      with StaticDataExp with IfThenElseExpOpt

  private[scalaviews] trait ScalaDriver extends CompileScala { self: ViewExp =>
    trait Codegen extends ScalaCodeGenPkg with ScalaGenStaticData {
      val IR: self.type = self
    }
    val codegen = new Codegen {}

    override def compile[A, B](f: Exp[A] => Exp[B])(
      implicit mA: Manifest[A], mB: Manifest[B]
    ) = Console.withOut(reportOutputStream) {
      super.compile(f)
    }

    // Suppress status reporting by default, can be overridden
    def reportOutputStream = Util.nullOutputStream
  }

  private[scalaviews] object Util {
    val nullOutputStream = new java.io.OutputStream {
      override def write(b: Int) = {}
      override def write(b: Array[Byte]) = {}
      override def write(b: Array[Byte], off: Int, len: Int) = {}
    }
  }

  // TODO: add to LMS the following generalization of compile

  private[scalaviews] trait Compile extends Compile1 {
    def compile2[A1,A2,B](f: (Rep[A1], Rep[A2]) => Rep[B])(
      implicit mA1: Manifest[A1], mA2: Manifest[A2], mB: Manifest[B]
    ): (A1, A2) => B
  }

  private[scalaviews] trait CompileScala extends CompileScala1 {
    def compile2[A1,A2,B](f: (Exp[A1], Exp[A2]) => Exp[B])(
      implicit mA1: Manifest[A1], mA2: Manifest[A2], mB: Manifest[B]
    ): (A1, A2) => B = {
      if (this.compiler eq null)
        setupCompiler()

      val className = "staged$" + compileCount
      compileCount += 1

      val source = new java.io.StringWriter()
      val writer = new java.io.PrintWriter(source)
      val staticData = codegen.emitSource2(f, className, writer)

      codegen.emitDataStructures(writer)

      if (dumpGeneratedCode) println(source)

      val fileSystem = new scala.tools.nsc.io.VirtualDirectory("<vfs>", None)
      val compiler = this.compiler // TODO: is this necessary?
      compiler.settings.outputDirs.setSingleOutput(fileSystem)

      val run = new compiler.Run
      run.compileSources(
        List(new scala.reflect.internal.util.BatchSourceFile(
          "<stdin>", source.toString)))
      reporter.printSummary()

      if (!reporter.hasErrors)
        println("compilation: ok")
      else
        println("compilation: had errors")

      reporter.reset

      val parent = this.getClass.getClassLoader
      val loader = new scala.tools.nsc.interpreter.AbstractFileClassLoader(
        fileSystem,
        this.getClass.getClassLoader)

      val cls: Class[_] = loader.loadClass(className)
      val cons = cls.getConstructor(staticData.map(_._1.tp.runtimeClass): _*)

      cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]): _*)
        .asInstanceOf[(A1, A2) => B]
    }
  }
}
