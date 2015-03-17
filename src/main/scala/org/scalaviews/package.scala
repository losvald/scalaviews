/*
 * package.scala
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

package org

import scala.virtualization.lms.common._

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

  private[scalaviews] trait ScalaViewExp extends ScalaOpsPkgExp
      with CompileScala { self =>
    type Codegen = ScalaCodeGenPkg
    val codegen = new Codegen { val IR: self.type = self }

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
}
