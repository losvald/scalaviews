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
  trait ViewFactory extends Compile {
    // this allows us to call Expressions's reset after it is mixed in
    private[scalaviews] def reset
  }

  // helper for providing reusable thread-safe singleton factories
  private[scalaviews] trait ViewFactoryProvider[F <: ViewFactory] {
    def Factory = {
      val f = factory.get()
      // HACK: clear compilation state before each call of a factory method,
      // so we don't need to call reset() at the beginning of each method
      f.reset // XXX: might create problems when combined with lazy compilation
      f
    }

    protected def mkFactory: F

    private val factory = new java.lang.ThreadLocal[F] {
      override def initialValue = mkFactory
    }
  }

  private[scalaviews] trait ScalaViewExp extends ScalaOpsPkgExp
      with CompileScala { self =>
    type Codegen = ScalaCodeGenPkg
    val codegen = new Codegen { val IR: self.type = self }
  }

  private[scalaviews] object ExpOpt {
    import scala.reflect.SourceContext

    trait BooleanAnd extends BooleanOpsExpOpt {
      override def boolean_and(lhs: Exp[Boolean], rhs: Exp[Boolean])(
        implicit pos: SourceContext
      ): Exp[Boolean] = (lhs, rhs) match {
        case (Const(true), _) => rhs
        case (_, Const(true)) => lhs
        case (c @ Const(false), _) => c // short-circuit
        case _ => super.boolean_and(lhs, rhs)
      }
    }
  }
}
