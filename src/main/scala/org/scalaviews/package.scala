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
  }

  private[scalaviews] trait ScalaViewExp extends ScalaOpsPkgExp
      with CompileScala { self =>
    type Codegen = ScalaCodeGenPkg
    val codegen = new Codegen { val IR: self.type = self }
  }
}
