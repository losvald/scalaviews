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

package org.scalaviews

import org.scalatest.matchers.{BeMatcher,MatchResult}
import org.scalatest.{MustMatchers,Tag}

object Metatest extends Tag("Metatest")

trait ClassMatchers extends MustMatchers {
  import scala.reflect._

  def ofClass[T: ClassTag] = BeMatcher { obj: Any =>
    val cls = classTag[T].runtimeClass
    MatchResult(
      obj.getClass == cls,
      obj.toString + " was not an instance of " + cls.toString,
      obj.toString + " was an instance of " + cls.toString
    )
  }

  def anInstanceOf[T: ClassTag] = BeMatcher { obj: Any =>
    val cls = classTag[T].runtimeClass
    MatchResult(
      cls.isAssignableFrom(obj.getClass),
      obj.getClass.toString + " was not assignable from " + cls.toString,
      obj.getClass.toString + " was assignable from " + cls.toString
    )
  }
}

trait TypeMatchers extends MustMatchers {
  import scala.reflect.runtime.universe._

  def ofType[T: TypeTag] = BeMatcher { obj: TypeTag[_] =>
    val tpe = typeTag[T].tpe
    MatchResult(
      obj.tpe =:= tpe,
      obj.toString + " was not of type " + tpe.toString,
      obj.toString + " was of type " + tpe.toString
    )
  }
}
