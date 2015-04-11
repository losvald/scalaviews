/*
 * io.scala
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

package bench

import scala.io.Source

package object io {

  def fromMatrixMarket[T](source: Source)(
    implicit ev: Numeric[T]
  ): ((Int, Int), Iterator[(Int, Int, T)]) = {
    import ev.mkNumericOps
    def splitByWhitespace(line: String) = line.trim.split("\\s+")
    def splitNext(it: Iterator[String]) = splitByWhitespace(it.next())
    val (hdrIt, dataIt) = source.getLines().span(_.startsWith("%"))
    val Array("%%MatrixMarket", "matrix",
      format, valueType, sym) = splitNext(hdrIt)
    require(format == "coordinate", "only coordinate format is implemented")
    val Array(rowCnt, colCnt, entryCnt) = splitNext(dataIt).map(_.toInt)
    val it = dataIt.map { line =>
      val tokens = splitByWhitespace(line)
      (tokens(0).toInt - 1, tokens(1).toInt - 1, (valueType match {
        // TODO: widen the runtime type instead of assuming T = (runtime type)
        case "real" => tokens.last.toDouble.asInstanceOf[T]
        case "integer" => tokens.last.toInt.asInstanceOf[T]
      }))
    }
    ((rowCnt, colCnt), sym match {
      case "general" => it
      case _ =>
        val symTrans = sym match {
          case "symmetric" => (v: T) => v
          case "skew-symmetric" => (v: T) => -v
        }
        val (it1, it2) = it.duplicate
        it1 ++ it2.collect { case (row, col, value) if row != col =>
          (col, row, symTrans(value))
        }
    })
  }

}
