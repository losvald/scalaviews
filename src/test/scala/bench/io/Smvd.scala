/*
 * Smvd.scala
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

package org.scalaviews.bench.io

import org.scalaviews.{ArrayView2D,ArrayView2DFactory}

import scala.io.Source

object Smvd {
  def parse[T : Manifest](src: Source)(
    implicit factory: ArrayView2DFactory
  ): ArrayView2D[T] = {
    import scala.util.parsing.json.JSON
    val numParser = JSON.perThreadNumberParser
    JSON.perThreadNumberParser = (value: String) => Integer.parseInt(value)

    def parse0(node: Any): ArrayView2D[T] = {
      val View(v) = node
      val Direction(dir) = v("direction")
      def minMaxIndex(firstKey: String, lastKey: String) = {
        val Index(first) = v(firstKey)
        val Index(last) = v(lastKey)
        if (first < last) (first, last) else (last, first)
      }
      val (rowMin, rowMax) = minMaxIndex("first_row", "last_row")
      val (colMin, colMax) = minMaxIndex("first_col", "last_col")
      val size = (rowMax - rowMin + 1, colMax - colMin + 1)
      val Type(tp) = v("type") // convert to Int
      Type(tp) match {         // convert to Enumeration
        case Type.Diag => {
          require(dir < 2) // TODO: support mirrored diagonal view (2/3)?
          val Size(blockRowCnt) = v("block_rows")
          val Size(blockColCnt) = v("block_cols")
          if (blockRowCnt == 1 && blockColCnt == 1) {
            require(size._1 == size._2) // XXX: if not, pad with implicit view
            factory.diag[T](size._1, None)
          } else {
            val blockSize = (blockRowCnt, blockColCnt)
            require(size._1 % blockSize._1 == 0)
            require(size._2 % blockSize._2 == 0)
            // XXX: if below is not true, pad with an implicit view
            require(size._1 / blockSize._1 == size._2 / blockSize._2)
            factory.blockDiag[T](blockRowCnt, blockColCnt, None)
          }
        }
        case Type.Impl => factory.impl[T](size) // TODO: allow custom functions
        case Type.Chain => {
          val subviews = (for {
            (key, View(nested)) <- v if key.startsWith("_")
          } yield parse0(nested)).toSeq
          factory.chain(dir, subviews: _*)
        }
      }
    }

    try {
      JSON.parseFull(src.mkString) match {
        case Some(View(v)) => parse0(v)
      }
    } finally {
      JSON.perThreadNumberParser = numParser
    }
  }

  private[io] trait JsonNode[T] {
    def unapply(a: Any): Option[T] = Some(a.asInstanceOf[T])
  }

  private[io] object View extends JsonNode[Map[String, Any]]
  private[io] object Type extends Enumeration with JsonNode[Int] {
    type Type = Value
    // copied from: cppviews' src/bench/gui/sm/view_type.hpp
    val Chain,
      Mono,
      Full,
      Sparse,
      Diag,
      Impl = Value
  }

  private[io] object Direction extends JsonNode[Int]
  private[io] object Index extends JsonNode[Int]
  private[io] val Size = Index
}
