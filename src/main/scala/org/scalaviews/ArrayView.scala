/*
 * ArrayView.scala
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

import scala.collection.mutable.ArrayBuilder

private[scalaviews] trait ArrayViewLike[T, +This <: ArrayView[T]]
    extends View[T] with (Int => T) {
  override val size: Int
  def reversed: This
  def sliced(from: Int, until: Int = size): This
  def from(ind: Int): This = sliced(ind)
  def until(ind: Int): This = sliced(0, ind)
  def downTo(ind: Int): This = sliced(ind).reversed.asInstanceOf[This]
  def at(ind: Int): This = sliced(ind, ind + 1)
  protected[scalaviews] def preorder(f: This => Unit): Unit =
    f(this.asInstanceOf[This])
  protected[scalaviews] def inorder(f: This => Unit): Unit =
    f(this.asInstanceOf[This])
}

trait ArrayView[@specialized(Int, Double) T]
    extends ArrayViewLike[T, ArrayView[T]] {
  def :++(that: ArrayView[T]): ArrayView[T]
  def ++:(that: ArrayView[T]): ArrayView[T]
  override def foreach(f: T => Unit): Unit = {
    val iter = iterator
    while (iter.hasNext)
      f(iter.next())
  }
  override def iterator = new Iterator[T] {
    private var ind = 0
    override def next = {
      val ret = ArrayView.this.apply(ind)
      ind += 1
      ret
    }
    override def hasNext = ind != ArrayView.this.size
  }
  protected[scalaviews] val depth: Int = 0
  protected def checkSliceSize(from: Int, until: Int): Boolean = {
    val sliceSize = until - from
    require(0 <= from & sliceSize >= 0)
    sliceSize > 0
  }
}

private[scalaviews]
trait ArrayViewFactory extends ViewFactory with ScalaOpsPkg
    with LiftNumeric with LiftBoolean
    with StaticData
    with IfThenElse
    with NumericOps with PrimitiveOps with BooleanOps
    with Functions
    with LiftVariables
    // with RangeOps with SeqOps with While with Variables
    // with Equal
    // with RangeOps with OrderingOps with MiscOps with ArrayOps with StringOps
    // with SeqOps with While with Variables// with LiftVariables
    // with ObjectOps
{

  private[scalaviews] trait ApplyS[T] { this: ArrayView[T] =>
    override final def apply(i: Int): T = applyC(i)
    private[scalaviews] final lazy val applyC = compile(applyS)

    // public is not allowed when this method is overriden (see below and 3.2.7)
    private[scalaviews] def applyS(i: Rep[Int]): Rep[T]

    private[scalaviews] implicit val t: Manifest[T]
  }

  private[scalaviews] trait ForeachS[T] { this: ArrayView[T] =>
    override final def foreach(f: T => Unit): Unit = foreachC(f)
    private[scalaviews] final lazy val foreachC = compile(foreachS)
    private[scalaviews] def foreachS(f: Rep[T => Unit]): Rep[Unit]
    private[scalaviews] implicit val t: Manifest[T]
  }

  private[scalaviews] trait IteratorS[T] { this: ViewS[T] =>
    override def iterator: Iterator[T] = new Iter
    // make Iter a non-anonymous class so that test code can inspect it
    private[scalaviews] class Iter extends Iterator[T] {
      // compile { u: Rep[Unit] =>
      //   println("[Iter] localIndBegs(0)=" +
      //     staticData(localIndBegs).apply(0) +
      //     " localIndEnds(0)=" + localIndEnds(0))
      // } apply()
      override final def next(): T = nextC()
      override final def hasNext = hasNextC()
      private[scalaviews] final lazy val nextC = compile(nextS)
      private[scalaviews] def nextS(u: Rep[Unit]): Rep[T] = {
        val ret = chunks(chunkInd).apply(localInd)
        // FIXME: changes made to state are not seen until the end of the call
        // XXX: as workaround, introduce a val for each write-read dependency
        // print("[nextS] chunkCnt=" + staticData(localIndBegs.length))
        // print(" state=" + chunkInd + "," + localInd)
        val localIndNext = localInd + (if (chunkRevs(chunkInd)) -1 else 1)
        // print("->" + chunkInd + "," + localIndNext)
        if (localIndNext == localIndEnds(chunkInd)) {
          val chunkIndNext = chunkInd + 1
          // using setter directly avoids a compilation bug (see FIXME below)
          localInd_=(staticData(localIndBegs).apply(chunkIndNext))
          // print("-(next)->" + chunkIndNext + "," + localIndNext)
          chunkInd_=(chunkIndNext)
        } else {
          localInd_=(localIndNext)
          // print("-(same)->" + chunkInd + "," + localIndNext)
        }
        // println(" ret=" + ret)
        ret
      }
      private[scalaviews] lazy val hasNextC = compile(hasNextS)
      private[scalaviews] def hasNextS(u: Rep[Unit]): Rep[Boolean] =
        chunkInd < chunkCnt

      import IteratorS.State
      import State.{ChunkInd,LocalInd} // import values only
      private val state = staticData({
        val a = new Array[Int](State.maxId)
        a(LocalInd.id) = localIndBegs(0)
        a
      })
      private[scalaviews] def chunkInd: Rep[Int] = state.apply(ChunkInd.id)
      private def chunkInd_=(x: Rep[Int]) = state.update(ChunkInd.id, x)
      private[scalaviews] def localInd: Rep[Int] = state.apply(LocalInd.id)
      private def localInd_=(x: Rep[Int]) = state.update(LocalInd.id, x)
    }
    protected def chunkCnt = localIndBegs.length - 1
    private lazy val (chunks, localIndBegs, localIndEnds, chunkRevs) = {
      val begsBuilder = new ArrayBuilder.ofInt
      val endsBuilder = new ArrayBuilder.ofInt
      val csBuf = ArrayBuilder.make[Chunk]
      computeChunks(csBuf, begsBuilder, endsBuilder)
      begsBuilder += 0
      endsBuilder += 0
      val begs = begsBuilder.result()
      val ends = endsBuilder.result()
      // TODO: implement staging of BitSet and use it instead of Array(Buffer)
      val cs = csBuf.result()
      val revs = for ((beg, end) <- begs.zip(ends)) yield beg > end
      (staticData(cs), begs, staticData(ends), staticData(revs))
    }
  }

  private object IteratorS {
    private object State extends Enumeration {
      type State = Value
      val ChunkInd, LocalInd = Value
    }
  }

  private[scalaviews] trait ViewS[T] extends ArrayView[T]
      with ArrayViewLike[T, ViewS[T]]
      with ApplyS[T] with ForeachS[T] {
    override final def :++(that: ArrayView[T]) = that match {
      case that: ViewS[T] => if (that.size > 0) this :++ that else this
      case _ => that ++: this // non-ViewS might have dynamic size, so no check
    }
    override final def ++:(that: ArrayView[T]) = that match {
      case that: ViewS[T] => if (that.size > 0) that ++: this else this
      case _ => this :++ that // non-ViewS might have dynamic size, so no check
    }

    def :++(that: ViewS[T]): ViewS[T] = ViewS.concat(this, that)
    def ++:(that: ViewS[T]): ViewS[T] = ViewS.concat(that, this)

    private[scalaviews] type Chunk = Array[T]
    private[scalaviews] def computeChunks(
      cs: ArrayBuilder[Chunk], begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
    )

    protected def append(that: ViewS[T]): ViewS[T] = new Nested2[T](this, that)
    protected def prepend(that: ViewS[T]): ViewS[T] = new Nested2[T](that, this)
  }

  private[scalaviews] object ViewS {
    def concat[T](v1: ViewS[T], v2: ViewS[T]): ViewS[T] = {
      // do v2.prepend(v1) if it results in the lesser average depth
      val depthDiff = v1.depth - v2.depth
      if (depthDiff < 0 || (depthDiff == 0 && v1.size < v2.size))
        v2.prepend(v1)
      else
        v1.append(v2)
    }
  }

  // TODO: this is a bit messy, since Empty should really belong to
  // ArrayView companion object, but this would require changing the
  // interface of ArrayView not to return subtypes (i.e. give up on *Like)
  private case object Empty extends ViewS[Any] {
    final override val size = 0
    final override def reversed = this
    final override def sliced(from: Int, until: Int = size) = this
    final override def from(ind: Int) = this
    final override def until(ind: Int) = this
    final override def downTo(ind: Int) = this
    final override def at(ind: Int) = this
    final override def :++(that: ViewS[Any]) = that
    final override def ++:(that: ViewS[Any]) = that
    final override def iterator = Nil.iterator // fast & avoids object creation
    final override protected[scalaviews] def preorder(
      f: ViewS[Any] => Unit): Unit = { }
    final override protected[scalaviews] def inorder(
      f: ViewS[Any] => Unit): Unit = { }
    final override protected[scalaviews] val depth = 0
    final override private[scalaviews] def computeChunks(
      cs: ArrayBuilder[Chunk], begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
    ) { }

    // staged methods will not be called, so they can return anything
    final override private[scalaviews] def applyS(i: Rep[Int]) = 0
    final override private[scalaviews] def foreachS(f: Rep[Any => Unit]):
        Rep[Unit] = {}
    final override private[scalaviews] val t = manifest[Any]
  }

  def empty[T: Manifest] = Empty.asInstanceOf[ViewS[T]]

  private def unrollIf(c: Boolean, r: Range) = new {
    def foreach(f: Rep[Int] => Rep[Unit]): Rep[Unit] = {
      if (c) for (i <- r) f(i)
      else for (i <- (r.start until r.end): Rep[Range]) f(i)
    }
  }

  private def unrollForArray(r: Range) = unrollIf(r.size <= 3, r)

  private[scalaviews] case class Array1[T: Manifest](
    override val size: Int,
    a: Array[T]
  ) extends ViewS[T] with IteratorS[T] {
    override lazy val reversed = new ReversedArray1(this) // cache it
    override def sliced(from: Int, until: Int) = {
      if (!checkSliceSize(from, until)) empty
      else new Array1Slice[T](this, from, until)
    }

    override private[scalaviews] def applyS(i: Rep[Int]) =
      staticData(a).apply(i)
    override private[scalaviews] def foreachS(f: Rep[T => Unit]): Rep[Unit] = {
      import scala.language.reflectiveCalls // needed for unrollForArray
      for (i <- unrollForArray(0 until size))
        f(applyS(i))
    }
    override private[scalaviews] val t = manifest[T]
    override private[scalaviews] def computeChunks(
      cs: ArrayBuilder[Chunk], begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
    ) {
      cs += a
      begs += 0
      ends += size
    }
  }

  def apply[T: Manifest](len: Int
  ) = {
    require(len > 0)
    (arr: Array[T]) => {
      require(arr != null)
      new Array1[T](len, arr)
    }
  }

  private[scalaviews] case class Array1Slice[T: Manifest](
    a: Array1[T],
    from: Int, until: Int
  ) extends ViewS[T] with IteratorS[T] {
    override val size = until - from
    override lazy val reversed = // call ctor directly to avoid check & cache it
      new ReversedArray1Slice(a.reversed, a.size - until, a.size - from)
    override final def sliced(from: Int, until: Int) =
      a.sliced(this.from + from, this.from + until)
    override private[scalaviews] def applyS(i: Rep[Int]) = a.applyS(from + i)
    override private[scalaviews] def foreachS(f: Rep[T => Unit]): Rep[Unit] = {
      import scala.language.reflectiveCalls // needed for unrollForArray
      for (i <- unrollForArray(from until until))
        f(a.applyS(i))
    }
    override private[scalaviews] val t = manifest[T]
    override private[scalaviews] def computeChunks(
      cs: ArrayBuilder[Chunk], begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
    ) {
      cs += a.a
      begs += from
      ends += until
    }
  }

  private[scalaviews] case class Array2[T: Manifest](
    override val size: Int,
    a1: Array[T], len1: Int,
    a2: Array[T]
  ) extends ViewS[T] with IteratorS[T] {
    // TODO: consider adding len2 and letting len1/len2 be private[scalaviews]
    override lazy val reversed = // cache it
      new ReversedArray2[T](this, size - len1)
    override def sliced(from: Int, until: Int): ViewS[T] = {
      if (!checkSliceSize(from, until)) empty
      else if (from >= len1)
        new Array1[T](size - len1, a2).sliced(from - len1, until - len1)
      else if (until <= len1) new Array1[T](len1, a1).sliced(from, until)
      else new Array2Slice[T](this, from, until)
    }
    override private[scalaviews] def applyS(i: Rep[Int]) = {
      if (i < len1) staticData(a1).apply(i)
      else staticData(a2).apply(i - len1)
    }
    override private[scalaviews] def foreachS(f: Rep[T => Unit]): Rep[Unit] = {
      import scala.language.reflectiveCalls // needed for unrollForArray
      for (i <- unrollForArray(0 until len1))
        f(staticData(a1).apply(i))
      for (i <- unrollForArray(len1 until size))
        f(staticData(a2).apply(i - len1))
    }
    override private[scalaviews] val t = manifest[T]
    override private[scalaviews] def computeChunks(
      cs: ArrayBuilder[Chunk], begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
    ) {
      cs += a1 += a2
      begs += 0 += 0
      ends += len1 += size - len1
    }
  }

  def apply[T: Manifest](len1: Int, len2: Int) = {
    require(len1 > 0 & len2 > 0)
    (arr1: Array[T], arr2: Array[T]) => { // this compiles for each a1/2
      require(arr1 != null & arr2 != null)
      new Array2(len1 + len2, arr1, len1, arr2)
    }
  }

  private[scalaviews] case class Array2Slice[T: Manifest](
    a: Array2[T],
    from: Int, until: Int
  ) extends ViewS[T] with IteratorS[T] {
    override val size = until - from
    override lazy val reversed = // call ctor directly to avoid check & cache it
      new ReversedArray2Slice(a.reversed, a.size - until, a.size - from)
    override def sliced(from: Int, until: Int) =
      a.sliced(this.from + from, this.from + until)
    override private[scalaviews] def applyS(i: Rep[Int]): Rep[T] =
      a.applyS(from + i)
    override private[scalaviews] def foreachS(f: Rep[T => Unit]): Rep[Unit] = {
      import scala.language.reflectiveCalls // needed for unrollForArray
      for (i <- unrollForArray(from until a.len1))
        f(staticData(a.a1).apply(i))
      for (i <- unrollForArray(a.len1 until until))
        f(staticData(a.a2).apply(i - a.len1))
    }
    override private[scalaviews] val t = manifest[T]
    override private[scalaviews] def computeChunks(
      cs: ArrayBuilder[Chunk], begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
    ) {
      cs += a.a1 += a.a2
      begs += from += 0
      ends += a.len1 += until - a.len1
    }
  }

  private[scalaviews] trait ReversedApplyS[T] extends ApplyS[T] {
    this: ArrayView[T] =>
    private[scalaviews] val rev: ApplyS[T]
    override private[scalaviews] def applyS(i: Rep[Int]): Rep[T] =
       rev.applyS(size - 1 - i)
  }

  private[scalaviews] case class ReversedArray1[T: Manifest](
    private[scalaviews] val rev: Array1[T]
  ) extends ViewS[T] with ReversedApplyS[T] with IteratorS[T]{
    override val size = rev.size
    override val reversed = rev
    override def sliced(from: Int, until: Int) = {
      if (!checkSliceSize(from, until)) empty
      else new ReversedArray1Slice(this, from, until)
    }
    override private[scalaviews] def foreachS(f: Rep[T => Unit]): Rep[Unit] = {
      import scala.language.reflectiveCalls // needed for unrollForArray
      // FIXME: lms compiles this to a while loop with "< 0" instead of "> 0"
      // for (i <- unrollForArray(Range(size - 1, -1, -1))) // size-1 down to 0
      //   f(applyS(i))
      // Also, lms compiles "X to Y" into the same code as "X until Y",
      // so we cannot use "to 0" which would work in scala
      for (i <- unrollForArray(-(size - 1) until 1)) // XXX: hack to avoid bug
        f(rev.applyS(-1 * i)) // XXX: unary minus not supported, so use * -1
    }
    override private[scalaviews] val t = manifest[T]
    override private[scalaviews] def computeChunks(
      cs: ArrayBuilder[Chunk], begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
    ) {
      cs += rev.a
      begs += size - 1
      ends += -1
    }
  }

  private[scalaviews] case class ReversedArray1Slice[T: Manifest](
    a: ReversedArray1[T],
    from: Int, until: Int
  ) extends ViewS[T] with IteratorS[T] {
    override val size = until - from
    override lazy val reversed = // call ctor directly to avoid check & cache it
      new Array1Slice(a.reversed, a.size - until, a.size - from)
    override final def sliced(from: Int, until: Int) =
      a.sliced(this.from + from, this.from + until)
    override private[scalaviews] def applyS(i: Rep[Int]): Rep[T] =
      a.applyS(from + i)
    override private[scalaviews] def foreachS(f: Rep[T => Unit]): Rep[Unit] = {
      import scala.language.reflectiveCalls // needed for unrollForArray
      for (i <- unrollForArray(-(a.size - 1 - from) until 1 - (a.size - until)))
        f(a.rev.applyS(-1 * i)) // XXX: circumvent bugs with step<0 and unary -
    }
    override private[scalaviews] val t = manifest[T]
    override private[scalaviews] def computeChunks(
      cs: ArrayBuilder[Chunk], begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
    ) {
      cs += a.rev.a
      begs += a.size - from - 1
      ends += a.size - until - 1
    }
  }

  private[scalaviews] case class ReversedArray2[T: Manifest](
    private[scalaviews] val rev: Array2[T],
    len2: Int // cache it to avoid the frequent computation (size - rev.len1)
  ) extends ViewS[T] with ReversedApplyS[T] with IteratorS[T] {
    override val size = rev.size
    override val reversed = rev
    override def sliced(from: Int, until: Int): ViewS[T] = {
      // TODO: consider optimizing by storing Array1 portions inside Array2?
      if (!checkSliceSize(from, until)) empty
      else if (from >= len2)
        new Array1[T](rev.len1, rev.a1).reversed.sliced(
          from - len2, until - len2)
      else if (until <= len2)
        new Array1[T](len2, rev.a2).reversed.sliced(from, until)
      else new ReversedArray2Slice[T](this, from, until)
    }
    override private[scalaviews] def foreachS(f: Rep[T => Unit]): Rep[Unit] = {
      import scala.language.reflectiveCalls // needed for unrollForArray
      for (i <- unrollForArray(-(size - 1) until 1)) // XXX: hack to avoid bug
        f(rev.applyS(-1 * i)) // XXX: unary minus not supported, so use * -1
    }
    override private[scalaviews] val t = manifest[T]
    override private[scalaviews] def computeChunks(
      cs: ArrayBuilder[Chunk], begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
    ) {
      cs += rev.a2 += rev.a1
      begs += len2 - 1 += rev.len1 - 1
      ends += -1 += -1
    }
  }

  private[scalaviews] case class ReversedArray2Slice[T: Manifest](
    a: ReversedArray2[T],
    from: Int, until: Int
  ) extends ViewS[T] with IteratorS[T] {
    override val size = until - from
    // XXX: this creates a copy of the original Array2Slice
    override lazy val reversed = // call ctor directly to avoid check & cache it
      new Array2Slice(a.reversed, a.size - until, a.size - from)
    override final def sliced(from: Int, until: Int) =
      a.sliced(this.from + from, this.from + until)
    override private[scalaviews] def applyS(i: Rep[Int]): Rep[T] =
      a.applyS(from + i)
    override private[scalaviews] def foreachS(f: Rep[T => Unit]): Rep[Unit] = {
      import scala.language.reflectiveCalls // needed for unrollForArray
      for (i <- unrollForArray(-(a.len2 - 1 - from) until 1))
        f(staticData(a.rev.a2).apply(-1 * i))
      for (i <- unrollForArray(1 - (a.rev.len1) until 1 - (a.size - until)))
        f(staticData(a.rev.a1).apply(-1 * i))
    }
    override private[scalaviews] val t = manifest[T]
    override private[scalaviews] def computeChunks(
      cs: ArrayBuilder[Chunk], begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
    ) {
      cs += a.rev.a2 += a.rev.a1
      begs += a.len2 - from - 1 += a.rev.len1 - 1
      ends += -1 += a.size - until - 1
    }
  }

  private[scalaviews] val stagedIterThreshold = 1 << 16 // 65K

  private[scalaviews] case class Nested2[T: Manifest](
    v1: ViewS[T],
    v2: ViewS[T],
    vFlatFirst: ViewS[T],
    vFlatLast: ViewS[T]
  ) extends ViewS[T] with IteratorS[T] {
    def this(v1: ViewS[T], v2: ViewS[T]) = this(v1, v2,
      if (v1.isInstanceOf[Nested2[T]]) v1.asInstanceOf[Nested2[T]].vFlatFirst
      else v1,
      if (v2.isInstanceOf[Nested2[T]]) v2.asInstanceOf[Nested2[T]].vFlatLast
      else v2)

    override val size = v1.size + v2.size
    override lazy val reversed = // cache it
      new Nested2[T](v2.reversed, v1.reversed)
    override def sliced(from: Int, until: Int): ViewS[T] = {
      if (!checkSliceSize(from, until)) empty
      else if (from >= v1.size) v2.sliced(from - v1.size, until - v1.size)
      else if (until <= v1.size) v1.sliced(from, until)
      else new Nested2[T](v1.from(from), v2.until(until - v1.size))
    }
    override def iterator: Iterator[T] = {
      // stage the iterator unless the number of chunks is huge,
      // to avoid memory hog due to staging of several huge arrays
      // (which are lazily created, so there will be no memory overhead)
      if (chunkCnt < stagedIterThreshold) super.iterator
      else iteratorUnstaged
    }
    protected[scalaviews] def iteratorUnstaged: Iterator[T] =
      new Nested2.Iter[T](this)

    override protected[scalaviews] val depth =
      1 + scala.math.max(v1.depth, v2.depth)
    override protected[scalaviews] def preorder(f: ViewS[T] => Unit): Unit = {
      f(this)
      v1.preorder(f)
      v2.preorder(f)
    }
    override protected[scalaviews] def inorder(f: ViewS[T] => Unit): Unit = {
      v1.inorder(f)
      f(this)
      v2.inorder(f)
    }
    override private[scalaviews] def applyS(i: Rep[Int]) = {
      if (i < v1.size) v1.applyS(i)
      else v2.applyS(i - v1.size)
    }
    override private[scalaviews] def foreachS(f: Rep[T => Unit]): Rep[Unit] = {
      import scala.language.reflectiveCalls // needed for unrollForArray
      v1.foreachS(f);
      v2.foreachS(f);
    }
    override private[scalaviews] val t = manifest[T]

    override protected def append(that: ViewS[T]) = {
      var v1s: scala.List[ViewS[T]] = scala.Nil
      var last: ViewS[T] = null
      var cur: ViewS[T] = this
      var maxDepth: Int = cur.depth
      while (cur ne vFlatLast) {
        maxDepth -= 1
        val curNested = cur.asInstanceOf[Nested2[T]]
        v1s = curNested.v1 :: v1s
        last = curNested.v2
        cur = if (scala.math.max(last.depth, that.depth) < maxDepth) {
          v1s = last :: v1s
          vFlatLast
        } else last
      }
      if (v1s.head ne last)
        v1s = scala.List(this)

      (that /: v1s)((v2, v1) => new Nested2[T](v1, v2))
    }

    override protected def prepend(that: ViewS[T]) = {
      var v2s: scala.List[ViewS[T]] = scala.Nil
      var first: ViewS[T] = null
      var cur: ViewS[T] = this
      var maxDepth: Int = cur.depth
      while (cur ne vFlatFirst) {
        maxDepth -= 1
        val curNested = cur.asInstanceOf[Nested2[T]]
        v2s = curNested.v2 :: v2s
        first = curNested.v1
        cur = if (scala.math.max(first.depth, that.depth) < maxDepth) {
          v2s = first :: v2s
          vFlatFirst
        } else first
      }
      if (v2s.head ne first)
        v2s = scala.List(this)

      (that /: v2s)(new Nested2[T](_, _))
    }

    override private[scalaviews] def computeChunks(
      cs: ArrayBuilder[Chunk], begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
    ) = {
      v1.computeChunks(cs, begs, ends)
      v2.computeChunks(cs, begs, ends)
    }
  }

  private object Nested2 {
    // declare Iter outside the case class to avoid the implicit this reference
    // (the ctor argument v is not used outside ctor, so no overhead)
    private class Iter[T](v: Nested2[T]) extends scala.Iterator[T] {
      // FIXME: lms framework breaks if a "var" field is reassigned in a block
      // var x = 3
      // def changeX(): Unit = {
      //   x = 42 // ok
      //   if (true) { setX(666) } // ok
      //   // if (true) { x = 666 } // bad
      //   // for (i <- 0 to 2) x = i // bad
      // }
      // def setX(x: Int) { this.x = x }
      // // def x_(x: Int) { this.x = x } // an explicit setter doesn't help
      // As a workaround, we create helper methods:
      def setStack(value: scala.List[Nested2[T]]) { this.stack = value }
      def setFlatIter(value: scala.Iterator[T]) { this.flatIter = value }

      // TODO: perhaps move descend to companion object and make it pure, then:
      // var (stack, flatIter) = descend(v, scala.Nil)
      var flatIter: scala.Iterator[T] = null
      var stack: scala.List[Nested2[T]] = scala.Nil
      descend(v)

      override def hasNext = stack != scala.Nil || flatIter.hasNext
      override def next(): T = {
        if (!flatIter.hasNext) {
          val inorderPred = stack.head; setStack(stack.tail) // pop from stack
          inorderPred.v2 match {
            case v2: Nested2[T] => descend(v2)
            case _ => setFlatIter(inorderPred.v2.iterator)
          }
        }
        flatIter.next()
      }
      private def descend(cur: Nested2[T]): Unit = {
        val vLeftmost = cur.vFlatFirst
        var v: ViewS[T] = cur
        do {
          val vNested = v.asInstanceOf[Nested2[T]]
          stack = vNested :: stack
          v = vNested.v1
        } while (v != vLeftmost);
        flatIter = v.iterator
      }
    }
  }

  // XXX: this belongs to test code, but we apparently cannot:
  // - mix the staged code (Rep etc.) from here with the one from subclass
  // - use Rep from outside the factory
  private[scalaviews] def _doubled[T: Manifest](v0: ArrayView[T])(
    implicit n: Numeric[T]
  ): ArrayView[T] = {
    val v = v0.asInstanceOf[ArrayView[T] with ViewS[T]]
    class Doubled(from: Int, until: Int) extends ViewS[T] {
      override val size = until - from
      import n.mkNumericOps // needed in applyS and foreachS
      override private[scalaviews] def applyS(i0: Rep[Int]) = {
        val i = from + i0
        v.applyS(i) + v.applyS(i)
      }
      override private[scalaviews] def foreachS(f: Rep[T => Unit]):
          Rep[Unit] = {
        v.sliced(from, until).foreachS((x: Rep[T]) => f(x + x))
      }
      override private[scalaviews] def computeChunks(
        cs: ArrayBuilder[Chunk],
        begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
      ) {
        begs += from
        ends += until
      }
      override lazy val reversed: Doubled = new Doubled(from, until)
          with ReversedApplyS[T] {
        override lazy val reversed = rev
        override def sliced(from: Int, until: Int) =
          Doubled.this.sliced(v.size - until, v.size - from).reversed
        override private[scalaviews] val rev = Doubled.this
        override private[scalaviews] def computeChunks(
          cs: ArrayBuilder[Chunk],
          begs: ArrayBuilder[Int], ends: ArrayBuilder[Int]
        ) {
          begs += Doubled.this.until - 1
          ends += Doubled.this.from - 1
        }
      }
      override def sliced(from: Int, until: Int) =
        new Doubled(from, until)
      override private[scalaviews] val t = manifest[T]
    }
    new Doubled(0, v.size)
  }

  def nested[T: Manifest](as: Array[T]*): ArrayView[T] = {
    require(as.forall(_.nonEmpty))
    nest(as.toIndexedSeq, 0, roundUpToPow2(as.size))
  }

  private def roundUpToPow2(n: Int) = {
    var p: Int = 1
    while (p < n) p <<= 1
    p
  }

  private def nest[T: Manifest](
    as: IndexedSeq[Array[T]], from: Int, until: Int
  ): ViewS[T] = {
    val size: Int = (if (as.size < until) as.size else until) - from
    (size: @scala.annotation.switch) match {
      case 1 => {
        val f = apply[T](as(from).size)
        f(as(from))
      }
      case 2 => {
        val f = apply[T](as(from).size, as(from + 1).size)
        f(as(from), as(from + 1))
      }
      case _ => {
        val mid = (from + until) / 2
        new Nested2[T](nest(as, from, mid), nest(as, mid, until))
      }
    }
  }
}

object ArrayView extends ViewFactoryProvider[ArrayViewFactory] {
  def apply[T: Manifest](a: Array[T]): ArrayView[T] = {
    val f = Factory(a.size)
    f(a)
  }

  def apply[T: Manifest](a1: Array[T], a2: Array[T]): ArrayView[T] = {
    val f = Factory(a1.size, a2.size)
    f(a1, a2)
  }

  def apply[T: Manifest](
    a1: Array[T], a2: Array[T], a3: Array[T],
    aRest: Array[T]*
  ): ArrayView[T] = {
    // TODO: optimize for 3 or 4 chunks (avoid ++ and toIndexedSeq overhead)
    Factory.nested(IndexedSeq(a1, a2, a3) ++ aRest.toIndexedSeq: _*)
  }

  def empty[T] = Factory.empty

  trait Implicits {
    import scala.language.implicitConversions

    implicit def array2ArrayView[T: Manifest](a: Array[T]):
        ArrayView[T] =
      Implicits.arrayCache.get().getOrElseUpdate(a, ArrayView(a))
        .asInstanceOf[ArrayView[T]]
  }

  object Implicits extends Implicits {
    import scala.collection.mutable.{Map => MutableMap,WeakHashMap}

    private val arrayCache = new ThreadLocal[MutableMap[Array[_], Any]] {
      override def initialValue = new WeakHashMap[Array[_], Any]
    }
  }

  private[scalaviews] trait FactoryImpl extends ArrayViewFactory
      with ViewExp
      with ScalaDriver

  override protected def mkFactory = new FactoryImpl {}
}
