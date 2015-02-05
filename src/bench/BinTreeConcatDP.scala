import scala.collection.mutable.HashMap

// scalac src/bench/BinTreeConcatDP.scala -d target/manual/
// scala -cp target/manual/ BinTreeConcatDP
object BinTreeConcatDP {
  object Conservative {
    val na = Int.MinValue / 4
    val dpCache = new HashMap[Tuple2[_, _], Int]
    def dp(h: Int, r: Int): Int = {
      if (r >= h) return na
      if (h < 2) return 0
      if (h == 2) return if (r == 1) 1 else 0
      dpCache.getOrElseUpdate((h, r),
        if (r == h - 1) {
          val dpRs = Array.tabulate(r) { h2 =>
            val s2 = (1 << h2)
            if (h2 > 0)
              (for (r2 <- 0 until h2) yield {
                // printf("%d %d: %d %d -> %d\n", h, r, h2, r2, dp(h2, r2) - s2)
                dp(h2, r2) - s2 }).max
            else na
          }

          // TODO: reduce to O(r) by precomputing suffix maxima
          (1 << (h - 1)) + (for {
            i <- 0 until r - 1 // -1 to be conservative
            dpL = dp(h - 1, i)
            h2 <- i + 1 until r // +1 to be conservative
          } yield {
            // printf("%d %d| %d %d -> %d\n", h, r, h2, i, dpL + dpRs(h2))
            dpL + dpRs(h2)
          }).max
        }
        else {
          val dpR = dp(h - 1, r)
          dpR + (for (i <- 0 until (h - 1)) yield {
            val dpL = dp(h - 1, i)
            // printf("%d %d> %d -> %d+%d = %d\n", h, r, i, dpL, dpR, dpL + dpR)
            dpL
          }
          ).max
        }
      )
    }
    def printTable() {
      val hMax = 12
      val t = Array.tabulate(hMax + 1, hMax)(dp(_, _))
      t foreach {
        a => a foreach(x => printf("%3d ", if (x == na) -1 else x))
        println
      }
      for ((a, h) <- t.zipWithIndex) {
        a foreach(x => printf("%.2f ", 1.0 / (1<<h) * (if (x == na) 0 else x)))
        println
      }
    }
  }

  object ByConstruction {
    import scala.collection.mutable.{HashMap,MutableList,LinkedHashSet}
    type Num = Short
    type BT = Tuple2[Num, Num]
    type ConsMap = HashMap[BT, MutableList[(BT, BT)]]
    val maxCapLg2 = 6
    lazy val m = precompute
    def size(t: BT) = t._2 - t._1
    val dpCache = new HashMap[BT, (Int, (BT, BT))]
    def dp(t: BT): (Int, (BT, BT)) = {
      // compute the number of missing leaves with indices in range [t._1, t._2)
      if (m(t).isEmpty) (0, null)
      else dpCache.getOrElseUpdate(t, {
        (for ((t1, t2) <- m(t))
          yield (dp(t1)._1 + dp(t2)._1 + size(t) - (size(t1) + size(t2)),
            (t1, t2))
        ).max
      })
    }
    def backtrack(t: BT) {
      def btStr(t: BT) = "[" + t._1 + "," + t._2 + ")"
      val (n, ts) = dp(t)
      if (ts ne null) {
        println(n + " <- " + btStr(t) + " = " + btStr(ts._1) + " ++ " +
          btStr(ts._2))
        backtrack(ts._1)
        backtrack(ts._2)
      }
    }
    def precompute = {
      val q = LinkedHashSet.empty[BT]
      val qNext = MutableList.empty[BT]
      def dequeue() {
        q ++= qNext
        qNext.clear()
      }
      val m = new ConsMap
      val mOverflow = MutableList.empty[(BT, BT)]
      def enqueue(t: BT) = {
        if (roundUpToPow2(t._2) <= (1 << maxCapLg2)) {
          qNext += t
          // above is same as adding to LinkedHashSet while iterating over it
          // q += t
          m.getOrElseUpdate(t, MutableList())
        } else mOverflow
      }
      enqueue((0, 1)); dequeue()
      def add(tBig: BT, tSmall: BT) {
        enqueue(append(tBig, tSmall)) += ((tBig, tSmall))
        enqueue(prepend(tSmall, tBig)) += ((tSmall, tBig))
        // printf("app(%s, %s) = %s\n", tBig, tSmall, append(tBig, tSmall))
        // printf("pre(%s, %s) = %s\n", tSmall, tBig, prepend(tSmall, tBig))
      }
      var ind = 0
      import scala.util.control.Breaks._
      while (ind < q.size) {
        val last = q.iterator.drop(ind).next()
        val it = q.iterator
        for (itr <- 0 until ind) {
          val cur = it.next()
          if (cur._2 >= last._2) add(cur, last)
          if (cur._2 <= last._2) add(last, cur)
        }
        add(last, last)
        dequeue()
        ind += 1
      }
      var hsh = 1
      def updateHsh(t: BT) {
        hsh = 31*hsh + t._1
        hsh = 31*hsh + t._2
      }
      for (k <- q) {
        updateHsh(k)
        for (ts <- m(k)) {
          updateHsh(ts._1); updateHsh(ts._2)
        }
      }
      println("hsh = " + hsh)
      println("tot ins = " + m.values.foldLeft(0)(_ + _.size))
      // println("q.size = " + q.size)
      m
    }
    def printTable() {
      testAppend
      testPrepend
      for (d <- 1 to maxCapLg2) {
        val mid = 1 << (d - 1)
        val (n, t) = (
          for {
            lo <- 0 until mid
            hi <- mid + 1 to (1 << d)
            val t = (lo.asInstanceOf[Num], hi.asInstanceOf[Num])
            if (m.contains(t))
          } yield (size(t) - dp(t)._1, t)
        ).min
        printf("%d: %2d (%.2f)\n", d, n, n * 1.0 / (1 << d))
        // backtrack(t)
      }
    }
    private def append(t1: BT, t2: BT): BT = {
      val (lo1, hi1) = t1
      val (lo2, hi2) = t2
      val cap1 = roundUpToPow2(hi1)
      val cap2 = roundUpToPow2(hi2)
      require(cap1 >= cap2)
      val tail1 = hiBit(hi1 - 1)
      if (hi1 - tail1 <= hiBit(lo2)) { // can concat t1 tail with t2 head
        (lo1, (tail1 + cap2).asInstanceOf[Num])
      } else {
        val head2 = (hi1 + cap2 - 1) / cap2 * cap2 // round up to mult. of cap2
        if (head2 + cap2 <= cap1) { // can concat after t1 tail, no height inc
          (lo1, (head2 + hi2).asInstanceOf[Num])
        } else {
          (lo1, (cap1 + hi2).asInstanceOf[Num])
        }
      }
    }
    private def prepend(t1: BT, t2: BT): BT = {
      def rev(t: BT) = {
        val (lo, hi) = t
        val cap = roundUpToPow2(hi)
        ((cap - hi).asInstanceOf[Num], (cap - lo).asInstanceOf[Num])
      }
      rev(append(rev(t2), rev(t1)))
    }
    private def hiBit(n: Int) = java.lang.Integer.highestOneBit(n)
    private def roundUpToPow2(n: Int) = {
      var v = n
      v -= 1
      v |= v >> 1; v |= v >> 2; v |= v >> 4; v |= v >> 8; v |= v >> 16
      v + 1
    }
    def assertBT(exp: BT, act: BT) {
      assert(exp == act, "\nExp: " + exp + "\nAct: " + act)
    }
    def testAppend {
      assertBT((0, 2), append((0, 1), (0, 1)))
      assertBT((0, 3), append((0, 2), (0, 1)))
      assertBT((0, 4), append((0, 2), (0, 2)))
      assertBT((1, 8), append((1, 5), (0, 2)))
      assertBT((3, 16), append((3, 8), (0, 8)))
      assertBT((2, 16), append((2, 11), (0, 4)))
      assertBT((2, 12), append((2, 11), (0, 1)))
      assertBT((2, 15), append((2, 11), (0, 3)))
      assertBT((2, 16), append((2, 11), (1, 4)))
      assertBT((2, 16), append((2, 10), (3, 7)))
      assertBT((2, 23), append((2, 11), (3, 7)))
    }
    def testPrepend {
      assertBT((0, 2), prepend((0, 1), (0, 1)))
      assertBT((1, 4), prepend((0, 1), (0, 2)))
      assertBT((0, 4), prepend((0, 2), (0, 2)))
      assertBT((0, 7), prepend((0, 2), (3, 7)))
      assertBT((0, 13), prepend((0, 8), (0, 5)))
      assertBT((0, 14), prepend((0, 4), (5, 14)))
      assertBT((4, 14), prepend((0, 1), (5, 14)))
      assertBT((1, 14), prepend((1, 4), (5, 14)))
      assertBT((0, 14), prepend((0, 3), (5, 14)))
      assertBT((0, 14), prepend((1, 5), (6, 14)))
      assertBT((9, 30), prepend((1, 5), (5, 14)))
    }
  }

  def main(args: Array[String]) {
    val Approach = ByConstruction
    Approach.printTable()
  }
}
