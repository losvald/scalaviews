"""Empirically proves the following upper bound on catenated binary tree height:
  h_max <= -0.626716 + 1.813358 log2(s),
where s is the number of leaf nodes."""

from __future__ import print_function

import math
import sys

def s(h):
    """Computes the minimum size of the concatenated tree of height h.
    By a conjecture, this is a solution to the following recurrence:
      s(h) = s(h-1) + s(h-3),   for h >= 3
      s(h) = h + 1,             otherwise
    The conjecture was verified for h <= 8 by dynamic programming in C++,
    and for h <= 7 in Scala (with backtracking as well)."""
    cur, prev, prev2 = 1, 1, 1
    while h > 0:
        h -= 1
        cur, prev, prev2 = cur + prev2, cur, prev
    return cur

if __name__ == '__main__':
    # a = 0.415037499278844  # for 0 <= n < 1500
    # a = 0.551409941010129  # for 13 <= n < 1500
    # a = 0.551463089745567 # 51 <= n < 400
    a = 0.5514630897455161107 # for 50 <= n < 1500 (n >= 0 if ... / (... + 1))
    b = 0.75731544872758
    last = 0
    sCur, sPrev, sPrev2 = 1, 1, 0
    for h in range(1500):
        # Avoid quadratic time complexity by not calling s(h)
        sCur, sPrev, sPrev2 = sCur + sPrev2, sCur, sPrev
        assert h > 10 or s(h) == sCur # do a sanity check for small
        cur = sCur / (2**(a * h) + 1) # adding ~0.9 makes it work for small h
        # s(h) / (2^(ah) + 1) >= 0.5
        # lg s(h) - lg(2^(ah) + 1) >= -1
        # (1 + lg s) >= lg(2^(ah) + 1) >= ah    ->      h_max = (1 + lg s) / a
        h_max = ((b + math.log(sCur, 2)) / a) - 2
        print("%4d %7.2f %.12f" % (h, h_max, cur), sCur)
        assert cur - last >= 0  # helps in maximizing the value of a
        assert h == 0 or h <= h_max
        # assert h == 0 or h <= -0.626716 + 1.813358 * math.log(sCur, 2)
        last = cur

    print("h_max <= %f + %f lg s" % (b / a - 2, 1 / a))
