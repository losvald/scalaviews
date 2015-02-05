#include <cassert>

#include <algorithm>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <list>
#include <unordered_map>
#include <unordered_set>
#include <utility>

uint32_t HiBit(uint32_t v) {
  if (v == 0) return 0;

  static const int multiplyDeBruijnBitPosition[32] = {
    0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30,
    8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31
  };

  v |= v >> 1, v |= v >> 2, v |= v >> 4, v |= v >> 8, v |= v >> 16;

  return 1 << multiplyDeBruijnBitPosition[(uint32_t)(v * 0x07C4ACDDU) >> 27];
}

uint32_t RoundUpToPow2(uint32_t v) {
  --v;
  return 1 + (v |= v >> 1, v |= v >> 2, v |= v >> 4, v |= v >> 8, v |= v >> 16);
}

typedef uint16_t Num;
typedef std::pair<Num, Num> BT;

BT Append(const BT& t1, const BT& t2) {
  Num lo1, hi1, lo2, hi2;
  std::tie(lo1, hi1) = t1;
  std::tie(lo2, hi2) = t2;
  auto cap1 = RoundUpToPow2(hi1);
  auto cap2 = RoundUpToPow2(hi2);
  assert(cap1 >= cap2);

  auto tail1 = HiBit(hi1 - 1);
  if (hi1 - tail1 <= HiBit(lo2)) // can concat t1 tail with t2 head
    return BT(lo1, tail1 + cap2);

  auto head2 = (hi1 + cap2 - 1) / cap2 * cap2; // round up to mult. of cap2
  return BT(lo1, head2 + cap2 <= cap1 // can concat after t1 tail, no ht inc
            ? head2 + hi2
            : cap1 + hi2);
}

void Rev(BT* t) {
  auto cap = RoundUpToPow2(t->second);
  auto fst = t->first;
  t->first = cap - t->second;
  t->second = cap - fst;
}

BT Prepend(BT t1, BT t2) {
  Rev(&t1), Rev(&t2);
  auto r = Append(t2, t1);
  Rev(&r);
  return r;
}

void assertBT(const BT& exp, const BT& act) {
  // std::cerr << __FILE__ ":" << __LINE__ << "\n\t"
  //           << "exp=" << exp.first << ' ' << exp.second << "\n\t"
  //           << "act=" << act.first << ' ' << act.second << std::endl;
  assert(exp == act);
}
void TestAppend() {
  assertBT(BT(0, 2), Append(BT(0, 1), BT(0, 1)));
  assertBT(BT(0, 3), Append(BT(0, 2), BT(0, 1)));
  assertBT(BT(0, 4), Append(BT(0, 2), BT(0, 2)));
  assertBT(BT(1, 8), Append(BT(1, 5), BT(0, 2)));
  assertBT(BT(3, 16), Append(BT(3, 8), BT(0, 8)));
  assertBT(BT(2, 16), Append(BT(2, 11), BT(0, 4)));
  assertBT(BT(2, 12), Append(BT(2, 11), BT(0, 1)));
  assertBT(BT(2, 15), Append(BT(2, 11), BT(0, 3)));
  assertBT(BT(2, 16), Append(BT(2, 11), BT(1, 4)));
  assertBT(BT(2, 16), Append(BT(2, 10), BT(3, 7)));
  assertBT(BT(2, 23), Append(BT(2, 11), BT(3, 7)));
}
void TestPrepend() {
  assertBT(BT(0, 2), Prepend(BT(0, 1), BT(0, 1)));
  assertBT(BT(1, 4), Prepend(BT(0, 1), BT(0, 2)));
  assertBT(BT(0, 4), Prepend(BT(0, 2), BT(0, 2)));
  assertBT(BT(0, 7), Prepend(BT(0, 2), BT(3, 7)));
  assertBT(BT(0, 13), Prepend(BT(0, 8), BT(0, 5)));
  assertBT(BT(0, 14), Prepend(BT(0, 4), BT(5, 14)));
  assertBT(BT(4, 14), Prepend(BT(0, 1), BT(5, 14)));
  assertBT(BT(1, 14), Prepend(BT(1, 4), BT(5, 14)));
  assertBT(BT(0, 14), Prepend(BT(0, 3), BT(5, 14)));
  assertBT(BT(0, 14), Prepend(BT(1, 5), BT(6, 14)));
  assertBT(BT(9, 30), Prepend(BT(1, 5), BT(5, 14)));
}

namespace std {
template<>
struct hash<BT> {
  size_t operator()(const BT& bt) const {
    return (bt.first << 16) | bt.second;
  }
};
}  // namespace std

typedef std::pair<BT, BT> BTPair;
typedef std::vector<BTPair> ConsList;
typedef std::unordered_map<BT, ConsList> ConsMap;

void Precompute(unsigned max_cap_lg2, ConsMap* m) {
  std::unordered_set<BT> s;
  std::list<BT> q;
  ConsList mOverflow;

  auto enqueue = [&](const BT& t) -> ConsList& {
    if (RoundUpToPow2(t.second) <= (1 << max_cap_lg2)) {
      if (s.insert(t).second == true)
        q.push_back(t);
      return (*m)[t];
    }
    return mOverflow;
  };
  auto add = [&](const BT& t_big, const BT& t_small) {
    enqueue(Append(t_big, t_small)).emplace_back(t_big, t_small);
    enqueue(Prepend(t_small, t_big)).emplace_back(t_small, t_big);
  };
  enqueue(BT(0, 1));
  for (size_t ind = 0; ind < q.size(); ++ind) {
    auto last = q.cbegin();
    std::advance(last, ind);
    for (auto cur = q.cbegin(); cur != last; ++cur) {
      if (cur->second >= last->second) add(*cur, *last);
      if (cur->second <= last->second) add(*last, *cur);
    }
    add(*last, *last);
  }

  int32_t hsh = 1;
  auto update_hsh = [&](const BT& t) {
    hsh = 31*hsh + t.first;
    hsh = 31*hsh + t.second;
  };
  size_t size_sum = 0;
  for (const auto& k : q) {
    update_hsh(k);
    size_sum += m->at(k).size();
    for (const auto& ts : m->at(k))
      update_hsh(ts.first), update_hsh(ts.second);
  }
  std::cout << "hsh = " << hsh << std::endl;
  std::cout << "tot ins = " << size_sum << std::endl;
}

Num size(const BT& t) { return t.second - t.first; }

struct DP {
  DP(const ConsMap& m) : m_(m) {}
  Num operator()(const BT& t) const {
    // compute the number of missing leaves with indices in range:
    // [t.first, t.second)
    if (!m_.count(t)) return 0;
    if (memo_.count(t)) return memo_.at(t);
    Num max = 0;
    for (const auto& ts : m_.at(t)) {
      const auto& t1 = ts.first;
      const auto& t2 = ts.second;
      Num cur = (*this)(t1) + (*this)(t2) + size(t) - (size(t1) + size(t2));
      max = std::max(max, cur);
    }
    return memo_[t] = max;
  }
 private:
  mutable std::unordered_map<BT, Num> memo_;
  const ConsMap& m_;
};

/*
  g++-4.8 -std=c++11 -O2 -o target/manual/BinTreeConcatDP \
  src/bench/BinTreeConcatDP.cpp && ./target/manual/BinTreeConcatDP 8
*/
int main(int argc, char* argv[]) {
  const unsigned max_cap_lg2 = (argc > 1 ? atoi(argv[1]) : 6);
  TestAppend();
  TestPrepend();

  ConsMap m;
  Precompute(max_cap_lg2, &m);

  DP dp(m);
  for (int d = 1; d <= max_cap_lg2; ++d) {
    const int mid = 1 << (d - 1);
    int min = mid << 1;
    for (Num lo = 0; lo < mid; ++lo)
      for (Num hi = mid + 1; hi <= (1 << d); ++hi) {
        BT t(lo, hi);
        if (!m.count(t)) continue;
        min = std::min(min, size(t) - dp(t));
      }
    std::cout << d << ": " << min << " ("
              << std::fixed << std::setprecision(2)
              << min * 1.0 / (1 << d) << ')' << std::endl;
  }

  return 0;
}
// while true; do ps aux | grep 'BinTreeConcatDP\s'; sleep 0.5; done
