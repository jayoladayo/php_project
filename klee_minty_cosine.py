#!/usr/bin/env python3
"""
Cosine-rule (angle-based) simplex method applied to the Klee-Minty cube.

Problem (maximise form, n variables):
    max   sum_j 2^(n-j) x_j
    s.t.  2 * sum_{j<i} 2^(i-j) x_j + x_i <= 5^i      i = 1..n
          x >= 0

Dantzig's largest-coefficient rule visits all 2^n vertices (2^n - 1 pivots).
The cosine rule instead ranks each improving edge d_j by

    cos(theta_j) = (c . d_j) / (||c|| ||d_j||),   ||d_j||^2 = 1 + sum_i A[i,j]^2

and enters the variable with the largest cosine (equivalently the smallest angle
to the objective gradient).  Ties in the minimum-ratio test (the signature of a
degenerate vertex) are broken by the same cosine criterion applied to the tied
rows: leave the row whose tableau vector makes the smallest angle with c.

All arithmetic uses fractions.Fraction, so ties and degeneracy are detected
exactly, with no floating-point tolerance.  Cycling is detected by remembering
every basis visited.

Usage:
    python3 klee_minty_cosine.py            # n = 3..10 comparison table
    python3 klee_minty_cosine.py 6          # a single n, with pivot trace
    python3 klee_minty_cosine.py --degenerate   # degeneracy / cycling tests
    python3 klee_minty_cosine.py --scaled       # rescaled cubes (cosine rule is not scale-invariant)
"""
import sys
from fractions import Fraction as F
from math import isqrt


# --------------------------------------------------------------------------
# Exact helpers
# --------------------------------------------------------------------------
def dot(u, v):
    return sum(a * b for a, b in zip(u, v))


# --------------------------------------------------------------------------
# Tableau simplex with pluggable entering / leaving rules
# --------------------------------------------------------------------------
class Result:
    def __init__(self):
        self.status = "?"
        self.pivots = 0
        self.obj = None
        self.x = None
        self.ties = 0           # number of degenerate (tied-ratio) pivots
        self.zero_steps = 0     # pivots with step length 0 (degenerate moves)
        self.path = []          # entering variable names, in order


def simplex(A, b, c, entering="cosine", leaving="cosine", max_iter=200000, trace=False):
    """Maximise c.x subject to A x <= b, x >= 0 (b >= 0) on a slack tableau.

    entering: 'dantzig' | 'cosine' | 'bland'
    leaving : 'cosine'  | 'bland'  | 'index'   (only used to break ratio ties)
    """
    m, n = len(A), len(c)
    N = n + m
    names = [f"x{j+1}" for j in range(n)] + [f"s{i+1}" for i in range(m)]
    T = [[F(v) for v in A[i]] + [F(1) if k == i else F(0) for k in range(m)] + [F(b[i])]
         for i in range(m)]
    cfull = [F(v) for v in c] + [F(0)] * m
    basis = list(range(n, N))
    cnorm2 = dot(cfull, cfull)
    res = Result()
    seen = {tuple(sorted(basis))}

    for _ in range(max_iter):
        # reduced costs  rc_j = c_j - sum_i c_B(i) * T[i][j]
        rc = [cfull[j] - sum(cfull[basis[i]] * T[i][j] for i in range(m)) for j in range(N)]
        cand = [j for j in range(N) if j not in basis and rc[j] > 0]
        if not cand:
            res.status = "optimal"
            break

        # ---- entering variable ------------------------------------------
        if entering == "dantzig":
            enter = max(cand, key=lambda j: (rc[j], -j))
        elif entering == "bland":
            enter = min(cand)
        else:  # cosine:  maximise rc^2 / ||d||^2  (rc > 0, ||c|| constant)
            def score(j):
                d2 = 1 + sum(T[i][j] ** 2 for i in range(m))
                return (rc[j] ** 2 / d2, -j)
            enter = max(cand, key=score)

        # ---- minimum-ratio test -----------------------------------------
        rows = [i for i in range(m) if T[i][enter] > 0]
        if not rows:
            res.status = "unbounded"
            break
        ratios = {i: T[i][-1] / T[i][enter] for i in rows}
        rmin = min(ratios.values())
        tied = [i for i in rows if ratios[i] == rmin]
        if len(tied) > 1:
            res.ties += 1
        if rmin == 0:
            res.zero_steps += 1

        if len(tied) == 1:
            r = tied[0]
        elif leaving == "bland":
            r = min(tied, key=lambda i: basis[i])
        elif leaving == "index":
            r = min(tied)
        else:  # cosine tie-break: smallest angle between tableau row and c
            def rscore(i):
                row = T[i][:N]
                rn2 = dot(row, row)
                return (dot(row, cfull) ** 2 / (rn2 * cnorm2) if rn2 else F(0), -i)
            r = max(tied, key=rscore)

        if trace:
            print(f"  pivot {res.pivots+1:>3}: enter {names[enter]:>3}  leave {names[basis[r]]:>3}"
                  f"  step={rmin}{'  (TIE)' if len(tied) > 1 else ''}")

        # ---- pivot --------------------------------------------------------
        p = T[r][enter]
        T[r] = [v / p for v in T[r]]
        for i in range(m):
            if i != r and T[i][enter] != 0:
                f = T[i][enter]
                T[i] = [a - f * bb for a, bb in zip(T[i], T[r])]
        res.path.append(names[enter])
        basis[r] = enter
        res.pivots += 1

        key = tuple(sorted(basis))
        if key in seen:
            res.status = "CYCLE"
            break
        seen.add(key)
    else:
        res.status = "iteration cap"

    x = [F(0)] * N
    for i in range(m):
        x[basis[i]] = T[i][-1]
    res.x = x[:n]
    res.obj = sum(cj * xj for cj, xj in zip(c, res.x))
    return res


# --------------------------------------------------------------------------
# Klee-Minty cube
# --------------------------------------------------------------------------
def klee_minty(n):
    c = [2 ** (n - 1 - j) for j in range(n)]
    A = [[0] * n for _ in range(n)]
    for i in range(n):
        for j in range(i):
            A[i][j] = 2 ** (i - j + 1)          # 2 * 2^(i-j)
        A[i][i] = 1
    b = [5 ** (i + 1) for i in range(n)]
    return A, b, c


def run_klee_minty(n, trace=False):
    A, b, c = klee_minty(n)
    out = {}
    for label, ent, lea in (("Dantzig", "dantzig", "index"),
                            ("Bland", "bland", "bland"),
                            ("Cosine", "cosine", "cosine")):
        if trace:
            print(f"\n[{label}]")
        out[label] = simplex(A, b, c, ent, lea, trace=trace)
    return out


def klee_minty_table(ns):
    print("Klee-Minty cube: number of pivots to reach the optimum x = (0,...,0,5^n), Z = 5^n")
    print(f"{'n':>3} {'2^n - 1':>9} | {'Dantzig':>9} {'Bland':>9} {'Cosine':>9} | "
          f"{'cos ties':>8} {'cos 0-steps':>11} | {'status / Z check'}")
    print("-" * 94)
    for n in ns:
        r = run_klee_minty(n)
        ok = all(v.status == "optimal" and v.obj == 5 ** n for v in r.values())
        print(f"{n:>3} {2**n - 1:>9} | {r['Dantzig'].pivots:>9} {r['Bland'].pivots:>9} "
              f"{r['Cosine'].pivots:>9} | {r['Cosine'].ties:>8} {r['Cosine'].zero_steps:>11} | "
              f"{'all optimal, Z = 5^n' if ok else 'CHECK'}")


# --------------------------------------------------------------------------
# Degeneracy / cycling tests
# --------------------------------------------------------------------------
def degenerate_tests():
    tests = {}
    # Beale (1955): Dantzig + smallest-index ties cycles forever.  Optimum Z = 5/4.
    tests["Beale (1955), opt 5/4"] = (
        [[F(1, 4), -8, -1, 9], [F(1, 2), -12, F(-1, 2), 3], [0, 0, 1, 0]],
        [0, 0, 1], [F(3, 4), -20, F(1, 2), -6])
    # Simple degenerate vertex: three constraints meet at (4,4) in 2-D.  Opt 20.
    tests["Degenerate vertex (4,4), opt 20"] = (
        [[1, 0], [0, 1], [1, 1]], [4, 4, 8], [3, 2])
    # Klee-Minty with b perturbed to zero in every row but the last
    # (fully degenerate start: every ratio is 0).
    A, b, c = klee_minty(4)
    tests["Klee-Minty n=4, degenerate b"] = (A, [0, 0, 0, b[3]], c)

    configs = (("Dantzig+index", "dantzig", "index"),
               ("Dantzig+Bland", "dantzig", "bland"),
               ("Bland+Bland", "bland", "bland"),
               ("Cosine+cosine", "cosine", "cosine"))
    print("Degeneracy tests: status (pivots, tied ratio tests, zero-length steps) -> objective")
    for name, (A, b, c) in tests.items():
        print(f"\n{name}")
        for label, ent, lea in configs:
            r = simplex(A, b, c, ent, lea, max_iter=500)
            print(f"  {label:<14} {r.status:<10} ({r.pivots:>3} pivots, {r.ties} ties, "
                  f"{r.zero_steps} zero-steps)  Z = {r.obj}")


def scaled_tests(ns=(4, 6, 8, 10)):
    """The cosine rule is NOT scale-invariant, so rescale variables x_j -> s_j x_j
    to check the one-pivot result is not an artefact of the standard scaling."""
    print("Rescaled Klee-Minty cubes: cosine pivots vs Dantzig pivots")
    for n in ns:
        A, b, c = klee_minty(n)
        for name, base in (("s_j = 2^j", F(2)), ("s_j = 1/2^j", F(1, 2)),
                           ("s_j = 1/5^j", F(1, 5))):
            s = [base ** j for j in range(n)]
            A2 = [[A[i][j] * s[j] for j in range(n)] for i in range(n)]
            c2 = [c[j] * s[j] for j in range(n)]
            rc = simplex(A2, b, c2, "cosine", "cosine")
            rd = simplex(A2, b, c2, "dantzig", "index")
            print(f"  n={n:>2} {name:<12} cosine {rc.pivots:>3} ({rc.status}, Z={rc.obj == 5**n and '5^n'})"
                  f"   dantzig {rd.pivots:>4}   (2^n-1 = {2**n - 1})")


# --------------------------------------------------------------------------
def main(argv):
    if "--scaled" in argv:
        scaled_tests()
    elif "--degenerate" in argv:
        degenerate_tests()
    elif len(argv) > 1 and argv[1].isdigit():
        n = int(argv[1])
        r = run_klee_minty(n, trace=True)
        print()
        for k, v in r.items():
            print(f"{k:<8} {v.status}, {v.pivots} pivots, Z = {v.obj}, x = {[str(t) for t in v.x]}")
    else:
        klee_minty_table(range(3, 11))
        print()
        degenerate_tests()
        print()
        scaled_tests()


if __name__ == "__main__":
    main(sys.argv)
