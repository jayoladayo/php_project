"""
Stress tests and improvements for the slack-checked run-out rule (see runout_sim.py).

  v1 : slack rule from runout_sim.py (never runs a cycle shorter than T_min).
  v2 : same check, but when stock is tight it is allowed to run SHORT cycles
       (below T_min) for the urgent items and lengthen them again as slack is rebuilt.

Starting positions fall into three zones:
  pass       : every item fits with a sustainable cycle (largest cycle >= T_min)
  grey       : every setup fits before its item runs out, but only with a cycle below T_min
  impossible : for some item, the setups alone do not fit before it runs out.
               No schedule can avoid a stockout there (proof: every item that runs out
               no later than item k needs its setup finished before item k runs out).
"""
import random, sys, itertools
from multiprocessing import Pool
from runout_sim import Sim, feasibility, stockout_area

class Sim2(Sim):
    safety = 0.0            # safety time subtracted from every run-out time (for noisy demand)
    noise = 0.0             # actual demand rate = planned * (1 +/- noise), redrawn every step
    rng = None

    def rot(self, i): return self.I[i] / self.d_plan[i] - self.safety

    def __init__(self, d, p, s, I0, T):
        super().__init__(d, p, s, I0, T); self.d_plan = list(d)

    def jitter(self):
        if self.noise: self.d = [x * (1 + self.rng.uniform(-self.noise, self.noise)) for x in self.d_plan]

    def step_v1(self):
        self.jitter(); self.step_slack()

    def step_v2(self):
        self.jitter()
        i, T, spare = self.slack_cycle()
        if spare > 1e-12:
            self.advance(spare); i, T, spare = self.slack_cycle()
        if T <= 0:                       # setups alone no longer fit: shortage is unavoidable
            self.alarms += 1; T = self.Tmin
        self.produce_for(i, T * self.rho[i])

    def clone(self):
        c = Sim2(self.d_plan, self.p, self.s, self.I, self.T)
        c.safety = self.safety; return c

    def step_v3(self):
        """Look-ahead: when stock is tight, try several first moves, play each one forward
        with v2, and keep the move with the least shortage (ties: the longest run)."""
        self.jitter()
        i, T, spare = self.slack_cycle()
        if spare > 1e-12:
            self.advance(spare); i, T, spare = self.slack_cycle()
        if T >= self.Tmin:
            self.produce_for(i, T * self.rho[i]); return
        best = None
        for j in sorted(range(self.n), key=self.rot)[:3]:
            w = min(self.rot(k) - self.s[k] for k in range(self.n) if k != j) - self.s[j]
            w = min(max(w, 0.0), self.T * self.rho[j])
            for f in (1.0, 0.75, 0.5, 0.25):
                c = self.clone(); c.produce_for(j, f * w); guard = 0
                while c.t < self.look * self.T and guard < 400 and c.so_area < (best[0] if best else 1e99): c.step_v2(); guard += 1
                score = (c.so_area, -f * w)
                if best is None or score < best[:2]: best = (*score, j, f * w)
                if c.so_area <= 1e-9 and f == 1.0: break
            if best[0] <= 1e-9: break
        if best[0] > 1e-9: self.alarms += 1
        self.produce_for(best[2], best[3])
    look = 8

def run(pol, d, p, s, I0, T, horizon, safety=0.0, noise=0.0, seed=0):
    sim = Sim2(d, p, s, I0, T); sim.safety = safety; sim.noise = noise; sim.rng = random.Random(seed)
    step = getattr(sim, 'step_' + pol); guard = 0
    while sim.t < horizon and guard < 200000: step(); guard += 1
    return sim.so_area > 1e-6, sim.so_time / (sim.t * sim.n), sim.setups / sim.t, sim.inv_area / (sim.t * sim.n)

def instance(rng, n, util, tfac, spread, skew):
    """skew > 0 makes loads, demands and setups very uneven across items."""
    w = [rng.uniform(0.5, 1.5) ** (1 + 4 * skew) for _ in range(n)]
    rho = [util * x / sum(w) for x in w]
    d = [rng.uniform(1, 10) for _ in range(n)]
    p = [d[i] / rho[i] for i in range(n)]
    s = [rng.uniform(0.05, 0.3) * (rng.choice([1, 1, 5]) if skew else 1) for _ in range(n)]
    Tmin = sum(s) / (1 - util); T = tfac * Tmin
    I0 = [d[i] * rng.uniform(*spread) * T for i in range(n)]
    return d, p, s, I0, T, Tmin

def zone(d, p, s, I0, Tmin):
    ok, Tmax = feasibility(d, p, s, I0)
    return 'impossible' if not ok else ('pass' if Tmax >= Tmin else 'grey')

# ---------- exhaustive search: can ANY schedule save a grey-zone start? (small N only) ----------
def savable(d, p, s, I, T, Tmin, depth=7, fracs=(1.0, 0.75, 0.5, 0.25)):
    n = len(d)
    def rec(I, depth):
        if zone(d, p, s, I, Tmin) == 'pass': return True
        if depth == 0: return False
        rot = [I[i] / d[i] for i in range(n)]
        for i in sorted(range(n), key=lambda j: rot[j]):
            if rot[i] < s[i]: continue
            w = min(rot[j] - s[j] for j in range(n) if j != i) - s[i]   # longest run before another item must set up
            if w <= 0: continue
            w = min(w, T * d[i] / p[i])
            for f in fracs:
                run_t = f * w; e = s[i] + run_t
                J = [I[j] - d[j] * e for j in range(n)]; J[i] += p[i] * run_t
                if rec(J, depth - 1): return True
        return False
    return rec(list(I0 := I), depth)

# ---------- experiments ----------
def job_pass(args):
    seed, n, util, tfac, spread, skew = args
    rng = random.Random(seed); out = []
    for _ in range(40):
        d, p, s, I0, T, Tmin = instance(rng, n, util, tfac, spread, skew)
        z = zone(d, p, s, I0, Tmin)
        r1 = run('v1', d, p, s, I0, T, 40 * T); r2 = run('v2', d, p, s, I0, T, 40 * T)
        out.append((z, r1, r2))
    return (n, util, skew), out

def job_grey(seed):
    rng = random.Random(seed); out = []
    while len(out) < 12:
        d, p, s, I0, T, Tmin = instance(rng, 3, rng.choice([0.6, 0.85]), 1.5, (0.05, 0.7), rng.choice([0, 1]))
        if zone(d, p, s, I0, Tmin) != 'grey': continue
        out.append((run('v1', d, p, s, I0, T, 40 * T)[0], run('v2', d, p, s, I0, T, 40 * T)[0], savable(d, p, s, I0, T, Tmin), run('v3', d, p, s, I0, T, 40 * T)[0]))
    return out

def job_noise(args):
    seed, noise, safety_mult = args
    rng = random.Random(seed); bad = tot = 0; inv = 0.0
    for _ in range(25):
        n = rng.choice([3, 5, 10]); util = rng.choice([0.6, 0.85])
        d, p, s, I0, T, Tmin = instance(rng, n, util, 1.5, (0.5, 1.2), 0)
        safety = safety_mult * noise * T
        I0s = [I0[i] + d[i] * safety for i in range(n)]                # the safety stock is held on top
        if zone(d, p, s, I0, Tmin) != 'pass': continue
        r = run('v2', d, p, s, I0s, T, 40 * T, safety=safety, noise=noise, seed=seed)
        tot += 1; bad += r[0]; inv += r[3] / (sum(T * (1 - d[i] / p[i]) / 2 for i in range(n)) / n)
    return (noise, safety_mult), bad, tot, inv

if __name__ == '__main__':
    which = sys.argv[1] if len(sys.argv) > 1 else 'all'
    with Pool() as pool:
        if which in ('all', 'pass'):
            print("1. Stress test by zone: number of runs with any stockout  (v1 / v2)")
            grid = [(seed, n, util, tfac, spread, skew)
                    for seed, (n, util, tfac, spread, skew) in enumerate(itertools.product(
                        (2, 3, 5, 10, 20), (0.6, 0.85, 0.95, 0.98), (1.05, 1.5), ((0.05, 0.7), (0.3, 1.2), (0.8, 1.0)), (0, 1)))]
            agg = {}
            for key, out in pool.imap_unordered(job_pass, grid):
                for z, r1, r2 in out:
                    for k in (('N', key[0]), ('load', key[1]), ('uneven', key[2]), ('ALL', '')):
                        a = agg.setdefault((k, z), [0, 0, 0]); a[0] += 1; a[1] += r1[0]; a[2] += r2[0]
            for k in sorted({k for k, _ in agg}, key=str):
                row = f"{k[0]:>7} {str(k[1]):>5} |"
                for z in ('pass', 'grey', 'impossible'):
                    a = agg.get((k, z), [0, 0, 0])
                    row += f" {z}: n={a[0]:5d} short {a[1]:4d} / {a[2]:4d} |"
                print(row)
        if which in ('all', 'grey'):
            print("\n2. Grey zone, 3 products: could ANY schedule have avoided the stockout? (exhaustive search)")
            rows = [r for out in pool.imap_unordered(job_grey, range(100, 120)) for r in out]
            m = len(rows); sv = sum(r[2] for r in rows)
            print(f"   cases={m}  savable by search={sv}  v1 saved={sum(not r[0] for r in rows)}  v2 saved={sum(not r[1] for r in rows)}"
                  f"  v3 saved={sum(not r[3] for r in rows)}  savable by search or any rule={sum(r[2] or not r[1] or not r[3] for r in rows)}  savable but v3 failed={sum((r[2] or not r[1]) and r[3] for r in rows)}")
        if which in ('all', 'noise'):
            print("\n3. Uncertain demand (rate varies +/- x%), v2 with a safety time = mult * x * cycle")
            agg = {}
            for key, bad, tot, inv in pool.imap_unordered(job_noise, [(sd, nz, sm) for sd in range(12) for nz in (0.1, 0.2, 0.3) for sm in (0, 0.5, 1.0, 1.5)]):
                a = agg.setdefault(key, [0, 0, 0.0]); a[0] += tot; a[1] += bad; a[2] += inv
            for key in sorted(agg):
                a = agg[key]; print(f"   demand +/-{int(key[0]*100):2d}%  safety mult {key[1]:3.1f}: runs short {100*a[1]/a[0]:5.1f}%   stock vs ideal {a[2]/a[0]:4.2f}   (n={a[0]})")
