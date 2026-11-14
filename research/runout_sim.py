"""
Run-out time (ROT) rule on one machine, N products: naive rule vs. slack-checked fix.

Model (deterministic, continuous time, backorders allowed so stockouts are measurable):
  item i: demand rate d, production rate p (> d), setup time s, inventory I
  rho_i = d/p. Long-run capacity needs sum(rho) < 1 and a cycle of at least
  T_min = sum(s) / (1 - sum(rho)).

Policies
  pairwise : the two-product rule applied as-is to N products. Make the item with the
             smallest run-out time now, and keep making it until the NEXT item to run
             out has to start its setup (or the item reaches its target stock).
             It only ever looks at one other item.
  greedy   : textbook "smallest run-out time first" with a fixed lot per item.
  slack    : the fix. Sort items by run-out time and check every item, not just the next:
                sum_{j<=k} s_j + T * sum_{j<k} rho_j <= ROT_k      for every position k
             Take the largest cycle T <= T_target that satisfies all of them and run the
             first item for T * rho_1. Idle only while every constraint has spare time.
"""
import random, sys

def stockout_area(I, rate, dt):
    """Integral of max(0, -inventory) over a segment where inventory moves linearly."""
    a, b = I, I + rate * dt
    if a >= 0 and b >= 0: return 0.0, 0.0
    if a < 0 and b < 0:  return (-(a + b) / 2) * dt, dt
    tz = -a / rate                      # time of zero crossing
    if a < 0: return (-a / 2) * tz, tz
    return (-b / 2) * (dt - tz), dt - tz

class Sim:
    def __init__(self, d, p, s, I0, T_target):
        self.d, self.p, self.s, self.I = d, p, s, list(I0)
        self.n = len(d); self.T = T_target
        self.rho = [d[i] / p[i] for i in range(self.n)]
        self.t = 0.0; self.so_time = 0.0; self.so_area = 0.0
        self.inv_area = 0.0; self.setups = 0; self.alarms = 0
        self.Tmin = sum(s) / (1 - sum(self.rho))

    def advance(self, dt, making=None):
        if dt <= 0: return
        for i in range(self.n):
            rate = (self.p[i] if i == making else 0.0) - self.d[i]
            area, tm = stockout_area(self.I[i], rate, dt)
            self.so_area += area / self.d[i]      # in "time-units of demand" so items are comparable
            self.so_time += tm
            new = self.I[i] + rate * dt
            self.inv_area += (max(self.I[i], 0) + max(new, 0)) / 2 * dt / self.d[i]
            self.I[i] = new
        self.t += dt

    def rot(self, i): return self.I[i] / self.d[i]

    def produce(self, i, level):
        self.setups += 1
        self.advance(self.s[i])
        run = (level - self.I[i]) / (self.p[i] - self.d[i])
        self.advance(max(run, 0.0), making=i)

    def produce_for(self, i, run):
        self.setups += 1
        self.advance(self.s[i]); self.advance(run, making=i)

    def S(self, i, T): return T * self.d[i] * (1 - self.rho[i])   # peak stock in a cycle of length T

    # ---- policies: each call makes one decision and advances time ----
    def step_pairwise(self):
        order = sorted(range(self.n), key=self.rot)
        i, j = order[0], order[1]
        window = self.rot(j) - self.s[j] - self.s[i]          # production time before j must set up
        to_cap = (self.S(i, self.T) - (self.I[i] - self.d[i]*self.s[i])) / (self.p[i] - self.d[i])
        if to_cap <= 0:                                        # i is full: wait, but keep room for i's run before j
            full_run = self.T * self.rho[i]
            wait = max(min(self.rot(i) - self.s[i], window - full_run), 0.0)
            self.advance(wait)
            self.produce_for(i, max(min(window - wait, full_run), 0.0)); return
        self.produce_for(i, max(min(window, to_cap), 0.0))

    def step_greedy(self):
        i = min(range(self.n), key=self.rot)
        if self.I[i] >= self.S(i, self.T):                          # everything is full: wait a little
            self.advance(max(self.rot(i) - self.s[i], 0.0) or 0.01); return
        self.produce_for(i, self.T * self.rho[i])

    def slack_cycle(self):
        """Largest cycle T' (capped at target) for which no item in the queue stocks out,
        plus the spare time before a decision is forced."""
        order = sorted(range(self.n), key=self.rot)
        cum_s = cum_rho = 0.0; Tmax = float('inf'); spare = float('inf')
        for k, i in enumerate(order):
            cum_s += self.s[i]
            room = self.rot(i) - cum_s                 # time left for earlier items' production runs
            spare = min(spare, room - self.T * cum_rho)
            if k > 0: Tmax = min(Tmax, room / cum_rho)
            cum_rho += self.rho[i]
        return order[0], min(self.T, Tmax), spare

    def step_slack(self):
        i, T, spare = self.slack_cycle()
        if spare > 1e-12:
            self.advance(spare); i, T, spare = self.slack_cycle()
        if T < self.Tmin: self.alarms += 1                    # rule says: shortage ahead, cannot be scheduled away
        self.produce_for(i, max(T, self.Tmin) * self.rho[i])

def feasibility(d, p, s, I0):
    """The up-front test of the fix: largest sustainable cycle the starting stock allows."""
    sim = Sim(d, p, s, I0, float('inf'))
    order = sorted(range(sim.n), key=sim.rot)
    cum_s = cum_rho = 0.0; Tmax = float('inf'); first_ok = True
    for k, i in enumerate(order):
        cum_s += s[i]; room = sim.rot(i) - cum_s
        if room < 0: first_ok = False
        if k > 0: Tmax = min(Tmax, room / cum_rho)
        cum_rho += sim.rho[i]
    return first_ok, Tmax

def run(policy, d, p, s, I0, T, horizon):
    sim = Sim(d, p, s, I0, T); step = getattr(sim, 'step_' + policy)
    while sim.t < horizon: step()
    return dict(so_time=sim.so_time / (sim.t * sim.n), so_area=sim.so_area,
                setups=sim.setups / sim.t, inv=sim.inv_area / (sim.t * sim.n), alarms=sim.alarms)

def instance(n, util, rng, spread):
    w = [rng.uniform(0.5, 1.5) for _ in range(n)]
    rho = [util * x / sum(w) for x in w]
    d = [rng.uniform(1, 10) for _ in range(n)]
    p = [d[i] / rho[i] for i in range(n)]
    s = [rng.uniform(0.05, 0.3) for _ in range(n)]
    Tmin = sum(s) / (1 - util); T = 1.5 * Tmin
    I0 = [d[i] * rng.uniform(spread[0], spread[1]) * T for i in range(n)]
    return d, p, s, I0, T, Tmin

def experiment(only_ok, spread=(0.3, 1.2), reps=400, seed=7):
    rng = random.Random(seed); pols = ('pairwise', 'greedy', 'slack')
    print(f"{'N':>2} {'util':>5} {'cases':>5} | " + " | ".join(f"{pol:^31}" for pol in pols))
    print(" " * 15 + " | " + " | ".join(f"{'%runs short':>11} {'%time':>6} {'setups':>6} {'stock':>5}" for _ in pols))
    for n in (2, 3, 4, 6, 10):
        for util in (0.6, 0.85):
            res = {k: [] for k in pols}
            for _ in range(reps):
                d, p, s, I0, T, Tmin = instance(n, util, rng, spread)
                first_ok, Tmax = feasibility(d, p, s, I0)
                if (first_ok and Tmax >= Tmin) != only_ok: continue
                ideal_setups = n / T; ideal_stock = sum(T * (1 - d[i]/p[i]) / 2 for i in range(n)) / n
                for pol in pols:
                    r = run(pol, d, p, s, I0, T, 60 * T)
                    r['setups'] /= ideal_setups; r['inv'] /= ideal_stock; res[pol].append(r)
            m = len(res['slack'])
            if not m: continue
            cells = [f"{100*sum(x['so_area']>1e-6 for x in res[q])/m:>10.1f}% {100*sum(x['so_time'] for x in res[q])/m:>5.2f}% "
                     f"{sum(x['setups'] for x in res[q])/m:>6.2f} {sum(x['inv'] for x in res[q])/m:>5.2f}" for q in pols]
            print(f"{n:>2} {util:>5} {m:>5} | " + " | ".join(cells))

if __name__ == '__main__':
    print("A. Starting stock passes the up-front slack test (setups and stock are relative to an ideal rotation cycle = 1.00)")
    experiment(True)
    print("\nB. Tight starting stock, test FAILS at time 0 (a shortage cannot be fully scheduled away)")
    experiment(False, spread=(0.05, 0.6))
