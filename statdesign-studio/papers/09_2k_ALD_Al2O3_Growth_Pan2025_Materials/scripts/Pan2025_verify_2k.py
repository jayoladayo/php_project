"""Pan2025_verify_2k.py - Pan & Lei (2025), Materials 18:1918, doi:10.3390/ma18091918.
2^4 full factorial with 2 replicates (32 runs). The paper fits main effects + two-factor interactions
and pools the 3- and 4-factor interactions with pure error (21 df); Table 7.  python3 Pan2025_verify_2k.py"""
import itertools, pandas as pd
from doe_anova import anova, show, compare
d = pd.read_csv("../data/Pan2025_ALD_Al2O3_2to4_factorial_2_replicates_GPC.csv")
Y = "Growth per cycle GPC (Angstrom/cycle)"
f = ["Deposition temperature (C)", "Argon flow rate (sccm)", "Precursor pulse time (ms)", "Purge time (s)"]
two = f + [f"{a}:{b}" for a, b in itertools.combinations(f, 2)]
t = anova(d, Y, two); show("Paper's model: main effects + 2-factor interactions", t, "{:.6f}")
paper = [0.276187, 0.024741, 0.018841, 0.004772, 0.015741, 0.019931, 0.050763, 0.003207, 0.012673, 0.037082, 0.125998, 0.589936]
pF = [46.03, 4.12, 3.14, 0.80, 2.62, 3.32, 8.46, 0.53, 2.11, 6.18]
for i, v in enumerate(paper): compare(f"SS {t.Source[i]}", t.SS[i], v, tol_rel=0.05)
for i, v in enumerate(pF): compare(f"F  {t.Source[i]}", t.F[i], v, tol_rel=0.10)
full = anova(d, Y, f + ["{}:{}".format(*c) for c in itertools.combinations(f, 2)] +
             ["{}:{}:{}".format(*c) for c in itertools.combinations(f, 3)] + [":".join(f)])
show("Full 2^4 model against pure error (16 df)", full, "{:.6f}")
print("\nSmall SS differences come from GPC printed to 2 decimals. The lack-of-fit F (paper 82.5) shows the pooled"
      " 3- and 4-factor interactions are large; against pure error most terms, incl. BCD, are highly significant.")
