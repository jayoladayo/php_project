"""Tessema2022_verify_RCBD.py - Tessema, Mohammed & Abebe (2022), PeerJ 10:e12860, doi:10.7717/peerj.12860.
Reproduces (a) the single-location RCBD for marketable tuber yield at Holetta and the genetic
parameters derived from it (paper Table 5) and (b) the combined ANOVA over two locations (Table 3).
Run from this folder: python3 Tessema2022_verify_RCBD.py   (needs numpy, pandas, scipy)."""
import numpy as np, pandas as pd
from doe_anova import anova, show, compare
d = pd.read_csv("../data/Tessema2022_potato_21_varieties_RCBD_2_locations.csv")
Y = "Marketable tuber yield (t/ha)"
h = d[d.Location == "Holetta"]
t = anova(h, Y, ["Replication (block)", "Variety"]); show("RCBD at Holetta: marketable tuber yield (t/ha)", t)
msg, mse = t.MS[1], t.MS[2]; r = 3
s2g = (msg - mse) / r; s2p = s2g + mse / r; m = h[Y].mean()
print("\nGenetic parameters at Holetta (paper Table 5, MTY row):")
compare("sigma2 g = (MSG - MSE)/r", s2g, 58.38); compare("sigma2 p = sigma2 g + MSE/r", s2p, 60.20)
compare("GCV (%)", 100 * np.sqrt(s2g) / m, 30.20); compare("PCV (%)", 100 * np.sqrt(s2p) / m, 30.66)
compare("Broad-sense heritability H2 (%)", 100 * s2g / s2p, 96.98)
compare("Genetic advance GA (5%) = 2.06*sqrt(s2p)*H2", 2.06 * np.sqrt(s2p) * s2g / s2p, 15.50)
d["Loc-Rep"] = d.Location + "-" + d["Replication (block)"].astype(str)
print("\nCombined ANOVA over locations (paper Table 3 mean squares):")
paper = {"Marketable tuber yield (t/ha)": [15.02, 205.99, 1927.87, 44.39, 3.82, 9.13],
         "Total tuber yield (t/ha)": [16.99, 218.57, 2556.72, 49.37, 4.39, 8.29],
         "Average tuber weight (g)": [21.24, 600.91, 4538.05, 378.75, 21.27, 8.95]}
for y, pv in paper.items():
    t = anova(d, y, ["Location", "Loc-Rep", "Variety", "Location:Variety"]); ms = dict(zip(t.Source, t.MS))
    print(f" {y}")
    for lab, key, v in zip(["Rep(Location)", "Variety", "Location", "Location x Variety", "Error"],
                           ["Loc-Rep", "Variety", "Location", "Location x Variety", "Error"], pv):
        compare("MS " + lab, ms[key], v)
    compare("CV (%)", 100 * np.sqrt(ms["Error"]) / d[y].mean(), pv[5])
