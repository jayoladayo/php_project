"""Maulana2020_verify_RCBD.py - Maulana et al. (2020), Data in Brief 32:106297, doi:10.1016/j.dib.2020.106297.
(a) RCBD at Sumedang (23 genotypes x 3 blocks) on the raw scale, as run in StatDesign Studio, with the
location means of paper Table 2; (b) the combined ANOVA of paper Table 1, which only reproduces when
yield is square-root transformed (the paper does not say so).  python3 Maulana2020_verify_RCBD.py"""
import numpy as np, pandas as pd
from doe_anova import anova, show, compare
d = pd.read_csv("../data/Maulana2020_sweet_potato_23_genotypes_RCBD_3_locations.csv")
Y, S = "Tuber yield (t/ha)", "Square root of tuber yield (sqrt t/ha)"
s = d[d.Location == "Sumedang"]
show("RCBD at Sumedang, raw tuber yield (t/ha)", anova(s, Y, ["Replication (block)", "Genotype"]))
print("\nLocation means (paper Table 2):")
for loc, pm in [("Sumedang", 17.28), ("Bandung", 10.22), ("Karawang", 8.22)]:
    compare(f"Mean yield {loc} (t/ha)", d[d.Location == loc][Y].mean(), pm)
gm = s.groupby("Genotype")[Y].mean()
for g, pm in [("MZ202", 33.5), ("MZ237", 37.23), ("MZ236", 2.7)]:
    compare(f"Sumedang mean {g}", gm[g], pm)
d["Loc-Rep"] = d.Location + "-" + d["Replication (block)"].astype(str)
for lab, col in [("raw yield", Y), ("square-root yield", S)]:
    t = anova(d, col, ["Location", "Loc-Rep", "Genotype", "Location:Genotype"])
    show(f"Combined ANOVA on {lab}", t)
t = anova(d, S, ["Location", "Loc-Rep", "Genotype", "Location:Genotype"]); ss = dict(zip(t.Source, t.SS))
print("\nPaper Table 1 vs square-root analysis:")
compare("SS Block(Environment)", ss["Loc-Rep"], 0.36); compare("SS Genotypes", ss["Genotype"], 134.65)
compare("SS Locations", ss["Location"], 72.52); compare("SS G x E", ss["Location x Genotype"], 176.88)
compare("CV (%)", 100 * np.sqrt(t.MS[4]) / d[S].mean(), 13.82)
compare("F Locations tested against Block(Env)", t.MS[0] / t.MS[1], 641.61)
