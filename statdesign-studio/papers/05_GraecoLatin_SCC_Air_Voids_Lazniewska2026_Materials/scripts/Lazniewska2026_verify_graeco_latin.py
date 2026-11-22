"""Lazniewska2026_verify_graeco_latin.py - Lazniewska-Piekarczyk, Miera & Moskal (2026), Materials 19:2730,
doi:10.3390/ma19132730. 5x5 Graeco-Latin square: rows AEA dosage, columns binder type, Latin letters
w/b ratio, Greek letters paste ratio phi; residual 8 df. Paper Tables 9-14.  python3 Lazniewska2026_verify_graeco_latin.py"""
import pandas as pd
from doe_anova import anova, show, compare
d = pd.read_csv("../data/Lazniewska2026_SCC_5x5_graeco_latin_square_air_voids.csv")
f = ["AEA dosage (% of cement mass)", "Binder type", "Water-to-binder ratio w/b", "Paste-to-aggregate void ratio phi"]
for c in f: d[c] = d[c].astype(str)
paper = {"Fresh air content after 5 min A5 (%)": [3.888, 36.784, 26.704, 1.296, 23.568],
         "Air loss 5 to 15 min dA (percentage points)": [0.0416, 0.0483, 0.2431, 0.0541, 0.1160],
         "Hardened total air content A (%)": [1.711, 37.058, 30.007, 10.990, 22.160],
         "Spacing factor L (mm)": [0.0048, 0.0332, 0.0547, 0.0223, 0.0436],
         "Micropore content A300 (%)": [2.869, 0.254, 6.012, 2.404, 2.963],
         "Specific surface of air voids alpha (1/mm)": [122.300, 156.095, 240.823, 65.694, 136.069]}
for y, pv in paper.items():
    t = anova(d, y, f); show(y, t)
    for i, v in enumerate(pv): compare(f"SS {t.Source[i]}", t.SS[i], v)
# design check: every pair of factors appears once at each level combination
print("\nDesign check (each pair of factors balanced):",
      all(d.groupby([a, b]).size().eq(1).all() for i, a in enumerate(f) for b in f[i + 1:]))
