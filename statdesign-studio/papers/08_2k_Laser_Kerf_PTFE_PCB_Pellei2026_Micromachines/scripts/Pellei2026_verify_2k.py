"""Pellei2026_verify_2k.py - Pellei et al. (2026), Micromachines 17:404, doi:10.3390/mi17040404.
Two unreplicated 2^3 designs (aperture A, pulse duration B, loops C) on log10 kerf depth; the paper
keeps the main effects and pools the three two-factor and the three-factor interactions (4 df).
Paper Tables 4 and 7.  python3 Pellei2026_verify_2k.py"""
import pandas as pd
from doe_anova import anova, show, compare
f = ["Aperture A (um)", "Pulse duration B (us)", "Number of loops C"]
for lam, paper in [("RO3003", [0.0271, 0.2999, 0.1726, 7.075e-6, 5.42, 60.02, 34.55]),
                   ("RT_Duroid_5880", [0.0262, 0.3032, 0.0573, 5.275e-6, 6.77, 78.41, 14.81])]:
    d = pd.read_csv(f"../data/Pellei2026_laser_kerf_2cubed_{lam}.csv")
    t = anova(d, "Log10 of kerf depth", f); show(f"{lam}: log10 kerf depth, interactions pooled", t, "{:.6f}")
    sst = t.SS.iloc[-1]
    for i in range(3):
        compare(f"SS {t.Source[i]}", t.SS[i], paper[i]); compare(f"% contribution {t.Source[i]}", 100 * t.SS[i] / sst, paper[4 + i])
    compare("Residual mean square", t.MS[3], paper[3], tol_rel=0.05)
    show(f"{lam}: interactions shown separately", anova(d, "Log10 of kerf depth", f + [f"{f[0]}:{f[1]}", f"{f[0]}:{f[2]}", f"{f[1]}:{f[2]}"]), "{:.6f}")
print("\nF-values differ by about 2% only because the paper's ANOVA used unrounded depths; "
      "both papers' tables also repeat the same 'Total MS 0.3855', a copy error.")
