"""Carlos2026_verify_factorial.py - Carlos et al. (2026), PLOS ONE 21:e0357473, doi:10.1371/journal.pone.0357473.
3 x 4 x 4 factorial (mode x Zn x Fe) in an RCBD with 4 blocks; paper Table 3.  python3 Carlos2026_verify_factorial.py"""
import pandas as pd, numpy as np
from doe_anova import anova, show, compare
d = pd.read_csv("../data/Carlos2026_cowpea_3x4x4_factorial_RCBD_grain_yield.csv")
Y = "Grain yield (kg/ha)"; M, Z, F = "Fertilization mode", "Zinc rate (kg/ha)", "Iron rate (kg/ha)"
t = anova(d, Y, ["Block", M, Z, F, f"{M}:{Z}", f"{M}:{F}", f"{Z}:{F}", f"{M}:{Z}:{F}"]); show("Grain yield", t, "{:,.2f}")
for i, v in enumerate([16632.49, 576142.53, 2456502.23, 499419.15, 95889.82, 21576.92, 152749.41, 15286.84, 2140.87, 3836340.24]):
    compare(f"SS {t.Source[i]}", t.SS[i], v)
print(f"\nCV = {100*np.sqrt(t.MS[8])/d[Y].mean():.2f}%  (residual SD {np.sqrt(t.MS[8]):.1f} kg/ha on a mean of {d[Y].mean():.0f} kg/ha)")
print("A field CV near 0.2% is far below the 5-15% usual for grain yield; worth asking the authors how plot yields were obtained.")
