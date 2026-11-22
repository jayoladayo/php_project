"""Zhu2019_verify_latin_square.py - Zhu et al. (2019), Materials 12:2174, doi:10.3390/ma12132174.
L9(3^4) array with three factors = 3x3 Latin square (rows helix angle, columns cutting speed,
Latin letters cutting depth); error = the empty column (2 df). Paper Tables 5 and 7.
python3 Zhu2019_verify_latin_square.py"""
import pandas as pd
from doe_anova import anova, show, compare
d = pd.read_csv("../data/Zhu2019_LVT_milling_L9_latin_square_force_roughness.csv")
f = ["Helix angle (deg)", "Cutting speed (m/min)", "Cutting depth (mm)"]
for y, paper in [("Resultant cutting force FR (N)", [(284.333, 1.832), (3.446, 0.022), (5560.164, 35.817), (155.24, None)]),
                 ("Surface roughness Ra (um)", [(0.019, 2.375), (0.005, 0.625), (0.013, 1.625), (0.010, None)])]:
    t = anova(d, y, f); show(y, t)
    for i, (ss, F) in enumerate(paper):
        compare(f"SS {t.Source[i]}", t.SS[i], ss)
        if F: compare(f"F  {t.Source[i]}", t.F[i], F)
print("\nNote (Ra): the paper's F-values equal SS_factor / 0.008, i.e. an error SS of 0.008, "
      "although its table prints 0.010. Our error SS is 0.0083, so the F-values tally only with 0.008.")
