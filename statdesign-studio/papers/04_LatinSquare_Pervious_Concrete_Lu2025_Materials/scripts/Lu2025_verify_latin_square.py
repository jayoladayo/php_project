"""Lu2025_verify_latin_square.py - Lu, Yang & Jiang (2025), Materials 18:4129, doi:10.3390/ma18174129.
L9(3^3)+null column = 3x3 Latin square (rows sand rate, columns VMA dosage, Latin letters target
porosity); error = null column (2 df). Paper Tables 7, 9, 10 and 11.  python3 Lu2025_verify_latin_square.py"""
import pandas as pd
from doe_anova import anova, show, compare
d = pd.read_csv("../data/Lu2025_pervious_concrete_L9_latin_square.csv")
f = ["Sand rate (%)", "VMA dosage (% of cement)", "Target porosity (%)"]
paper = {"Permeability coefficient (mm/s)": [(1.98, 4.23), (0.58, 1.23), (40.64, 86.66), (0.47, None)],
         "Compressive strength 7 d (MPa)": [(106.78, 7.43), (25.04, 1.74), (257.83, 17.93), (14.38, None)],
         "Compressive strength 28 d (MPa)": [(194.30, 13.42), (15.40, 1.00), (257.39, 17.91), (15.40, None)],
         "Splitting tensile strength 28 d (MPa)": [(0.36, 89.75), (0.11, 28.50), (1.21, 303.25), (0.0, None)]}
for y, pv in paper.items():
    t = anova(d, y, f); show(y, t)
    for i, (ss, F) in enumerate(pv):
        compare(f"SS {t.Source[i]}", t.SS[i], ss)
        if F is not None: compare(f"F  {t.Source[i]}", t.F[i], F)
print("\nThe permeability and 7-day tables tally. For 28-day strength the paper's error SS (15.40) is a copy of the"
      " VMA SS, and for splitting tensile strength it reports an error of 0 although the null column has the"
      " largest range (K-range 0.43); with the correct error no factor is significant for STS.")
