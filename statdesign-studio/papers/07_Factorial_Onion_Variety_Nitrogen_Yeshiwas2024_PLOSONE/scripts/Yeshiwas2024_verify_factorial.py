"""Yeshiwas2024_verify_factorial.py - Yeshiwas, Alemayehu & Adgo (2024), PLOS ONE 19:e0312394,
doi:10.1371/journal.pone.0312394. 4 varieties x 4 N rates factorial in RCBD (3 blocks) at 3 locations.
The paper prints pooled means, LSD and CV (Tables 5-7) but no ANOVA table. The model that reproduces
its CV and LSD is Location + Rep + Variety x N (location interactions pooled into error).
python3 Yeshiwas2024_verify_factorial.py"""
import numpy as np, pandas as pd
from scipy import stats
from doe_anova import anova, show, compare
d = pd.read_csv("../data/Yeshiwas2024_onion_4x4_factorial_RCBD_3_locations.csv")
V, N, L, R = "Variety", "Nitrogen rate (kg/ha)", "Location", "Replication (block)"
d["Loc-Rep"] = d[L] + "-" + d[R].astype(str)
paper_means = {"Marketable bulb yield (t/ha)": ({"Russet": 26.50, "Red Coach": 21.84, "Jambar": 24.57, "Bombay Red": 19.86},
                                                {0: 19.09, 41: 22.39, 82: 26.77, 123: 24.51}, 20.76, 2.25),
               "Total dry bulb yield (t/ha)": ({"Russet": 26.93, "Red Coach": 22.35, "Jambar": 25.09, "Bombay Red": 20.453},
                                               {0: 19.68, 41: 22.91, 82: 27.26, 123: 24.97}, 20.22, 2.24),
               "Average bulb weight (g)": ({"Russet": 93.12, "Red Coach": 91.16, "Jambar": 94.28, "Bombay Red": 81.7},
                                           {0: 79.84, 41: 85.87, 82: 101.18, 123: 93.38}, 8.74, 3.68),
               "Bulb diameter (mm)": ({"Russet": 69.54, "Red Coach": 65.85, "Jambar": 68.53, "Bombay Red": 64.56},
                                      {0: 60.44, 41: 63.99, 82: 73.4, 123: 70.66}, 12.22, 3.82),
               "Days to physiological maturity (days)": ({"Russet": 94.69, "Red Coach": 101.72, "Jambar": 100.97, "Bombay Red": 108.47},
                                                         {0: 100.13, 41: 100.55, 82: 100.94, 123: 104.22}, 5.6, 3.27)}
for y, (vm, nm, cv, lsd) in paper_means.items():
    print(f"\n### {y}")
    for k, v in vm.items(): compare(f"mean {k}", d[d[V] == k][y].mean(), v)
    for k, v in nm.items(): compare(f"mean N = {k}", d[d[N] == k][y].mean(), v)
    t = anova(d, y, [L, R, V, N, f"{V}:{N}"]); mse, dfe = t.MS.iloc[-2], t.df.iloc[-2]
    compare("CV (%)  [Location + Rep + V x N]", 100 * np.sqrt(mse) / d[y].mean(), cv)
    compare("LSD 0.05 for 36-plot means", stats.t.ppf(.975, dfe) * np.sqrt(2 * mse / 36), lsd)
show("Marketable yield: paper's pooled model (Location + Rep + Variety x N)", anova(d, "Marketable bulb yield (t/ha)", [L, R, V, N, f"{V}:{N}"]))
show("Marketable yield: full combined RCBD (blocks nested in locations, all location interactions)",
     anova(d, "Marketable bulb yield (t/ha)", [L, "Loc-Rep", V, N, f"{V}:{N}", f"{L}:{V}", f"{L}:{N}", f"{L}:{V}:{N}"]))
