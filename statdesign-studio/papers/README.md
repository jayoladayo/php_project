# StatDesign Studio – replication of fifteen published experimental-design papers

KFUPM Experimental Design, Group 4. App: https://boxwish.shinyapps.io/statdesign-studio/ (papers 01 to 09 run 15 November 2026; papers 10 to 15 added 22 November 2026 from the group's confirmation results).

Each numbered folder holds one paper and has the same layout:

| Sub-folder | Contents |
|---|---|
| `paper/` | The open-access PDF, or a link file for the four large PDFs (03, 04, 05, 09) |
| `data/` | Labelled CSV(s) used in the app (full column names with units); `source_supplement/` keeps the authors' original file where there was one |
| `analysis/` | StatDesign Studio saved analyses (`.sdsa`), the app's ANOVA + assumption-check text, and the Python verification output |
| `scripts/` | Independent Python re-computation (`*_verify_*.py` + shared `doe_anova.py`) |
| `README.md` | Citation, licence, what the paper did, app settings, tally table, insights and additions |

`StatDesign_15_papers_all_datasets.sdproj` (this folder) reloads all sixteen datasets into the app via *Open project*.

## Summary

| # | Design | Paper | App module | Do the values tally? | Main finding |
|---|---|---|---|---|---|
| 01 | RCBD | Tessema et al. 2022, PeerJ – potato varieties | RCBD | Yes, exactly (MS, σ²g, GCV, H²) | Total-yield row of the combined ANOVA does not match the supplement |
| 02 | RCBD | Maulana et al. 2020, Data in Brief – sweet potato genotypes | RCBD | Means yes; ANOVA only on √yield | Undisclosed √ transform; check-variety data stored as exact square roots |
| 03 | Latin square | Zhu et al. 2019, Materials – LVT milling | Latin Square | Force exact; roughness F's use error 0.008 not the printed 0.010 | Depth of cut drives cutting force |
| 04 | Latin square | Lu et al. 2025, Materials – pervious concrete | Latin Square | Permeability and 7-day yes; 28-day and tensile **no** | Paper's error SS wrong (copied / zero); significance reverses |
| 05 | Graeco-Latin square | Łaźniewska-Piekarczyk et al. 2026, Materials – SCC air voids | Graeco-Latin Square | Yes, all six ANOVAs exactly | Only w/b significant (air loss, micropores) |
| 06 | Factorial (3×4×4 in RCBD) | Carlos et al. 2026, PLOS ONE – cowpea Zn × Fe | Factorial | Yes, exactly | CV 0.23 % is implausible for field data; variance and independence checks fail |
| 07 | Factorial (4×4 in RCBD, 3 sites) | Yeshiwas et al. 2024, PLOS ONE – onion variety × N | Factorial (row/column) | Yes (means, CV 20.77 %, LSD 2.25) | Pooled model hides significant location × variety and location × N |
| 08 | 2^k (two 2³) | Pellei et al. 2026, Micromachines – CO₂ laser kerf | 2^k Factorial | Yes (SS and % contributions) | Interactions negligible on log scale; table copy errors |
| 09 | 2^k (2⁴, 2 replicates) | Pan & Lei 2025, Materials – Al₂O₃ ALD | 2^k Factorial | Yes (to rounding) | Lack of fit F = 82: against pure error every effect is significant |
| 10 | One sample | Uddin et al. 2023, Heliyon – groundwater pH | One sample | Yes, exactly (t = −9.52, df = 17) | Mean pH 7.67 is significantly below the 8.5 limit |
| 11 | k samples | Adawi et al. 2024, Medical Science Monitor – bond strength and aging | k samples | Table 2 gives F = 9.57; the printed F = 13.55 does not follow | Significant either way; the printed F is inconsistent with the paper's table (data rebuilt from means and SDs) |
| 12 | CRD | Chacrabati et al. 2013, Bangladesh J. Animal Science – broiler diets | CRD | Yes: everything printed agrees (means, SE, **); no F printed | Only plantain 0.5 % differs (data rebuilt from means and SEs) |
| 13 | RCBD | Yadeta 2024, J. Scientific Agriculture – vetch and oats | RCBD | Yes: means, CV 18.68 % and P agree; no F printed | Bonsa oats and the Bonsa + vetch mixture yield most; block structure assumed (data rebuilt) |
| 14 | Latin square | Bacharach, Chance & Middleton 1940, Biochemical Journal – diffusing factor | Latin Square | Yes, all three F ratios (0.172, 1.168, 3.910) | Order of injection has no effect; rabbits differ |
| 15 | Factorial (3×3×3 CRD) | Chan & Asrah 2026, EKST – fruit shelf life | Factorial | Yes (total SS 2522.1, whole ANOVA table) | Preservative and storage dominate; two interactions |

## Notes
- Split plot was dropped at the group's request; the mix of papers 01 to 09 is RCBD 2, Latin square 2, Graeco-Latin 1, factorial 2, 2^k 2. Papers 10 to 15 add one sample 1, k samples 1, CRD 1, RCBD 1, Latin square 1 and factorial 1.
- Papers 11, 12 and 13 publish only summary statistics (means and SDs, means and SEs, treatment means and CV), so their data are **reconstructions** that reproduce those statistics exactly; only the F test and the means are checked, not the authors' raw data. Papers 10, 14 and 15 use the authors' own raw data.
- Papers 10 to 15 carry no PDF (the DOI button opens the paper where a DOI is known); their licences were not recorded. Their checks are base-R scripts (`Rscript`), not Python.
- All papers are open access (CC BY 4.0, except 02 which is CC BY-NC-ND 4.0 and is included unmodified).
- The RCBD analyses (01, 02) were run in a fresh app session because the RCBD dataset list did not refresh in the first session; their data are also in the `.sdproj`.
- Python checks need numpy, pandas and scipy only: `cd <folder>/scripts && python3 <script>.py`.
