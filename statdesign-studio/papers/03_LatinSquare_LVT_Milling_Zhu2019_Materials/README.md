# 03 LatinSquare LVT Milling Zhu2019 Materials

**Paper.** Zhu, Z., Cao, P., Guo, X., Wang, X. (A.), Zhang, F., & Gao, Y. (2019). Machinability of Luxury Vinyl Tiles during Plain Milling Using a Helical Cutter. Materials, 12(13), 2174. https://doi.org/10.3390/ma12132174

**Licence.** CC BY 4.0. Paper: `paper/PAPER_LINK.txt` and `Download_paper_PDF.url` (open access, CC BY 4.0; the PDF is large, so the link is given).

**Design.** L9(3^4) orthogonal array with 3 factors = 3x3 Latin square: rows helix angle (54, 62, 70 deg), columns cutting speed (1760, 2200, 2640 m/min), Latin letters cutting depth (0.5, 1.0, 1.5 mm); error = empty column (2 df).

## What the paper did
Plain-milled luxury vinyl floor tiles with a PCD helical cutter, measured resultant cutting force and surface roughness (3 repeats, averaged), and used range analysis and ANOVA to rank the three cutting parameters.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
Latin Square module. Response = Resultant cutting force FR (N) or Surface roughness Ra (um); Treatment = Cutting depth (mm); Row = Helix angle (deg); Column = Cutting speed (m/min).

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`) and pinned to the report. To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_9_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / Python) | Tally |
|---|---|---|---|
| FR: SS helix / speed / depth / error (Table 5) | 284.333 / 3.446 / 5560.164 / 155.24 | 284.333 / 3.446 / 5560.16 / 155.236 | Yes |
| FR: F helix / speed / depth | 1.832 / 0.022 / 35.817 | 1.832 / 0.022 / 35.818 | Yes |
| Ra: SS helix / speed / depth / error (Table 7) | 0.019 / 0.005 / 0.013 / 0.010 | 0.0192 / 0.0053 / 0.0126 / 0.0083 | Yes except error SS |
| Ra: F helix / speed / depth | 2.375 / 0.625 / 1.625 | 2.314 / 0.638 / 1.520 | Close; paper used error 0.008 |

## Insights and possible additions
- Cutting force replicates to every digit: depth of cut dominates (F = 35.8, p = 0.027).
- For roughness the paper prints an error SS of 0.010 and a total of 0.047 (Table 7), but its F-values only work with an error of 0.008 (0.019/0.008 = 2.375, 0.005/0.008 = 0.625, 0.013/0.008 = 1.625). From the paper's own Table 6 data the app gives error SS 0.0083 and total 0.0454, so the printed 0.010 and 0.047 do not come from the data, while 0.008 is the correct error rounded to three decimals. The paper's F-values are also ratios of rounded SS (the data in Table 6 are identical to the CSV), so they differ slightly from the unrounded ones (2.31, 0.64, 1.52). The conclusion (no factor significant) does not change. The stated R-squared values (0.94 for force, 0.95 for roughness) could not be reproduced either: the data give 0.974 and 0.817.
- With 2 error df the test has very little power; the paper took 3 measurements per run but analysed only the means. Entering the 27 individual measurements would give a proper replicated Latin square.
- Residuals are borderline non-normal (Shapiro-Wilk p = 0.03), which is common with 9 points; the app offers the rank-based alternative automatically.

## Files in this folder
- `analysis/StatDesign_app_output_milling_cutting_force.txt`
- `analysis/StatDesign_app_output_milling_surface_roughness.txt`
- `analysis/Zhu2019_LVT_milling_force_latin_square.sdsa`
- `analysis/Zhu2019_LVT_milling_roughness_latin_square.sdsa`
- `analysis/Zhu2019_verify_latin_square_output.txt`
- `data/Zhu2019_LVT_milling_L9_latin_square_force_roughness.csv`
- `paper/Download_paper_PDF.url`
- `paper/PAPER_LINK.txt`
- `scripts/Zhu2019_verify_latin_square.py`
- `scripts/doe_anova.py`

`scripts/` recomputes everything independently with Python (numpy, pandas, scipy); run it from inside `scripts/`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
