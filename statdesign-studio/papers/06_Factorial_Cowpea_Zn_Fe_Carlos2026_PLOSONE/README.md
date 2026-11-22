# 06 Factorial Cowpea Zn Fe Carlos2026 PLOSONE

**Paper.** Carlos, T. J., Moiana, L. D., Sabbag, O. J., Massawe, B. H. J., Mwanri, A. W., & Sibuga, K. P. (2026). Bioeconomic optimization of zinc and iron agronomic biofortification in cowpea (Vigna unguiculata [L.] Walp.) in Mozambique under tropical field conditions. PLOS ONE, 21, e0357473. https://doi.org/10.1371/journal.pone.0357473

**Licence.** CC BY 4.0. Paper: `paper/Carlos2026_PLOSONE_cowpea_Zn_Fe_biofortification.pdf` (open-access PDF, CC BY 4.0).

**Design.** 3 x 4 x 4 factorial (fertilization mode x Zn rate x Fe rate) in an RCBD with 4 blocks; 192 plots.

## What the paper did
Tested three ways of applying zinc and iron (soil, foliar, soil + foliar) at four rates each on cowpea, analysed grain yield by factorial ANOVA, fitted a quadratic response surface, and found the economic optimum (16.9 kg Zn, 10.1 kg Fe per ha) below the biological optimum.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
Factorial module. Response = Grain yield (kg/ha); Factor A = Fertilization mode; Factor B = Zinc rate (kg/ha); Factor C = Iron rate (kg/ha); Block = Block.

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`) and pinned to the report. To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_9_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / Python) | Tally |
|---|---|---|---|
| SS Block / M / Zn / Fe (Table 3) | 16,632.49 / 576,142.53 / 2,456,502.23 / 499,419.15 | 16,632.5 / 576,143 / 2,456,500 / 499,419 | Yes |
| SS MxZn / MxFe / ZnxFe / MxZnxFe | 95,889.82 / 21,576.92 / 152,749.41 / 15,286.84 | 95,889.8 / 21,576.9 / 152,749 / 15,286.8 | Yes |
| Error SS, df, MS | 2,140.87, 141, 15.18 | 2,140.87, 141, 15.18 | Yes |
| F for Zn | 53,929.37 | 53,929 | Yes |

## Insights and possible additions
- The factorial ANOVA replicates to every digit.
- Data-quality flag: the residual SD is about 3.9 kg/ha on a mean of about 1,700 kg/ha (CV 0.23 %). Field grain-yield trials typically have CVs of 5-15 %, so the F-values in the tens of thousands are not realistic; the plot data look smoothed or generated from treatment means.
- The app's assumption checks fail on constant variance (Breusch-Pagan p = 0.0008) and independence (Breusch-Godfrey p < 0.0001), which the paper does not report; the app moves to the rank-based (Friedman) analysis.
- Addition: because Zn and Fe rates are quantitative, the app's factorial output could be followed by polynomial contrasts (linear/quadratic) to link the ANOVA to the response-surface model in the paper.

## Files in this folder
- `analysis/Carlos2026_cowpea_ZnFe_factorial_RCBD.sdsa`
- `analysis/Carlos2026_verify_factorial_output.txt`
- `analysis/StatDesign_app_output_cowpea_grain_yield.txt`
- `data/Carlos2026_cowpea_3x4x4_factorial_RCBD_grain_yield.csv`
- `data/source_supplement/PLOSONE_0357473_S1_Dataset.xlsx`
- `paper/Carlos2026_PLOSONE_cowpea_Zn_Fe_biofortification.pdf`
- `scripts/Carlos2026_verify_factorial.py`
- `scripts/doe_anova.py`

`scripts/` recomputes everything independently with Python (numpy, pandas, scipy); run it from inside `scripts/`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
