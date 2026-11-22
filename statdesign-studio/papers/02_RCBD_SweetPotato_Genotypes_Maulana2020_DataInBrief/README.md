# 02 RCBD SweetPotato Genotypes Maulana2020 DataInBrief

**Paper.** Maulana, H., Dewayani, S., Solihin, M. A., Arifin, M., Amien, S., & Karuniawan, A. (2020). Yield stability dataset of new orange fleshed sweet potato (Ipomoea batatas L. (lam)) genotypes in West Java, Indonesia. Data in Brief, 32, 106297. https://doi.org/10.1016/j.dib.2020.106297

**Licence.** CC BY-NC-ND 4.0 (PDF included unmodified). Paper: `paper/Maulana2020_DataInBrief_sweet_potato_yield_stability.pdf` (open-access PDF, CC BY-NC-ND 4.0 (PDF included unmodified)).

**Design.** RCBD: 23 genotypes (17 MZ clones + 6 checks) x 3 blocks at each of 3 locations (Sumedang, Bandung, Karawang).

## What the paper did
Measured tuber yield of new orange-fleshed sweet potato clones and checks in three environments, ran a combined ANOVA, compared genotypes within each location, and ranked yield stability with AMMI, GGE and parametric/non-parametric stability statistics.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
RCBD module. Response = Tuber yield (t/ha); Treatment = Genotype; Block = Replication (block). Data: Sumedang subset (69 plots).

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`) and pinned to the report. To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_9_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / Python) | Tally |
|---|---|---|---|
| Location means (Table 2) | 17.28 / 10.22 / 8.22 | 17.28 / 10.22 / 8.22 | Yes |
| Genotype means at Sumedang (e.g. MZ202, MZ237, MZ236) | 33.5 / 37.23 / 2.7 | 33.5 / 37.23 / 2.7 | Yes |
| Combined SS: Genotypes / Locations / GxE (Table 1) | 134.65 / 72.52 / 176.88 | 134.72 / 72.52 / 176.75 (square-root yield) | Yes, only on sqrt scale |
| Combined CV (Table 1) | 13.82 % | 13.82 % (sqrt scale); 25.96 % raw | Yes, only on sqrt scale |
| Per-location CV Bandung / Karawang (Table 2) | 13.65 / 18.28 % | 13.66 / 18.29 % (sqrt scale) | Yes, only on sqrt scale |
| Per-location CV Sumedang | 9.29 % | 11.00 % (sqrt); 21.49 % (raw) | No |

## Insights and possible additions
- The paper never says so, but its ANOVA (Table 1) and CVs were computed on square-root-transformed yield; on the raw t/ha scale the SS are 30-50 times larger. Anyone re-running the paper must transform first.
- Checked against the paper's text and the supplement: the paper states no transformation (Section 2.4 says only that yields were converted to t/ha; Section 2.5 gives the plain model). The supplement's values are not already transformed: the ANOVA of the values as given is far from Table 1 (genotype SS 4422), and only the square root of every value reproduces it (genotype F 32.27, G×E F 21.17, CV 13.82 %); fourth root, log(y+1) and √(y+0.5) do not. The paper's min, max, average and Table 2 means are the plain t/ha values.
- Data oddity (separate from the mismatch): the six check varieties are stored at full precision as square roots of whole numbers (41 of 54 plots; 7.0711 = √50 six times), the 17 clones as two-decimal t/ha values. Their size is that of yields in t/ha (mean 13.1 against 11.5 for the clones), so the checks are not on a smaller, transformed scale; where the numbers came from is not explained.
- At Sumedang genotypes differ strongly (app: F = 23.64, p < 0.0001); the residuals are slightly non-normal (Shapiro-Wilk p = 0.030), which also argues for the square-root scale.
- Addition: rerun the RCBD on the square-root column (provided in the CSV) and compare the decision; the GxE term is larger than the genotype term, so stability ranking matters more than overall means.

## Files in this folder
- `analysis/Maulana2020_sweet_potato_Sumedang_RCBD.sdsa`
- `analysis/Maulana2020_verify_RCBD_output.txt`
- `analysis/StatDesign_app_output_sweet_potato_Sumedang_yield.txt`
- `data/Maulana2020_sweet_potato_23_genotypes_RCBD_3_locations.csv`
- `data/Maulana2020_sweet_potato_Sumedang_yield_RCBD.csv`
- `data/source_supplement/DataInBrief_106297_Supplementary_mmc2_raw_data.xlsx`
- `paper/Maulana2020_DataInBrief_sweet_potato_yield_stability.pdf`
- `scripts/Maulana2020_verify_RCBD.py`
- `scripts/doe_anova.py`

`scripts/` recomputes everything independently with Python (numpy, pandas, scipy); run it from inside `scripts/`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
