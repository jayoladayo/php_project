# 01 RCBD Potato Varieties Tessema2022 PeerJ

**Paper.** Tessema, G. L., Mohammed, A. W., & Abebe, D. T. (2022). Genetic variability studies for tuber yield and yield attributes in Ethiopian released potato (Solanum tuberosum L.) varieties. PeerJ, 10, e12860. https://doi.org/10.7717/peerj.12860

**Licence.** CC BY 4.0. Paper: `paper/Tessema2022_PeerJ_potato_genetic_variability_RCBD.pdf` (open-access PDF, CC BY 4.0).

**Design.** Randomized complete block design (RCBD): 21 varieties x 3 blocks at each of 2 locations (Holetta, Adaberga); combined analysis over locations.

## What the paper did
Planted 20 released potato varieties and a local cultivar at two sites in central Ethiopia, ran an RCBD ANOVA for 14 traits at each site and over sites, and used the mean squares to estimate genotypic and phenotypic variance, GCV, PCV, heritability and genetic advance to pick traits worth selecting on.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
RCBD module. Response = Marketable tuber yield (t/ha); Treatment = Variety; Block = Replication (block). Data: Holetta subset (63 plots).

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`) and pinned to the report. To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_9_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / Python) | Tally |
|---|---|---|---|
| MS variety (Holetta, MTY) | 180.594 | 180.594 (gives sigma2g 58.38) | Yes |
| MSE (Holetta, MTY) | 5.455 | 5.455 | Yes |
| sigma2 g / sigma2 p (Table 5) | 58.38 / 60.20 | 58.38 / 60.20 | Yes |
| GCV / PCV / H2 (Table 5) | 30.20 / 30.66 / 96.98 % | 30.20 / 30.66 / 96.98 % | Yes |
| Combined MS for MTY: Rep(L), V, L, LxV, Error (Table 3) | 15.02, 205.99, 1927.87, 44.39, 3.82 | 15.02, 205.99, 1927.86, 44.39, 3.81 | Yes |
| Combined MS for total tuber yield | 16.99, 218.57, 2556.72, 49.37, 4.39 | 16.06, 218.54, 2556.74, 49.38, 4.81 | Partly (Rep and Error differ) |

## Insights and possible additions
- The app gives the paper's Holetta numbers exactly, so the genetic parameters in Table 5 can be derived straight from the app's ANOVA table (sigma2g = (MSG - MSE)/r).
- For total tuber yield the Rep(L) and error mean squares in Table 3 do not match the supplementary data, although the variety and location rows do; the TTY row appears to come from a different version of the data.
- Location x variety is large for every yield trait (MS 44.39 vs error 3.82 for MTY), so variety rankings change between sites; a stability analysis (AMMI or GGE) would be a natural addition.
- The app's assumption checks: residuals normal (Shapiro-Wilk p = 0.36) and independent; nothing in the RCBD needs a transformation.

## Files in this folder
- `analysis/StatDesign_app_output_potato_Holetta_marketable_yield.txt`
- `analysis/Tessema2022_potato_Holetta_RCBD.sdsa`
- `analysis/Tessema2022_verify_RCBD_output.txt`
- `data/Tessema2022_potato_21_varieties_RCBD_2_locations.csv`
- `data/Tessema2022_potato_Holetta_marketable_yield_RCBD.csv`
- `data/source_supplement/PeerJ_12860_Supplemental_S2_raw_data.xlsx`
- `paper/Tessema2022_PeerJ_potato_genetic_variability_RCBD.pdf`
- `scripts/Tessema2022_verify_RCBD.py`
- `scripts/doe_anova.py`

`scripts/` recomputes everything independently with Python (numpy, pandas, scipy); run it from inside `scripts/`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
