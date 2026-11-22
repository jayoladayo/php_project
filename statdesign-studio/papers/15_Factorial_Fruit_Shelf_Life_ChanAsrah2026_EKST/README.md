# 15 Factorial: fruit shelf life, Chan and Asrah 2026, EKST

**Paper.** Chan, J. W., & Asrah, N. B. (2026). Effect of preservative type, moisture control and storage condition on fruit shelf life. EKST. (Volume and DOI not recorded in the confirmation pack.)

**Licence.** Not recorded in the confirmation pack (see the journal page). No PDF is included here; the DOI button opens the paper.

**Design.** 3 x 3 x 3 factorial in a completely randomized design with 3 replicates per cell (81 observations): preservative (natural, chemical, none) x moisture control (no sprinkling, every 3 days, every 5 days) x storage condition (open, partial, full); response = shelf life (days). The paper's own raw data.

## What the paper did
Studied how the type of preservative, the moisture control and the storage condition affect the shelf life of fruit, and printed the three-factor ANOVA table of the 81 shelf-life values (total SS 2522.10).

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
Factorial page (CRD ANOVA). Response = shelf life (days); Factor A = preservative, B = moisture control, C = storage condition; the Replicate column is not used. The level 'None' of the preservative is named 'No preservative' in the CSV: pandas reads the literal text None as a missing value and silently drops a whole level.

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`). To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_15_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / R) | Tally |
|---|---|---|---|
| Total sum of squares | 2522.10 | 2522.10 (2522.099) | Yes |
| Main effects A, B, C | All significant | F = 282.68, 17.06, 315.99 (all p < 0.0001) | Yes |
| Interactions | AC and BC significant; AB and ABC not | AC F = 8.898 (p < 0.0001), BC F = 3.066 (p = 0.0239); AB F = 0.658 (p = 0.624), ABC F = 1.008 (p = 0.441) | Yes |

## Insights and possible additions
- The whole ANOVA table agrees with the paper's, from the paper's own raw data: preservative and storage condition carry most of the variation (SS 1061 and 1186 of 2522), moisture control much less (64).
- The significant Preservative x Storage and Moisture x Storage interactions mean the main effects of preservative and moisture should not be read alone: the benefit of a preservative depends on how the fruit is stored.
- Data trap found while checking: the level 'None' was read as a missing value by pandas, collapsing a three-level factor to two levels and giving a total SS of 1530.83 instead of 2522.10. The level is called 'No preservative' in the CSV.
- The app's three residual checks pass (Shapiro-Wilk p = 0.12), so the parametric ANOVA is the analysis to report; with 81 observations and 54 error df the design is well powered.

## Files in this folder
- `analysis/ChanAsrah2026_shelf_life_factorial.sdsa`
- `analysis/ChanAsrah2026_verify_factorial_output.txt`
- `analysis/StatDesign_app_output_shelf_life.txt`
- `data/ChanAsrah2026_shelf_life_3x3x3_factorial.csv`
- `scripts/ChanAsrah2026_verify_factorial.R`

`scripts/` recomputes the analysis independently in base R (no packages); run it from inside `scripts/` with `Rscript`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
