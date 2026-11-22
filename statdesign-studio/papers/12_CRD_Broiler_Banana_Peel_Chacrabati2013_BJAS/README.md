# 12 CRD: broiler live weight and banana peel meal, Chacrabati 2013, Bangladesh Journal of Animal Science

**Paper.** Chacrabati, et al. (2013). Effect of dietary supplementation of banana (Musa sapientum) peel meal with probiotics on broiler performance. Bangladesh Journal of Animal Science. (DOI not recorded in the confirmation pack.)

**Licence.** Not recorded in the confirmation pack (see the journal page). No PDF is included here; the DOI button opens the paper.

**Design.** Completely randomized design: 4 diets (control, plantain/banana peel meal 0.5 %, Bio-Sel E, a commercial additive) with 4 replicates each, 16 observations; response = live weight (g). Only the diet means, SEs and significance letters are published, so the raw data are rebuilt.

## What the paper did
Compared broiler performance on diets supplemented with banana peel meal and probiotics. The table of live weights gives the mean, the standard error and a significance letter for each diet (control 1136 a, plantain 0.5 % 939 b, Bio-Sel E 1138 a, commercial 1105 a) and flags the diets as significantly different (**, p < 0.01).

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
CRD page (CRD ANOVA). Response = live weight (g), Treatment = diet. The 4 values per diet were rebuilt to have exactly the printed mean and SD = SE × √4, so F depends only on the published summary statistics.

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`). To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_15_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / R) | Tally |
|---|---|---|---|
| Significance of the diet effect | ** (p < 0.01) | F = 30.351, df = 3 and 12, p = 6.9e-06 | Yes (consistent; the paper prints no F) |
| F recomputed from the published means and SEs | - | 30.3512 by hand and in R; 30.351 in the app | Yes |
| Significance letters | Plantain 0.5 % (b) differs from the other three (a) | Plantain 0.5 % has the lowest mean (939 g against 1105 to 1138 g) | Yes (consistent) |

## Insights and possible additions
- The F of 30.35 on 3 and 12 df is fixed by the printed means and SEs (SS between 108 020, SS error 14 236); it agrees with the paper's highly-significant flag but there is no printed F to match to the digit.
- Only the plantain 0.5 % diet stands apart: it lowers live weight by roughly 170 to 200 g compared with the other three diets, which are close to each other.
- The raw data of this article were never published, so the 16 values are a reconstruction. The confirmation pack records this article as a substitute source: the original seven-treatment paper could not be found open access.
- With only 4 replicates per diet, 12 error df and a large effect, the conclusion is not sensitive to the reconstruction, but residual checks on rebuilt data say nothing about the real broilers.

## Files in this folder
- `analysis/Chacrabati2013_broiler_live_weight_CRD.sdsa`
- `analysis/Chacrabati2013_verify_CRD_output.txt`
- `analysis/StatDesign_app_output_live_weight.txt`
- `data/Chacrabati2013_broiler_live_weight_4_treatments_reconstructed.csv`
- `data/source_supplement/Chacrabati2013_published_means_SE.csv`
- `scripts/Chacrabati2013_verify_CRD.R`

`scripts/` recomputes the analysis independently in base R (no packages); run it from inside `scripts/` with `Rscript`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
