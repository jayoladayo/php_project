# 13 RCBD: vetch and oats mixtures, Yadeta 2024, Journal of Scientific Agriculture

**Paper.** Yadeta, G. L. (2024). Evaluation of biomass yield and seed yield of vetch and oats variety mixtures. Journal of Scientific Agriculture, 8, 51-58. https://doi.org/10.25081/jsa.2024.v8.8955

**Licence.** Not recorded in the confirmation pack (see the journal page). No PDF is included here; the DOI button opens the paper.

**Design.** Randomized complete block design: 5 treatments (sole Oat Bona-bas, sole Oat Bonsa, sole Vetch Gebisa, Vetch Gebisa + Oat Bonsa, Vetch Gebisa + Oat Bona-bas) in 5 blocks (replications), 25 observations; response = seed yield (t/ha). Only the treatment means, the grand mean and the CV are published, so the data are rebuilt.

## What the paper did
Evaluated biomass and seed yield of vetch and oats varieties grown alone and in mixtures. Table 5 gives the seed yield means (1.2, 2.7, 0.9, 2.8, 1.4 t/ha, letters c, a, d, a, b), a grand mean of 1.8, CV = 18.68 % and P = 0.001.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
RCBD page (RCBD ANOVA). Response = seed yield (t/ha), Treatment, Block. The 25 values were rebuilt so that every treatment mean matches Table 5 and the error mean square matches CV = 18.68 % at the grand mean 1.8; the block means are all equal (no block effect), because no block values were published.

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`). To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_15_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / R) | Tally |
|---|---|---|---|
| Treatment means | 1.2, 2.7, 0.9, 2.8, 1.4 t/ha; grand mean 1.8 | Identical (built in) | Yes |
| CV | 18.68 % | 18.68 % (18.6799) | Yes |
| Treatment effect | P = 0.001 (no F printed) | F = 34.718, df = 4 and 16, p = 1.1e-07 | Yes (consistent; the paper prints no F) |

## Insights and possible additions
- The grand mean of 1.8 is the mean of the five treatment means (9.0 / 5), so Table 5 is internally consistent; the app's F = 34.718 equals the independent calculation (34.7176).
- The block structure is an assumption (no block effect, block F = 0 by construction). This is the weakest check in the pack: it shows the app reproduces an ANOVA consistent with the paper's summary statistics, not the paper's field data.
- Sole Oat Bonsa (2.7 t/ha) and the Vetch Gebisa + Oat Bonsa mixture (2.8 t/ha) give the highest seed yield; sole vetch gives the lowest (0.9 t/ha).
- The paper has small inconsistencies elsewhere (the abstract quotes 2.9 and 1.28 t/ha against 2.8 and 1.2 t/ha in the table, and the text names a lowest-yielding treatment that is not the lowest row of the table); they do not touch the figures used here.

## Files in this folder
- `analysis/StatDesign_app_output_seed_yield.txt`
- `analysis/Yadeta2024_seed_yield_RCBD.sdsa`
- `analysis/Yadeta2024_verify_RCBD_output.txt`
- `data/Yadeta2024_seed_yield_5_treatments_5_blocks_reconstructed.csv`
- `data/source_supplement/Yadeta2024_Table5_published_means.csv`
- `scripts/Yadeta2024_verify_RCBD.R`

`scripts/` recomputes the analysis independently in base R (no packages); run it from inside `scripts/` with `Rscript`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
