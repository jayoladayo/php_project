# 10 One-sample t-test: groundwater pH, Uddin 2023, Heliyon

**Paper.** Uddin, et al. (2023). Assessment of hydrogeochemistry in groundwater using water quality index model and indices approaches. Heliyon, 9(9), e19668. https://doi.org/10.1016/j.heliyon.2023.e19668

**Licence.** Not recorded in the confirmation pack (see the journal page). No PDF is included here; the DOI button opens the paper.

**Design.** One sample: 18 groundwater pH values (9 sites, GW1 to GW9, each in the dry and the wet season) compared with the 8.5 upper guideline limit for pH (WHO and Bangladesh standards).

## What the paper did
Assessed the hydrogeochemistry and the drinking-water quality of groundwater at nine sites in two seasons, and tested the pH against the guideline limit with a one-sample t-test (t = -9.52, df = 17, p < 0.05).

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
One sample page. Response = groundwater pH (the Site and Season columns are not used: the 18 values are treated as one sample, as in the paper's df = 17); H0: mean = 8.5, two-sided, alpha = 0.05, one-sample t-test.

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`). To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_15_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / R) | Tally |
|---|---|---|---|
| t statistic and df | t = -9.52, df = 17 | t = -9.5167, df = 17 | Yes |
| p-value | p < 0.05 | p = 3.2e-08 (< 0.0001) | Yes |
| Decision | The mean pH is below the limit of 8.5 | Reject H0: mean 7.666, 95 % CI [7.481, 7.851] | Yes |

## Insights and possible additions
- The paper's t = -9.52 reproduces exactly from the raw pH values, in the app and in base R; it is also the strongest kind of check in this pack because the paper's own raw data are used.
- The 18 values are 9 sites measured twice, so they are not independent observations: the same site appears in both seasons. A paired or a mixed-model view (site as a block, season as a treatment) would use that structure; the one-sample test treats them as 18 independent draws, as the paper did.
- The test only says the mean pH is below 8.5. Whether any single site breaks the guideline cannot be read from a mean; the largest value is 8.27 (GW1, dry season), still below 8.5.
- Wet-season pH is more uniform (7.66 to 7.98) than dry-season pH (6.90 to 8.27): a season effect is visible and could be tested with the Two samples page.

## Files in this folder
- `analysis/StatDesign_app_output_groundwater_pH.txt`
- `analysis/Uddin2023_groundwater_pH_one_sample_t.sdsa`
- `analysis/Uddin2023_verify_one_sample_output.txt`
- `data/Uddin2023_groundwater_pH_9_sites_2_seasons.csv`
- `scripts/Uddin2023_verify_one_sample.R`

`scripts/` recomputes the analysis independently in base R (no packages); run it from inside `scripts/` with `Rscript`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
