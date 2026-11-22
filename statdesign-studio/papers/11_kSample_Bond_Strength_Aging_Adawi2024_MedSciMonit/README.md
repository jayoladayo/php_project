# 11 k samples: bond strength and artificial aging, Adawi 2024, Medical Science Monitor

**Paper.** Adawi, et al. (2024). Effects of Artificial Aging of Direct Resin Nano-Hybrid Composite on Mean Bond Strength Values for Veneer Ceramic Samples. Medical Science Monitor, 30, e945243. https://doi.org/10.12659/MSM.945243

**Licence.** Not recorded in the confirmation pack (see the journal page). No PDF is included here; the DOI button opens the paper.

**Design.** k samples (one-way, completely randomized): shear bond strength (MPa) in 5 groups of 16 specimens: a control (TC) and four thermocycling-aging periods of 1, 3, 6 and 12 months. The raw values are not published, so the data are rebuilt from the printed means and SDs.

## What the paper did
Compared the shear bond strength of a veneer ceramic bonded to a direct resin nano-hybrid composite after different periods of thermocycling aging, and tested the five groups by ANOVA. Its Table 2 gives each group's mean and SD (TC 20.97 ± 1.40, T1 20.35 ± 1.26, T3 19.11 ± 1.28, T6 18.90 ± 1.73, T12 18.27 ± 1.44); the text states F = 13.55, df = 4, p = 0.00001.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
k samples page (CRD ANOVA). Response = shear bond strength (MPa), Group = aging group. Because only means and SDs are published, 16 values per group were generated with exactly the printed mean and SD (a reconstruction, not the specimens' measurements); the F of a one-way ANOVA depends only on those summary statistics, so it is determined by Table 2.

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`). To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_15_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / R) | Tally |
|---|---|---|---|
| F statistic as printed in the text | F = 13.55, df = 4, p = 0.00001 | F = 9.571, df = 4 and 75, p = 2.6e-06 | No (it does not follow from the paper's own Table 2) |
| F recomputed from Table 2 (means, SDs, n = 16) | - | 9.5711 by hand, in R and in the app | Yes (three routes agree) |
| Conclusion | Highly significant differences between the groups | Reject H0 (eta squared 0.338) | Yes |

## Insights and possible additions
- The printed F = 13.55 cannot be reproduced from the paper's own means, SDs and group sizes: all of them give F = 9.57. Both values are highly significant, so the finding stands, but the statistic in the text is inconsistent with the table.
- The residual checks of the app (normality, constant variance, independence) are not informative here: the spread inside each group is built, not measured, so every group has exactly the printed SD.
- The means fall steadily with ageing time (20.97 at TC to 18.27 at 12 months). Post-hoc comparisons on the rebuilt data rest on the same summary statistics, so they show which periods differ only as far as the printed means and SDs allow.
- With a reconstruction only the F test and the group means are checked, not the paper's raw data; the Data step lists the rebuilt file and the printed Table 2 separately.

## Files in this folder
- `analysis/Adawi2024_bond_strength_k_samples.sdsa`
- `analysis/Adawi2024_verify_k_samples_output.txt`
- `analysis/StatDesign_app_output_bond_strength.txt`
- `data/Adawi2024_bond_strength_5_aging_groups_reconstructed.csv`
- `data/source_supplement/Adawi2024_Table2_published_means_SD.csv`
- `scripts/Adawi2024_verify_k_samples.R`

`scripts/` recomputes the analysis independently in base R (no packages); run it from inside `scripts/` with `Rscript`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
