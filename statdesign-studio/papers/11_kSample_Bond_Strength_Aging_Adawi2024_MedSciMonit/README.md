# 11 k samples: bond strength and artificial aging, Adawi 2024, Medical Science Monitor

**Paper.** Adawi, et al. (2024). Effects of Artificial Aging of Direct Resin Nano-Hybrid Composite on Mean Bond Strength Values for Veneer Ceramic Samples. Medical Science Monitor, 30, e945243. https://doi.org/10.12659/MSM.945243

**Licence.** Not recorded in the confirmation pack (see the journal page). No PDF is included here; the DOI button opens the paper.

**Design.** k samples (one-way, completely randomized): shear bond strength (MPa) in 5 groups of 16 specimens: a control (TC) and four thermocycling-aging periods of 1, 3, 6 and 12 months. The raw values are not published, so the data are rebuilt from the printed means and SDs.

## What the paper did
Compared the shear bond strength of a veneer ceramic bonded to a direct resin nano-hybrid composite after different periods of thermocycling aging, and tested the five groups by ANOVA. Its Table 2 gives each group's mean and SD (TC 20.97 ± 1.40, T1 20.35 ± 1.26, T3 19.11 ± 1.28, T6 18.90 ± 1.73, T12 18.27 ± 1.44); the same Table 2 prints F = 13.55 (df 4) and P = 0.00001. The methods and the abstract say 90 specimens in 5 groups of n = 18, while Table 2 gives N = 16 per group. Statistics: one-way ANOVA, then Tukey HSD (Table 3), in SPSS 22.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
k samples page (CRD ANOVA). Response = shear bond strength (MPa), Group = aging group. Because only means and SDs are published, 16 values per group were generated with exactly the printed mean and SD (a reconstruction, not the specimens' measurements); the F of a one-way ANOVA depends only on those summary statistics, so it is determined by Table 2.

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`). To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_15_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / R) | Tally |
|---|---|---|---|
| F statistic printed in Table 2 | F = 13.55, df = 4, p = 0.00001 | F = 9.571, df = 4 and 75, p = 2.6e-06 | No (it does not follow from Table 2's own means, SDs and N) |
| F recomputed from Table 2 (means, SDs, N = 16) | - | 9.5711 by hand, in R and in the app | Yes (three routes agree) |
| Tukey p-values (Table 3) | 0.591 / 0.040 / 0.011 / 0.989 / 0.302 / 0.578 (TC-T1, T1-T3, T1-T6, T3-T6, T3-T12, T6-T12) | With Table 2's SDs: 0.737 / 0.114 / 0.042 / 0.994 / 0.465 / 0.726; with the error variance implied by F = 13.55: 0.593 / 0.037 / 0.009 / 0.988 / 0.289 / 0.578 | The paper's Tukey and its F agree with each other, not with Table 2's SDs |
| Conclusion | Highly significant differences between the groups | Reject H0 (eta squared 0.338) | Yes |

## Insights and possible additions
- The F = 13.55 printed in Table 2 cannot be reproduced from Table 2's own means, SDs and N = 16: all of them give F = 9.57. Both are highly significant, so the finding stands, but the table is inconsistent with itself.
- Table 3 (Tukey) sides with the F, not with the SDs: its p-values are reproduced by an error variance of 1.45 (pooled SD 1.20), which is exactly what F = 13.55 implies, whereas the printed SDs give 2.05 (pooled SD 1.43). With the printed SDs the T1 versus T3 comparison would not be significant (p = 0.114, the paper has 0.040). So the SDs in Table 2 (or the group size) are the likeliest source of the inconsistency, but the raw data are not published, so it cannot be settled.
- Group size: the methods and the abstract give n = 18 per group (90 specimens), Table 2 gives N = 16. With n = 18 and the printed SDs the F would be 10.77, still not 13.55; the printed means and SDs would need about 22.6 specimens per group to give 13.55.
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
