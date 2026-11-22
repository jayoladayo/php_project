# 14 Latin square: bioassay of the testicular diffusing factor, Bacharach, Chance and Middleton 1940, Biochemical Journal

**Paper.** Bacharach, A. L., Chance, M. R. A., & Middleton, T. R. (1940). The biological assay of testicular diffusing factor. Biochemical Journal, 34, 1464-1471. https://doi.org/10.1042/bj0341464

**Licence.** Not recorded in the confirmation pack (see the journal page). No PDF is included here; the DOI button opens the paper.

**Design.** 6 x 6 Latin square, 36 observations: rows = injection position on the skin (a to f), columns = rabbit (1 to 6), Latin letters = order of injection (i to vi, the treatment); response = blister area (cm2). The paper's own raw data.

## What the paper did
Standardised a biological assay of the diffusing factor by measuring the area of the blister after injections into the skin of rabbits, laid out as a Latin square so that rabbits and positions are removed from the comparison of the injection orders. It prints the analysis of variance with F = 0.172 for order, 1.168 for positions and 3.910 for rabbits.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
Latin Square page (LSD ANOVA). Response = blister area (cm2), Treatment = order of injection, Row = position, Column = rabbit. All 36 raw values are the paper's.

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`). To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_15_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / R) | Tally |
|---|---|---|---|
| F for order of injection (treatment) | 0.172 | 0.1716 (p = 0.9701) | Yes |
| F for positions (rows) | 1.168 | 1.1678 (p = 0.3592) | Yes |
| F for rabbits (columns) | 3.910 | 3.9096 (p = 0.0124) | Yes |

## Insights and possible additions
- All three F ratios reproduce exactly from the paper's complete raw data, in the app and in base R: the strongest confirmation in this set, and a paper from 1940.
- Order of injection has no effect (p = 0.97) and positions do not matter (p = 0.36), which is what a well-standardised assay should show; rabbits do differ (p = 0.012), which is the reason for using them as a blocking factor.
- Error SS = 13.13 on 20 df: with 5 df per factor and 20 error df, the Latin square is a small, efficient design (36 runs for three factors).
- The Latin square assumes no interactions; with one observation per cell nothing can be tested about them.

## Files in this folder
- `analysis/Bacharach1940_blister_area_latin_square.sdsa`
- `analysis/Bacharach1940_verify_latin_square_output.txt`
- `analysis/StatDesign_app_output_blister_area.txt`
- `data/Bacharach1940_blister_area_6x6_latin_square.csv`
- `scripts/Bacharach1940_verify_latin_square.R`

`scripts/` recomputes the analysis independently in base R (no packages); run it from inside `scripts/` with `Rscript`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
