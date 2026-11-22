# 09 2k ALD Al2O3 Growth Pan2025 Materials

**Paper.** Pan, D., & Lei, Y. (2025). Investigation and Optimization of Process Parameters on Growth Rate in Al2O3 Atomic Layer Deposition (ALD) Using Statistical Approach. Materials, 18(9), 1918. https://doi.org/10.3390/ma18091918

**Licence.** CC BY 4.0. Paper: `paper/PAPER_LINK.txt` and `Download_paper_PDF.url` (open access, CC BY 4.0; the PDF is large, so the link is given).

**Design.** 2^4 full factorial with 2 replicates (32 runs): deposition temperature, argon flow, precursor pulse time, purge time; response growth per cycle (GPC).

## What the paper did
Studied how four ALD process settings affect the growth per cycle of Al2O3 films, fitted main effects plus two-factor interactions in Minitab, and concluded that only deposition temperature (plus two interactions) matters.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
2^k Factorial module. Response = Growth per cycle GPC (Angstrom/cycle); Factors A-D = temperature, argon flow, pulse time, purge time. With 2 replicates the app fits the full model against pure error (16 df).

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`) and pinned to the report. To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_9_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / Python) | Tally |
|---|---|---|---|
| SS temperature (A) | 0.276187 | 0.273800 | Yes (GPC rounded to 2 dp) |
| SS AD / CD | 0.050763 / 0.037082 | 0.051200 / 0.035113 | Yes |
| Paper's error (3-, 4-way + pure error), 21 df | 0.125998 | 0.125337 | Yes |
| F temperature in paper's model | 46.03 | 45.87 | Yes |
| Pure error SS (16 df) | 0.004706 | 0.004500 | Yes |
| Full model against pure error | not reported | all 15 effects significant (app) | New result |

## Insights and possible additions
- The paper's numbers replicate (small differences come from GPC printed to 2 decimals).
- The paper's own lack-of-fit test (F = 82.5, p < 0.001) shows its reduced model is wrong: the pooled 3- and 4-factor interactions are large (BCD alone has SS 0.045). Fitted correctly against pure error, every effect is significant, so 'only temperature is significant' does not hold.
- The app makes this visible at once: with replicated runs it uses pure error by default and lists every interaction.
- Residuals are non-normal (Shapiro-Wilk p = 0.0002), driven by a few replicate pairs that disagree (e.g. run 'c': 1.08 vs 1.00); a check of those runs would be the first step before any optimisation.

## Files in this folder
- `analysis/Pan2025_ALD_Al2O3_2to4_factorial.sdsa`
- `analysis/Pan2025_verify_2k_output.txt`
- `analysis/StatDesign_app_output_ALD_growth_per_cycle.txt`
- `data/Pan2025_ALD_Al2O3_2to4_factorial_2_replicates_GPC.csv`
- `paper/Download_paper_PDF.url`
- `paper/PAPER_LINK.txt`
- `scripts/Pan2025_verify_2k.py`
- `scripts/doe_anova.py`

`scripts/` recomputes everything independently with Python (numpy, pandas, scipy); run it from inside `scripts/`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
