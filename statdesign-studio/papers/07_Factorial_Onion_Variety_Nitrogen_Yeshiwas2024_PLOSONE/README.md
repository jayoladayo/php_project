# 07 Factorial Onion Variety Nitrogen Yeshiwas2024 PLOSONE

**Paper.** Yeshiwas, Y., Alemayehu, M., & Adgo, E. (2024). Enhancing bulb yield through nitrogen fertilization and the use of hybrid onion (Allium cepa L.) varieties in northwest Ethiopia. PLOS ONE, 19(10), e0312394. https://doi.org/10.1371/journal.pone.0312394

**Licence.** CC BY 4.0. Paper: `paper/Yeshiwas2024_PLOSONE_onion_nitrogen_hybrid_varieties.pdf` (open-access PDF, CC BY 4.0).

**Design.** 4 x 4 factorial (variety x nitrogen rate) in an RCBD with 3 replications at 3 locations; 144 plots.

## What the paper did
Compared three hybrid onion varieties with Bombay Red at four nitrogen rates at three irrigated sites, reporting pooled means with LSD and CV for growth, yield and quality traits, and a partial budget analysis.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
Factorial module with the Row/Column layout. Response = Marketable bulb yield (t/ha); Factor A = Variety; Factor B = Nitrogen rate (kg/ha); Row = Location; Column = Replication (block). This removes location and replication as additive blocks, the model that reproduces the paper's CV and LSD.

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`) and pinned to the report. To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_9_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / Python) | Tally |
|---|---|---|---|
| Variety means: Russet / Red Coach / Jambar / Bombay Red (MY) | 26.50 / 21.84 / 24.57 / 19.86 | 26.50 / 21.85 / 24.57 / 19.86 | Yes |
| N means: 0 / 41 / 82 / 123 (MY) | 19.09 / 22.39 / 26.77 / 24.51 | 19.10 / 22.40 / 26.78 / 24.51 | Yes |
| CV and LSD 0.05 (MY) | 20.76 % / 2.25 | 20.77 % / 2.25 (app MSE 23.20) | Yes |
| Total yield CV / LSD | 20.22 % / 2.24 | 20.27 % / 2.24 | Yes |
| Bulb diameter, plant height, leaf number means | as printed | differ by 1-4 % | No (minor) |

## Insights and possible additions
- The paper prints no ANOVA table; reverse-engineering its CV and LSD shows it fitted Location + Rep + Variety x N, pooling every location interaction into error.
- That pooling hides real interactions: in the full combined model Location x Variety (p = 0.010) and Location x N (p = 0.006) are significant, so the 'best N rate' is site-specific and the pooled LSD overstates precision for some comparisons.
- Variety x N is clearly not significant (app: F = 0.18, p = 0.996), so the paper is right to report main-effect means.
- Means for bulb diameter, plant height and leaf number in the paper do not exactly match the supplementary file (yield and maturity traits do).

## Files in this folder
- `analysis/StatDesign_app_output_onion_marketable_yield.txt`
- `analysis/Yeshiwas2024_onion_variety_nitrogen_factorial.sdsa`
- `analysis/Yeshiwas2024_verify_factorial_output.txt`
- `data/Yeshiwas2024_onion_4x4_factorial_RCBD_3_locations.csv`
- `data/source_supplement/PLOSONE_0312394_S2_Dataset.xlsx`
- `paper/Yeshiwas2024_PLOSONE_onion_nitrogen_hybrid_varieties.pdf`
- `scripts/Yeshiwas2024_verify_factorial.py`
- `scripts/doe_anova.py`

`scripts/` recomputes everything independently with Python (numpy, pandas, scipy); run it from inside `scripts/`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
