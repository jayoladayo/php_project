# 05 GraecoLatin SCC Air Voids Lazniewska2026 Materials

**Paper.** Łaźniewska-Piekarczyk, B., Miera, P., & Moskal, M. (2026). Air-Void Stability in Self-Compacting Concrete: Linking Fresh-Air Retention with Hardened Pore Structure Through a Synthetic Dispersion Approach. Materials, 19(13), 2730. https://doi.org/10.3390/ma19132730

**Licence.** CC BY 4.0. Paper: `paper/PAPER_LINK.txt` and `Download_paper_PDF.url` (open access, CC BY 4.0; the PDF is large, so the link is given).

**Design.** 5x5 Graeco-Latin square: rows AEA dosage (0-0.20 %), columns binder type (5 cements), Latin letters w/b ratio (0.29-0.41), Greek letters paste ratio phi (1.1-1.5); 25 mixes, residual 8 df.

## What the paper did
Made 25 self-compacting concrete mixes in a Graeco-Latin square, measured fresh air at 5 and 15 min and the hardened air-void system (total air, spacing factor, micropores, specific surface), and tested the four mix factors by ANOVA for each response.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
Graeco-Latin Square module. Response = air loss dA or micropore content A300; Treatment = Water-to-binder ratio w/b; Row = AEA dosage; Column = Binder type; Greek = Paste-to-aggregate void ratio phi.

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`) and pinned to the report. To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_9_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / Python) | Tally |
|---|---|---|---|
| dA: SS AEA / binder / w/b / phi / residual (Table 10) | 0.0416 / 0.0483 / 0.2431 / 0.0541 / 0.1160 | 0.0416 / 0.0483 / 0.2431 / 0.0541 / 0.1160 | Yes |
| dA: F and p for w/b | 4.190, p = 0.040 | 4.1905, p = 0.0404 | Yes |
| A300: SS w/b / residual and F, p (Table 13) | 6.012 / 2.963; 4.058, p = 0.044 | 6.0117 / 2.9626; 4.0584, p = 0.0437 | Yes |
| A5, A, L, alpha: all SS (Tables 9, 11, 12, 14) | as printed | identical (see verification output) | Yes |

## Insights and possible additions
- All six ANOVAs replicate exactly in the app and in Python - the cleanest result in this pack.
- Only w/b reaches significance, for early air loss and micropore content; AEA dosage, surprisingly, never does, which the authors explain by binder-dependent air entrainment.
- The app flags non-normal residuals for dA (Shapiro-Wilk p = 0.0001); the authors also say so. A rank-based check (Quade's test, offered by the app) would strengthen the claim.
- A Graeco-Latin square assumes no interactions among the four factors; binder x AEA interaction is chemically plausible and cannot be tested here - worth stating as a limitation.

## Files in this folder
- `analysis/Lazniewska2026_SCC_air_loss_graeco_latin.sdsa`
- `analysis/Lazniewska2026_SCC_micropores_graeco_latin.sdsa`
- `analysis/Lazniewska2026_verify_graeco_latin_output.txt`
- `analysis/StatDesign_app_output_SCC_air_loss.txt`
- `analysis/StatDesign_app_output_SCC_micropore_content.txt`
- `data/Lazniewska2026_SCC_5x5_graeco_latin_square_air_voids.csv`
- `paper/Download_paper_PDF.url`
- `paper/PAPER_LINK.txt`
- `scripts/Lazniewska2026_verify_graeco_latin.py`
- `scripts/doe_anova.py`

`scripts/` recomputes everything independently with Python (numpy, pandas, scipy); run it from inside `scripts/`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
