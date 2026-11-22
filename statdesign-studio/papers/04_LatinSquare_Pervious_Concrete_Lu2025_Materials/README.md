# 04 LatinSquare Pervious Concrete Lu2025 Materials

**Paper.** Lu, F., Yang, L., & Jiang, Y. (2025). Optimum Mix Design and Correlation Analysis of Pervious Concrete. Materials, 18(17), 4129. https://doi.org/10.3390/ma18174129

**Licence.** CC BY 4.0. Paper: `paper/PAPER_LINK.txt` and `Download_paper_PDF.url` (open access, CC BY 4.0; the PDF is large, so the link is given).

**Design.** L9(3^3) orthogonal array + empty column = 3x3 Latin square: rows sand rate (4, 6, 8 %), columns VMA dosage (0.015, 0.01, 0.005 %), Latin letters target porosity (14, 20, 26 %); error = empty column (2 df).

## What the paper did
Designed pervious concrete mixes for the trade-off between permeability and strength, using single-factor tests and an L9 orthogonal test analysed by range analysis and ANOVA for 7- and 28-day compressive strength, splitting tensile strength and permeability.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
Latin Square module. Response = each property; Treatment = Target porosity (%); Row = Sand rate (%); Column = VMA dosage (% of cement).

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`) and pinned to the report. To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_9_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / Python) | Tally |
|---|---|---|---|
| Permeability: SS sand / VMA / porosity / error | 1.98 / 0.58 / 40.64 / 0.47 | 1.98 / 0.58 / 40.64 / 0.47 | Yes |
| Permeability: F porosity | 86.66 | 86.68 | Yes |
| 7-day strength: SS / F porosity | 257.83 / 17.93 | 259.81 / 17.62 | Close (rounding) |
| 28-day strength: error SS | 15.40 | 46.35 | No - paper reused the VMA SS |
| 28-day strength: F sand / porosity | 13.42 / 17.91 (significant) | 4.19 / 5.60 (p = 0.19 / 0.15) | No - conclusion reverses |
| Splitting tensile: error SS / F porosity | 0 / 303.25 (all significant) | 0.359 / 0.89 (p = 0.53) | No - conclusion reverses |

## Insights and possible additions
- Permeability and 7-day strength replicate. The 28-day strength and splitting-tensile ANOVAs do not: the paper's error SS for 28-day strength (15.40) is a copy of the VMA row, and for tensile strength it reports an error of 0 even though the empty column has the largest range in the paper's own range table (0.43).
- With the correct error no factor is significant for 28-day or tensile strength at alpha = 0.05; the paper's 'significant' sand-rate effect at 28 days is not supported by its data.
- A 3x3 Latin square with 2 error df is a very weak design for strength (test-to-test scatter of concrete is large); replicated specimens per mix would have allowed a proper test.
- Good teaching example: the app shows the error SS automatically, so a reader can spot an impossible 'error = 0' table immediately.

## Files in this folder
- `analysis/Lu2025_pervious_concrete_28d_strength_latin_square.sdsa`
- `analysis/Lu2025_pervious_concrete_permeability_latin_square.sdsa`
- `analysis/Lu2025_pervious_concrete_tensile_strength_latin_square.sdsa`
- `analysis/Lu2025_verify_latin_square_output.txt`
- `analysis/StatDesign_app_output_pervious_concrete_28d_strength.txt`
- `analysis/StatDesign_app_output_pervious_concrete_permeability.txt`
- `analysis/StatDesign_app_output_pervious_concrete_tensile_strength.txt`
- `data/Lu2025_pervious_concrete_L9_latin_square.csv`
- `paper/Download_paper_PDF.url`
- `paper/PAPER_LINK.txt`
- `scripts/Lu2025_verify_latin_square.py`
- `scripts/doe_anova.py`

`scripts/` recomputes everything independently with Python (numpy, pandas, scipy); run it from inside `scripts/`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
