# 08 2k Laser Kerf PTFE PCB Pellei2026 Micromachines

**Paper.** Pellei, G., Di Stefano, P., Mascalchi, L., & Centi, R. (2026). CO2 Laser Micromachining of PTFE-Based PCBs: Predictive Modeling of Kerf Depth Through Design of Experiments. Micromachines, 17(4), 404. https://doi.org/10.3390/mi17040404

**Licence.** CC BY 4.0. Paper: `paper/Pellei2026_Micromachines_CO2_laser_kerf_PTFE_PCB.pdf` (open-access PDF, CC BY 4.0).

**Design.** Two unreplicated 2^3 designs (one per laminate): aperture A (127, 254 um), pulse duration B (5, 9 us), loops C (10/20 or 10/15); response log10 kerf depth; interactions pooled (4 df).

## What the paper did
Drilled micro-vias in two PTFE-based PCB laminates with a CO2 laser, measured kerf depth, modelled log10 depth with a first-order model in aperture, pulse duration and loops, and validated it for process prediction.

## How it was replicated in StatDesign Studio (https://boxwish.shinyapps.io/statdesign-studio/)
2^k Factorial module. Response = Log10 of kerf depth; Factor A = Aperture A (um); Factor B = Pulse duration B (us); Factor C = Number of loops C. The app pools AB, AC, BC, ABC as error automatically for an unreplicated design.

Each analysis was saved with *Share analysis* (`analysis/*.sdsa`) and pinned to the report. To reopen one, drag the `.sdsa` file onto the app (or use Open analysis). The project file `StatDesign_9_papers_all_datasets.sdproj` in the pack root reloads every dataset.

## Do the values tally?
| Quantity | Paper | Ours (app / Python) | Tally |
|---|---|---|---|
| RO3003: SS A / B / C (Table 4) | 0.0271 / 0.2999 / 0.1726 | 0.02707 / 0.29985 / 0.17256 | Yes |
| RO3003: % contribution A / B / C | 5.42 / 60.02 / 34.55 | 5.42 / 60.03 / 34.55 | Yes |
| Duroid 5880: SS A / B / C (Table 7) | 0.0262 / 0.3032 / 0.0573 | 0.02617 / 0.30319 / 0.05727 | Yes |
| Duroid 5880: % contribution | 6.77 / 78.41 / 14.81 | 6.77 / 78.41 / 14.81 | Yes |
| F-values | 3827 / 42390 / 24400 (RO3003) | 3913 / 43345 / 24943 | Close (~2 %, rounded depths) |

## Insights and possible additions
- Both designs replicate; small F differences arise only because the paper analysed unrounded depths.
- Copy errors in the paper: both ANOVA tables show the same 'Total MS = 0.3855', and the RO3003 residual row carries a p-value (0.2679) that belongs to lack of fit.
- Interactions are truly negligible on the log scale (all below 0.01 % of SS), which justifies pooling - the app shows each interaction SS so this can be checked rather than assumed.
- Addition: the RO3003 design had a centre point; adding it would allow a curvature test, and a half-normal plot of effects is a good visual check for unreplicated 2^k designs.

## Files in this folder
- `analysis/Pellei2026_laser_kerf_Duroid5880_2cubed.sdsa`
- `analysis/Pellei2026_laser_kerf_RO3003_2cubed.sdsa`
- `analysis/Pellei2026_verify_2k_output.txt`
- `analysis/StatDesign_app_output_laser_kerf_Duroid5880.txt`
- `analysis/StatDesign_app_output_laser_kerf_RO3003.txt`
- `data/Pellei2026_laser_kerf_2cubed_RO3003.csv`
- `data/Pellei2026_laser_kerf_2cubed_RT_Duroid_5880.csv`
- `paper/Pellei2026_Micromachines_CO2_laser_kerf_PTFE_PCB.pdf`
- `scripts/Pellei2026_verify_2k.py`
- `scripts/doe_anova.py`

`scripts/` recomputes everything independently with Python (numpy, pandas, scipy); run it from inside `scripts/`. Its printed output is saved in `analysis/*_output.txt`; the app's own ANOVA and assumption-check text is in `analysis/StatDesign_app_output_*.txt`.
