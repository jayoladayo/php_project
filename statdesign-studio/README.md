# StatDesign Studio

A no-code R Shiny app for the KFUPM Experimental Design course.

## Run it on your computer

1. Install R (4.2 or newer) and RStudio.
2. In the R console, install the packages once:

   ```r
   install.packages(c("shiny", "bslib", "readxl", "DT", "rhandsontable",
                      "ggplot2", "car", "lmtest", "nortest", "emmeans",
                      "multcomp", "writexl", "jsonlite", "rsconnect", "coin",
                      "officer", "flextable", "gridExtra", "png", "base64enc",
                      "curl", "promises", "later", "commonmark"))
   ```

3. Open `app.R` in RStudio and click **Run App**, or run `shiny::runApp("path/to/statdesign-studio")`.

`sample-data/fertilizer_trial.xlsx` is a test workbook with three sheets. Season 2 has a "49 kg" cell so you can see the data health fix.

## What is in stage 1

> Stages 1 to 3 describe the first version. Since stage 9 the analysis pages follow the five-step process described under "What is new in stage 9"; the result tabs and the R code tab below were replaced.

- App shell in KFUPM colours, light and dark mode, page search, save and open project.
- Data Studio: Excel and CSV upload with a sheet picker, header and skip-row options, missing value codes, automatic delimiter and decimal detection; typing and editing in a grid; pasting from Excel; class examples; data health checks with one-click fixes; column types.
- CRD, RCBD, Latin Square and Graeco-Latin Square: choose data, map columns to roles (numeric codes become categories), design check with layout preview, context and display names, level labels with colours and order, Dunnett control, α, confidence level, post-hoc choices, assumption handling, hypotheses.

## What is in stage 2

Press **Run analysis** on any of CRD, RCBD, Latin square or Graeco-Latin square to get seven result tabs:

- **Summary**: plain-English conclusion, key numbers, means chart with grouping letters, assumption scorecard, the decision made three ways, and a paragraph you can copy.
- **Explore**: box plots, a summary table, and a block profile (RCBD) or the square as laid out (LSD/GLSD).
- **ANOVA**: full table with F critical and partial eta squared, fit statistics, relative efficiency, and the p-value, rejection-region and confidence-interval methods.
- **Assumptions**: Shapiro-Wilk and Anderson-Darling (normality), Breusch-Pagan (constant variance of the residuals), Durbin-Watson and Breusch-Godfrey (independence), with QQ, residual and run-order plots. The CRD page also shows Levene and Bartlett as a separate check of the equal variances of the groups.
- **Post-hoc**: Tukey, Fisher LSD, Bonferroni, Scheffé and Dunnett with intervals, letters and a side-by-side comparison.
- **Non-parametric**: Kruskal-Wallis (CRD), Friedman and Quade (RCBD), or Quade's test (LSD/GLSD, followed by Dunn's test with Holm adjustment or the Wilcoxon rank-sum test with Bonferroni adjustment), with post-hoc tests. In automatic mode it becomes the recommended result when normality or equal variance fails.
- **R code**: a script that reproduces every number, ready to run in R.

Every chart downloads as PNG and every table downloads together as one Excel file.

## What is in stage 3

- **Test Advisor**: answer up to five questions (goal, number of groups, pairing, grouping, response type) and get the recommended analysis with the reasons, what it will run, and alternatives. One click opens that page with the right settings.
- **Descriptive statistics**: n, missing, mean, median, mode, min, max, range, quartiles, IQR, any percentile, variance, SD, standard error, CV, skewness, kurtosis, Shapiro-Wilk and outliers; histogram with normal curve, box plot (by group), QQ plot, stem-and-leaf; scatter plot with covariance and Pearson, Spearman and Kendall correlations; correlation heat map.
- **One sample**: t-test, z-test (when σ is known), Wilcoxon signed-rank and sign test.
- **Two samples**: independent (F test and Levene for variances, pooled and Welch t-tests, z-test, Mann-Whitney U) or paired (paired t-test, Wilcoxon signed-rank, sign test). Data can be stacked (response + group) or one column per sample; paired data can be matched by a Pair ID column.
- **k samples**: one-way ANOVA, Welch ANOVA and Kruskal-Wallis, with Levene and Bartlett tests and Tukey, Games-Howell and Dunn post-hoc comparisons.

In automatic mode each page recommends the test whose assumptions hold and explains why. Every page shows the R code, which follows the class script "One, Two & K Samples Analysis". One fix: the class script writes `wilcox.test(..., m = 25)`; R ignores `m`, so that line tests against 0. The generated code uses `mu =`.

## What is in stage 4

- **Design Planner**: choose CRD, RCBD, Latin or Graeco-Latin, name the treatments (and the Greek factor, rows and columns), set a seed and randomise. You get a colour-coded field layout with a validity check, the error df, a power estimate from a difference and SD you enter, a field map PDF and an Excel data sheet with an empty response column to fill in.
- **Report**: every results page has a **Pin to report** button. Pinned tables, charts and interpretations are snapshots, so rerunning an analysis does not change them. Reorder them by dragging, add notes and headings, pick a template (Lab report, Thesis, Brief) and download Word, PDF, HTML or Excel. The appendix holds each dataset's notes and the R code.
- **Learn**: eleven short guides (descriptive statistics, one, two and k sample tests, CRD, RCBD, Latin, Graeco-Latin, assumption checks, post-hoc tests, three ways to decide), each with when to use it, the model, the assumptions and their fallbacks, a worked class example, and a button that opens that example in the app.

## What is in stage 6

**Problem Sets** (sidebar, first item under Design of experiments): 30 exercises from the course problem sheets, each with its full statement and complete dataset, solved with the step-by-step hypothesis-testing process taught in the course.

- **Grouped by chapter** (tabs Chapter 2, Chapter 3, Custom Problem) and then in collapsible categories:
  - Chapter 2: **One sample**, **Two sample** and **K sample**, each split into **Parametric** and **Non-parametric** (two-sample problems also say independent or paired). Chapter 2 has no k-sample problem; that group points to Chapter 3.
  - Chapter 3 (all completely randomized designs): **Parametric** and **Non-parametric**.
  - A Chapter 2 problem is parametric or non-parametric by the rule step 4 uses: the population is stated to be normal, or the Shapiro-Wilk test finds no evidence against normality, or every sample has n ≥ 30 (2.32 fails normality and is non-parametric). A Chapter 3 problem is non-parametric when an assumption check after the ANOVA fails (for example 3.10, 3.32), so it ends with Kruskal-Wallis.
- **One- and two-sample problems (Chapter 2): 6 steps**, each a tab with Back and Next buttons:
  1. **Problem & data**: the question and the data as printed.
  2. **Hypotheses**: H0 and H1 in words, suggested from the question (type of alternative and hypothesised value included) and editable; μ for parametric tests, η (median) for non-parametric tests.
  3. **Significance level**: α from the question when it states one (else 0.05), editable; the confidence level is 1 − α.
  4. **Information available**: what is known (response, number and size of samples, independent or paired, σ known or not, stated normality) and the data again. One sample: the normality check (Shapiro-Wilk, QQ plot). Two samples: the independence check first (Breusch-Godfrey), then normality and, for independent samples with unknown σ1 and σ2, equal variances (when both σ are known the z-test uses them, so no equal-variance test is run). The result is the parametric or non-parametric choice with the suggested test; the test type, a known σ and the equal-variance assumption are editable.
  5. **Test**: the suggested test (one-sample t or z, χ² on a variance, pooled or Welch t, two-sample z, paired t, or the rank tests: Wilcoxon signed-rank, sign test, Mann-Whitney), changeable, with a **Run test** button, the statistic worked out with the numbers and the result table.
  6. **Decision**: reject or fail to reject H0, by the rejection region, the confidence interval and the p-value, each with its plot, and the conclusion in the words of H1. This is the last step.
- **Single-factor problems (Chapter 3): the parametric process runs to the end first, then starts again for the non-parametric test only if an assumption fails.**
  - *Phase 1, parametric (one-way ANOVA):*
    1. **Problem & data**.
    2. **Hypotheses**: about the population means μ only.
    3. **Significance level**.
    4. **Information available**: the facts and the data again, and the **equal variances of the groups** (Levene or Bartlett, the student's choice): equal → one-way ANOVA, unequal → Welch ANOVA. The other assumptions are checked after the test, and the page shows the workflow.
    5. **Test**: the CRD ANOVA (or Welch ANOVA), with the ANOVA table.
    6. **Decision**: by the rejection region, the Tukey (or Games-Howell) intervals and the p-value.
    7. **Post-hoc** (only when H0 is rejected): Tukey HSD, Fisher LSD, Bonferroni or Scheffé (Games-Howell after Welch ANOVA), with grouping letters and an interval plot. Dunnett's test is offered, and suggested, only when the problem names a control group (3.10: Sham).
    8. **Assumption checks** (on the ANOVA residuals): normality (Shapiro-Wilk, Anderson-Darling, QQ plot), independence (Breusch-Godfrey, residuals in order) and constant variance of the residuals (Breusch-Pagan, residuals versus fitted; not required after Welch ANOVA). If all pass, the analysis ends with the ANOVA conclusion.
  - *Phase 2, non-parametric (Kruskal-Wallis), only when an assumption fails.* The page names the failed assumption(s) and the process starts again from step 2:
    2. **Hypotheses**: now about the population medians η, suggested and editable.
    3. **Significance level**: the same α as phase 1.
    4. **Information available**: the facts, a summary of the assumption checks, the data, and why the Kruskal-Wallis test is used.
    5. **Kruskal-Wallis test**: the H statistic with the tie correction and a rank table.
    6. **Decision**: by the rejection region and the p-value.
    7. **Post-hoc** (only when H0 is rejected): Dunn's test, and the final conclusion.
  - The stepper shows the phase in progress in full and folds the other one to its dots; phase 2 stays greyed out unless an assumption fails.
- **Custom Problem**: write or paste a statement, paste, type or upload the data (or start from a class example), stack wide tables, and say which column is the response, the group (treatment) and, for paired data or a block design, the block or pair ID; for a Latin square the row and column (and the Greek letter for a Graeco-Latin square). If the problem names a control group, choose it to enable Dunnett's test. Block designs and squares follow the Chapter 4 steps. **Copy to Custom Problem** copies any exercise into the editor.
- The page remembers each problem's step, hypotheses, α and test, so returning to the list loses nothing. **Pin to report** sends the hypotheses, test result, decision and post-hoc table to the Report builder.

The problem sheets supplied are Montgomery Chapter 2 (Simple Comparative Experiments), Chapter 3 (Experiments with a Single Factor) and Chapter 4 (Randomized Blocks, Latin Squares, and Related Designs). The tests are the course's own (the same engine as the One, Two and k sample pages).

## What is in stage 7

- **Categories and filters in Problem Sets.** The chapter tabs stay (Chapter 2, Chapter 3, Chapter 4, Custom Problem). Inside a chapter, filter by **problem type** (one sample, two sample independent or paired, k sample, RCBD, Latin square, Graeco-Latin square, BIBD), **test type** (parametric or non-parametric) and **post-hoc** (Tukey HSD, Fisher LSD, Dunnett, Games-Howell, Dunn, Nemenyi, none), and group the problems by any of these. Every card shows its problem type, test type, test and post-hoc as chips.
- **Chapter 4 (23 problems with their data; the two balanced-incomplete-block problems 4.45 and 4.47 were removed because the course does not cover BIBD)**: 4.8, 4.9, 4.10, 4.11, 4.12, 4.13, 4.14, 4.15, 4.16, 4.17, 4.18, 4.19, 4.23, 4.24, 4.26, 4.27, 4.28, 4.29, 4.31, 4.34, 4.40, 4.41 and 4.59 (the problems of Section 4.5 that have data and can be solved with the steps). Each follows the same steps as Chapter 3: the ANOVA for the design (RCBD, Latin square or Graeco-Latin square) first, post-hoc if H0 is rejected, then the assumption checks on the residuals. If one fails, the process starts again with the course's rank test: Friedman (then Nemenyi) for an RCBD, and for the squares **Quade's test** (remove the blocking factors, rank the residuals, Kruskal-Wallis statistic with the permutation p-value, then Dunn). Missing values (4.23, 4.24, 4.34) are estimated with the textbook formula and the exact analysis is shown; random blocks (4.29, 4.31) get the block variance component; 4.10, 4.11 and 4.15 show the treatment means against a scaled t distribution.
- **Constant variance of the residuals is the Breusch-Pagan test** in every ANOVA assumption check (Problem Sets, CRD, RCBD, Latin and Graeco-Latin pages, and the k samples page). Levene and Bartlett are kept only to compare the **variances of the groups**, which decides between the one-way ANOVA and the Welch ANOVA.
- **Dunnett's test** is suggested only when the problem names a control group.
- **Home page**: the KFUPM logo, the aim and objectives, and the project team. Clicking a name opens **About the team** (also in the sidebar), with every member's photo, department and role. To change a photo, replace the file in `www/team/` (or set `photo` in `R/mod_team.R`); a member without a photo shows initials.

## What is in stage 8

- **Chapter 5 in Problem Sets (34 problems with their data)**: the factorial exercises of Montgomery's Chapter 5 that come with data: 5.8 to 5.18 (including 5.10, 5.16 and 5.17, which reuse the data of 5.8 and 5.9), 5.20, 5.21, 5.23, 5.24, 5.26 to 5.36, 5.38 and the "replicates as blocks" re-analyses 5.39 to 5.45. Exercises that only give an ANOVA table or ask for a derivation (5.1 to 5.7, 5.19, 5.22, 5.25, 5.46 to 5.52) have no raw data and are not included.
- **The same five-step process, for every effect.** Following the lecturer's Topic 6 notes: a pair of hypotheses for each factor and each interaction (step 2); the factorial ANOVA with an F test for each effect, in a completely randomized layout or in blocks (step 5); a decision for each effect, chosen from a list, with the three methods and an effect plot (step 6); post-hoc comparisons for the significant effects (step 7): Tukey HSD, Fisher LSD, Bonferroni and Scheffé for the levels of a main effect, and **simple effects** (each factor compared within every level of the other) for a significant interaction; then the assumption checks on the residuals (step 8).
- **If an assumption fails**, the process starts again with the rank test of the design, for each effect: **Kruskal-Wallis** (completely randomized layout, with Dunn's test with Holm or Bonferroni adjustment), **Friedman** (blocks, with Nemenyi or Wilcoxon signed-rank with Bonferroni) and **Quade's test** (Latin and Graeco-Latin squares, with Dunn's test with Holm adjustment or the Wilcoxon rank-sum test with Bonferroni adjustment).
- **One observation per combination (5.20, 5.21)**: only the main effects are tested, and the interaction is checked with **Tukey's one-degree-of-freedom test for nonadditivity**.
- **Blocks (5.26 to 5.45)**: the block effect is tested; for the re-analyses that ask for it, the block variance component and the efficiency of blocking against a completely randomized design are shown.
- **Assumption checks for factorial data.** The observations are listed replicate by replicate (every combination once, then the next replicate), which is the natural run order for the independence check. With two replicates in a model that has a term for every combination, the two residuals of a cell are equal and opposite, so the usual Breusch-Pagan test would reject whatever the data; for factorial designs it is run on the fitted values.
- **Custom problems** can be factorial: choose Factor A, Factor B (and Factor C) in the editor.
- **Guided tour (Guided tour button, top of every page).** A box is drawn around each part of the screen, one at a time, with Back and Next (or the arrow keys; Esc closes). It offers a **tour of the page you are on**, a **hands-on walk-through** that does the steps for you (Problem 3.11 on Problem Sets; the class example on the one, two, k sample and design pages) and an **overview of the whole app**. It knows where you are: the stage of an analysis (Data, Map variables, Context & settings, Results) or the step of a Problem Sets problem, and on Problem Sets it explains exactly the step on screen. A first-time visitor sees a small invitation to take the tour.

## What is new in stage 9

- **One process everywhere.** The analysis pages (**One sample, Two samples, k samples, CRD, RCBD, Latin Square, Graeco-Latin Square** and the new **Factorial** page) now run the same five-step process as the Problem Sets on your own data: pick the dataset (from Data Studio, pasted, uploaded or a class example), say which column is which, and follow the steps. Designs with three or more treatments add the post-hoc step and the assumption checks, and restart with the rank test (steps 9 to 14) when an assumption fails. The old four-stage wizard and the generated R script were replaced; Data Studio, the Advisor, Learn and the Report work as before.
- **CRD naming.** Chapter 3 problems and problem 4.59 are typed **CRD**, and the test is named "CRD ANOVA (F test)". Factorial layouts name their ANOVA after the layout (CRD, RCBD, LSD or GLSD ANOVA).
- **BIBD removed.** Problems 4.45 and 4.47 (balanced incomplete blocks) are gone because the course does not cover them; a layout with incomplete blocks gets a clear message instead of an analysis.
- **Quade's test** is the non-parametric test for every Latin and Graeco-Latin square (and factorial squares), with Dunn's test (Holm adjustment) or the Wilcoxon rank-sum test (Bonferroni adjustment) after it.
- **Step 5 test can be changed.** The test that fits the design is selected and marked ★; choose another (for example the rank test) and a note explains whether it suits the data: a warning when the ANOVA assumptions hold and the rank test is chosen, or when the group variances differ and the ordinary ANOVA is chosen.
- **Factorial information page** shows the main-effects plot, the interaction plots and boxplots by treatment combination.
- **Download solution (PDF)** at the end of every analysis (problems and analysis pages), offered only once the student has been through the steps: the statement, the data and every step with its numbers, tables and plots.
- **Edit data.** In Problem Sets press **Edit data** on step 1 to change a value, add or delete a row, then **Apply** (the steps restart on your values) or **Reset data**.
- **English and Arabic.** The button at the top switches the menus, instructions, step texts, results sentences, tables, chart labels, the guided tour and the Learn guides to Arabic (right-to-left layout and Arabic statistical terms). The PDF solutions and the Word, PDF and HTML reports follow the language chosen. The statements of the textbook problems, the data, your own names and the formulas stay as written. The dictionary is in `R/i18n*.R` (one file per area) and the switch in `www/i18n.js`.

## What is new in stage 11

- **Chapter 6 in Problem Sets: the 2^k factorial design (28 problems with their data).** All the exercises of Montgomery's Chapter 6 that come with data in the problem sheet: 6.5, 6.9, 6.11 to 6.14, 6.16, 6.19, 6.24 to 6.26, 6.28, 6.30, 6.32, 6.34, 6.35 (molecular weight and viscosity), 6.39, 6.40, 6.42 to 6.44, 6.46 (the four responses, as 6.46a, c, e and g), 6.51 and 6.52. They follow the same five steps as the factorial problems of Chapter 5, with two to five factors:
  - **Replicated designs** (6.5, 6.9, 6.11 to 6.14, 6.16, 6.19, 6.24, 6.25, 6.28, 6.34, 6.46): every main effect and interaction is tested against the pure error.
  - **Unreplicated designs** (one observation per combination: 6.26, 6.30, 6.32, 6.35, 6.39, 6.40, 6.42 to 6.44, 6.51, 6.52): there is no pure error, so the **three-factor and higher interactions are pooled into the error** and every main effect and two-factor interaction is tested against it (the usual practice for an unreplicated 2^k design).
  - **Effect estimates and the normal probability plot of the effects** (with Lenth's rule to label the effects that stand out) appear in step 4 for every two-level design, and in the solution PDF.
  - Factorial analysis now handles **up to five factors** (roles Factor D and Factor E in custom problems and on the Factorial page), including the rank-test restart, the post-hoc (simple effects), the R script and the solution PDF.
  - Not included: the exercises that need centre points (6.10, 6.27, 6.31, 6.37, 6.41, 6.45) or the data of the book's Example 6.2 (6.29, 6.37, 6.38), because the sheet does not contain that data; the centre runs of 6.35 and 6.52 are not used.

## What is new in stage 10

- **R script.** At the **end of every analysis** (the last step, under "End of the analysis") you find **Download solution (PDF)** and **R script**. The R script writes the worked solution as a script you can run top to bottom: the data, then steps 1 to 8 (hypotheses, α, information, test, decision, post-hoc, assumption checks) and, when an assumption fails, the rank test (steps 9 to 14). It follows your choices (the test chosen in step 5, α, the alternative, the post-hoc method, edited data) and uses base R and `lmtest`. The small helper functions it needs (Tukey-Kramer, Dunn, Games-Howell, Nemenyi, Scheffé, Levene) are written inside the script, so there is nothing else to install. For all 87 problems the script gives the same p-values as the app (tested).
- **Share an analysis.** **Share analysis** (problem header and every analysis page) opens a box with your name and a note for the lecturer, a **Download file** button (`.sdsa`) and a text code to copy. **Open shared analysis** (top bar) takes the file or the code, opens the right page at the step the student had reached, with their data (including edited values), settings, wording of the hypotheses, test and post-hoc, and then recomputes the results and tells you whether they are identical to those in the file. The box also gives a **share link** (the address of the app plus `#share=code`): whoever opens it lands on the same analysis, with no file and no menu to find. The same information is available as a text code that can be pasted into a chat or an email. Files do not need the internet or an account; nothing is stored on the server.

## Ask Professor Riaz (AI assistant)

The **Ask Professor Riaz** button at the bottom right opens a chat with an AI course assistant. It can see the page you are on: your dataset, your column mapping, and on a results page the ANOVA or test tables, assumption checks, post-hoc tables, the interpretation and the generated R code. Ask it to explain results, check assumptions, walk through the R code line by line, or show you how to use the app. Untick **Let Professor Riaz see my current data and results** in its settings to keep your data private.

### Built-in key (students need no key)

To give every student a working assistant without asking for a key, put your own key on the server:

1. Copy `ai_key.example.txt` to a file named `ai_key.txt` in the same folder as `app.R` (or make `ai_key.txt` hold just the key itself, for a Google Gemini key).
2. Set `RIAZ_PROVIDER` (gemini, openrouter, groq, openai, anthropic or custom), `RIAZ_API_KEY`, and optionally `RIAZ_MODEL` and `RIAZ_SESSION_LIMIT` (questions per student session, default 40; 0 = no limit).
3. Restart the app. The R console prints "Ask Professor Riaz: built-in key loaded …" and Professor Riaz's settings say "Ready to use: no key needed". If the console says no key was found, it also shows the folder the file must be in. A `.Renviron` file works too, and a byte-order mark from Windows Notepad is handled.
4. To publish, run `deploy.R` as usual: it uploads `ai_key.txt` with the app. (If you use RStudio's Publish button instead, make sure `ai_key.txt` is ticked in the file list.) On Posit Connect you can set the same variables under the app's Vars tab instead.

The key stays on the server: it is never sent to the browser, stored in the browser, or written to project files, and `ai_key.txt` is in `.gitignore` so it is never committed. Every student's questions are billed to this key, so use a free-tier service (Google Gemini Flash or Groq) or set a spending limit with your provider. Students can still connect their own key, and switch back to the course service, in the settings.

### Each student's own key

Without a built-in key, each user connects their own AI service in the chat's settings (gear icon):

| Service | Free option | Default model | Get a key |
|---|---|---|---|
| Google Gemini | Yes (Flash, Flash-Lite, Gemma) | newest Flash | aistudio.google.com/apikey |
| OpenRouter | Yes, models marked Free | a free Llama model | openrouter.ai/keys |
| Groq | Yes | llama-3.3-70b-versatile | console.groq.com/keys |
| OpenAI | No | newest mini model | platform.openai.com/api-keys |
| Anthropic Claude | No | claude-opus-5-5 | console.anthropic.com |
| Other (OpenAI-compatible) | Depends | you type it | DeepSeek, Mistral, Together, a local Ollama at `http://localhost:11434/v1`, and others |

After you paste a key, the Model list fills with every chat model that key can use, each marked Free or Paid (OpenRouter's marks come from its own prices; Gemini's follow Google's free tier for Flash, Flash-Lite and Gemma). Pick any model, or type a name. A remembered model that the service has retired is swapped for a current one automatically. **Save and test** sends a one-word test message. The key lives only in that browser session, plus the browser's own storage if you tick **Remember on this device**. It is never written to the server's disk, the logs or the `.sdproj` project file. When the app is online, questions and the shared context go through the app's server to the service you chose. The persona is an AI assistant named after the course lecturer; it says so if asked.

## Publish on shinyapps.io

Open `deploy.R` in RStudio, paste your account name, token and secret from shinyapps.io (Account > Tokens), and press Source. Keep the token private.

## Files

- `R/problems.R`: the Problem Sets exercises and their data (Chapters 2 to 6). `R/problem_engine.R`: the step-by-step process (information available, hypotheses, tests, decision, post-hoc, assumption checks, classification for the filters). `R/problem_blocks.R`: the Chapter 4 designs (RCBD, Latin, Graeco-Latin, BIBD, missing values, Friedman and the rank test on residuals). `R/mod_problems.R`: the Problem Sets page, its categories and filters, its steps and the custom problem editor.

- `R/problem_factorial.R`: the Chapter 5 and 6 factorial designs (hypotheses for each effect, factorial ANOVA with up to five factors, nonadditivity, pooled error for unreplicated 2^k designs, effect estimates and the normal plot of effects, rank tests, simple effects, effect plots). `www/tour.js`: the guided tour.

- `R/mod_team.R`: the team, aim and objectives (home page and About the team). `www/team/`: team photos. `www/kfupm_logo.svg`: the KFUPM logo.

- `R/ai.R`: Ask Professor Riaz (providers, built-in key, non-blocking requests, page context, chat panel). `ai_key.example.txt`: template for the built-in key.

- `R/script_export.R`: the R script of a worked solution. `R/share.R`: sharing and opening an analysis (file and code).
- `R/mod_problems.R`: the five-step process (Problem Sets and, in page mode, every analysis page). `R/solution_pdf.R`: the worked-solution PDF. `R/i18n.R`, `R/i18n_*.R`, `www/i18n.js`: the English/Arabic switch.
- `R/mod_planner.R`: Design Planner. `R/report.R`: pinning and the report exports. `R/mod_learn.R`: Learn guides. `deploy.R`: publish script.

- `R/samples.R`: one, two and k sample tests. `R/mod_compare.R`: their pages. `R/mod_desc.R`: descriptive statistics. `R/mod_advisor.R`: Test Advisor.

- `R/analysis.R`: the statistics engine. `R/plots.R`: charts. `R/results.R`: result text and tables. `R/codegen.R`: the R script.

- `app.R`: layout, navigation, save and open project.
- `R/data_utils.R`: reading files, type detection, data health.
- `R/designs.R`: roles per design and the design checks.
- `R/mod_data_studio.R`, `R/mod_doe.R`, `R/mod_home.R`: pages.
- `R/examples.R`: class datasets from the course scripts.
- `www/app.css`, `www/app.js`: styling and small browser helpers.
