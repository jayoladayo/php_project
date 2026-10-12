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

- App shell in KFUPM colours, light and dark mode, page search, save and open project.
- Data Studio: Excel and CSV upload with a sheet picker, header and skip-row options, missing value codes, automatic delimiter and decimal detection; typing and editing in a grid; pasting from Excel; class examples; data health checks with one-click fixes; column types.
- CRD, RCBD, Latin Square and Graeco-Latin Square: choose data, map columns to roles (numeric codes become categories), design check with layout preview, context and display names, level labels with colours and order, Dunnett control, α, confidence level, post-hoc choices, assumption handling, hypotheses.

## What is in stage 2

Press **Run analysis** on any of CRD, RCBD, Latin square or Graeco-Latin square to get seven result tabs:

- **Summary**: plain-English conclusion, key numbers, means chart with grouping letters, assumption scorecard, the decision made three ways, and a paragraph you can copy.
- **Explore**: box plots, a summary table, and a block profile (RCBD) or the square as laid out (LSD/GLSD).
- **ANOVA**: full table with F critical and partial eta squared, fit statistics, relative efficiency, and the p-value, rejection-region and confidence-interval methods.
- **Assumptions**: Shapiro-Wilk and Anderson-Darling, Breusch-Pagan and Levene, Durbin-Watson and Breusch-Godfrey, with QQ, residual and run-order plots.
- **Post-hoc**: Tukey, Fisher LSD, Bonferroni, Scheffé and Dunnett with intervals, letters and a side-by-side comparison.
- **Non-parametric**: Kruskal-Wallis (CRD), Friedman and Quade (RCBD), or the class rank-on-residuals permutation test (LSD/GLSD), with post-hoc tests. In automatic mode it becomes the recommended result when normality or equal variance fails.
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

- **Problem Sets** (sidebar, first item under Design of experiments): 30 exercises from the course problem sheets, each with its full statement and complete dataset.
  - **Chapter 2 problems** (15): simple comparative experiments: one-sample t and z tests, a χ² test on a variance, two-sample t and z tests, and three paired problems (2.34, 2.35, 2.36). A paired problem is a block design: each pair is a block, so it is analysed as an RCBD with two treatments, and the answer shows that the paired t-test and the RCBD ANOVA agree (F = t²).
  - **Chapter 3 problems** (15): single-factor ANOVA: balanced and unbalanced CRDs, count data, a problem that needs a log transformation (3.32) and a random-effects problem with variance components (3.34).
  - **Create custom problem**: write or paste a statement, paste, type or upload the data (or start from a class example, including the Latin square), stack wide tables into long format, say which column is the response, treatment, block, row or column, and edit everything before running. **Edit this problem** copies any exercise into this editor.
- Every problem follows **Problem → Data → Recommended analysis → Run analysis**: the statement and questions, the factors and variables, the design type, the assumptions and data requirements, and the data as printed or in long format (scrollable, downloadable as CSV).
- **Automatic recommendation** from the data structure, with the reasons: one sample, two independent samples, paired (RCBD with two treatments), CRD, random effects, RCBD (complete blocks), Latin square (checked to be a valid square) or a general linear model for unbalanced or incomplete layouts. It also flags unequal replication, count responses and very unequal variances.
- **Override the analysis**: one-sample test, variance test, two-sample t-test, paired t-test, one-way ANOVA, random-effects ANOVA, RCBD ANOVA, Latin Square ANOVA, general linear model, rank tests, descriptive statistics, and residual diagnostics, with an optional log or square-root transformation. An analysis that does not fit the data says why and what to change. A manual choice is labelled as yours, not the system's.
- **Answer Model** after **Run analysis**: the design, the variables, the hypotheses, the model, the calculations with the numbers filled in (sums of squares, standard errors, test statistics), the ANOVA or test table, the statistics with df, P-values and critical values, the decision, treatment comparisons (Tukey or Fisher LSD with grouping letters, or a confidence interval for two groups), assumption checks with QQ and residual plots, the statistical conclusion and a plain-language interpretation. **Pin to report** sends it to the Report builder, and Ask Dr. Riaz can see it.
- The page remembers the selected problem and the analysis chosen for each problem, so returning to the list loses nothing.

The problem sheets supplied are Montgomery Chapter 2 (Simple Comparative Experiments) and Chapter 3 (Experiments with a Single Factor). Neither contains a Latin square exercise, so no Latin square problem is invented; the Latin Square ANOVA is available for custom problems and through the class example. The chapter labels are set in `PS_CHAPTERS` at the top of `R/problems.R`.

## Ask Dr. Riaz (AI assistant)

The **Ask Dr. Riaz** button at the bottom right opens a chat with an AI course assistant. It can see the page you are on: your dataset, your column mapping, and on a results page the ANOVA or test tables, assumption checks, post-hoc tables, the interpretation and the generated R code. Ask it to explain results, check assumptions, walk through the R code line by line, or show you how to use the app. Untick **Let Dr. Riaz see my current data and results** in its settings to keep your data private.

Each user connects their own AI service in the chat's settings (gear icon):

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

- `R/problems.R`: the Problem Sets exercises and their data. `R/problem_engine.R`: structure detection, recommendation, validation, the analyses and the Answer Model. `R/mod_problems.R`: the Problem Sets page and the custom problem editor.

- `R/ai.R`: Ask Dr. Riaz (providers, non-blocking requests, page context, chat panel).

- `R/mod_planner.R`: Design Planner. `R/report.R`: pinning and the report exports. `R/mod_learn.R`: Learn guides. `deploy.R`: publish script.

- `R/samples.R`: one, two and k sample tests. `R/mod_compare.R`: their pages. `R/mod_desc.R`: descriptive statistics. `R/mod_advisor.R`: Test Advisor.

- `R/analysis.R`: the statistics engine. `R/plots.R`: charts. `R/results.R`: result text and tables. `R/codegen.R`: the R script.

- `app.R`: layout, navigation, save and open project.
- `R/data_utils.R`: reading files, type detection, data health.
- `R/designs.R`: roles per design and the design checks.
- `R/mod_data_studio.R`, `R/mod_doe.R`, `R/mod_home.R`: pages.
- `R/examples.R`: class datasets from the course scripts.
- `www/app.css`, `www/app.js`: styling and small browser helpers.
