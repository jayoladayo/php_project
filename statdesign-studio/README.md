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
