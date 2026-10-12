# Learn: short method guides with the model, the assumptions and a worked class example.

fx <- function(...) HTML(paste0(...))   # formula markup

ex_grid <- function(rows, cols, cells) list(rows = rows, cols = cols, cells = cells)

CRD_CELLS <- matrix(c(50, 55, 53, 48, 47, 49, 60, 62, 61), 3, dimnames = list(paste("Plot", 1:3), c("A", "B", "C")))
RCBD_CELLS <- matrix(c(50, 55, 53, 48, 47, 49, 60, 62, 61), 3, dimnames = list(paste("Block", 1:3), c("A", "B", "C")))
LSD_CELLS <- matrix(c("A 50", "B 48", "C 60", "B 55", "C 47", "A 62", "C 53", "A 49", "B 61"), 3, dimnames = list(paste("Row", 1:3), paste("Column", 1:3)))
GLSD_CELLS <- local({
  ex <- CLASS_EXAMPLES$glsd$data; gk <- c(Organic = "α", Chemical = "β", Mixed = "γ", Untreated = "δ")
  rl <- c("High", "Medium-High", "Medium-Low", "Low"); cl <- c("Very Low", "Low", "Medium", "High")
  m <- matrix("", 4, 4, dimnames = list(rl, cl))
  for (i in seq_len(nrow(ex))) m[ex$SoilQuality[i], ex$Irrigation[i]] <- sprintf("%s%s %.1f", ex$Treatment[i], gk[[ex$FertilizerType[i]]], ex$Yield[i])
  m
})

LEARN <- list(
  desc = list(nav = "Descriptive statistics", title = "Descriptive statistics", open = list(page = "desc", data = "crd"),
    when = "Start every analysis here. Descriptive statistics show the centre, spread and shape of each variable, catch typing errors and outliers, and tell you whether a normal-theory test is reasonable before you run one.",
    model = fx("<i>x&#772;</i> = &Sigma;<i>x</i><sub>i</sub> / <i>n</i> &nbsp;&nbsp; <i>s</i><sup>2</sup> = &Sigma;(<i>x</i><sub>i</sub> &minus; <i>x&#772;</i>)<sup>2</sup> / (<i>n</i> &minus; 1)"),
    model_notes = c("CV = s / x̄ × 100%, the spread relative to the mean", "Outliers: below Q1 − 1.5 × IQR or above Q3 + 1.5 × IQR"),
    a_title = "What to look at", a_head = c("Feature", "Statistic", "What it tells you"),
    a = list(c("Centre", "Mean, median", "If they are far apart the data are skewed"), c("Spread", "SD, IQR, CV", "How much the values vary"),
             c("Shape", "Skewness, histogram, QQ plot", "Whether the data look normal"), c("Outliers", "1.5 × IQR rule, box plot", "Values to check before testing")),
    ex = list(df = data.frame(Statistic = c("n", "Mean", "Median", "SD", "Min", "Max"), Yield = c("9", "53.89", "53", "5.88", "47", "62"), check.names = FALSE),
              text = "Across the nine CRD plots the mean yield is 53.89 and the median 53, so the data are close to symmetric. The SD is 5.88 and values run from 47 to 62. The box plot by fertilizer shows that most of this spread comes from the differences between fertilizers, which is what the CRD analysis then tests.")),
  one = list(nav = "One-sample tests", title = "One-sample tests", open = list(page = "one", data = "crd"),
    when = "Use a one-sample test to compare the mean (or median) of one sample with a target value μ0, for example whether the mean yield differs from 50 kg per plot.",
    model = fx("<i>t</i> = (<i>x&#772;</i> &minus; &mu;<sub>0</sub>) / (<i>s</i> / &radic;<i>n</i>), &nbsp; df = <i>n</i> &minus; 1"),
    model_notes = c("H0: μ = μ0 against μ ≠ μ0, μ < μ0 or μ > μ0", "The z-test uses a known σ in place of s"),
    a_title = "Assumptions and fallbacks", a_head = c("Assumption", "Check", "If it fails"),
    a = list(c("Normality", "Shapiro-Wilk, QQ plot", "Wilcoxon signed-rank or sign test"), c("Independence", "How the data were collected", "Fix the design"),
             c("σ known (z-test only)", "Past data or a standard", "Use the t-test")),
    ex = list(df = data.frame(Test = c("One-sample t", "Wilcoxon signed-rank", "Sign test"), Statistic = c("t(8) = 1.98", "V = 29.5", "S = 5"), p = c("0.0827", "0.1230", "0.7266"), check.names = FALSE),
              text = "All nine CRD yields against μ0 = 50: the mean is 53.89, t(8) = 1.98, p = 0.0827, so at α = 0.05 we do not reject H0. Shapiro-Wilk passes, so the t-test is the one to report; the rank-based tests agree.")),
  two = list(nav = "Two-sample tests", title = "Two-sample tests", open = list(page = "two", data = "two"),
    when = "Use a two-sample test to compare two groups. If the groups are different units (plots given fertilizer A or B) use the independent tests; if each unit is measured twice (before and after) use the paired tests.",
    model = fx("<i>t</i> = (<i>x&#772;</i><sub>1</sub> &minus; <i>x&#772;</i><sub>2</sub>) / (<i>s</i><sub>p</sub>&radic;(1/<i>n</i><sub>1</sub> + 1/<i>n</i><sub>2</sub>)), &nbsp; df = <i>n</i><sub>1</sub> + <i>n</i><sub>2</sub> &minus; 2"),
    model_notes = c("Welch's test uses separate variances and adjusted df", "Paired t-test: a one-sample t-test on the differences"),
    a_title = "Assumptions and fallbacks", a_head = c("Assumption", "Check", "If it fails"),
    a = list(c("Normality", "Shapiro-Wilk in each sample", "Mann-Whitney U (paired: Wilcoxon signed-rank)"), c("Equal variance", "F test, Levene", "Welch t-test"),
             c("Independence", "Study design", "Use the paired test if units are matched")),
    ex = list(df = data.frame(Test = c("Pooled t", "Welch t", "Mann-Whitney U"), Statistic = c("t(4) = 2.98", "t(2.6) = 2.98", "W = 9"), p = c("0.0405", "0.0693", "0.0809"), check.names = FALSE),
              text = "Fertilizer A (50, 55, 53) against B (48, 47, 49). Both samples pass Shapiro-Wilk and the F test finds no difference in variance (F = 6.33, p = 0.273), so the pooled t-test is the one to report: t(4) = 2.98, p = 0.0405, and A gives the higher yield. With three plots per group the Welch and Mann-Whitney tests are not significant, which is a reminder that small samples have little power.")),
  k = list(nav = "k-sample tests", title = "k-sample tests", open = list(page = "k", data = "crd"),
    when = "Use a k-sample test to compare three or more independent groups when there is no blocking: one-way ANOVA when the assumptions hold, Welch ANOVA when the variances differ, and Kruskal-Wallis when normality fails.",
    model = fx("<i>y</i><sub>ij</sub> = &mu; + &tau;<sub>i</sub> + &epsilon;<sub>ij</sub>"),
    model_notes = c("τi: effect of group i · εij ~ N(0, σ²), independent", "H0: τ1 = τ2 = … = τk = 0"),
    a_title = "Assumptions and fallbacks", a_head = c("Assumption", "Check", "If it fails"),
    a = list(c("Normality", "Shapiro-Wilk on residuals", "Kruskal-Wallis, then Dunn"), c("Equal variance", "Levene, Bartlett", "Welch ANOVA, then Games-Howell"),
             c("Independence", "Study design", "Fix the design")),
    ex = list(df = data.frame(Test = c("One-way ANOVA", "Welch ANOVA", "Kruskal-Wallis"), Statistic = c("F(2, 6) = 46.84", "F = 108.10", "χ²(2) = 7.20"), p = c("0.0002", "0.0005", "0.0273"), check.names = FALSE),
              text = "The three fertilizers from the CRD example. All three tests reject H0: F(2, 6) = 46.84, p = 0.0002. Tukey HSD finds every pair different (B − A p = 0.032, C − A p = 0.002, C − B p = 0.0002).")),
  crd = list(nav = "CRD", title = "Completely Randomized Design", open = list(page = "crd", ex = "crd"),
    when = "Use a CRD when the experimental units are alike, such as plots on uniform land or identical samples in a lab. Each treatment is given to units chosen completely at random.",
    model = fx("<i>y</i><sub>ij</sub> = &mu; + &tau;<sub>i</sub> + &epsilon;<sub>ij</sub>"),
    model_notes = c("τi: effect of treatment i · εij ~ N(0, σ²), independent", "H0: τ1 = τ2 = … = τt = 0"),
    a_title = "Assumptions and fallbacks", a_head = c("Assumption", "Check", "If it fails"),
    a = list(c("Normality", "Shapiro-Wilk, QQ plot", "Kruskal-Wallis"), c("Equal variance", "Breusch-Pagan, residual plot", "Kruskal-Wallis or transform"),
             c("Independence", "Durbin-Watson, order plot", "Fix the design")),
    ex = list(m = CRD_CELLS,
              text = "Fertilizer F(2, 6) = 46.84, p = 0.0002, so fertilizers differ. Residuals pass all three checks (Shapiro-Wilk p = 0.891), and Tukey HSD separates all three: C (a), A (b), B (c). Kruskal-Wallis gives the same conclusion (p = 0.027).")),
  rcbd = list(nav = "RCBD", title = "Randomized Complete Block Design", open = list(page = "rcbd", ex = "rcbd"),
    when = "Use an RCBD when experimental units differ in a known way (farm, day, batch, operator) and you can group them into blocks so that every treatment appears once in each block. Blocking removes that nuisance variation from the error term.",
    model = fx("<i>y</i><sub>ij</sub> = &mu; + &tau;<sub>i</sub> + &beta;<sub>j</sub> + &epsilon;<sub>ij</sub>"),
    model_notes = c("τi: effect of treatment i · βj: effect of block j · εij ~ N(0, σ²), independent", "H0: τ1 = τ2 = … = τt = 0"),
    a_title = "Assumptions and fallbacks", a_head = c("Assumption", "Check", "If it fails"),
    a = list(c("Normality", "Shapiro-Wilk, QQ plot", "Friedman"), c("Equal variance", "Breusch-Pagan, residual plot", "Friedman or transform"),
             c("Independence", "Durbin-Watson, order plot", "Fix the design"), c("No interaction", "Design assumption", "Replicate within blocks")),
    ex = list(m = RCBD_CELLS,
              text = "Fertilizer F(2, 4) = 53.23, p = 0.0013, so fertilizers differ. Block F = 1.41, p = 0.344. Residuals pass all three checks, and Tukey HSD separates all three fertilizers. These are the same numbers the class R script prints.")),
  lsd = list(nav = "Latin Square", title = "Latin Square Design", open = list(page = "lsd", ex = "lsd"),
    when = "Use a Latin square when there are two known sources of nuisance variation, such as a soil gradient in one direction and irrigation in the other. With t treatments the field is a t × t grid and each treatment appears once in every row and every column.",
    model = fx("<i>y</i><sub>ijk</sub> = &mu; + &rho;<sub>i</sub> + &gamma;<sub>j</sub> + &tau;<sub>k</sub> + &epsilon;<sub>ijk</sub>"),
    model_notes = c("ρi: row effect · γj: column effect · τk: treatment effect", "Error df = (t − 1)(t − 2), so a 3 × 3 square has only 2"),
    a_title = "Assumptions and fallbacks", a_head = c("Assumption", "Check", "If it fails"),
    a = list(c("Normality", "Shapiro-Wilk, QQ plot", "Rank test on residuals"), c("Equal variance", "Breusch-Pagan, residual plot", "Rank test or transform"),
             c("Independence", "Durbin-Watson, order plot", "Fix the design"), c("No interactions", "Design assumption", "Use a factorial design")),
    ex = list(m = LSD_CELLS,
              text = "Treatment F(2, 2) = 0.42, p = 0.705, so there is no evidence the treatments differ. The row effect is significant (F = 37.77, p = 0.026), so blocking on rows was worthwhile. With only 2 error df and a failed Shapiro-Wilk (p = 0.002) the app recommends the rank test on residuals (p = 0.446), which agrees.")),
  glsd = list(nav = "Graeco-Latin Square", title = "Graeco-Latin Square Design", open = list(page = "glsd", ex = "glsd"),
    when = "Use a Graeco-Latin square to study two treatment factors with two blocking factors in one t × t square. Latin letters are one factor, Greek letters the other; each appears once per row and column, and each Latin–Greek pair appears exactly once.",
    model = fx("<i>y</i><sub>ijkl</sub> = &mu; + &rho;<sub>i</sub> + &gamma;<sub>j</sub> + &tau;<sub>k</sub> + &omega;<sub>l</sub> + &epsilon;<sub>ijkl</sub>"),
    model_notes = c("τk: Latin factor · ωl: Greek factor · ρi, γj: row and column", "Error df = (t − 1)(t − 3), so you need t ≥ 4"),
    a_title = "Assumptions and fallbacks", a_head = c("Assumption", "Check", "If it fails"),
    a = list(c("Normality", "Shapiro-Wilk, QQ plot", "Rank test on residuals"), c("Equal variance", "Breusch-Pagan, residual plot", "Rank test or transform"),
             c("Independence", "Durbin-Watson, order plot", "Fix the design"), c("No interactions", "Design assumption", "Use a factorial design")),
    ex = list(m = GLSD_CELLS, note = "Latin letter: variety · Greek letter: fertilizer (α Organic, β Chemical, γ Mixed, δ Untreated)",
              text = "Variety F(3, 3) = 40.10, p = 0.0064, so the varieties differ: A and C (a) yield more than B and D (b). Fertilizer type F(3, 3) = 5.90, p = 0.089, is not significant at 0.05, nor are soil quality (p = 0.461) and irrigation (p = 0.134). Residuals pass all three checks.")),
  assump = list(nav = "Assumption checks", title = "Assumption checks", open = list(page = "rcbd", ex = "rcbd"),
    when = "Check the assumptions after every ANOVA or t-test, before you trust the p-value. The checks use the residuals: what is left after the model explains the treatment and block effects.",
    model = fx("<i>e</i><sub>ij</sub> = <i>y</i><sub>ij</sub> &minus; <i>&ycirc;</i><sub>ij</sub>"),
    model_notes = c("A check's H0 is that the assumption holds, so p > α is a pass", "p between 0.05 and 0.10 is shown as borderline"),
    a_title = "Assumptions and fallbacks", a_head = c("Assumption", "Check", "If it fails"),
    a = list(c("Normality", "Shapiro-Wilk (Anderson-Darling as backup), QQ plot", "Non-parametric test: Kruskal-Wallis, Friedman, rank test"),
             c("Equal variance", "Breusch-Pagan, Levene, residuals vs fitted", "Welch and Games-Howell, or a log or square-root transform"),
             c("Independence", "Durbin-Watson, Breusch-Godfrey, order plot", "Fix the design; randomise the run order")),
    ex = list(df = data.frame(Check = c("Normality", "Equal variance", "Independence"), Test = c("Shapiro-Wilk", "Breusch-Pagan", "Durbin-Watson"), p = c("0.752", "0.155", "0.635"), Result = c("Pass", "Pass", "Pass"), check.names = FALSE),
              text = "The RCBD class example passes all three checks, so the ANOVA F test is the one to report. If normality had failed, the app would recommend the Friedman test instead and say why on the Summary tab.")),
  posthoc = list(nav = "Post-hoc tests", title = "Post-hoc tests", open = list(page = "crd", ex = "crd"),
    when = "After a significant F test, post-hoc tests find which pairs of treatments differ. They adjust for making many comparisons at once so the overall error rate stays near α.",
    model = fx("HSD = <i>q</i><sub>&alpha;; t, df<sub>E</sub></sub> &radic;(MSE / <i>r</i>) &nbsp;&nbsp; LSD = <i>t</i><sub>&alpha;/2; df<sub>E</sub></sub> &radic;(2 MSE / <i>r</i>)"),
    model_notes = c("Two means differ when their difference is larger than HSD or LSD", "Groups that share a letter are not significantly different"),
    a_title = "Which method", a_head = c("Method", "Controls", "Use it when"),
    a = list(c("Tukey HSD", "Family-wise error, all pairs", "The default for comparing every pair"), c("Fisher LSD", "Each comparison only", "A few planned pairs after a significant F"),
             c("Bonferroni", "Family-wise, conservative", "Only a few comparisons"), c("Scheffé", "All contrasts", "Complex contrasts, not just pairs"),
             c("Dunnett", "Family-wise, vs control", "Each treatment against one control"), c("Games-Howell", "Unequal variances", "After Welch ANOVA"),
             c("Dunn (Holm)", "Ranks", "After Kruskal-Wallis")),
    ex = list(df = data.frame(Pair = c("B − A", "C − A", "C − B"), Difference = c("−4.67", "8.33", "13.00"), `95% CI` = c("[−8.84, −0.49]", "[4.16, 12.51]", "[8.82, 17.18]"), p = c("0.0323", "0.0021", "0.0002"), check.names = FALSE),
              text = "Tukey HSD on the CRD example. Every interval excludes 0, so all three fertilizers differ: C (mean 61.0) gets letter a, A (52.7) b and B (48.0) c.")),
  decide = list(nav = "Three ways to decide", title = "Three ways to decide", open = list(page = "crd", ex = "crd"),
    when = "Every test can be decided three ways, and with matching α and confidence level they always agree: compare the p-value with α, compare the statistic with its critical value, or check whether the confidence interval contains the value in H0.",
    model = fx("p &lt; &alpha; &nbsp;&hArr;&nbsp; |<i>t</i>| &gt; <i>t</i><sub>&alpha;/2</sub> &nbsp;&hArr;&nbsp; CI<sub>1&minus;&alpha;</sub> excludes &mu;<sub>0</sub>"),
    model_notes = c("For the F test: reject H0 when F > F critical", "Each results page shows all three on the Summary tab"),
    a_title = "The three approaches", a_head = c("Approach", "Reject H0 when", "Example (CRD)"),
    a = list(c("P-value", "p < α", "p = 0.0002 < 0.05"), c("Rejection region", "Statistic beyond the critical value", "F = 46.84 > F(0.05; 2, 6) = 5.14"),
             c("Confidence interval", "Interval excludes the H0 value", "All 3 Tukey intervals exclude 0")),
    ex = list(df = data.frame(Approach = c("P-value", "Rejection region", "Confidence interval"), `Two-sample t (A vs B)` = c("p = 0.0405 < 0.05", "t = 2.98 > 2.78", "95% CI [0.33, 9.01] excludes 0"), check.names = FALSE),
              text = "The pooled two-sample t-test of fertilizer A against B reaches the same decision all three ways: reject H0, A yields more than B."))
)

learn_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("head")),
    div(class = "split learn",
        kcard(class = "lnav", div(class = "lt", "Methods"),
              lapply(names(LEARN), function(k) tags$a(href = "#", class = "lnk", `data-k` = k,
                                                      onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("pick"), k), LEARN[[k]]$nav))),
        uiOutput(ns("body"), class = "col")))
}

learn_server <- function(id, open_example, show) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cur <- reactiveVal("rcbd")
    observeEvent(input$pick, { if (input$pick %in% names(LEARN)) { cur(input$pick); show(input$pick) } })
    observe(session$sendCustomMessage("learn_on", list(id = ns(""), k = cur())))
    output$head <- renderUI({
      L <- LEARN[[cur()]]
      page_head(sprintf("Learn: %s", L$title), "When to use it, the model, the assumptions and a worked example from class.",
                btn(ns("open"), "Open the class example", "play", "bp", "bsm"))
    })
    observeEvent(input$open, open_example(LEARN[[cur()]]$open))
    output$body <- renderUI({
      L <- LEARN[[cur()]]
      ex <- L$ex
      ex_tab <- if (!is.null(ex$m)) {
        m <- ex$m
        tags$table(class = "kt", tags$thead(tags$tr(tags$th(""), lapply(colnames(m), function(c) tags$th(class = "n", c)))),
                   tags$tbody(lapply(seq_len(nrow(m)), function(i) tags$tr(tags$td(rownames(m)[i]), lapply(seq_len(ncol(m)), function(j) tags$td(class = "n", format(m[i, j])))))))
      } else html_table(ex$df)
      tagList(
        kcard(h2(class = "ct", "When to use it"), p(class = "ltext", L$when)),
        div(class = "grid g2",
            kcard(h2(class = "ct", "The model"), div(class = "formula", L$model),
                  lapply(L$model_notes, function(n) p(class = "small", style = "margin:8px 0 0", n))),
            kcard(h2(class = "ct", L$a_title),
                  div(class = "tscroll", style = "margin-top:10px", tags$table(class = "kt",
                    tags$thead(tags$tr(lapply(L$a_head, tags$th))),
                    tags$tbody(lapply(L$a, function(r) tags$tr(lapply(r, tags$td)))))))),
        kcard(h2(class = "ct", "Worked class example"),
              div(class = "lex", div(class = "tscroll", ex_tab, if (!is.null(ex$note)) p(class = "small", style = "margin:8px 0 0", ex$note)),
                  p(class = "ltext", ex$text))))
    })

    list(set = function(k) if (k %in% names(LEARN)) cur(k),
         ai = function(full) { L <- LEARN[[cur()]]
           list(label = sprintf("the %s guide", L$nav), kind = "general",
                text = if (full) sprintf("The student is reading the Learn guide on %s. When to use it: %s", L$title, gsub("<[^>]+>", "", paste(unlist(L$when), collapse = " ")))) })
  })
}
