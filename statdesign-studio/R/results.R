# The Results step: tabs, tables, charts and plain-language text.

RESULT_TABS <- list(
  list(key = "summary", label = "Summary", icon = "spark"),
  list(key = "explore", label = "Explore", icon = "bar"),
  list(key = "anova", label = "ANOVA", icon = "table"),
  list(key = "assump", label = "Assumptions", icon = "scale"),
  list(key = "posthoc", label = "Post-hoc", icon = "columns"),
  list(key = "nonpar", label = "Non-parametric", icon = "trend"),
  list(key = "code", label = "R code", icon = "code")
)

POSTHOC_INFO <- list(
  tukey = list(title = "Tukey's Honestly Significant Difference",
               desc = "Controls the family-wise error rate across all pairs. Recommended for comparing every pair."),
  lsd = list(title = "Fisher's Least Significant Difference",
             desc = "Plain t-tests using the ANOVA error. Most powerful, but the chance of at least one false difference grows with the number of pairs."),
  bonferroni = list(title = "Bonferroni",
                    desc = "Multiplies each p-value by the number of pairs. Simple and conservative."),
  scheffe = list(title = "Scheffé",
                 desc = "Protects every possible contrast, not only pairs. The most conservative for pairwise comparisons."),
  dunnett = list(title = "Dunnett (each treatment vs control)",
                 desc = "Compares every treatment with one control only, so it has more power than Tukey for that question.")
)

design_model_name <- function(design) switch(design$key, CRD = "One-way ANOVA", RCBD = "Two-way ANOVA (RCBD)",
                                             LSD = "Latin square ANOVA", GLSD = "Graeco-Latin square ANOVA")

status_pill <- function(st) switch(st,
  pass = pill("Pass", "pass", "check"), fail = pill("Fail", "fail", "x"),
  borderline = pill("Borderline", "warn", "alert"), pill("Not available", "neu"))

p_txt <- function(p) if (is.na(p)) "not available" else if (p < 1e-4) "p < 0.0001" else sprintf("p = %s", formatC(p, format = "f", digits = 4))

trt_row <- function(res, term = "treatment") res$anova$table[res$anova$table$term == term, ]

# ---------- Text ----------

headline <- function(res) {
  tr <- trt_row(res); a <- res$alpha; nm <- res$names$treatment; y <- res$settings$response_name %||% res$map$response
  if (res$recommend == "nonpar" && !is.null(res$nonpar$main)) {
    p <- res$nonpar$main$p
    sig <- !is.na(p) && p < a
    return(list(sig = sig,
                title = if (sig) sprintf("%s has a significant effect on %s", nm, y) else sprintf("No significant difference in %s between %s levels", y, nm),
                why = sprintf("%s: %s, which is %s α = %s. %s", res$nonpar$main$name, p_txt(p), if (sig) "below" else "above", format(a),
                              if (res$failed) "An assumption of ANOVA failed, so the non-parametric test is the one to report." else "You chose to report the non-parametric test.")))
  }
  sig <- tr$p < a
  n_ok <- sum(res$assumptions$status == "pass")
  list(sig = sig,
       title = if (sig) sprintf("%s has a significant effect on %s", nm, y) else sprintf("No significant difference in %s between %s levels", y, nm),
       why = sprintf("%s: F(%d, %d) = %.2f, %s, which is %s α = %s, so we %s H0. %s",
                     design_model_name(res$design), tr$df, res$anova$dfe, tr$F, p_txt(tr$p), if (sig) "below" else "above", format(a),
                     if (sig) "reject" else "do not reject",
                     if (n_ok == 3) "All three assumptions hold, so the ANOVA result is the one to report."
                     else if (res$settings$assump == "anova") "You chose to report ANOVA even though not every assumption holds; see the Assumptions tab."
                     else "Some assumptions are borderline; see the Assumptions tab."))
}

interpretation <- function(res) {
  s <- res$settings; d <- res$data; tr <- trt_row(res)
  y <- s$response_name %||% res$map$response; unit <- if (nzchar(s$unit %||% "")) paste0(" ", s$unit) else ""
  nm <- res$names$treatment; lv <- levels(d$treatment)
  ph <- res$posthoc$treatment; m <- ph$means[order(-ph$means$mean), ]
  asm <- res$assumptions
  design_txt <- switch(res$design$key,
    CRD = sprintf("A completely randomised design was used to compare %s across %d %s levels (%s).", y, length(lv), nm, paste(lv, collapse = ", ")),
    RCBD = sprintf("A randomised complete block design was used to compare %s across %d %s levels (%s), with %d %s as blocks.",
                   y, length(lv), nm, paste(lv, collapse = ", "), nlevels(d$block), res$names$block),
    LSD = sprintf("A %d × %d Latin square design was used to compare %s across %s levels (%s), blocking on %s and %s.",
                  length(lv), length(lv), y, nm, paste(lv, collapse = ", "), res$names$row, res$names$column),
    GLSD = sprintf("A %d × %d Graeco-Latin square design was used to compare %s across %s (%s) and %s, blocking on %s and %s.",
                   length(lv), length(lv), y, nm, paste(lv, collapse = ", "), res$names$greek, res$names$row, res$names$column))
  eff <- sprintf("%s %s a significant effect on %s, F(%d, %d) = %.2f, %s.", nm, if (tr$p < res$alpha) "had" else "did not have",
                 y, tr$df, res$anova$dfe, tr$F, p_txt(tr$p))
  a1 <- sprintf("The residuals were %s with normality (Shapiro-Wilk W = %.3f, %s), %s with constant variance (Breusch-Pagan %s) and %s with independence (Durbin-Watson = %.2f, %s).",
                if (asm$status["normality"] == "fail") "not consistent" else "consistent", asm$normal$value[1], p_txt(asm$normal$p[1]),
                if (asm$status["variance"] == "fail") "not consistent" else "consistent", p_txt(asm$variance$p[1]),
                if (asm$status["independence"] == "fail") "not consistent" else "consistent", asm$indep$value[1], p_txt(asm$indep$p[1]))
  tk <- ph$methods$tukey
  ph_txt <- if (tr$p < res$alpha) {
    nsig <- sum(tk$signif)
    sprintf("Tukey's HSD found %d of %d pairs significantly different. %s had the highest mean %s (%.2f%s), and %s the lowest (%.2f).",
            nsig, nrow(tk), m$level[1], y, m$mean[1], unit, m$level[nrow(m)], m$mean[nrow(m)])
  } else sprintf("The highest mean %s was for %s (%.2f%s), but the differences are not statistically significant.", y, m$level[1], m$mean[1], unit)
  np <- res$nonpar$main
  np_txt <- if (!is.null(np)) sprintf("The %s gave %s (%s).", np$name, if (np$p < res$alpha) "the same conclusion of a significant difference" else "no significant difference", p_txt(np$p)) else ""
  rec <- if (res$recommend == "nonpar") "Because an assumption of ANOVA failed, the non-parametric result is the one to report." else ""
  paste(design_txt, eff, a1, ph_txt, np_txt, rec)
}

# ---------- Tables ----------

anova_tbl_ui <- function(res) {
  an <- res$anova; tab <- an$table; a <- res$alpha
  tags$table(class = "kt",
    tags$thead(tags$tr(tags$th("Source"), tags$th(class = "n", "df"), tags$th(class = "n", "Sum of squares"), tags$th(class = "n", "Mean square"),
                       tags$th(class = "n", "F"), tags$th(class = "n", "p-value"), tags$th(class = "n", "F critical"), tags$th(class = "n", "Partial η²"), tags$th("Reading"))),
    tags$tbody(
      lapply(seq_len(nrow(tab)), function(i) { r <- tab[i, ]
        tags$tr(tags$td(tags$b(r$source), if (r$term != "treatment" && r$term != "greek") span(class = "small", " (block)")),
                tags$td(class = "n", r$df), tags$td(class = "n", fmt_num(r$ss)), tags$td(class = "n", fmt_num(r$ms)),
                tags$td(class = "n", fmt_g(r$F)), tags$td(class = "n", fmt_p(r$p), tags$b(style = "color:var(--green)", paste0(" ", p_stars(r$p)))),
                tags$td(class = "n", fmt_g(r$Fcrit)), tags$td(class = "n", fmt_g(r$eta2p, 3)),
                tags$td(if (!is.na(r$p) && r$p < a) pill("Significant", "pass") else pill("Not significant", "neu"))) }),
      tags$tr(tags$td("Residuals"), tags$td(class = "n", an$dfe), tags$td(class = "n", fmt_num(an$sse)), tags$td(class = "n", fmt_num(an$mse)),
              tags$td(), tags$td(), tags$td(), tags$td(), tags$td()),
      tags$tr(style = "font-weight:600", tags$td("Total"), tags$td(class = "n", an$dft), tags$td(class = "n", fmt_num(an$sst)), tags$td(), tags$td(), tags$td(), tags$td(), tags$td(), tags$td())))
}

ph_table_ui <- function(tab, conf, alpha) {
  tags$table(class = "kt",
    tags$thead(tags$tr(tags$th("Comparison"), tags$th(class = "n", "Difference"), tags$th(class = "n", sprintf("%g%% CI", 100 * conf)),
                       tags$th(class = "n", "Adjusted p"), tags$th("Result"))),
    tags$tbody(lapply(seq_len(nrow(tab)), function(i) { r <- tab[i, ]
      tags$tr(tags$td(tags$b(r$pair)), tags$td(class = "n", fmt_g(r$diff)),
              tags$td(class = "n", sprintf("[%s, %s]", fmt_g(r$lwr), fmt_g(r$upr))), tags$td(class = "n", fmt_p(r$p)),
              tags$td(if (r$p < alpha) pill("Different", "pass") else pill("Not different", "neu"))) })))
}

np_ph_table_ui <- function(tab, alpha, diff_name) {
  tags$table(class = "kt",
    tags$thead(tags$tr(tags$th("Comparison"), if (!is.null(diff_name)) tags$th(class = "n", diff_name),
                       tags$th(class = "n", "p-value"), tags$th("Result"))),
    tags$tbody(lapply(seq_len(nrow(tab)), function(i) { r <- tab[i, ]
      tags$tr(tags$td(tags$b(r$pair)), if (!is.null(diff_name)) tags$td(class = "n", fmt_g(r$diff)),
              tags$td(class = "n", fmt_p(r$p, 3)),
              tags$td(if (!is.na(r$p) && r$p < alpha) pill("Different", "pass") else pill("Not different", "neu"))) })))
}

test_tbl_ui <- function(tab) {
  tags$table(class = "kt",
    tags$thead(tags$tr(tags$th("Test"), tags$th(class = "n", "Statistic"), tags$th(class = "n", "p-value"))),
    tags$tbody(lapply(seq_len(nrow(tab)), function(i) { r <- tab[i, ]
      tags$tr(tags$td(r$test), tags$td(class = "n", if (is.na(r$value)) "—" else sprintf("%s = %s%s", r$statistic, fmt_g(r$value, 3),
                                                                          if (!is.na(r$df) && nzchar(r$df)) paste0(", df ", r$df) else "")),
              tags$td(class = "n", fmt_p(r$p, 3))) })))
}

means_tbl_ui <- function(res, term = "treatment") {
  m <- res$posthoc[[term]]$means; d <- res$data
  med <- tapply(d$y, d[[term]], stats::median); mn <- tapply(d$y, d[[term]], min); mx <- tapply(d$y, d[[term]], max)
  tags$table(class = "kt",
    tags$thead(tags$tr(tags$th(res$names[[term]]), tags$th(class = "n", "n"), tags$th(class = "n", "Mean"), tags$th(class = "n", "SD"),
                       tags$th(class = "n", sprintf("%g%% CI", 100 * res$conf)), tags$th(class = "n", "Median"), tags$th(class = "n", "Min"), tags$th(class = "n", "Max"))),
    tags$tbody(lapply(seq_len(nrow(m)), function(i) tags$tr(
      tags$td(tags$b(m$level[i])), tags$td(class = "n", m$n[i]), tags$td(class = "n", fmt_g(m$mean[i])), tags$td(class = "n", fmt_g(m$sd[i])),
      tags$td(class = "n", sprintf("[%s, %s]", fmt_g(m$lwr[i]), fmt_g(m$upr[i]))), tags$td(class = "n", fmt_g(med[[i]])),
      tags$td(class = "n", fmt_g(mn[[i]])), tags$td(class = "n", fmt_g(mx[[i]]))))))
}

# All results as a list of data frames for the Excel download.
results_tables <- function(res) {
  an <- res$anova
  t1 <- an$table[, c("source", "df", "ss", "ms", "F", "p", "Fcrit", "eta2p")]
  t1 <- rbind(t1, data.frame(source = "Residuals", df = an$dfe, ss = an$sse, ms = an$mse, F = NA, p = NA, Fcrit = NA, eta2p = NA),
              data.frame(source = "Total", df = an$dft, ss = an$sst, ms = NA, F = NA, p = NA, Fcrit = NA, eta2p = NA))
  out <- list(ANOVA = t1,
              Assumptions = rbind(cbind(check = "Normality", res$assumptions$normal), cbind(check = "Equal variance", res$assumptions$variance),
                                  cbind(check = "Independence", res$assumptions$indep)))
  for (t in names(res$posthoc)) {
    ph <- res$posthoc[[t]]
    out[[paste0("Means_", t)]] <- ph$means
    for (mth in names(ph$methods)) if (!is.null(ph$methods[[mth]]))
      out[[substr(paste0(mth, "_", t), 1, 31)]] <- ph$methods[[mth]][, intersect(c("pair", "diff", "se", "stat", "p", "lwr", "upr", "signif"), names(ph$methods[[mth]]))]
  }
  np <- res$nonpar
  if (!is.null(np$main)) {
    out$Nonparametric <- data.frame(test = np$main$name, statistic = np$main$stat, df = np$main$df %||% NA, p = np$main$p)
    for (x in np$posthoc) out[[substr(paste0("NP_", x$key), 1, 31)]] <- x$table
  }
  out$Data <- res$data[, setdiff(names(res$data), ".order")]
  out
}
