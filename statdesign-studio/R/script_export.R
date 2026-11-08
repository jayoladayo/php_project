# The worked solution as an R script: the data, then every step of the process (hypotheses, α, information,
# test, decision, post-hoc, assumption checks and, when an assumption fails, the rank test), written with plain
# stats functions so a student can run it, read it and check the numbers by hand. The script follows the
# student's choices (test, α, alternative, post-hoc). Plain R, no Shiny.

ps_rq <- function(x) paste0('"', gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", x)), '"')
ps_rvec <- function(x, numeric = FALSE) {
  if (numeric) paste0("c(", paste(ifelse(is.na(x), "NA", format(x, trim = TRUE, digits = 15)), collapse = ", "), ")")
  else paste0("c(", paste(ifelse(is.na(x), "NA", ps_rq(as.character(x))), collapse = ", "), ")")
}
# One vector over several lines of at most `width` characters.
ps_rwrap <- function(s, indent = "  ", width = 96) {
  parts <- strsplit(s, ", ", fixed = TRUE)[[1]]; lines <- character(0); cur <- ""
  for (p in parts) {
    add <- if (nzchar(cur)) paste0(cur, ", ", p) else p
    if (nchar(add) > width && nzchar(cur)) { lines <- c(lines, paste0(cur, ",")); cur <- p } else cur <- add
  }
  paste(c(lines, cur), collapse = paste0("\n", indent))
}

# Small functions the script carries with it, so it needs no packages beyond stats and lmtest.
PS_SCRIPT_HELPERS <- list(
  levene = c("# Levene's test (centre = median): an ANOVA on the absolute deviations from each group's median.",
             "levene <- function(y, g) { z <- abs(y - ave(y, g, FUN = median)); a <- anova(lm(z ~ g)); c(F = a$`F value`[1], p = a$`Pr(>F)`[1]) }"),
  tukey = c("# Tukey-Kramer comparisons of every pair of levels, from the error mean square and its df.",
            "tukey_pairs <- function(y, g, mse, dfe, alpha = 0.05) {",
            "  g <- droplevels(as.factor(g)); lv <- levels(g); k <- length(lv); m <- tapply(y, g, mean); n <- tapply(y, g, length)",
            "  P <- which(upper.tri(diag(k)), arr.ind = TRUE); i <- P[, 1]; j <- P[, 2]",
            "  d <- unname(m[j] - m[i]); se <- sqrt(mse * (1 / n[i] + 1 / n[j])); q <- abs(d) / (se / sqrt(2))",
            "  p <- ptukey(q, k, dfe, lower.tail = FALSE); qc <- qtukey(1 - alpha, k, dfe) / sqrt(2)",
            "  data.frame(pair = paste(lv[j], \"-\", lv[i]), diff = d, lwr = d - qc * se, upr = d + qc * se, p = p, different = p < alpha)",
            "}"),
  dunn = c("# Dunn's test on the ranks of y (ties corrected), p-values adjusted with `adjust` (Holm or Bonferroni).",
           "dunn_test <- function(y, g, adjust = \"holm\") {",
           "  g <- droplevels(as.factor(g)); lv <- levels(g); k <- length(lv); N <- length(y); r <- rank(y)",
           "  Rb <- tapply(r, g, mean); n <- tapply(r, g, length); ties <- table(y); tc <- sum(ties^3 - ties) / (12 * (N - 1))",
           "  P <- which(upper.tri(diag(k)), arr.ind = TRUE); i <- P[, 1]; j <- P[, 2]",
           "  z <- unname((Rb[j] - Rb[i]) / sqrt((N * (N + 1) / 12 - tc) * (1 / n[i] + 1 / n[j]))); p <- 2 * pnorm(-abs(z))",
           "  data.frame(pair = paste(lv[j], \"-\", lv[i]), z = z, p_unadjusted = p, p_adjusted = p.adjust(p, adjust))",
           "}"),
  gh = c("# Games-Howell comparisons (unequal variances).",
         "games_howell <- function(y, g, alpha = 0.05) {",
         "  g <- droplevels(as.factor(g)); lv <- levels(g); k <- length(lv); m <- tapply(y, g, mean); v <- tapply(y, g, var); n <- tapply(y, g, length)",
         "  P <- which(upper.tri(diag(k)), arr.ind = TRUE); i <- P[, 1]; j <- P[, 2]",
         "  a <- v[i] / n[i]; b <- v[j] / n[j]; d <- unname(m[j] - m[i]); se <- unname(sqrt(a + b))",
         "  df <- unname((a + b)^2 / (a^2 / (n[i] - 1) + b^2 / (n[j] - 1))); q <- abs(d) / se * sqrt(2); p <- ptukey(q, k, df, lower.tail = FALSE)",
         "  data.frame(pair = paste(lv[j], \"-\", lv[i]), diff = d, p = p, different = p < alpha)",
         "}"),
  nemenyi = c("# Nemenyi comparisons after the Friedman test (y: values, trt: treatments, blk: blocks).",
              "nemenyi <- function(y, trt, blk, alpha = 0.05) {",
              "  trt <- droplevels(as.factor(trt)); lv <- levels(trt); k <- length(lv); m <- tapply(y, list(blk, trt), mean); b <- nrow(m)",
              "  Rb <- colMeans(t(apply(m, 1, rank))); P <- which(upper.tri(diag(k)), arr.ind = TRUE); i <- P[, 1]; j <- P[, 2]",
              "  d <- unname(Rb[j] - Rb[i]); q <- abs(d) / sqrt(k * (k + 1) / (6 * b)); p <- ptukey(q * sqrt(2), k, Inf, lower.tail = FALSE)",
              "  data.frame(pair = paste(lv[j], \"-\", lv[i]), rank_diff = d, p = p, different = p < alpha)",
              "}"),
  scheffe = c("# Scheffe comparisons from the error mean square and its df.",
              "scheffe_pairs <- function(y, g, mse, dfe, alpha = 0.05) {",
              "  g <- droplevels(as.factor(g)); lv <- levels(g); k <- length(lv); m <- tapply(y, g, mean); n <- tapply(y, g, length)",
              "  P <- which(upper.tri(diag(k)), arr.ind = TRUE); i <- P[, 1]; j <- P[, 2]",
              "  d <- unname(m[j] - m[i]); se <- sqrt(mse * (1 / n[i] + 1 / n[j])); f <- (d / se)^2 / (k - 1)",
              "  p <- pf(f, k - 1, dfe, lower.tail = FALSE)",
              "  data.frame(pair = paste(lv[j], \"-\", lv[i]), diff = d, p = p, different = p < alpha)",
              "}"))

ps_solution_script <- function(m, s = NULL, test = NULL, ph = NULL, hyp = NULL) {
  st <- ps_structure(m$data, m$roles); kind <- ps_kind(st, m)
  if (is.na(kind)) stop(ps_kind_problem(kind, st))
  s <- s %||% ps_default_settings(m, kind)
  st <- ps_fact_set_pool(st, s)   # the effects the student put into the error (one observation per combination)
  info <- ps_info(kind, st, m, s); multi <- ps_multi(kind); fact <- ps_is_fact(kind)
  test <- test %||% info$suggest
  rank_first <- multi && test %in% PS_NONPAR_TESTS
  d <- st$d; alpha <- s$alpha; alt <- s$alt %||% "two.sided"
  out <- character(0); add <- function(...) out <<- c(out, ...)
  sec <- function(title) add("", sprintf("# %s", strrep("-", 3)), sprintf("# %s", title), sprintf("# %s", strrep("-", 3)))
  helpers_used <- character(0); need <- function(...) helpers_used <<- union(helpers_used, c(...))
  fl <- function(x) paste0("# ", x)
  finalize <- function() {
    o <- out
    if (length(helpers_used)) {
      hh <- character(0); for (h in helpers_used) hh <- c(hh, "", PS_SCRIPT_HELPERS[[h]])
      i <- match("results <- list()   # the key numbers are collected here", o); o <- c(o[1:i], hh, o[-(1:i)])
    }
    c(o, "")
  }
  drop_nuis <- function(f) gsub("(block|row|col|greek) \\+ |\\+ (block|row|col|greek)\\b", "", f)
  comment <- function(x) for (l in strsplit(x, "\n", fixed = TRUE)[[1]]) add(fl(l))

  # ---- names: the data frame uses short names; the original names are in comments ----
  fn <- c(treatment = if (fact) "A" else "treatment", factorB = "B", factorC = "C", factorD = "D", factorE = "E", block = "block", row = "row", column = "col", greek = "greek")
  cols <- c("y", intersect(c("treatment", "factorB", "factorC", "factorD", "factorE", "block", "row", "column", "greek"), names(d)))
  dd <- d[, cols, drop = FALSE]
  if (!is.null(st$missing) && nrow(st$missing) && kind %in% c("rcbd", "latin", "glsd")) {
    mi <- st$missing; extra <- dd[rep(1, nrow(mi)), , drop = FALSE]; extra$y <- NA_real_
    for (k in names(mi)) if (k %in% names(extra)) extra[[k]] <- factor(mi[[k]], levels = levels(dd[[k]]))
    dd <- rbind(dd, extra)
  }
  names(dd) <- c("y", unname(fn[names(dd)[-1]]))
  resp <- ps_lab(m, "response"); lab <- function(k) ps_lab(m, k)

  add(sprintf("# StatDesign Studio: worked solution as an R script"),
      sprintf("# %s%s", if (identical(m$id, "custom")) (if (nzchar(m$title %||% "")) m$title else "Your problem") else sprintf("Problem %s: %s", m$id, m$title),
              if (isTRUE(m$edited)) " (edited data)" else ""),
      sprintf("# Layout: %s. Test chosen in step 5: %s.", PS_KIND_LABEL[[kind]], ps_test_choices(kind, s)[[test]]),
      "# Run it from top to bottom. It uses base R (stats) and the lmtest package (install.packages(\"lmtest\") once).",
      "", "results <- list()   # the key numbers are collected here")

  # ---- Step 1 ----
  sec("Step 1 - Problem and data")
  comment(paste(c(m$statement, m$parts), collapse = "\n"))
  mapping <- paste(sprintf("%s = %s", names(dd), c(resp, vapply(names(d)[-1][names(d)[-1] %in% cols], function(k) ps_lab(m, k), ""))[seq_along(names(dd))]), collapse = "; ")
  add(fl(sprintf("Columns: %s", mapping)), "dat <- data.frame(")
  for (j in seq_along(dd)) {
    v <- dd[[j]]; nm <- names(dd)[j]
    body <- if (is.numeric(v)) ps_rwrap(ps_rvec(v, TRUE)) else ps_rwrap(ps_rvec(as.character(v)))
    add(sprintf("  %s = %s%s", nm, body, if (j < length(dd)) "," else ""))
  }
  add(")")
  for (j in seq_along(dd)[-1]) { v <- dd[[j]]; add(sprintf("dat$%s <- factor(dat$%s, levels = %s)", names(dd)[j], names(dd)[j], ps_rvec(levels(v)))) }
  if (isTRUE(st$cpool)) add("# The effects of a 2^k design are contrasts of the coded columns (low = -1, high = +1); the effects put into the error are not in the model.",
                            vapply(c("A", "B", "C", "D", "E")[seq_along(st$ffacs)], function(l) sprintf("dat$.%s <- ifelse(as.integer(dat$%s) == 2, 1, -1)", l, l), ""))
  add("str(dat)")

  # ---- Step 2 and 3 ----
  sec("Step 2 - Hypotheses")
  fam <- if (rank_first) "nonpar" else if (multi) "param" else info$family
  if (fact) {
    fh <- ps_fact_hypotheses(st, m, fam)
    for (cd in names(fh)) comment(sprintf("%s: H0: %s\n    H1: %s", cd, hyp$fh[[cd]]$h0 %||% fh[[cd]]$h0, hyp$fh[[cd]]$h1 %||% fh[[cd]]$h1))
  } else {
    h <- ps_hypotheses(kind, st, m, s, fam)
    comment(sprintf("H0: %s\nH1: %s", hyp$h0 %||% h$h0, hyp$h1 %||% h$h1))
  }
  sec("Step 3 - Significance level")
  add(sprintf("alpha <- %s", format(alpha)))
  if (!multi) add(sprintf("alt <- %s   # alternative: two.sided, greater or less", ps_rq(alt)), sprintf("value <- %s   # the value in H0", format(s$value %||% 0)))

  # ---- Step 4 ----
  sec("Step 4 - Information available")
  add("n <- nrow(dat)", "n")
  if (multi || kind %in% c("two", "paired")) add(if ("treatment" %in% names(dd)) "table(dat$treatment)" else if ("A" %in% names(dd)) "table(dat$A, dat$B)")
  if (kind == "one") add("shapiro.test(dat$y)   # normality of the sample", "results$p_shapiro <- shapiro.test(dat$y)$p.value")
  if (kind == "two") {
    need("levene")
    add("x1 <- dat$y[dat$treatment == levels(dat$treatment)[1]]", "x2 <- dat$y[dat$treatment == levels(dat$treatment)[2]]",
        "shapiro.test(x1); shapiro.test(x2)   # normality of each sample", "var.test(x1, x2)       # F test for equal variances", "levene(dat$y, dat$treatment)")
  }
  if (kind == "paired") {
    add("w <- reshape(dat[, c(\"y\", \"treatment\", \"block\")], idvar = \"block\", timevar = \"treatment\", direction = \"wide\")",
        "x1 <- w[[paste0(\"y.\", levels(dat$treatment)[1])]]; x2 <- w[[paste0(\"y.\", levels(dat$treatment)[2])]]", "dif <- x1 - x2",
        "shapiro.test(dif)   # normality of the differences")
  }
  if (kind == "fact" && ps_fact_2level(st)) {
    fs <- intersect(c("A", "B", "C", "D", "E"), names(dd))
    add("# Effects of the 2^k design (low = -1, high = +1) and the normal Q-Q plot of the effects used in class",
        sprintf("cd <- data.frame(y = dat$y, %s)", paste(sprintf("%s = ifelse(as.integer(dat$%s) == 2, 1, -1)", fs, fs), collapse = ", ")),
        sprintf("fit_all <- lm(y ~ %s, data = cd)   # every effect, the interactions up to the highest order", paste(fs, collapse = " * ")),
        "effects <- 2 * coef(fit_all)[-1]; round(effects, 4)   # effect = (mean at +) - (mean at -)",
        "qq <- qqnorm(abs(effects)); text(qq$x, qq$y, labels = names(effects), pos = 4, cex = 0.7)   # the effects that stand away from the small ones are active")
  }
  if (kind == "k") {
    need("levene")
    add("tapply(dat$y, dat$treatment, mean); tapply(dat$y, dat$treatment, sd)", "levene(dat$y, dat$treatment)          # equal group variances (Levene, median)",
        "bartlett.test(y ~ treatment, data = dat)  # Bartlett")
  }

  # ---- model formulas ----
  terms <- if (fact) strsplit(paste(deparse(ps_fact_formula(kind, st), width.cutoff = 500), collapse = ""), "~")[[1]][2] else if (kind %in% PS_BLOCK_KINDS) paste(ps_block_terms(kind), collapse = " + ") else "treatment"
  terms <- trimws(terms)
  terms <- gsub("\\bfactorE\\b", "E", gsub("\\bfactorD\\b", "D", gsub("\\bfactorB\\b", "B", gsub("\\bfactorC\\b", "C", gsub("\\btreatment\\b", if (fact) "A" else "treatment", terms)))))
  terms <- gsub("\\bcolumn\\b", "col", terms)
  frm <- sprintf("y ~ %s", terms)
  trt <- if (fact) "A" else "treatment"
  miss <- !is.null(st$missing) && nrow(st$missing) && kind %in% c("rcbd", "latin", "glsd")

  # ---- Step 5: the test ----
  sec(sprintf("Step 5 - The test: %s", ps_test_choices(kind, s)[[test]]))
  p_main <- NULL
  if (!multi) {
    add("conf <- 1 - alpha")
    if (kind == "one") switch(test,
      t = add("res <- t.test(dat$y, mu = value, alternative = alt, conf.level = conf)", "res"),
      z = add(sprintf("sigma <- %s   # known population standard deviation", format(s[["sigma"]])), "z <- (mean(dat$y) - value) / (sigma / sqrt(n))",
              "p_z <- switch(alt, two.sided = 2 * min(pnorm(z), 1 - pnorm(z)), less = pnorm(z), greater = 1 - pnorm(z))", "res <- list(statistic = z, p.value = p_z)", "res"),
      wilcox = add("res <- wilcox.test(dat$y, mu = value, alternative = alt, conf.int = TRUE, conf.level = conf, exact = FALSE)", "res"),
      sign = add("dif <- dat$y - value; dif <- dif[dif != 0]", "res <- binom.test(sum(dif > 0), length(dif), 0.5, alternative = alt, conf.level = conf)", "res"),
      chisq = add("s2 <- var(dat$y); chi2 <- (n - 1) * s2 / value", "cdf <- pchisq(chi2, n - 1)",
                  "p_chi <- switch(alt, two.sided = 2 * min(cdf, 1 - cdf), less = cdf, greater = 1 - cdf)", "res <- list(statistic = chi2, p.value = p_chi)", "res"))
    if (kind == "variance") add("s2 <- var(dat$y); chi2 <- (n - 1) * s2 / value", "cdf <- pchisq(chi2, n - 1)",
                                "p_chi <- switch(alt, two.sided = 2 * min(cdf, 1 - cdf), less = cdf, greater = 1 - cdf)", "res <- list(statistic = chi2, p.value = p_chi)", "res")
    if (kind == "two") switch(test,
      pooled = add("res <- t.test(x1, x2, mu = value, alternative = alt, conf.level = conf, var.equal = TRUE)", "res"),
      welch = add("res <- t.test(x1, x2, mu = value, alternative = alt, conf.level = conf, var.equal = FALSE)", "res"),
      z = add(sprintf("sigma1 <- %s; sigma2 <- %s   # known population standard deviations", format(s[["sigma"]]), format(s[["sigma2"]] %||% s[["sigma"]])),
              "se <- sqrt(sigma1^2 / length(x1) + sigma2^2 / length(x2)); z <- (mean(x1) - mean(x2) - value) / se",
              "p_z <- switch(alt, two.sided = 2 * min(pnorm(z), 1 - pnorm(z)), less = pnorm(z), greater = 1 - pnorm(z))", "res <- list(statistic = z, p.value = p_z)", "res"),
      mw = add("res <- wilcox.test(x1, x2, mu = value, alternative = alt, conf.int = TRUE, conf.level = conf, exact = FALSE)", "res"))
    if (kind == "paired") switch(test,
      paired = add("res <- t.test(x1, x2, mu = value, alternative = alt, conf.level = conf, paired = TRUE)", "res"),
      wsr = add("res <- wilcox.test(x1, x2, mu = value, alternative = alt, conf.int = TRUE, conf.level = conf, exact = FALSE, paired = TRUE)", "res"),
      sign = add("dif <- (x1 - x2) - value; dif <- dif[dif != 0]", "res <- binom.test(sum(dif > 0), length(dif), 0.5, alternative = alt, conf.level = conf)", "res"))
    add("results$p_value <- res$p.value")
    sec("Step 6 - Decision")
    add("cat(sprintf(\"Statistic = %.4f, p-value = %.4g, alpha = %g -> %s H0\\n\", res$statistic, res$p.value, alpha, if (res$p.value < alpha) \"reject\" else \"fail to reject\"))",
        "if (!is.null(res$conf.int)) print(res$conf.int)   # H0 is rejected when the interval leaves out the value in H0 (for a two-sided test)")
    return(finalize())
  }

  # ---------- designs with three or more treatments ----------
  if (miss) {
    add("# Missing observation(s): each is replaced by the value that makes the error sum of squares smallest (found by iteration),",
        "# and one error degree of freedom is removed for each, as in the course method.",
        "obs <- !is.na(dat$y); nmiss <- sum(!obs); dd <- dat; dd$y[!obs] <- mean(dat$y[obs])",
        sprintf("for (it in 1:500) { fv <- fitted(lm(%s, data = dd)); new <- fv[!obs]; if (max(abs(new - dd$y[!obs])) < 1e-12) break; dd$y[!obs] <- new }", frm),
        "dd$y[!obs]   # the estimated values")
  } else add("dd <- dat; nmiss <- 0")
  add(sprintf("fit <- aov(%s, data = dd)", frm), "an <- summary(fit)[[1]]; an; rn <- trimws(rownames(an))",
      "dfe <- an[\"Residuals\", \"Df\"] - nmiss; mse <- an[\"Residuals\", \"Sum Sq\"] / dfe")

  effects <- if (fact) setdiff(rownames(anova(stats::lm(ps_fact_formula(kind, st), data = d))), "Residuals") else "treatment"
  eff_tab <- function(cd) NULL
  tn <- function(x) gsub("factorE", "E", gsub("factorD", "D", gsub("factorC", "C", gsub("factorB", "B", gsub("treatment", "A", x)))))
  eterms <- if (fact) vapply(ps_fact_effects(st, m), function(e) tn(e$term), "") else NULL   # the effects in the order of the app
  if (fact && isTRUE(st$unrep)) eterms <- eterms[vapply(ps_fact_effects(st, m), function(e) e$main, TRUE)]   # two factors, one observation per cell: only the main effects are in the model
  if (!rank_first) {
    if (test == "welch") add("res <- oneway.test(y ~ treatment, data = dd, var.equal = FALSE)", "res", "results$p_param <- res$p.value")
    else if (fact) {
      codes <- names(ps_fact_effects(st, m))
      if (isTRUE(st$cpool)) add("# One observation per combination: there is no pure error. The effects put into the error are not in the model;",
                                sprintf("# they are pooled into the residual (%s), which is the error for every F test below.", paste(st$pool_codes, collapse = ", ")))
      else if (isTRUE(st$pool)) add("# One observation per combination: there is no pure error. The three-factor and higher interactions are not in the model;",
                               "# they are pooled into the residual, which is the error for every F test below (the usual practice for an unreplicated 2^k design).")
      add(sprintf("# One F test for each effect, against the error mean square (error df = %s)", "dfe"),
          sprintf("ef <- an[match(%s, rn), ]   # in the order A, B, ..., AB, AC, ...", ps_rvec(unname(eterms))),
          "rownames(ef) <- trimws(rownames(ef))",
          "ef$F <- ef$`Mean Sq` / mse; ef$p <- pf(ef$F, ef$Df, dfe, lower.tail = FALSE)")
      if (isTRUE(st$unrep)) add("# One observation per combination: there is no pure error. The interaction is checked with Tukey's test for nonadditivity (1 df).",
                                sprintf("fit_add <- lm(%s, data = dd)", sub("A \\* B", "A + B", frm)), "tuk <- anova(lm(y ~ A + B + I(fitted(fit_add)^2), data = dd)); tuk")
      add("print(ef[, c(\"Df\", \"Sum Sq\", \"Mean Sq\", \"F\", \"p\")])",
          if (isTRUE(st$unrep)) "results$p_param <- c(setNames(ef$p, rownames(ef)), AB = tuk[[\"Pr(>F)\"]][3])   # AB: nonadditivity test" else "results$p_param <- setNames(ef$p, rownames(ef))")
    } else {
      add(sprintf("tr <- an[rn == \"%s\", ]; Fstat <- tr$`Mean Sq` / mse; p_F <- pf(Fstat, tr$Df, dfe, lower.tail = FALSE)", trt),
          "cat(sprintf(\"F(%d, %d) = %.4f, p-value = %.4g\\n\", tr$Df, dfe, Fstat, p_F))", "results$p_param <- p_F")
    }
    sec("Step 6 - Decision")
    if (test == "welch") add("crit <- qf(1 - alpha, res$parameter[1], res$parameter[2]); cat(sprintf(\"F = %.4f, critical value %.4f, p-value = %.4g -> %s H0\\n\", res$statistic, crit, res$p.value, if (res$p.value < alpha) \"reject\" else \"fail to reject\"))")
    else if (fact) add("for (e in rownames(ef)) cat(sprintf(\"%-6s F = %8.4f, p-value = %.4g -> %s H0\\n\", e, ef[e, \"F\"], ef[e, \"p\"], if (ef[e, \"p\"] < alpha) \"reject\" else \"fail to reject\"))")
    else add("crit <- qf(1 - alpha, tr$Df, dfe); cat(sprintf(\"F0 = %.4f, critical value F(%g; %d, %d) = %.4f -> %s H0\\n\", Fstat, alpha, tr$Df, dfe, crit, if (p_F < alpha) \"reject\" else \"fail to reject\"))")
    # ---- Step 7 ----
    sec("Step 7 - Post-hoc (only when H0 is rejected)")
    chs <- ps_posthoc_choices(test, kind, m$control); mth <- if (!is.null(ph) && ph %in% names(chs)) ph else ps_default_ph(s, chs)
    cond <- if (fact) "any(ef$p < alpha)" else if (test == "welch") "res$p.value < alpha" else "p_F < alpha"
    add(sprintf("# method: %s", chs[[mth]]), sprintf("if (%s) {", cond))
    gv <- if (fact) "dat$A" else "dat$treatment"
    if (fact) {
      need("tukey"); add("  for (e in rownames(ef)[ef$p < alpha]) {", "    cat(\"\\n== \", e, \" ==\\n\")")
      add("    fs <- sub(\"^\\\\.\", \"\", strsplit(e, \":\", fixed = TRUE)[[1]])   # (the coded columns of a 2^k design are named .A, .B, ...)",
          "    if (length(fs) == 1) print(tukey_pairs(dd$y, dd[[fs]], mse, dfe, alpha))   # the levels of a significant main effect",
          "    else for (w in fs) { oth <- setdiff(fs, w)   # an interaction: compare the levels of w within each level of the other factor",
          "      for (lv in levels(interaction(dd[oth]))) { sub <- interaction(dd[oth]) == lv; cat(\"  within\", paste(oth, collapse = \":\"), \"=\", lv, \"\\n\"); print(tukey_pairs(dd$y[sub], dd[[w]][sub], mse, dfe, alpha)) } }", "  }")
    } else switch(mth,
      tukey = { need("tukey"); add(sprintf("  print(tukey_pairs(dd$y, dd$%s, mse, dfe, alpha))   # same as TukeyHSD(fit, \"%s\")", trt, trt)) },
      lsd = add(sprintf("  print(pairwise.t.test(dd$y, dd$%s, p.adjust.method = \"none\", pool.sd = TRUE))   # Fisher LSD", trt)),
      bonferroni = add(sprintf("  print(pairwise.t.test(dd$y, dd$%s, p.adjust.method = \"bonferroni\", pool.sd = TRUE))", trt)),
      scheffe = { need("scheffe"); add(sprintf("  print(scheffe_pairs(dd$y, dd$%s, mse, dfe, alpha))", trt)) },
      dunnett = add(sprintf("  d2 <- dd; d2$%s <- relevel(d2$%s, ref = %s)   # the control group", trt, trt, ps_rq(m$control %||% "")),
                    sprintf("  print(summary(multcomp::glht(aov(%s, data = d2), linfct = multcomp::mcp(%s = \"Dunnett\"))))   # needs the multcomp package", frm, trt)),
      gh = { need("gh"); add("  print(games_howell(dd$y, dd$treatment, alpha))") })
    add("} else cat(\"H0 was not rejected: no post-hoc analysis is needed.\\n\")")
  } else add("# (the ANOVA is not run: the rank test was chosen in step 5; the assumptions are still checked in step 8)")

  # ---- Step 8: assumption checks ----
  sec("Step 8 - Assumption checks (residuals of the ANOVA model)")
  add(sprintf("fit_obs <- lm(%s, data = dat)   # observed values only", frm), "res_obs <- residuals(fit_obs)",
      "sw <- shapiro.test(res_obs); sw                       # normality",
      if (fact) "bp <- lmtest::bptest(fit_obs, ~ fv, data = data.frame(fv = fitted(fit_obs))); bp   # constant variance (on the fitted values)"
      else if (!rank_first && test == "welch") "bp <- NULL   # Welch ANOVA does not assume equal variances"
      else "bp <- lmtest::bptest(fit_obs); bp                 # constant variance (Breusch-Pagan)",
      "bg <- lmtest::bgtest(fit_obs, order = 1); bg           # independence (Breusch-Godfrey)",
      "results$p_assumptions <- c(shapiro = sw$p.value, bp = if (is.null(bp)) NA else bp$p.value, bg = bg$p.value)",
      "ok <- all(results$p_assumptions >= alpha, na.rm = TRUE)",
      "cat(if (ok) \"All assumptions hold: the analysis stops here.\\n\" else \"An assumption fails: continue with the rank test below.\\n\")")

  # ---- Steps 9 to 14 ----
  npt <- ps_np_test(kind); nm <- ps_np_name(kind)
  sec(sprintf("Steps 9 to 14 - The rank test: %s (used when an assumption fails)", nm))
  add(if (rank_first) "# (chosen directly in step 5: it is run whatever the assumption checks say)" else "# Hypotheses about the medians; the same alpha.",
      if (rank_first) "if (TRUE) {" else "if (!ok) {")
  if (kind == "k") {
    need("dunn"); add("  kw <- kruskal.test(y ~ treatment, data = dat); kw", "  results$p_rank <- kw$p.value",
                      "  if (kw$p.value < alpha) print(dunn_test(dat$y, dat$treatment, \"holm\"))   # Dunn test, Holm adjustment")
  } else if (kind == "rcbd") {
    need("nemenyi")
    add("  m2 <- aggregate(y ~ treatment + block, data = dat, FUN = mean); full <- names(which(table(m2$block) == nlevels(m2$treatment)))",
        "  m2 <- m2[m2$block %in% full, ]; m2$block <- droplevels(m2$block); m2$treatment <- droplevels(m2$treatment)",
        "  fr <- friedman.test(y ~ treatment | block, data = m2); fr", "  results$p_rank <- fr$p.value",
        "  if (fr$p.value < alpha) print(nemenyi(m2$y, m2$treatment, m2$block, alpha))   # Nemenyi")
  } else if (kind %in% c("latin", "glsd")) {
    need("dunn")
    add(sprintf("  bm <- lm(y ~ %s, data = dat)   # the blocking factors only", paste(setdiff(strsplit(terms, " \\+ ")[[1]], "treatment"), collapse = " + ")),
        "  rr <- rank(residuals(bm))        # Quade's test: rank the residuals", "  kw <- kruskal.test(rr ~ dat$treatment[!is.na(dat$y)]); kw   # (rows with a missing y are left out)", "  results$p_rank <- kw$p.value",
        "  g <- droplevels(dat$treatment[!is.na(dat$y)])",
        "  if (kw$p.value < alpha) { print(dunn_test(rr, g, \"holm\"))                        # Dunn test, Holm adjustment",
        "    print(pairwise.wilcox.test(rr, g, p.adjust.method = \"bonferroni\")) }             # Wilcoxon rank-sum test, Bonferroni adjustment")
  } else if (fact) {
    need("tukey")
    frm_blk <- sub("^y ~ ", "", frm)
    if (kind == "fact") add("  rk <- rank(dat$y); MST <- var(rk)", sprintf("  ar <- summary(aov(rk ~ %s, data = dat))[[1]]", frm_blk))
    else if (kind %in% c("factl", "factg")) add(sprintf("  bm <- lm(y ~ %s, data = dat)", paste(intersect(c("row", "col", "greek"), strsplit(terms, " \\+ |\\*")[[1]]), collapse = " + ")),
                                              "  rk <- rank(residuals(bm)); MST <- var(rk)", sprintf("  ar <- summary(aov(rk ~ %s, data = dat[!is.na(dat$y), ]))[[1]]", drop_nuis(frm_blk)))
    else add("  # Friedman-type: rank the treatment combinations within each block (mean of y per block and combination),",
             "  # add the ranks of each combination over the blocks, and test each effect on these rank sums.",
             "  fac <- intersect(c(\"A\", \"B\", \"C\", \"D\", \"E\"), names(dat)); combo <- interaction(dat[fac], drop = TRUE, lex.order = FALSE)",
             "  ym <- tapply(dat$y, list(dat$block, combo), mean); ym <- ym[complete.cases(ym), , drop = FALSE]",
             "  mblk <- nrow(ym); Tn <- ncol(ym); Rs <- colSums(t(apply(ym, 1, rank)))",
             "  corr <- 1 - sum(apply(ym, 1, function(v) { t <- table(v); sum(t^3 - t) })) / (mblk * (Tn^3 - Tn))   # correction for ties",
             "  cdf <- unique(dat[fac]); cdf$cell <- interaction(cdf[fac], drop = TRUE, lex.order = FALSE)",
             "  cdf <- cdf[match(colnames(ym), as.character(cdf$cell)), ]; cdf$score <- as.numeric(Rs)",
             sprintf("  ar <- summary(aov(score ~ %s, data = cdf))[[1]]", drop_nuis(sub("^y ~ ", "", frm))))
    if (kind %in% c("fact", "factl", "factg")) add("  rn2 <- trimws(rownames(ar)); H <- ar[, \"Sum Sq\"] / MST; Hp <- pchisq(H, ar[, \"Df\"], lower.tail = FALSE)",
                                                  "  rk_tab <- data.frame(effect = rn2, H = H, df = ar[, \"Df\"], p = Hp)[rn2 != \"Residuals\", ]")
    else add("  rn2 <- trimws(rownames(ar)); H <- 12 * ar[, \"Sum Sq\"] / (mblk * Tn * (Tn + 1)) / corr; Hp <- pchisq(H, ar[, \"Df\"], lower.tail = FALSE)",
             "  rk_tab <- data.frame(effect = rn2, H = H, df = ar[, \"Df\"], p = Hp)[rn2 != \"Residuals\", ]")
    add(sprintf("  rk_tab <- rk_tab[match(%s, rk_tab$effect), ]   # in the order A, B, ..., AB, AC, ...", ps_rvec(unname(eterms))),
        "  print(rk_tab); results$p_rank <- setNames(rk_tab$p, rk_tab$effect)",
        "  cat(\"Effects with p < alpha:\", paste(rk_tab$effect[rk_tab$p < alpha], collapse = \", \"), \"\\n\")")
  }
  add("}", "", "# Conclusion: compare each p-value in `results` with alpha, as in steps 6 and 13.",
      "print(results)")
  finalize()
}
