# The statistics behind the Results step. Everything here is plain R with no Shiny,
# so it can be tested on its own and mirrored in the generated R script.

TERM_ORDER <- c("treatment", "block", "row", "column", "greek")

# Build the analysis data: numeric response, labelled factors in the user's order, run order.
prep_doe_data <- function(design, df, map, labels) {
  fk <- vapply(factor_roles(design), `[[`, "", "key")
  y <- suppressWarnings(as.numeric(as.character(df[[map$response]])))
  d <- data.frame(y = y)
  for (k in fk) {
    codes <- trimws(as.character(df[[map[[k]]]]))
    L <- labels[[k]]
    lev <- if (!is.null(L)) L$code else level_order(codes)
    lab <- if (!is.null(L)) make.unique(L$label) else lev
    d[[k]] <- factor(codes, levels = lev, labels = lab)
  }
  run <- map$run %||% ""
  if (nzchar(run) && run %in% names(df)) {
    r <- suppressWarnings(as.numeric(as.character(df[[run]])))
    if (all(is.na(r))) r <- as.numeric(factor(df[[run]], levels = unique(df[[run]])))
    d$.run <- r
  } else d$.run <- seq_len(nrow(d))
  keep <- !is.na(d$y)
  for (k in fk) keep <- keep & !is.na(d[[k]])
  dropped <- sum(!keep)
  d <- d[keep, , drop = FALSE]
  d <- d[order(d$.run), , drop = FALSE]
  d$.order <- seq_len(nrow(d))
  for (k in fk) d[[k]] <- droplevels(d[[k]])
  rownames(d) <- NULL
  attr(d, "dropped") <- dropped
  d
}


# Order used in the model formula: treatment first, then the blocking factors, Greek factor last (as in class).
doe_terms <- function(design) switch(design$key,
  CRD = "treatment", RCBD = c("treatment", "block"), LSD = c("treatment", "row", "column"),
  GLSD = c("treatment", "row", "column", "greek"))

# Factors that are compared with post-hoc tests.
compared_terms <- function(design) if (design$key == "GLSD") c("treatment", "greek") else "treatment"

p_stars <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ifelse(p < 0.1, ".", "")))))

fmt_p <- function(p, digits = 4) {
  ifelse(is.na(p), "—", ifelse(p < 1e-4, "< 0.0001", formatC(p, format = "f", digits = digits)))
}
fmt_num <- function(x, digits = 3) ifelse(is.na(x), "—", formatC(x, format = "f", digits = digits, big.mark = ","))
fmt_g <- function(x, digits = 2) ifelse(is.na(x), "—", formatC(x, format = "f", digits = digits))

# ---------- ANOVA ----------

anova_table <- function(fit, d, terms, names, sstype = "I", alpha = 0.05) {
  if (sstype == "I") {
    a <- stats::anova(fit)
  } else {
    old <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(old))
    f2 <- stats::lm(stats::formula(fit), data = d)
    a <- car::Anova(f2, type = sstype)
    a <- a[rownames(a) != "(Intercept)", , drop = FALSE]
    names(a) <- c("Sum Sq", "Df", "F value", "Pr(>F)")
    a$`Mean Sq` <- a$`Sum Sq` / a$Df
  }
  res <- a["Residuals", ]
  dfe <- res$Df; sse <- res$`Sum Sq`; mse <- sse / dfe
  rows <- lapply(terms, function(t) {
    r <- a[t, ]
    fval <- r$`F value`; p <- r$`Pr(>F)`
    data.frame(term = t, source = names[[t]], df = r$Df, ss = r$`Sum Sq`, ms = r$`Sum Sq` / r$Df,
               F = fval, p = p, Fcrit = stats::qf(1 - alpha, r$Df, dfe),
               eta2p = r$`Sum Sq` / (r$`Sum Sq` + sse), stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  sst <- sum((d$y - mean(d$y))^2)
  list(table = tab, dfe = dfe, sse = sse, mse = mse, sst = sst, dft = nrow(d) - 1,
       r2 = 1 - sse / sst, cv = 100 * sqrt(mse) / mean(d$y), grand_mean = mean(d$y), rmse = sqrt(mse))
}

relative_efficiency <- function(design, an, d) {
  tab <- an$table; mse <- an$mse
  if (design$key == "RCBD") {
    b <- nlevels(d$block); t <- nlevels(d$treatment)
    msb <- tab$ms[tab$term == "block"]
    return(list(value = ((b - 1) * msb + b * (t - 1) * mse) / ((b * t - 1) * mse), vs = "CRD"))
  }
  if (design$key == "LSD") {
    p <- nlevels(d$treatment)
    msr <- tab$ms[tab$term == "row"]; msc <- tab$ms[tab$term == "column"]
    return(list(value = (msr + msc + (p - 1) * mse) / ((p + 1) * mse), vs = "CRD"))
  }
  NULL
}

# ---------- Assumptions ----------

safe_test <- function(expr) tryCatch(expr, error = function(e) NULL)

assumption_checks <- function(fit, d, alpha = 0.05) {
  rs <- stats::rstandard(fit)
  rs <- rs[is.finite(rs)]
  n <- length(rs)
  sw <- if (n >= 3 && n <= 5000 && stats::var(rs) > 0) safe_test(stats::shapiro.test(rs)) else NULL
  ad <- if (n >= 8) safe_test(nortest::ad.test(rs)) else NULL
  bp <- safe_test(lmtest::bptest(fit))
  lev <- safe_test(car::leveneTest(y ~ treatment, data = d))
  dw <- safe_test(lmtest::dwtest(fit, alternative = "two.sided"))
  bg <- safe_test(lmtest::bgtest(fit, order = 1))
  row <- function(name, stat_lab, stat, df = NA, p) data.frame(test = name, statistic = stat_lab, value = stat, df = df, p = p, stringsAsFactors = FALSE)
  normal <- rbind(
    if (!is.null(sw)) row("Shapiro-Wilk", "W", unname(sw$statistic), NA, sw$p.value) else row("Shapiro-Wilk", "W", NA, NA, NA),
    if (!is.null(ad)) row("Anderson-Darling", "A", unname(ad$statistic), NA, ad$p.value) else row("Anderson-Darling", "A", NA, NA, NA))
  variance <- rbind(
    if (!is.null(bp)) row("Breusch-Pagan", "BP", unname(bp$statistic), unname(bp$parameter), bp$p.value) else row("Breusch-Pagan", "BP", NA, NA, NA),
    if (!is.null(lev)) row("Levene (median)", "F", lev$`F value`[1], paste(lev$Df, collapse = ", "), lev$`Pr(>F)`[1]) else row("Levene (median)", "F", NA, NA, NA))
  indep <- rbind(
    if (!is.null(dw)) row("Durbin-Watson", "DW", unname(dw$statistic), NA, dw$p.value) else row("Durbin-Watson", "DW", NA, NA, NA),
    if (!is.null(bg)) row("Breusch-Godfrey (lag 1)", "LM", unname(bg$statistic), unname(bg$parameter), bg$p.value) else row("Breusch-Godfrey (lag 1)", "LM", NA, NA, NA))
  verdict <- function(p) {
    if (is.na(p)) return("na")
    if (p < alpha) "fail" else if (alpha < 0.10 && p < 0.10) "borderline" else "pass"
  }
  p_norm <- if (!is.na(normal$p[1])) normal$p[1] else normal$p[2]
  p_var <- if (!is.na(variance$p[1])) variance$p[1] else variance$p[2]
  p_ind <- indep$p[1]
  list(normal = normal, variance = variance, indep = indep,
       status = c(normality = verdict(p_norm), variance = verdict(p_var), independence = verdict(p_ind)),
       primary = c(normality = p_norm, variance = p_var, independence = p_ind),
       resid = data.frame(order = d$.order, fitted = stats::fitted(fit), resid = stats::residuals(fit),
                          std = stats::rstandard(fit), treatment = d$treatment))
}

# ---------- Post-hoc ----------

pair_index <- function(k) { ij <- utils::combn(k, 2); list(i = ij[1, ], j = ij[2, ]) }

# Pairwise comparisons for one factor, using the model's error mean square.
posthoc_all <- function(fit, d, term, mse, dfe, alpha = 0.05, conf = 0.95, control = NULL, seed = 1) {
  f <- d[[term]]; lev <- levels(f); k <- length(lev)
  means <- tapply(d$y, f, mean); n <- tapply(d$y, f, length)
  sds <- tapply(d$y, f, stats::sd)
  P <- pair_index(k); m <- length(P$i)
  diff <- unname(means[P$j] - means[P$i])
  se <- sqrt(mse * (1 / n[P$i] + 1 / n[P$j])); se <- unname(se)
  tval <- diff / se
  lab <- paste(lev[P$j], "−", lev[P$i])
  base <- data.frame(pair = lab, i = lev[P$i], j = lev[P$j], diff = diff, se = se, stringsAsFactors = FALSE)
  out <- list()
  # Fisher LSD (unadjusted t)
  tc <- stats::qt(1 - (1 - conf) / 2, dfe)
  out$lsd <- transform(base, stat = tval, p = 2 * stats::pt(-abs(tval), dfe), lwr = diff - tc * se, upr = diff + tc * se,
                       crit = tc * se)
  # Tukey HSD (Tukey-Kramer when unbalanced)
  q <- abs(diff) / (se / sqrt(2))
  qc <- stats::qtukey(conf, k, dfe)
  out$tukey <- transform(base, stat = q, p = stats::ptukey(q, k, dfe, lower.tail = FALSE),
                         lwr = diff - qc / sqrt(2) * se, upr = diff + qc / sqrt(2) * se, crit = qc / sqrt(2) * se)
  # Bonferroni
  bc <- stats::qt(1 - (1 - conf) / (2 * m), dfe)
  out$bonferroni <- transform(base, stat = tval, p = pmin(1, m * 2 * stats::pt(-abs(tval), dfe)),
                              lwr = diff - bc * se, upr = diff + bc * se, crit = bc * se)
  # Scheffé
  sc <- sqrt((k - 1) * stats::qf(conf, k - 1, dfe))
  out$scheffe <- transform(base, stat = tval^2 / (k - 1), p = stats::pf(tval^2 / (k - 1), k - 1, dfe, lower.tail = FALSE),
                           lwr = diff - sc * se, upr = diff + sc * se, crit = sc * se)
  # Dunnett against the control
  ctrl <- if (!is.null(control) && control %in% lev) control else lev[1]
  out$dunnett <- tryCatch({
    d2 <- d; d2[[term]] <- stats::relevel(d2[[term]], ref = ctrl)
    fit2 <- stats::aov(stats::formula(fit), data = d2)
    set.seed(seed)
    g <- multcomp::glht(fit2, linfct = do.call(multcomp::mcp, stats::setNames(list("Dunnett"), term)))
    s <- summary(g); ci <- stats::confint(g, level = conf)
    others <- setdiff(levels(d2[[term]]), ctrl)
    data.frame(pair = paste(others, "−", ctrl), i = ctrl, j = others, diff = unname(s$test$coefficients),
               se = unname(s$test$sigma), stat = unname(s$test$tstat), p = unname(s$test$pvalues),
               lwr = unname(ci$confint[, "lwr"]), upr = unname(ci$confint[, "upr"]),
               crit = unname(ci$confint[, "upr"] - ci$confint[, "Estimate"]), stringsAsFactors = FALSE)
  }, error = function(e) NULL)
  info <- list(k = k, m = m, mse = mse, dfe = dfe, control = ctrl,
               tukey_q = qc, hsd = if (length(unique(n)) == 1) qc / sqrt(2) * se[1] else NA,
               lsd_value = if (length(unique(n)) == 1) tc * se[1] else NA)
  summ <- data.frame(level = lev, n = as.integer(n), mean = as.numeric(means), sd = as.numeric(sds),
                     se = sqrt(mse / as.numeric(n)), stringsAsFactors = FALSE)
  tcrit <- stats::qt(1 - (1 - conf) / 2, dfe)
  summ$lwr <- summ$mean - tcrit * summ$se; summ$upr <- summ$mean + tcrit * summ$se
  for (mth in c("lsd", "tukey", "bonferroni", "scheffe")) {
    out[[mth]]$signif <- out[[mth]]$p < alpha
  }
  if (!is.null(out$dunnett)) out$dunnett$signif <- out$dunnett$p < alpha
  letters <- lapply(c(lsd = "lsd", tukey = "tukey", bonferroni = "bonferroni", scheffe = "scheffe"), function(mth)
    cld_letters(lev, as.numeric(means), out[[mth]]$i[out[[mth]]$signif], out[[mth]]$j[out[[mth]]$signif]))
  list(methods = out, info = info, means = summ, letters = letters)
}

# Compact letter display (insert-absorb). Highest mean gets "a".
cld_letters <- function(lev, means, sig_i, sig_j) {
  ord <- lev[order(-means)]
  cols <- list(ord)
  if (length(sig_i)) for (p in seq_along(sig_i)) {
    a <- sig_i[p]; b <- sig_j[p]
    newcols <- list()
    for (cl in cols) {
      if (a %in% cl && b %in% cl) {
        newcols <- c(newcols, list(setdiff(cl, a)), list(setdiff(cl, b)))
      } else newcols <- c(newcols, list(cl))
    }
    # absorb: drop columns contained in another column
    keep <- rep(TRUE, length(newcols))
    for (x in seq_along(newcols)) for (y in seq_along(newcols)) {
      if (x != y && keep[y] && all(newcols[[x]] %in% newcols[[y]]) &&
          (length(newcols[[x]]) < length(newcols[[y]]) || x > y)) { keep[x] <- FALSE; break }
    }
    cols <- newcols[keep]
  }
  first <- vapply(cols, function(cl) min(match(cl, ord)), 0)
  cols <- cols[order(first)]
  out <- setNames(rep("", length(lev)), lev)
  for (c in seq_along(cols)) for (l in cols[[c]]) out[l] <- paste0(out[l], letters[c])
  out
}

# ---------- Non-parametric ----------

dunn_test <- function(y, g, adjust = "holm") {
  g <- droplevels(as.factor(g)); lev <- levels(g); k <- length(lev); N <- length(y)
  r <- rank(y); Rbar <- tapply(r, g, mean); n <- tapply(r, g, length)
  ties <- table(y); tc <- sum(ties^3 - ties) / (12 * (N - 1))
  P <- pair_index(k)
  z <- unname((Rbar[P$j] - Rbar[P$i]) / sqrt((N * (N + 1) / 12 - tc) * (1 / n[P$i] + 1 / n[P$j])))
  p <- 2 * stats::pnorm(-abs(z))
  data.frame(pair = paste(lev[P$j], "−", lev[P$i]), i = lev[P$i], j = lev[P$j],
             diff = unname(Rbar[P$j] - Rbar[P$i]), stat = z, p_unadj = p, p = stats::p.adjust(p, adjust),
             stringsAsFactors = FALSE)
}

nemenyi_friedman <- function(y, trt, blk) {
  trt <- droplevels(trt); lev <- levels(trt); k <- length(lev)
  m <- tapply(y, list(blk, trt), mean)
  m <- m[stats::complete.cases(m), , drop = FALSE]
  b <- nrow(m)
  R <- t(apply(m, 1, rank))
  Rbar <- colMeans(R)
  P <- pair_index(k)
  dif <- unname(Rbar[P$j] - Rbar[P$i])
  q <- abs(dif) / sqrt(k * (k + 1) / (6 * b))
  p <- stats::ptukey(q * sqrt(2), nmeans = k, df = Inf, lower.tail = FALSE)
  data.frame(pair = paste(lev[P$j], "−", lev[P$i]), i = lev[P$i], j = lev[P$j], diff = dif, stat = q, p = p,
             stringsAsFactors = FALSE)
}

pairwise_wilcox <- function(y, g, paired = FALSE, adjust = "bonferroni") {
  g <- droplevels(as.factor(g)); lev <- levels(g); P <- pair_index(length(lev))
  p <- vapply(seq_along(P$i), function(x) {
    a <- y[g == lev[P$i[x]]]; b <- y[g == lev[P$j[x]]]
    tryCatch(suppressWarnings(stats::wilcox.test(b, a, paired = paired, exact = NULL)$p.value), error = function(e) NA_real_)
  }, 0)
  data.frame(pair = paste(lev[P$j], "−", lev[P$i]), i = lev[P$i], j = lev[P$j], p_unadj = p,
             p = stats::p.adjust(p, adjust), stringsAsFactors = FALSE)
}

nonparametric <- function(design, d, alpha = 0.05, seed = 1, names = NULL) {
  nm <- function(k) names[[k]] %||% k
  out <- list()
  if (design$key == "CRD") {
    kw <- stats::kruskal.test(y ~ treatment, data = d)
    n <- nrow(d)
    out$main <- list(name = "Kruskal-Wallis rank sum test", stat_name = "H (χ²)", stat = unname(kw$statistic),
                     df = unname(kw$parameter), p = kw$p.value, dist = "chisq",
                     effect_name = "ε²", effect = unname(kw$statistic) / ((n^2 - 1) / (n + 1)),
                     about = "Ranks all observations together and compares the average rank of each treatment.")
    out$posthoc <- list(
      list(key = "dunn", name = "Dunn test (Holm)", table = dunn_test(d$y, d$treatment, "holm"), diff_name = "Mean rank difference"),
      list(key = "wilcox", name = "Pairwise Wilcoxon (Bonferroni)", table = pairwise_wilcox(d$y, d$treatment), diff_name = NULL))
    out$ranks <- data.frame(level = levels(d$treatment), n = as.integer(table(d$treatment)),
                            mean_rank = as.numeric(tapply(rank(d$y), d$treatment, mean)),
                            median = as.numeric(tapply(d$y, d$treatment, stats::median)))
  } else if (design$key == "RCBD") {
    m <- stats::aggregate(y ~ treatment + block, data = d, FUN = mean)
    replicated <- nrow(m) < nrow(d)
    fr <- stats::friedman.test(y ~ treatment | block, data = m)
    b <- nlevels(droplevels(m$block)); k <- nlevels(m$treatment)
    out$main <- list(name = "Friedman rank sum test", stat_name = "Friedman χ²", stat = unname(fr$statistic),
                     df = unname(fr$parameter), p = fr$p.value, dist = "chisq",
                     effect_name = "Kendall's W", effect = unname(fr$statistic) / (b * (k - 1)),
                     about = "Ranks the responses within each block, so block-to-block differences cannot affect the result.",
                     note = if (replicated) "Some block and treatment cells have several observations; their mean is used, as the Friedman test needs one value per cell.")
    qd <- safe_test(stats::quade.test(y ~ treatment | block, data = m))
    if (!is.null(qd)) out$alt <- list(name = "Quade test", stat_name = "F", stat = unname(qd$statistic), p = qd$p.value,
                                      about = "Weights blocks by their range; often more powerful with few treatments.")
    ms <- m[order(m$block, m$treatment), ]
    out$posthoc <- list(
      list(key = "nemenyi", name = "Nemenyi", table = nemenyi_friedman(m$y, m$treatment, m$block), diff_name = "Mean rank difference"),
      list(key = "wilcox", name = "Pairwise Wilcoxon signed-rank (Bonferroni)", table = pairwise_wilcox(ms$y, ms$treatment, paired = TRUE), diff_name = NULL))
    R <- stats::ave(m$y, m$block, FUN = rank)
    out$rank_grid <- stats::xtabs(R ~ block + treatment, data = cbind(m, R = R))
    out$ranks <- data.frame(level = levels(m$treatment), n = as.integer(table(m$treatment)),
                            mean_rank = as.numeric(tapply(R, m$treatment, mean)),
                            median = as.numeric(tapply(m$y, m$treatment, stats::median)))
  } else {
    # Latin and Graeco-Latin squares: the class approach. Remove the blocking factors,
    # rank the residuals, then run a permutation test on the ranks.
    one <- function(target, blocks, label) {
      bm <- stats::lm(stats::as.formula(paste("y ~", paste(blocks, collapse = " + "))), data = d)
      rr <- rank(stats::residuals(bm))
      dd <- data.frame(rr = rr, g = d[[target]])
      set.seed(seed)
      ot <- coin::oneway_test(rr ~ g, data = dd, distribution = coin::approximate(nresample = 10000))
      st <- as.numeric(coin::statistic(ot)); pv <- as.numeric(coin::pvalue(ot))
      kw <- stats::kruskal.test(rr ~ g, data = dd)
      list(main = list(name = if (target == "treatment") "Rank test on residuals" else sprintf("Rank test on residuals (%s)", nm(target)), stat_name = "Statistic",
                       stat = st, df = NA, p = pv, dist = "perm",
                       about = sprintf("Removes %s, ranks what is left, and compares %s with a permutation test (10,000 resamples), as in the class script.",
                                       paste(vapply(blocks, nm, ""), collapse = ", "), nm(target)),
                       asym = list(stat = unname(kw$statistic), df = unname(kw$parameter), p = kw$p.value)),
           posthoc = list(
             list(key = "wilcox", name = "Pairwise Wilcoxon on ranks (Bonferroni)", table = pairwise_wilcox(rr, dd$g), diff_name = NULL),
             list(key = "dunn", name = "Dunn test on ranks (Holm)", table = dunn_test(rr, dd$g, "holm"), diff_name = "Mean rank difference")),
           ranks = data.frame(level = levels(dd$g), n = as.integer(table(dd$g)), mean_rank = as.numeric(tapply(rr, dd$g, mean)),
                              median = as.numeric(tapply(d$y, d[[target]], stats::median))))
    }
    blocks <- setdiff(doe_terms(design), "treatment")
    r1 <- one("treatment", blocks, "treatment")
    out <- r1
    if (design$key == "GLSD") out$greek <- one("greek", setdiff(doe_terms(design), "greek"), "greek")
  }
  out
}

# ---------- Everything together ----------

run_doe <- function(design, df, map, labels, settings) {
  alpha <- settings$alpha %||% 0.05; conf <- settings$conf %||% 0.95
  d <- prep_doe_data(design, df, map, labels)
  terms <- doe_terms(design)
  names <- list(treatment = settings$display$treatment %||% map$treatment)
  for (k in terms) names[[k]] <- settings$display[[k]] %||% map[[k]]
  form <- stats::as.formula(paste("y ~", paste(terms, collapse = " + ")))
  fit <- stats::aov(form, data = d)
  an <- anova_table(fit, d, terms, names, settings$sstype %||% "I", alpha)
  re <- relative_efficiency(design, an, d)
  asm <- assumption_checks(fit, d, alpha)
  ph <- lapply(setNames(compared_terms(design), compared_terms(design)), function(t)
    posthoc_all(fit, d, t, an$mse, an$dfe, alpha, conf,
                control = if (t == "treatment") label_of(labels$treatment, settings$control) else NULL,
                seed = settings$seed %||% 1))
  np <- tryCatch(nonparametric(design, d, alpha, settings$seed %||% 1, names), error = function(e) list(error = conditionMessage(e)))
  failed <- any(asm$status[c("normality", "variance")] == "fail")
  recommend <- switch(settings$assump %||% "auto", anova = "anova", nonpar = "nonpar", if (failed) "nonpar" else "anova")
  list(design = design, data = d, fit = fit, anova = an, re = re, assumptions = asm, posthoc = ph,
       nonpar = np, recommend = recommend, failed = failed, alpha = alpha, conf = conf, names = names,
       settings = settings, map = map, dropped = attr(d, "dropped"))
}

label_of <- function(L, code) {
  if (is.null(L) || is.null(code)) return(NULL)
  i <- match(code, L$code); if (is.na(i)) NULL else make.unique(L$label)[i]
}
