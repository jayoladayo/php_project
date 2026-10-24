# Problem Sets, Chapter 4: designs with blocking factors (randomized complete block,
# Latin square, Graeco-Latin square and balanced incomplete block designs).
# The ANOVA comes first and its assumptions are checked on the residuals after the test.
# When one fails, the course's rank test replaces it: Friedman for an RCBD, and for the
# squares and the BIBD the class approach (remove the blocking factors, rank what is left).
# Plain R with no Shiny, so it can be tested on its own.

PS_BLOCK_KINDS <- c("rcbd", "latin", "glsd", "bibd")
PS_MULTI_KINDS <- c("k", PS_BLOCK_KINDS)
ps_multi <- function(kind) length(kind) == 1 && !is.na(kind) && kind %in% PS_MULTI_KINDS

ps_param_name <- function(kind, test = "anova") switch(kind,
  k = if (identical(test, "welch")) "Welch ANOVA" else "One-way ANOVA (F test)",
  rcbd = "RCBD ANOVA (F test)", latin = "Latin square ANOVA (F test)", glsd = "Graeco-Latin square ANOVA (F test)",
  bibd = "BIBD ANOVA, treatments adjusted for blocks (F test)")
ps_model_name <- function(kind) switch(kind, k = "one-way", rcbd = "RCBD", latin = "Latin square", glsd = "Graeco-Latin square", bibd = "BIBD")
ps_np_test <- function(kind) switch(kind, k = "kw", rcbd = "friedman", "rankres")
ps_np_name <- function(kind) switch(kind, k = "Kruskal-Wallis test", rcbd = "Friedman test", "Rank test on the residuals")
ps_np_short <- function(kind) switch(kind, k = "Kruskal-Wallis", rcbd = "Friedman", "Rank test")

ps_block_terms <- function(kind) switch(kind, rcbd = c("treatment", "block"), bibd = c("block", "treatment"),
  latin = c("treatment", "row", "column"), glsd = c("treatment", "row", "column", "greek"))
ps_nuisance <- function(kind) setdiff(ps_block_terms(kind), "treatment")
ps_block_formula <- function(kind) stats::as.formula(paste("y ~", paste(ps_block_terms(kind), collapse = " + ")))

# The fitted model whose residuals are checked: the observed data only (no estimated values).
ps_model_fit <- function(kind, st, d = st$d) {
  if (kind %in% PS_BLOCK_KINDS) stats::lm(ps_block_formula(kind), data = d) else stats::lm(y ~ treatment, data = d)
}

ps_term_label <- function(kind, term, meta) {
  role <- switch(term, treatment = if (kind == "bibd") "treatments, adjusted for blocks" else "treatments",
                 block = "blocks", row = "rows", column = "columns", greek = "Greek letters")
  sprintf("%s (%s)", ucfirst(ps_lab(meta, term)), role)
}
ps_ss_name <- function(term) switch(term, treatment = "SS_Treatments", block = "SS_Blocks", row = "SS_Rows", column = "SS_Columns", greek = "SS_Greek")

# a, b, k, r and λ of an incomplete block design, and whether it is balanced.
ps_bibd_params <- function(d) {
  n <- unclass(table(d$treatment, d$block) > 0) * 1
  lam <- (n %*% t(n))[upper.tri(diag(nrow(n)))]
  list(a = nrow(n), b = ncol(n), r = unique(rowSums(n)), k = unique(colSums(n)), lambda = unique(lam),
       balanced = length(unique(rowSums(n))) == 1 && length(unique(colSums(n))) == 1 && length(unique(lam)) == 1, n = n)
}

# Least squares estimate of missing observations (RCBD and squares): iterate fitted values
# until they settle, which gives the textbook formula for one missing value.
ps_fill_missing <- function(kind, st) {
  miss <- st$missing
  if (is.null(miss) || !nrow(miss) || !kind %in% c("rcbd", "latin", "glsd")) return(NULL)
  d <- st$d; terms <- ps_block_terms(kind)
  dd <- data.frame(y = c(d$y, rep(mean(d$y), nrow(miss))))
  for (t in terms) dd[[t]] <- factor(c(as.character(d[[t]]), miss[[t]]), levels = levels(d[[t]]))
  idx <- nrow(d) + seq_len(nrow(miss)); form <- ps_block_formula(kind)
  for (it in 1:500) {
    new <- stats::fitted(stats::lm(form, data = dd))[idx]
    done <- max(abs(new - dd$y[idx])) < 1e-12
    dd$y[idx] <- new
    if (done) break
  }
  lines <- character(0)
  if (nrow(miss) == 1) {
    obs <- d; tot <- sum(obs$y)
    if (kind == "rcbd") {
      a <- nlevels(d$treatment); b <- nlevels(d$block)
      yi <- sum(obs$y[obs$treatment == miss$treatment]); yj <- sum(obs$y[obs$block == miss$block])
      lines <- c(sprintf("Missing value for %s %s in %s %s. Totals without it: y'ᵢ. = %s, y'.ⱼ = %s, y'.. = %s.",
                         ps_lab_plain("treatment", st), miss$treatment, ps_lab_plain("block", st), miss$block, ps_fmt(yi, 7), ps_fmt(yj, 7), ps_fmt(tot, 7)),
                 sprintf("x = (a y'ᵢ. + b y'.ⱼ − y'..) / ((a − 1)(b − 1)) = (%d × %s + %d × %s − %s) / (%d × %d) = %s",
                         a, ps_fmt(yi, 7), b, ps_fmt(yj, 7), ps_fmt(tot, 7), a - 1, b - 1, ps_fmt(dd$y[idx], 6)))
    } else if (kind == "latin") {
      p <- nlevels(d$treatment)
      yr <- sum(obs$y[obs$row == miss$row]); yt <- sum(obs$y[obs$treatment == miss$treatment]); yc <- sum(obs$y[obs$column == miss$column])
      lines <- c(sprintf("Missing value in row %s, column %s (treatment %s). Totals without it: y'ᵢ.. = %s, y'.ⱼ. = %s, y'..ₖ = %s, y'... = %s.",
                         miss$row, miss$column, miss$treatment, ps_fmt(yr, 7), ps_fmt(yt, 7), ps_fmt(yc, 7), ps_fmt(tot, 7)),
                 sprintf("x = [p(y'ᵢ.. + y'.ⱼ. + y'..ₖ) − 2y'...] / ((p − 2)(p − 1)) = [%d × (%s + %s + %s) − 2 × %s] / (%d × %d) = %s",
                         p, ps_fmt(yr, 7), ps_fmt(yt, 7), ps_fmt(yc, 7), ps_fmt(tot, 7), p - 2, p - 1, ps_fmt(dd$y[idx], 6)))
    }
  }
  est <- miss[, terms, drop = FALSE]; est$Estimate <- dd$y[idx]
  list(d = dd, est = est, n = nrow(miss), lines = lines)
}
ps_lab_plain <- function(key, st) st$roles[[key]] %||% key
ps_missing_txt <- function(miss, meta) paste(vapply(seq_len(nrow(miss)), function(i)
  paste(sprintf("%s %s", vapply(names(miss), function(k) ps_lab(meta, k), ""), unlist(miss[i, ])), collapse = ", "), ""), collapse = "; ")

ps_block_anova <- function(kind, fit, d, nmiss = 0) {
  a <- stats::anova(fit); terms <- ps_block_terms(kind)
  sse <- a["Residuals", "Sum Sq"]; dfe <- a["Residuals", "Df"] - nmiss; mse <- sse / dfe
  tab <- do.call(rbind, lapply(terms, function(t) {
    ss <- a[t, "Sum Sq"]; df <- a[t, "Df"]; ms <- ss / df; f <- ms / mse
    data.frame(term = t, df = df, ss = ss, ms = ms, F = f, p = stats::pf(f, df, dfe, lower.tail = FALSE), stringsAsFactors = FALSE)
  }))
  tab <- tab[order(tab$term != "treatment"), , drop = FALSE]; rownames(tab) <- NULL
  list(table = tab, sse = sse, dfe = dfe, mse = mse, sst = sum((d$y - mean(d$y))^2), dft = nrow(d) - 1 - nmiss)
}

ps_block_anova_df <- function(res) {
  an <- res$an; tab <- an$table
  data.frame(Source = c(unname(res$term_labels[tab$term]), "Error", "Total"), df = c(tab$df, an$dfe, an$dft),
             `Sum of squares` = ps_fmt(c(tab$ss, an$sse, an$sst), 6), `Mean square` = c(ps_fmt(tab$ms, 6), ps_fmt(an$mse, 6), ""),
             F0 = c(ps_fmt(tab$F, 5), "", ""), `P-value` = c(ps_p(tab$p), "", ""), check.names = FALSE)
}

# Pairwise comparisons from (adjusted) means with one standard error for every difference,
# in the same form as posthoc_all, for designs whose means need adjusting (BIBD).
ps_pairwise <- function(lev, means, se_d, dfe, alpha, conf) {
  k <- length(lev); P <- pair_index(k); m <- length(P$i)
  diff <- unname(means[P$j] - means[P$i]); se <- rep(se_d, m); tval <- diff / se
  base <- data.frame(pair = paste(lev[P$j], "−", lev[P$i]), i = lev[P$i], j = lev[P$j], diff = diff, se = se, stringsAsFactors = FALSE)
  tc <- stats::qt(1 - (1 - conf) / 2, dfe); qc <- stats::qtukey(conf, k, dfe)
  bc <- stats::qt(1 - (1 - conf) / (2 * m), dfe); sc <- sqrt((k - 1) * stats::qf(conf, k - 1, dfe))
  q <- abs(diff) / (se / sqrt(2))
  out <- list(
    lsd = transform(base, stat = tval, p = 2 * stats::pt(-abs(tval), dfe), lwr = diff - tc * se, upr = diff + tc * se),
    tukey = transform(base, stat = q, p = stats::ptukey(q, k, dfe, lower.tail = FALSE), lwr = diff - qc / sqrt(2) * se, upr = diff + qc / sqrt(2) * se),
    bonferroni = transform(base, stat = tval, p = pmin(1, m * 2 * stats::pt(-abs(tval), dfe)), lwr = diff - bc * se, upr = diff + bc * se),
    scheffe = transform(base, stat = tval^2 / (k - 1), p = stats::pf(tval^2 / (k - 1), k - 1, dfe, lower.tail = FALSE), lwr = diff - sc * se, upr = diff + sc * se))
  for (mth in names(out)) out[[mth]]$signif <- out[[mth]]$p < alpha
  letters <- lapply(out, function(t) cld_letters(lev, as.numeric(means), t$i[t$signif], t$j[t$signif]))
  list(methods = out, letters = letters, means = data.frame(level = lev, mean = as.numeric(means), stringsAsFactors = FALSE))
}

# How the sums of squares are worked out, with the numbers.
ps_block_formula_lines <- function(kind, d, an, bp = NULL, fill = NULL) {
  f <- function(x, k = 7) ps_fmt(x, k); N <- nrow(d); tot <- sum(d$y); cf <- tot^2 / N
  tab <- an$table; row <- function(t) tab[tab$term == t, ]
  tl <- function(t) sprintf("%s = %s on %d df", ps_ss_name(t), f(row(t)$ss), row(t)$df)
  head <- c(sprintf("N = %d,   y.. = %s,   ȳ.. = %s%s", N, f(tot), f(tot / N, 6), if (!is.null(fill)) " (with the estimated value)" else ""),
            sprintf("SS_T = Σ y² − y..²/N = %s − %s = %s", f(sum(d$y^2), 8), f(cf, 8), f(an$sst)))
  body <- switch(kind,
    rcbd = c(sprintf("SS_Treatments = Σ yᵢ.²/b − y..²/N = %s on a − 1 = %d df", f(row("treatment")$ss), row("treatment")$df),
             sprintf("SS_Blocks = Σ y.ⱼ²/a − y..²/N = %s on b − 1 = %d df", f(row("block")$ss), row("block")$df)),
    latin = c(sprintf("SS_Treatments = Σ y.ⱼ.²/p − y...²/N = %s on p − 1 = %d df", f(row("treatment")$ss), row("treatment")$df),
              sprintf("SS_Rows = Σ yᵢ..²/p − y...²/N = %s,   SS_Columns = Σ y..ₖ²/p − y...²/N = %s (each on %d df)", f(row("row")$ss), f(row("column")$ss), row("row")$df)),
    glsd = c(sprintf("SS_Latin letters (treatments) = %s,   SS_Greek letters = %s", f(row("treatment")$ss), f(row("greek")$ss)),
             sprintf("SS_Rows = %s,   SS_Columns = %s (each sum of squares: Σ total²/p − y....²/N, on p − 1 = %d df)", f(row("row")$ss), f(row("column")$ss), row("row")$df)),
    bibd = {
      bp <- bp %||% ps_bibd_params(d)
      Q <- tapply(d$y, d$treatment, sum) - as.numeric(bp$n %*% tapply(d$y, d$block, sum)) / bp$k
      c(sprintf("a = %d treatments, b = %d blocks, k = %s per block, r = %s, λ = %s", bp$a, bp$b, paste(bp$k, collapse = "/"), paste(bp$r, collapse = "/"), paste(bp$lambda, collapse = "/")),
        sprintf("Adjusted treatment totals Qᵢ = yᵢ. − (1/k) Σⱼ nᵢⱼ y.ⱼ:  %s", paste(sprintf("Q%s = %s", names(Q), f(Q, 5)), collapse = ",  ")),
        sprintf("SS_Treatments(adjusted) = k Σ Qᵢ² / (λa) = %s on a − 1 = %d df", f(row("treatment")$ss), row("treatment")$df),
        sprintf("SS_Blocks = Σ y.ⱼ²/k − y..²/N = %s on b − 1 = %d df", f(row("block")$ss), row("block")$df))
    })
  tr <- row("treatment")
  c(head, body,
    sprintf("SS_E = SS_T − (the other sums of squares) = %s on %d df%s", f(an$sse), an$dfe, if (!is.null(fill)) sprintf(" (%d lost to the estimated value)", fill$n) else ""),
    sprintf("F0 = MS_Treatments / MS_E = %s / %s = %s,   df = (%d, %d)", f(tr$ms, 6), f(an$mse, 6), f(tr$F, 5), tr$df, an$dfe))
}

# ---------- Information available (step 4) for blocked designs ----------

ps_info_block <- function(kind, st, meta, s) {
  d <- st$d; lv <- levels(d$treatment); a <- length(lv); nm <- function(k) ucfirst(ps_lab(meta, k))
  lvl <- function(k) sprintf("%s: %d levels (%s)", nm(k), nlevels(d[[k]]), ps_list(levels(d[[k]]), 8))
  bp <- if (kind == "bibd") ps_bibd_params(d)
  miss <- st$missing; nmiss <- if (is.null(miss)) 0 else nrow(miss)
  design <- switch(kind,
    rcbd = sprintf("Randomized complete block design: every %s appears once in each of the %d %s.", ps_lab(meta, "treatment"), nlevels(d$block), pl_word(ps_lab(meta, "block"))),
    latin = sprintf("%d × %d Latin square: each %s appears once in every row (%s) and every column (%s).", a, a, ps_lab(meta, "treatment"), ps_lab(meta, "row"), ps_lab(meta, "column")),
    glsd = sprintf("%d × %d Graeco-Latin square: Latin letters (%s) and Greek letters (%s) each appear once in every row and column, and every Latin letter meets every Greek letter once.",
                   a, a, ps_lab(meta, "treatment"), ps_lab(meta, "greek")),
    bibd = if (bp$balanced) sprintf("Balanced incomplete block design: a = %d treatments in b = %d blocks of k = %d; each treatment appears r = %d times and every pair of treatments meets λ = %d times.",
                                    bp$a, bp$b, bp$k, bp$r, bp$lambda)
           else sprintf("Incomplete block design: %d treatments in %d blocks, not every treatment appears in every block.", bp$a, bp$b))
  blocks <- switch(kind, rcbd = , bibd = lvl("block"), latin = paste(lvl("row"), lvl("column"), sep = "; "),
                   glsd = paste(lvl("row"), lvl("column"), sep = "; "))
  model <- switch(kind, rcbd = "yᵢⱼ = μ + τᵢ + βⱼ + εᵢⱼ (treatment i, block j)", bibd = "yᵢⱼ = μ + τᵢ + βⱼ + εᵢⱼ, only for the treatments that appear in block j",
                  latin = "yᵢⱼₖ = μ + αᵢ + τⱼ + βₖ + εᵢⱼₖ (row i, treatment j, column k)", glsd = "yᵢⱼₖₗ = μ + θᵢ + τⱼ + ωₖ + Ψₗ + εᵢⱼₖₗ (row, Latin letter, Greek letter, column)")
  facts <- data.frame(What = c("Design", "Response", if (kind == "glsd") "Latin letters (treatments)" else "Treatments", if (kind == "glsd") "Greek letters" else NULL,
                               if (kind %in% c("latin", "glsd")) "Blocking factors (rows and columns)" else "Blocks", "Observations", "Model", "Population distribution"),
    Information = c(design,
                    sprintf("%s (%s%s)", st$roles$response, ps_lab(meta, "response"), if (nzchar(meta$labels$unit %||% "")) paste0(", ", meta$labels$unit) else ""),
                    lvl("treatment"), if (kind == "glsd") lvl("greek") else NULL, blocks,
                    if (nmiss) sprintf("%d observed; %s missing (%s), estimated in step 5.", st$n, plural(nmiss, "value"), ps_missing_txt(miss, meta))
                    else sprintf("%d (%s)", st$n, if (kind == "bibd") "one in each block for the treatments it contains" else "one in every cell"),
                    model,
                    "Not stated. Normality, constant variance (Breusch-Pagan) and independence of the residuals are checked after the test (assumption checks)."),
    stringsAsFactors = FALSE)
  if (isTRUE(meta$random_blocks)) facts <- rbind(facts, data.frame(What = "Blocks are", Information = "Random: a sample from a larger population of blocks, so the block variance component is estimated.", stringsAsFactors = FALSE))
  list(facts = facts, normality = NULL, independence = NULL, equal_var = NULL, stated = FALSE, family_auto = "param", family = "param",
       family_reason = sprintf("The parametric test (%s) is always run first. If an assumption check fails afterwards, the %s is used.", ps_param_name(kind), ps_np_name(kind)),
       suggest = "anova", suggest_reason = sprintf("The blocking factors are removed from the error, so the treatments are compared with the %s.", ps_param_name(kind)),
       sigma_known = FALSE, bibd = bp)
}

# ---------- Tests (step 5 and the non-parametric restart) ----------

ps_run_block <- function(kind, test, st, meta, s) {
  a <- s$alpha; conf <- 1 - a; d <- st$d; lv <- levels(d$treatment)
  out <- list(kind = kind, test = test, settings = s, alpha = a, conf = conf, alt = "greater", ref = NA, levels = lv, control = meta$control,
              term_labels = vapply(stats::setNames(ps_block_terms(kind), ps_block_terms(kind)), function(t) ps_term_label(kind, t, meta), ""))
  if (test == "anova") {
    fill <- ps_fill_missing(kind, st)
    dd <- if (!is.null(fill)) fill$d else d
    fit <- stats::aov(ps_block_formula(kind), data = dd)
    an <- ps_block_anova(kind, fit, dd, if (!is.null(fill)) fill$n else 0)
    tr <- an$table[an$table$term == "treatment", ]
    t <- mk_test("anova", ps_param_name(kind), "param", "F", tr$F, tr$p, "Partial η²", tr$ss / (tr$ss + an$sse),
                 df = sprintf("%d, %d", tr$df, an$dfe), dist = "F", about = "Treatments compared after removing the blocking factors.")
    t$df1 <- tr$df; t$df2 <- an$dfe
    bp <- if (kind == "bibd") ps_bibd_params(dd)
    ph <- if (kind == "bibd") {
      cf <- stats::coef(stats::lm(y ~ block + treatment, data = dd, contrasts = list(treatment = "contr.sum", block = "contr.sum")))
      tau <- c(cf[grepl("^treatment", names(cf))], 0); tau[length(tau)] <- -sum(tau[-length(tau)])
      adj <- stats::setNames(unname(cf["(Intercept)"]) + tau, lv)
      se_d <- if (bp$balanced) sqrt(2 * bp$k * an$mse / (bp$lambda * bp$a)) else sqrt(2 * an$mse / mean(bp$r))
      ps_pairwise(lv, adj, se_d, an$dfe, a, conf)
    } else posthoc_all(fit, dd, "treatment", an$mse, an$dfe, a, conf, control = meta$control, dunnett = FALSE)
    out <- c(out, list(t = t, fit = fit, d = dd, an = an, ph_all = ph, ci_tab = ph$methods$tukey, ci_name = "Tukey", fill = fill, bibd = bp,
                       formula = c(if (!is.null(fill)) fill$lines, ps_block_formula_lines(kind, dd, an, bp, fill))))
    out$anova_df <- ps_block_anova_df(out)
    if (!is.null(fill) && kind == "rcbd") {
      ex <- stats::anova(stats::lm(y ~ block + treatment, data = d))
      out$exact <- data.frame(Analysis = c("Estimated missing value (approximate)", "Exact (general regression significance test)"),
                              `SS_Treatments` = ps_fmt(c(tr$ss, ex["treatment", "Sum Sq"]), 6), df = c(sprintf("%d, %d", tr$df, an$dfe), sprintf("%d, %d", ex["treatment", "Df"], ex["Residuals", "Df"])),
                              F0 = ps_fmt(c(tr$F, ex["treatment", "F value"]), 5), `P-value` = ps_p(c(tr$p, ex["treatment", "Pr(>F)"])), check.names = FALSE)
    }
    if (isTRUE(meta$random_blocks)) {
      msb <- an$table$ms[an$table$term == "block"]; nb <- nlevels(dd$treatment)
      out$varcomp <- list(msb = msb, mse = an$mse, a = nb, value = max(0, (msb - an$mse) / nb), raw = (msb - an$mse) / nb)
    }
  } else if (test == "friedman") {
    m <- stats::aggregate(y ~ treatment + block, data = d, FUN = mean)
    full <- names(which(table(m$block) == nlevels(d$treatment))); dropped <- setdiff(levels(d$block), full)
    m <- m[m$block %in% full, ]; m$block <- droplevels(m$block); m$treatment <- droplevels(m$treatment)
    b <- nlevels(m$block); k <- nlevels(m$treatment)
    fr <- stats::friedman.test(y ~ treatment | block, data = m)
    R <- stats::ave(m$y, m$block, FUN = rank); Ri <- tapply(R, m$treatment, sum); chi0 <- 12 / (b * k * (k + 1)) * sum(Ri^2) - 3 * b * (k + 1)
    t <- mk_test("friedman", "Friedman test", "nonpar", "χ²r", fr$statistic, fr$p.value, "Kendall's W", unname(fr$statistic) / (b * (k - 1)),
                 df = k - 1, dist = "chisq", about = "Ranks the responses within each block.")
    t$df1 <- k - 1
    nem <- nemenyi_friedman(m$y, m$treatment, m$block); nem$signif <- nem$p < a
    out <- c(out, list(t = t, d = m, centre = stats::setNames(as.numeric(tapply(R, m$treatment, mean)), levels(m$treatment)), np_ph = nem,
                       formula = c(if (length(dropped)) sprintf("Blocks with a missing value are left out (%s), as the Friedman test needs every treatment in every block.", paste(dropped, collapse = ", ")),
                                   sprintf("Rank the %d treatments within each of the b = %d blocks, then add the ranks of each treatment: %s", k, b,
                                           paste(sprintf("R(%s) = %s", names(Ri), ps_fmt(Ri, 5)), collapse = ",  ")),
                                   sprintf("χ²r = 12 / (b k (k + 1)) × Σ Rᵢ² − 3b(k + 1) = %s", ps_fmt(chi0, 5)),
                                   if (abs(chi0 - unname(fr$statistic)) > 1e-8) sprintf("Corrected for ties within blocks: χ²r = %s", ps_fmt(unname(fr$statistic), 5)),
                                   sprintf("df = k − 1 = %d;   χ²r is compared with the χ² distribution", k - 1)),
                       rank_table = data.frame(Treatment = levels(m$treatment), Median = ps_fmt(tapply(m$y, m$treatment, stats::median)),
                                               `Rank sum` = ps_fmt(Ri, 6), `Mean rank` = ps_fmt(Ri / b, 4), check.names = FALSE),
                       rank_title = "Ranks within blocks"))
  } else {
    bl <- ps_nuisance(kind)
    bm <- stats::lm(stats::as.formula(paste("y ~", paste(bl, collapse = " + "))), data = d)
    rr <- rank(stats::residuals(bm)); g <- d$treatment
    kw <- stats::kruskal.test(rr ~ g); k <- nlevels(g)
    perm <- tryCatch({ set.seed(1); ot <- coin::oneway_test(rr ~ g, data = data.frame(rr = rr, g = g), distribution = coin::approximate(nresample = 10000)); as.numeric(coin::pvalue(ot)) },
                     error = function(e) NA_real_)
    t <- mk_test("rankres", "Rank test on the residuals", "nonpar", "H", kw$statistic, kw$p.value, NULL, NA, df = k - 1, dist = "chisq",
                 about = "Removes the blocking factors, ranks the residuals and compares the treatments.")
    t$df1 <- k - 1
    dn <- dunn_test(rr, g, "holm"); dn$signif <- dn$p < a
    out <- c(out, list(t = t, d = d, centre = stats::setNames(as.numeric(tapply(rr, g, mean)), levels(g)), np_ph = dn, perm_p = perm,
                       formula = c(sprintf("Fit the blocking factors only (y ~ %s) and rank the N = %d residuals.", paste(vapply(bl, function(x) ps_lab(meta, x), ""), collapse = " + "), length(rr)),
                                   sprintf("H = 12 / (N(N + 1)) × Σ Rᵢ²/nᵢ − 3(N + 1) on the ranked residuals = %s,   df = k − 1 = %d", ps_fmt(unname(kw$statistic), 5), k - 1),
                                   if (is.finite(perm)) sprintf("Permutation p-value (10,000 resamples, as in the class script): %s", ps_p(perm))),
                       rank_table = data.frame(Treatment = levels(g), Median = ps_fmt(tapply(d$y, g, stats::median)),
                                               `Mean rank of the residuals` = ps_fmt(tapply(rr, g, mean), 4), check.names = FALSE),
                       rank_title = "Ranks of the residuals by treatment"))
  }
  t <- out$t
  out$dist <- t$dist; out$df1 <- t$df1 %||% NA; out$df2 <- t$df2 %||% NA
  out$p <- t$p; out$reject <- !is.na(t$p) && t$p < a
  out
}

# ---------- Displays ----------

# Treatment means against a t distribution scaled by √(MSE/n) (Montgomery, Section 3.5.3).
ps_plot_scaled_t <- function(res) {
  ph <- res$ph_all; mse <- (res$an %||% res$anova)$mse; dfe <- (res$an %||% res$anova)$dfe
  n <- if (!is.null(res$bibd)) mean(res$bibd$r) else length(res$d$y %||% res$r$data$y) / length(res$levels)
  s <- sqrt(mse / n); m <- stats::setNames(ph$means$mean, ph$means$level); ctr <- mean(m)
  x <- seq(min(m, ctr - 4 * s) - s, max(m, ctr + 4 * s) + s, length.out = 400)
  dens <- data.frame(x = x, y = stats::dt((x - ctr) / s, dfe) / s)
  pts <- data.frame(x = m, lab = names(m))
  ggplot(dens, aes(x, y)) + geom_area(fill = GREEN, alpha = .12) + geom_line(colour = GREEN, linewidth = 1) +
    geom_point(data = pts, aes(x, 0), colour = PETROL, size = 3.4, inherit.aes = FALSE) +
    geom_text(data = pts, aes(x, 0, label = lab), vjust = -1.2, colour = INK, size = 3.4, inherit.aes = FALSE) +
    labs(x = sprintf("Treatment means (scale √(MSE/n) = %s)", ps_fmt(s, 4)), y = NULL) + theme_sds() +
    theme(axis.text.y = element_blank())
}
