# Problem Sets, Chapter 5: factorial designs (two or three factors), run in a completely
# randomized layout, in blocks (RCBD), or in a Latin or Graeco-Latin square.
# Follows the lecturer's Topic 6 notes:
#   ANOVA          main effects A, B (C) and the interactions, each with its F test
#   post-hoc       main effects: Fisher LSD, Tukey, Bonferroni, Scheffé (Dunnett against a control);
#                  interactions: simple effects (levels of one factor within each level of the other)
#   assumptions    normality, constant variance and independence of the residuals
#   if one fails   CRD: Kruskal-Wallis for A, B and A×B, then Dunn
#                  RCBD: Friedman (ranks within blocks), then Nemenyi or Wilcoxon signed-rank
#                  Latin and Graeco-Latin squares: Quade (rank the residuals), then Dunn or Wilcoxon
# Plain R with no Shiny, so it can be tested on its own.

PS_FACT_ROLES <- c("treatment", "factorB", "factorC")

ps_fact_nuis <- function(kind) switch(kind, fact = character(0), factb = "block", factl = c("row", "column"), factg = c("row", "column", "greek"))

ps_fact_formula <- function(kind, st) {
  f <- st$ffacs
  rhs <- if (isTRUE(st$unrep)) paste(f, collapse = " + ") else paste(f, collapse = " * ")
  stats::as.formula(paste("y ~", paste(c(ps_fact_nuis(kind), rhs), collapse = " + ")))
}

# Which factorial layout the data form, or NA when it cannot be analysed.
ps_fact_kind <- function(st) {
  d <- st$d; f <- st$ffacs; has <- function(k) k %in% st$facs
  if (any(vapply(f, function(r) nlevels(d[[r]]) < 2, TRUE))) return(NA_character_)
  if (any(table(d[f]) == 0)) return(NA_character_)
  extra <- intersect(c("block", "row", "column", "greek"), st$facs)
  kind <- if (has("row") && has("column")) { if (has("greek")) "factg" else "factl" }
          else if (!length(extra)) "fact" else if (identical(extra, "block")) "factb" else return(NA_character_)
  X <- tryCatch(stats::model.matrix(ps_fact_formula(kind, st), data = d), error = function(e) NULL)
  if (is.null(X) || nrow(d) - qr(X)$rank < 1) return(NA_character_)
  kind
}

# The effects tested, in textbook order: A, B, (C), AB, (AC, BC, ABC).
ps_fact_effects <- function(st, meta) {
  f <- st$ffacs; codes <- LETTERS[seq_along(f)]
  ix <- unlist(lapply(seq_along(f), function(k) utils::combn(seq_along(f), k, simplify = FALSE)), recursive = FALSE)
  out <- lapply(ix, function(i) list(code = paste(codes[i], collapse = ""), roles = f[i], term = paste(f[i], collapse = ":"),
                                     label = paste(vapply(f[i], function(r) ucfirst(ps_lab(meta, r)), ""), collapse = " × "), main = length(i) == 1))
  stats::setNames(out, vapply(out, `[[`, "", "code"))
}
ps_fact_unrep_codes <- function(ef) names(ef)[vapply(ef, function(e) e$main, TRUE)]
ps_fact_flabs <- function(st, meta) vapply(stats::setNames(st$ffacs, st$ffacs), function(r) ucfirst(ps_lab(meta, r)), "")

# ---------- Hypotheses (step 2 and step 9) ----------

ps_fact_hypotheses <- function(st, meta, family = "param") {
  d <- st$d; resp <- ps_lab(meta, "response"); np <- identical(family, "nonpar")
  centre <- if (np) "median" else "mean"; ef <- ps_fact_effects(st, meta)
  greek <- c("α", "β", "γ"); idxl <- c("i", "j", "k")
  lc <- function(r) tolower(ps_lab(meta, r))
  out <- lapply(ef, function(e) {
    ix <- match(e$roles, st$ffacs)
    if (e$main) {
      L <- nlevels(d[[e$roles]]); lab <- lc(e$roles)
      sym <- if (np) "η" else greek[ix]
      lev <- if (L <= 4) paste(sprintf("%s%d", sym, seq_len(L)), collapse = " = ") else sprintf("%s1 = … = %s%d", sym, sym, L)
      list(h0 = sprintf("%s has no effect: the %s %s is the same at every level of %s (%s).", ucfirst(lab), centre, resp, lab, ps_list(levels(d[[e$roles]]), 5)),
           h1 = sprintf("%s affects the %s %s: at least one level of %s gives a different %s.", ucfirst(lab), centre, resp, lab, centre),
           h0s = if (np) sprintf("H0: %s", lev) else sprintf("H0: %s = 0", lev),
           h1s = if (np) sprintf("H1: ηᵢ ≠ ηⱼ for at least one pair") else sprintf("H1: %sᵢ ≠ 0 for at least one level", greek[ix]))
    } else {
      labs <- vapply(e$roles, lc, ""); two <- length(labs) == 2
      sym <- paste0("(", paste(greek[ix], collapse = ""), ")", paste(idxl[seq_along(ix)], collapse = ""))
      list(h0 = if (two) sprintf("There is no interaction between %s and %s: the effect of %s on the %s %s is the same at every level of %s.", labs[1], labs[2], labs[1], centre, resp, labs[2])
                else sprintf("There is no %s interaction: the %s × %s interaction on the %s %s is the same at every level of %s.", paste(labs, collapse = " × "), labs[1], labs[2], centre, resp, labs[3]),
           h1 = if (two) sprintf("%s and %s interact: the effect of %s on the %s %s depends on the level of %s.", ucfirst(labs[1]), labs[2], labs[1], centre, resp, labs[2])
                else sprintf("The %s interaction is present: the %s × %s interaction on the %s %s depends on the level of %s.", paste(labs, collapse = " × "), labs[1], labs[2], centre, resp, labs[3]),
           h0s = if (np) sprintf("H0: no %s interaction", paste(e$code)) else sprintf("H0: %s = 0 for all levels", sym),
           h1s = if (np) sprintf("H1: %s interaction", paste(e$code)) else sprintf("H1: %s ≠ 0 for at least one combination", sym))
    }
  })
  if (isTRUE(st$unrep)) out <- out[ps_fact_unrep_codes(ef)]
  out
}

# ---------- Information available (step 4) ----------

ps_fact_model_text <- function(kind, st) {
  n <- length(st$ffacs)
  core <- if (isTRUE(st$unrep)) "yᵢⱼ = μ + αᵢ + βⱼ + εᵢⱼ (no interaction term: one observation per combination)"
          else if (n == 2) "yᵢⱼₖ = μ + αᵢ + βⱼ + (αβ)ᵢⱼ + εᵢⱼₖ"
          else "yᵢⱼₖₗ = μ + αᵢ + βⱼ + γₖ + (αβ)ᵢⱼ + (αγ)ᵢₖ + (βγ)ⱼₖ + (αβγ)ᵢⱼₖ + εᵢⱼₖₗ"
  switch(kind, fact = core, factb = paste(core, "plus a block effect"), factl = paste(core, "plus row and column effects"),
         factg = paste(core, "plus row, column and Greek-letter effects"))
}

ps_info_fact <- function(kind, st, meta, s) {
  d <- st$d; f <- st$ffacs; nf <- length(f); lv <- vapply(f, function(r) nlevels(d[[r]]), 0L)
  ncell <- prod(lv); r <- unique(st$cellreps)
  flab <- function(r) ucfirst(ps_lab(meta, r))
  design <- switch(kind,
    fact = sprintf("Factorial design in a completely randomized layout: the %d factors are crossed, so every combination of their levels is a treatment.", nf),
    factb = sprintf("Factorial design in %d blocks (RCBD): every combination of the factor levels appears once in each block.", nlevels(d$block)),
    factl = sprintf("Factorial design in a Latin square: the %d factors are crossed and the treatment combinations are placed by row and column.", nf),
    factg = sprintf("Factorial design in a Graeco-Latin square: the %d factors are crossed and the combinations are placed by row, column and Greek letter.", nf))
  fl <- vapply(seq_along(f), function(i) sprintf("%s · %s: %d levels (%s)", LETTERS[i], flab(f[i]), lv[i], ps_list(levels(d[[f[i]]]), 6)), "")
  reps <- if (isTRUE(st$unrep)) "1 observation per combination: there is no replicate, so the interaction cannot be separated from error."
          else if (kind == "factb") sprintf("%d (one per block)", nlevels(d$block))
          else if (length(r) == 1) sprintf("%d per combination (balanced)", r) else sprintf("%d to %d per combination (unbalanced; sequential sums of squares)", min(r), max(r))
  nuis <- intersect(c("block", "row", "column", "greek"), st$facs)
  rows <- list(c("Design", design), c("Response", sprintf("%s (%s%s)", st$roles$response, ps_lab(meta, "response"), if (nzchar(meta$labels$unit %||% "")) paste0(", ", meta$labels$unit) else "")),
               c("Factors", paste(fl, collapse = "; ")),
               c("Treatment combinations", sprintf("%s = %d", paste(lv, collapse = " × "), ncell)), c("Replicates", reps))
  if (length(nuis)) rows[[length(rows) + 1]] <- c("Blocking factors", paste(vapply(nuis, function(k) sprintf("%s: %d levels", flab(k), nlevels(d[[k]])), ""), collapse = "; "))
  rows[[length(rows) + 1]] <- c("Observations", as.character(st$n))
  rows[[length(rows) + 1]] <- c("Model", ps_fact_model_text(kind, st))
  if (isTRUE(meta$random_blocks)) rows[[length(rows) + 1]] <- c("Blocks are", "Random: a sample from a larger population of blocks, so the block variance component is estimated.")
  rows[[length(rows) + 1]] <- c("Population distribution", "Not stated. Normality, constant variance (Breusch-Pagan) and independence of the residuals are checked after the test (assumption checks).")
  facts <- data.frame(What = vapply(rows, `[`, "", 1), Information = vapply(rows, `[`, "", 2), stringsAsFactors = FALSE)
  list(facts = facts, normality = NULL, independence = NULL, equal_var = NULL, stated = FALSE, family_auto = "param", family = "param",
       family_reason = sprintf("The parametric test (%s) is always run first, for the main effect of each factor and for their interaction%s. If an assumption check fails afterwards, the %s is used.",
                               ps_param_name(kind), if (nf > 2) "s" else "", ps_np_name(kind)),
       suggest = "anova", suggest_reason = "Every effect is tested against the error mean square of the factorial model.", sigma_known = FALSE)
}

# ---------- The tests ----------

ps_fact_effects_df <- function(eff, np_stat = NULL) {
  stat <- if (is.null(np_stat)) "F0" else np_stat
  out <- data.frame(Effect = sprintf("%s (%s)", eff$label, eff$code), stat = ps_fmt(eff$stat, 5),
                    df = if (is.null(np_stat)) paste0(eff$df, ", ", eff$df2) else as.character(eff$df),
                    crit = ps_fmt(eff$crit, 4), p = ps_p(eff$p), Result = ifelse(eff$p < eff$alpha, "Significant", "Not significant"), stringsAsFactors = FALSE)
  names(out) <- c("Effect", stat, "df", if (is.null(np_stat)) "F critical" else "χ² critical", "P-value", "Result")
  out
}

ps_run_fact <- function(kind, test, st, meta, s) {
  a <- s$alpha; d <- st$d; ef <- ps_fact_effects(st, meta)
  out <- list(kind = kind, test = test, settings = s, alpha = a, conf = 1 - a, alt = "greater", ref = NA, levels = levels(d$treatment),
              control = meta$control, ef = ef, ffacs = st$ffacs, d = d, unrep = isTRUE(st$unrep), flabs = ps_fact_flabs(st, meta),
              resp = ps_lab(meta, "response"), unit = meta$labels$unit %||% "")
  out <- if (test == "anova") ps_fact_anova(out, kind, st, meta) else ps_fact_rank(out, kind, test, st, meta)
  eff <- out$effects; best <- eff[which.min(eff$p), ]
  out$p <- best$p; out$reject <- any(eff$p < a)
  out$dist <- if (test == "anova") "F" else "chisq"; out$df1 <- best$df; out$df2 <- if (test == "anova") best$df2 else NA
  out$summary_msg <- paste0(if (test == "anova") "Factorial ANOVA: " else paste0(ps_np_short(kind), ": "),
                            paste(sprintf("%s %s", eff$code, ps_pe(eff$p)), collapse = ";  "), ".")
  out
}

ps_fact_anova <- function(out, kind, st, meta) {
  a <- out$alpha; conf <- out$conf; d <- st$d; ef <- out$ef; unrep <- out$unrep; nuis <- ps_fact_nuis(kind)
  fit <- stats::aov(ps_fact_formula(kind, st), data = d)
  at <- stats::anova(fit)
  sse <- at["Residuals", "Sum Sq"]; dfe <- at["Residuals", "Df"]; mse <- sse / dfe; sst <- sum((d$y - mean(d$y))^2)
  erow <- function(cd, e, df, df2, ss, ms, f) data.frame(code = cd, term = e$term, label = e$label, main = e$main, df = df, df2 = df2, ss = ss, ms = ms,
                                                       stat = f, p = stats::pf(f, df, df2, lower.tail = FALSE), crit = stats::qf(1 - a, df, df2), alpha = a, stringsAsFactors = FALSE)
  nonadd <- NULL
  rows <- lapply(names(ef), function(cd) {
    e <- ef[[cd]]
    if (unrep && !e$main) {
      Y <- tapply(d$y, list(d$treatment, d$factorB), sum); rm <- rowMeans(Y); cm <- colMeans(Y); gm <- mean(Y)
      ssn <- sum(outer(rm - gm, cm - gm) * Y)^2 / (sum((rm - gm)^2) * sum((cm - gm)^2)); dfr <- dfe - 1
      nonadd <<- list(ss = ssn, df = 1, rem_ss = sse - ssn, rem_df = dfr, rem_ms = (sse - ssn) / dfr)
      return(erow(cd, e, 1, dfr, ssn, ssn, ssn / ((sse - ssn) / dfr)))
    }
    r <- at[e$term, ]
    erow(cd, e, r[["Df"]], dfe, r[["Sum Sq"]], r[["Mean Sq"]], r[["Mean Sq"]] / mse)
  })
  effects <- do.call(rbind, rows); rownames(effects) <- NULL
  nrows <- do.call(rbind, lapply(nuis, function(t) { r <- at[t, ]; f <- r[["Mean Sq"]] / mse
    data.frame(term = t, label = sprintf("%s (%s)", ucfirst(ps_lab(meta, t)), switch(t, block = "blocks", row = "rows", column = "columns", greek = "Greek letters")),
               df = r[["Df"]], ss = r[["Sum Sq"]], ms = r[["Mean Sq"]], stat = f, p = stats::pf(f, r[["Df"]], dfe, lower.tail = FALSE), stringsAsFactors = FALSE) }))
  main_eff <- effects[!(unrep & !effects$main), , drop = FALSE]
  tabdf <- data.frame(Source = c(if (!is.null(nrows)) nrows$label, sprintf("%s (%s)", main_eff$label, main_eff$code), "Error", "Total"),
                      df = c(if (!is.null(nrows)) nrows$df, main_eff$df, dfe, nrow(d) - 1),
                      `Sum of squares` = ps_fmt(c(if (!is.null(nrows)) nrows$ss, main_eff$ss, sse, sst), 6),
                      `Mean square` = c(if (!is.null(nrows)) ps_fmt(nrows$ms, 6), ps_fmt(main_eff$ms, 6), ps_fmt(mse, 6), ""),
                      F0 = c(if (!is.null(nrows)) ps_fmt(nrows$stat, 5), ps_fmt(main_eff$stat, 5), "", ""),
                      `P-value` = c(if (!is.null(nrows)) ps_p(nrows$p), ps_p(main_eff$p), "", ""), check.names = FALSE)
  ph_main <- lapply(Filter(function(e) e$main, ef), function(e) posthoc_all(fit, d, e$roles, mse, dfe, a, conf, dunnett = FALSE))
  an <- list(table = effects, nuis = nrows, sse = sse, dfe = dfe, mse = mse, sst = sst, dft = nrow(d) - 1)
  lines <- ps_fact_formula_lines(kind, st, d, effects, nrows, sse, dfe, mse, sst, nonadd)
  vc <- NULL
  if (kind == "factb" && !is.null(nrows)) {
    msb <- nrows$ms[nrows$term == "block"]; m <- nlevels(d$block); T <- prod(vapply(st$ffacs, function(r) nlevels(d[[r]]), 0L))
    vc <- list(msb = msb, mse = mse, ncell = T, m = m, raw = (msb - mse) / T, value = max(0, (msb - mse) / T), F = msb / mse,
               p = stats::pf(msb / mse, m - 1, dfe, lower.tail = FALSE), re = ((m - 1) * msb + m * (T - 1) * mse) / ((m * T - 1) * mse), df = m - 1)
  }
  best <- effects[which.min(effects$p), ]
  t <- mk_test("anova", ps_param_name(kind), "param", "F", best$stat, best$p, NULL, NA, df = sprintf("%d, %s", best$df, format(best$df2)), dist = "F",
               about = "Each effect is tested against the error mean square.")
  t$df1 <- best$df; t$df2 <- best$df2
  c(out, list(t = t, fit = fit, effects = effects, an = an, anova_df = tabdf, effects_df = ps_fact_effects_df(effects), ph_main = ph_main, formula = lines,
              varcomp = vc, nonadd = nonadd, nonadd_df = if (!is.null(nonadd)) data.frame(Source = c("Nonadditivity (Tukey, 1 df)", "Remainder (error)", "Total of the two"),
                df = c(1, nonadd$rem_df, 1 + nonadd$rem_df), `Sum of squares` = ps_fmt(c(nonadd$ss, nonadd$rem_ss, sse), 6),
                `Mean square` = c(ps_fmt(nonadd$ss, 6), ps_fmt(nonadd$rem_ms, 6), ""), F0 = c(ps_fmt(effects$stat[!effects$main], 5), "", ""),
                `P-value` = c(ps_p(effects$p[!effects$main]), "", ""), check.names = FALSE)))
}

ps_fact_formula_lines <- function(kind, st, d, effects, nrows, sse, dfe, mse, sst, nonadd) {
  f <- function(x, k = 7) ps_fmt(x, k); N <- nrow(d); nf <- length(st$ffacs); lv <- vapply(st$ffacs, function(r) nlevels(d[[r]]), 0L)
  head <- c(sprintf("N = %d observations; %s treatment combinations; ȳ... = %s", N, paste(lv, collapse = " × "), f(mean(d$y), 6)),
            sprintf("SS_T = Σ y² − y...²/N = %s − %s = %s", f(sum(d$y^2), 8), f(sum(d$y)^2 / N, 8), f(sst)))
  def <- if (isTRUE(st$unrep)) "SS_A = b Σ (ȳᵢ. − ȳ..)²,   SS_B = a Σ (ȳ.ⱼ − ȳ..)²,   SS_E = SS_T − SS_A − SS_B"
         else if (nf == 2 && kind %in% c("fact", "factb")) "SS_A = r b Σ (ȳᵢ.. − ȳ...)²,   SS_B = r a Σ (ȳ.ⱼ. − ȳ...)²,   SS_AB = r Σ Σ (ȳᵢⱼ. − ȳᵢ.. − ȳ.ⱼ. + ȳ...)²,   SS_E = SS_T − (the other sums of squares)"
         else "Each sum of squares comes from the ANOVA decomposition of the effect means (A, B, C, AB, AC, BC, ABC); SS_E = SS_T − (the other sums of squares)"
  eff <- sprintf("SS_%s = %s on %d df,   MS_%s = %s,   F0 = MS_%s / MS_E = %s / %s = %s", effects$code, f(effects$ss), effects$df, effects$code, f(effects$ms, 6), effects$code,
                 f(effects$ms, 6), f(ifelse(effects$main | !isTRUE(st$unrep), mse, nonadd$rem_ms %||% mse), 6), f(effects$stat, 5))
  blk <- if (!is.null(nrows)) sprintf("SS_%s = %s on %d df,   MS = %s", sub(" .*", "", nrows$label), f(nrows$ss), nrows$df, f(nrows$ms, 6))
  c(head, def, blk, eff, sprintf("SS_E = %s on %d df,   MS_E = SS_E / df = %s", f(sse), dfe, f(mse, 6)))
}

# ---------- Rank tests (the non-parametric restart) ----------

# Sums of squares per term from an anova on a response, with the effect rows named by term.
ps_fact_ss <- function(y, d, fm) {
  dd <- d; dd$.r <- y
  at <- suppressWarnings(stats::anova(stats::lm(stats::update(fm, .r ~ .), data = dd)))
  at[rownames(at) != "Residuals", c("Df", "Sum Sq"), drop = FALSE]
}

ps_fact_rank <- function(out, kind, test, st, meta) {
  a <- out$alpha; d <- st$d; ef <- out$ef; unrep <- out$unrep
  codes <- if (unrep) ps_fact_unrep_codes(ef) else names(ef)
  fm_eff <- stats::as.formula(paste("~", paste(c(if (unrep) paste(st$ffacs, collapse = " + ") else paste(st$ffacs, collapse = " * ")), collapse = " + ")))
  stat_name <- switch(test, kwf = "H", friedmanf = "χ²r", quadef = "H"); N <- nrow(d)
  rank_rows <- NULL; extra <- list(); lines <- character(0)
  if (test == "kwf") {
    rk <- rank(d$y); mst <- sum((rk - mean(rk))^2) / (N - 1)
    ss <- ps_fact_ss(rk, d, stats::update(fm_eff, y ~ .)); H <- ss[, "Sum Sq"] / mst; names(H) <- rownames(ss)
    units <- d; units$score <- rk; extra$units <- units; extra$N <- N
    lines <- c(sprintf("Rank all N = %d observations together (ties get the average rank); the variance of the ranks is MS_T = %s.", N, ps_fmt(mst, 6)),
               "H for an effect = (sum of squares of the ranks for that effect) / MS_T, compared with the χ² distribution with the effect's df.",
               "For a main effect this is the Kruskal-Wallis statistic H = 12 / (N(N + 1)) Σ Rᵢ²/nᵢ − 3(N + 1) on the levels of that factor (no ties).")
    rank_src <- list(kind = "obs", score = rk)
  } else if (test == "quadef") {
    bm <- stats::lm(stats::as.formula(paste("y ~", paste(ps_fact_nuis(kind), collapse = " + "))), data = d)
    rk <- rank(stats::residuals(bm)); mst <- sum((rk - mean(rk))^2) / (N - 1)
    ss <- ps_fact_ss(rk, d, stats::update(fm_eff, y ~ .)); H <- ss[, "Sum Sq"] / mst; names(H) <- rownames(ss)
    units <- d; units$score <- rk; extra$units <- units; extra$N <- N
    set.seed(1); B <- 2000; X <- stats::model.matrix(stats::update(fm_eff, y ~ .), data = d); qx <- qr(X); asg <- attr(X, "assign")
    termlab <- attr(stats::terms(stats::update(fm_eff, y ~ .)), "term.labels")
    cnt <- vapply(seq_len(B), function(b) { q <- qr.qty(qx, sample(rk))[seq_len(qx$rank)]; vapply(seq_along(termlab), function(i) sum(q[asg[qx$pivot[seq_len(qx$rank)]] == i]^2), 0) }, numeric(length(termlab)))
    obs <- vapply(seq_along(termlab), function(i) sum(qr.qty(qx, rk)[seq_len(qx$rank)][asg[qx$pivot[seq_len(qx$rank)]] == i]^2), 0)
    permp <- stats::setNames(rowMeans(cnt >= obs - 1e-9), termlab)
    extra$perm <- permp
    lines <- c(sprintf("Fit the blocking factors only (%s) and rank the N = %d residuals; MS_T = variance of these ranks = %s.", paste(vapply(ps_fact_nuis(kind), function(r) ps_lab(meta, r), ""), collapse = ", "), N, ps_fmt(mst, 6)),
               "H for an effect = (sum of squares of the ranked residuals for that effect) / MS_T, compared with the χ² distribution (class approach: Quade).",
               sprintf("Permutation p-values (%d resamples of the ranked residuals): %s", B, paste(sprintf("%s %s", codes, ps_p(permp[vapply(codes, function(cd) ef[[cd]]$term, "")])), collapse = ";  ")))
    rank_src <- list(kind = "obs", score = rk)
  } else {
    cells <- interaction(d[st$ffacs], drop = TRUE, lex.order = FALSE)
    cell_tab <- unique(d[st$ffacs]); cell_tab$cell <- interaction(cell_tab[st$ffacs], drop = TRUE)
    ym <- tapply(d$y, list(d$block, cells), mean); ym <- ym[stats::complete.cases(ym), , drop = FALSE]; m <- nrow(ym); Tn <- ncol(ym)
    R <- t(apply(ym, 1, rank)); Rcell <- colSums(R)
    ct <- cell_tab[match(colnames(ym), as.character(cell_tab$cell)), , drop = FALSE]; ct$score <- as.numeric(Rcell)
    tie_sum <- sum(apply(ym, 1, function(v) { t <- table(v); sum(t^3 - t) })); corr <- 1 - tie_sum / (m * (Tn^3 - Tn))
    ss <- ps_fact_ss(ct$score, ct, stats::update(fm_eff, y ~ .))
    H <- 12 * ss[, "Sum Sq"] / (m * Tn * (Tn + 1)) / corr; names(H) <- rownames(ss)
    extra$cells <- ct; extra$m <- m; extra$T <- Tn; extra$ym <- ym
    lines <- c(sprintf("Rank the %d treatment combinations within each of the m = %d blocks (ties get the average rank), then add the ranks of each combination over the blocks.", Tn, m),
               "χ²r for an effect = 12 × (sum of squares of the combination rank sums for that effect) / (m T (T + 1)), with T = number of combinations.",
               "For a main effect this is the Friedman-type statistic built from the rank sum of each level; the interaction uses the rank sums after removing the main effects.",
               if (corr < 1) sprintf("Correction for ties within blocks: divide by %s.", ps_fmt(corr, 5)))
    rank_src <- list(kind = "cell", score = ct$score)
    if (m < nlevels(d$block)) lines <- c(sprintf("%d blocks with a missing value are left out.", nlevels(d$block) - m), lines)
  }
  eff <- do.call(rbind, lapply(codes, function(cd) { e <- ef[[cd]]; df <- ss[e$term, "Df"]
    data.frame(code = cd, term = e$term, label = e$label, main = e$main, df = df, df2 = NA_real_, ss = ss[e$term, "Sum Sq"], ms = NA_real_, stat = unname(H[e$term]),
               p = stats::pchisq(unname(H[e$term]), df, lower.tail = FALSE), crit = stats::qchisq(1 - a, df), alpha = a, stringsAsFactors = FALSE) }))
  rownames(eff) <- NULL
  best <- eff[which.min(eff$p), ]
  t <- mk_test(test, ps_np_name(kind), "nonpar", stat_name, best$stat, best$p, NULL, NA, df = best$df, dist = "chisq", about = "Rank test for each effect.")
  t$df1 <- best$df
  # mean-rank tables: one per main effect, plus the combinations
  src <- if (test == "friedmanf") extra$cells else extra$units
  rt <- lapply(Filter(function(e) e$main, ef[codes]), function(e) {
    g <- src[[e$roles]]; sc <- src$score
    if (test == "friedmanf") data.frame(Level = levels(g), `Combinations` = as.integer(table(g)), `Rank sum` = ps_fmt(tapply(sc, g, sum), 6), `Mean rank` = ps_fmt(tapply(sc, g, sum) / (extra$m * as.integer(table(g))), 4), check.names = FALSE)
    else data.frame(Level = levels(g), n = as.integer(table(g)), `Rank sum` = ps_fmt(tapply(sc, g, sum), 6), `Mean rank` = ps_fmt(tapply(sc, g, mean), 4), check.names = FALSE) })
  c(out, list(t = t, effects = eff, effects_df = ps_fact_effects_df(eff, stat_name), formula = lines, rank_tables = rt, rank = c(extra, list(src = src, test = test)),
              stat_name = stat_name))
}

# ---------- One effect as a single test, for the decision step ----------

ps_fact_effect_res <- function(res, code) {
  e <- res$effects[res$effects$code == code, ]
  r <- res; anova_t <- identical(res$test, "anova")
  name <- if (anova_t) sprintf("F test for %s", e$label) else sprintf("%s for %s", ps_np_short(res$kind), e$label)
  r$t <- mk_test(res$test, name, if (anova_t) "param" else "nonpar", if (anova_t) "F" else res$stat_name, e$stat, e$p, NULL, NA,
                 df = if (anova_t) sprintf("%d, %s", e$df, format(e$df2)) else e$df, dist = if (anova_t) "F" else "chisq")
  r$dist <- r$t$dist; r$df1 <- e$df; r$df2 <- if (anova_t) e$df2 else NA; r$t$df1 <- e$df; r$t$df2 <- r$df2
  r$p <- e$p; r$reject <- e$p < res$alpha
  ph <- if (anova_t && e$main) res$ph_main[[code]] else NULL
  r$ci_tab <- if (!is.null(ph)) ph$methods$tukey else NULL; r$ci_name <- "Tukey"
  r
}

# ---------- Post-hoc ----------

# Dunnett's test for any family of groups with a named control: every group against the control,
# with the simultaneous critical value from the multivariate t distribution.
ps_dunnett_family <- function(lev, means, n, ctrl, mse, dfe, alpha, conf) {
  oth <- setdiff(lev, ctrl); k <- length(oth)
  diff <- unname(means[oth] - means[ctrl]); se <- unname(sqrt(mse * (1 / n[oth] + 1 / n[ctrl]))); tst <- diff / se
  if (k == 1) { crit <- stats::qt(1 - (1 - conf) / 2, dfe); p <- 2 * stats::pt(-abs(tst), dfe) }
  else {
    corr <- outer(seq_len(k), seq_len(k), function(i, j) (1 / n[ctrl]) / sqrt((1 / n[oth[i]] + 1 / n[ctrl]) * (1 / n[oth[j]] + 1 / n[ctrl]))); diag(corr) <- 1
    set.seed(1)
    crit <- as.numeric(mvtnorm::qmvt(conf, tail = "both.tails", df = dfe, corr = corr)$quantile)
    p <- vapply(abs(tst), function(t) { set.seed(1); 1 - as.numeric(mvtnorm::pmvt(lower = rep(-t, k), upper = rep(t, k), df = dfe, corr = corr)) }, 0)
  }
  data.frame(pair = paste(oth, "−", ctrl), i = ctrl, j = oth, diff = diff, se = se, stat = tst, p = pmax(p, 0), lwr = diff - crit * se, upr = diff + crit * se, stringsAsFactors = FALSE)
}

# Tables for one family of comparisons, in the layout of the other post-hoc steps.
ps_fact_ph_format <- function(tab, centre, letters, conf, alpha, method, rank_based = FALSE, within = NULL, centre_name = "Mean") {
  tab$signif <- tab$p < alpha
  ord <- names(sort(centre, decreasing = TRUE))
  df <- data.frame(Comparison = tab$pair, Difference = if (!is.null(tab$diff)) ps_fmt(tab$diff) else "—",
                   CI = if (!is.null(tab$lwr)) sprintf("[%s, %s]", ps_fmt(tab$lwr), ps_fmt(tab$upr)) else "—",
                   `Adjusted p` = ps_p(tab$p), Result = ifelse(tab$signif, "Different", "Not different"), check.names = FALSE)
  if (method == "lsd") names(df)[4] <- "p-value"
  if (rank_based) { names(df)[2] <- "Mean rank difference"; df$CI <- NULL } else if (!is.null(tab$lwr)) names(df)[3] <- sprintf("%g%% CI", 100 * conf)
  grp <- if (method == "dunnett") data.frame(Level = ord, Centre = ps_fmt(centre[ord]), `Compared with the control` = "", check.names = FALSE)
         else data.frame(Level = ord, Centre = ps_fmt(centre[ord]), Group = unname(letters[ord]), check.names = FALSE)
  names(grp)[2] <- centre_name
  if (!is.null(within)) { df <- cbind(Within = rep(within[1], nrow(df)), df); grp <- cbind(Within = rep(within[1], nrow(grp)), grp) }
  list(table = df, groups = grp, raw = tab)
}

# All the comparison families an effect needs: the levels of its factor (a main effect), or the
# levels of each factor within the levels of the others (the simple effects of an interaction).
ps_fact_families <- function(res, code) {
  e <- res$ef[[code]]; d <- res$d; roles <- e$roles
  dirs <- if (length(roles) == 1) roles else roles
  lapply(dirs, function(w) {
    others <- setdiff(roles, w)
    combos <- if (length(others)) unique(d[others]) else data.frame(.x = 1)
    list(w = w, others = others, combos = combos)
  })
}
ps_fact_within_label <- function(res, others, combo) if (!length(others)) NULL else paste(vapply(others, function(r) sprintf("%s = %s", res$flabs[[r]], as.character(combo[[r]])), ""), collapse = ", ")
ps_fact_subset <- function(d, others, combo) if (!length(others)) d else d[Reduce(`&`, lapply(others, function(r) d[[r]] == combo[[r]])), , drop = FALSE]

ps_fact_posthoc <- function(res, code, method) {
  a <- res$alpha; conf <- res$conf; e <- res$ef[[code]]; d <- res$d
  if (res$test == "anova") {
    an <- res$an; fams <- ps_fact_families(res, code); note <- NULL
    ctrl <- res$control; ctrl_role <- if (!is.null(ctrl)) { h <- res$ffacs[vapply(res$ffacs, function(r) ctrl %in% levels(d[[r]]), TRUE)]; if (length(h)) h[1] else NULL }
    blocks <- lapply(fams, function(fm) {
      w <- fm$w; use <- method; fb <- FALSE
      if (method == "dunnett" && !identical(w, ctrl_role)) { use <- "tukey"; fb <- TRUE }
      parts <- lapply(seq_len(nrow(fm$combos)), function(i) {
        combo <- fm$combos[i, , drop = FALSE]; dsub <- ps_fact_subset(d, fm$others, combo); dsub[[w]] <- droplevels(dsub[[w]])
        within <- ps_fact_within_label(res, fm$others, combo)
        if (use == "dunnett") {
          lev <- levels(dsub[[w]]); mn <- tapply(dsub$y, dsub[[w]], mean); n <- tapply(dsub$y, dsub[[w]], length)
          tab <- ps_dunnett_family(lev, mn, n, ctrl, an$mse, an$dfe, a, conf)
          f <- ps_fact_ph_format(tab, mn, NULL, conf, a, "dunnett", within = if (!is.null(within)) rep(within, nrow(tab)))
          f$groups <- data.frame(Level = lev, Mean = ps_fmt(mn[lev]), `Compared with the control` = ifelse(lev == ctrl, "Control", ifelse(lev %in% tab$j[tab$p < a], "Differs from the control", "Same as the control")), check.names = FALSE)
          if (!is.null(within)) f$groups <- cbind(Within = rep(within, nrow(f$groups)), f$groups)
        } else {
          ph <- posthoc_all(res$fit, dsub, w, an$mse, an$dfe, a, conf, dunnett = FALSE)
          tab <- ph$methods[[use]]; mn <- stats::setNames(ph$means$mean, ph$means$level)
          f <- ps_fact_ph_format(tab, mn, ph$letters[[use]], conf, a, use, within = if (!is.null(within)) rep(within, nrow(tab)), centre_name = "Mean")
        }
        if (!is.null(within)) f$raw$pair <- paste0(f$raw$pair, "  [", within, "]")
        f
      })
      tabs <- do.call(rbind, lapply(parts, function(p) p$table)); raw <- do.call(rbind, lapply(parts, function(p) p$raw))
      grp <- do.call(rbind, lapply(parts, function(p) { g <- p$groups; names(g) <- names(parts[[1]]$groups); g }))
      list(title = if (length(fm$others)) sprintf("%s compared within each combination of %s", res$flabs[[w]], paste(res$flabs[fm$others], collapse = " × "))
                   else sprintf("Levels of %s", res$flabs[[w]]), table = tabs, groups = grp, raw = raw, fallback = fb, role = w)
    })
    return(list(code = code, label = e$label, main = e$main, blocks = blocks, method = method))
  }
  ps_fact_np_posthoc(res, code, method)
}

# Rank-based comparisons: Dunn (global ranks), Wilcoxon rank-sum (ranked residuals), Nemenyi and
# Wilcoxon signed-rank (Friedman, ranks within blocks).
ps_fact_np_posthoc <- function(res, code, method) {
  a <- res$alpha; e <- res$ef[[code]]; rk <- res$rank; test <- rk$test; fams <- ps_fact_families(res, code)
  U <- rk$src; adj <- if (method == "dunnbonf" || method %in% c("wilcox", "wilcoxrs")) "bonferroni" else "holm"
  blocks <- lapply(fams, function(fm) {
    w <- fm$w
    parts <- lapply(seq_len(nrow(fm$combos)), function(i) {
      combo <- fm$combos[i, , drop = FALSE]; usub <- ps_fact_subset(U, fm$others, combo); g <- droplevels(usub[[w]]); lev <- levels(g); k <- length(lev)
      within <- ps_fact_within_label(res, fm$others, combo); P <- pair_index(k); sc <- usub$score
      if (test == "friedmanf") {
        m <- rk$m; Tn <- rk$T; ng <- as.integer(table(g)); names(ng) <- lev; Rg <- tapply(sc, g, sum)
        if (method == "nemenyi") {
          dif <- unname(Rg[P$j] - Rg[P$i]); gg <- unname(ng[P$i]); q <- abs(dif) / sqrt(m * gg * Tn * (Tn + 1) / 12)
          p <- stats::ptukey(q, nmeans = k, df = Inf, lower.tail = FALSE); diffm <- dif / (m * gg)
        } else {
          cells <- rk$cells; ym <- rk$ym
          gm <- vapply(lev, function(l) { keep <- cells[[w]] == l; for (r in fm$others) keep <- keep & cells[[r]] == combo[[r]]; rowMeans(ym[, which(keep), drop = FALSE]) }, numeric(nrow(ym)))
          p <- vapply(seq_along(P$i), function(x) tryCatch(suppressWarnings(stats::wilcox.test(gm[, P$j[x]], gm[, P$i[x]], paired = TRUE, exact = NULL)$p.value), error = function(e) NA_real_), 0)
          p <- stats::p.adjust(p, "bonferroni"); diffm <- unname((Rg[P$j] - Rg[P$i]) / (m * ng[P$j]))
        }
        centre <- Rg / (m * ng)
      } else {
        N <- rk$N; Rbar <- tapply(sc, g, mean); ni <- as.integer(table(g)); ties <- table(rk$units$score); tc <- sum(ties^3 - ties) / (12 * (N - 1))
        if (method %in% c("dunn", "dunnbonf")) {
          z <- unname((Rbar[P$j] - Rbar[P$i]) / sqrt((N * (N + 1) / 12 - tc) * (1 / ni[P$i] + 1 / ni[P$j]))); p <- stats::p.adjust(2 * stats::pnorm(-abs(z)), adj)
        } else p <- stats::p.adjust(vapply(seq_along(P$i), function(x) tryCatch(suppressWarnings(stats::wilcox.test(sc[g == lev[P$j[x]]], sc[g == lev[P$i[x]]], exact = NULL)$p.value), error = function(e) NA_real_), 0), "bonferroni")
        diffm <- unname(Rbar[P$j] - Rbar[P$i]); centre <- Rbar
      }
      tab <- data.frame(pair = paste(lev[P$j], "−", lev[P$i]), i = lev[P$i], j = lev[P$j], diff = diffm, p = p, stringsAsFactors = FALSE)
      lt <- cld_letters(lev, as.numeric(centre), tab$i[tab$p < a], tab$j[tab$p < a])
      f <- ps_fact_ph_format(tab, centre, lt, res$conf, a, method, rank_based = TRUE, within = if (!is.null(within)) rep(within, nrow(tab)), centre_name = "Mean rank")
      if (!is.null(within)) f$raw$pair <- paste0(f$raw$pair, "  [", within, "]")
      f
    })
    list(title = if (length(fm$others)) sprintf("%s compared within each combination of %s", res$flabs[[w]], paste(res$flabs[fm$others], collapse = " × ")) else sprintf("Levels of %s", res$flabs[[w]]),
         table = do.call(rbind, lapply(parts, function(p) p$table)), groups = do.call(rbind, lapply(parts, function(p) { g <- p$groups; names(g) <- names(parts[[1]]$groups); g })),
         raw = do.call(rbind, lapply(parts, function(p) p$raw)), fallback = FALSE, role = w)
  })
  list(code = code, label = e$label, main = e$main, blocks = blocks, method = method)
}

# ---------- Plots ----------

# Main effect: the means of each level. Interaction: lines for the levels of the second factor
# (and a panel for each level of a third), so parallel lines show no interaction.
ps_plot_fact_effect <- function(res, code) {
  e <- res$ef[[code]]; d <- res$d; r <- e$roles; ylab <- paste0(res$resp, if (nzchar(res$unit)) paste0(" (", res$unit, ")") else "")
  cells <- stats::aggregate(d$y, d[r], mean); names(cells)[ncol(cells)] <- "m"
  if (length(r) >= 3) cells$panel <- paste(res$flabs[[r[3]]], "=", cells[[r[3]]])
  if (length(r) == 1) {
    ggplot(cells, aes(.data[[r]], m, group = 1)) + geom_line(colour = GREEN, linewidth = 1) + geom_point(colour = GREEN, size = 3.4) +
      geom_jitter(data = d, aes(.data[[r]], y), width = .06, height = 0, colour = PETROL, alpha = .35, size = 1.8, inherit.aes = FALSE) +
      labs(x = res$flabs[[r]], y = paste("Mean", tolower(ylab)), subtitle = "Dots: observations · Line: level means") + theme_sds() + theme(plot.subtitle = element_text(colour = INK, size = 10.5))
  } else {
    cols <- stats::setNames(sample_cols(nlevels(d[[r[2]]])), levels(d[[r[2]]]))
    p <- ggplot(cells, aes(.data[[r[1]]], m, colour = .data[[r[2]]], group = .data[[r[2]]])) + geom_line(linewidth = 1) + geom_point(size = 3) +
      scale_colour_manual(values = cols, name = res$flabs[[r[2]]]) + labs(x = res$flabs[[r[1]]], y = paste("Mean", tolower(ylab)),
        subtitle = "Parallel lines: no interaction · Non-parallel or crossing lines: interaction") + theme_sds() + theme(plot.subtitle = element_text(colour = INK, size = 10.5), legend.position = "right")
    if (length(r) >= 3) p <- p + facet_wrap(~ panel)
    p
  }
}
