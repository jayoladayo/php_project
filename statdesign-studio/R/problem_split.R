# Problem Sets, Chapter 7: split-plot designs (Montgomery, Chapter 14).
# The factors are of two kinds: whole-plot factors (hard to change: a level is set once for a whole plot)
# and subplot factors (changed inside a whole plot). The experiment has two errors:
#   whole-plot error   tests the whole-plot factors
#   subplot error      tests the subplot factors and their interactions with the whole-plot factors
# Two layouts:
#   replicated    the whole plots are run in blocks (replicates): the ANOVA has strata Blocks, Whole plots and
#                 Within (aov with Error(block/whole-plot)); the whole-plot error is Block x whole-plot factor.
#   unreplicated  a 2^k design with one observation per combination (for example four whole-plot factors and one
#                 subplot factor): there is no pure error, so in each stratum the effects that sit with the small
#                 ones on the normal plot are put into that stratum's error (the student chooses them).
# The non-parametric restart is the rank-transform ANOVA: the same analysis on the ranks of the response.
# Plain R with no Shiny, so it can be tested on its own.

# ---------- Structure ----------

# Called by ps_structure when the problem says it is a split-plot (roles$split) and names the whole-plot factors (roles$wp).
ps_split_prepare <- function(st, roles) {
  f <- st$ffacs; w <- as.character(unlist(roles$wp %||% character(0)))   # the whole-plot factors, named by column or by role
  wp <- f[vapply(f, function(k) k %in% w || (!is.null(roles[[k]]) && roles[[k]] %in% w), TRUE)]; sp <- setdiff(f, wp)
  if (length(f) < 2 || !length(wp) || !length(sp)) return(st)
  d <- st$d
  d$.wpc <- interaction(d[wp], drop = TRUE, lex.order = TRUE)                       # the whole-plot treatment combination
  if ("block" %in% st$facs) d$.wpu <- interaction(d$block, d$.wpc, drop = TRUE)      # the whole plot itself
  st$d <- d; st$split <- TRUE; st$wp <- wp; st$sp <- sp
  st$unrep <- FALSE
  if ("block" %in% st$facs) { st$pool <- FALSE; st$cpool <- FALSE; st$pool_codes <- NULL }
  else if (isTRUE(st$pool) && isTRUE(st$cpool)) st$pool_codes <- ps_split_pool_suggest(st)
  else { st$pool <- FALSE; st$cpool <- FALSE }
  st
}

# "split" when the data form a balanced split-plot, otherwise NA.
ps_split_kind <- function(st) {
  d <- st$d; f <- st$ffacs; wp <- st$wp; sp <- st$sp
  if (!isTRUE(st$split) || any(vapply(f, function(r) nlevels(d[[r]]) < 2, TRUE))) return(NA_character_)
  spc <- interaction(d[sp], drop = TRUE)
  has_block <- "block" %in% st$facs
  if (has_block) {
    if (any(c("row", "column", "greek") %in% st$facs)) return(NA_character_)
    tab <- table(d$block, d$.wpc, spc); if (any(tab != 1) || nlevels(d$block) < 2) return(NA_character_)
  } else {
    if (!isTRUE(st$cpool) || !ps_fact_2level(st)) return(NA_character_)
    tab <- table(d$.wpc, spc); if (any(tab != 1)) return(NA_character_)
  }
  "split"
}

# ---------- Effects of the two strata ----------

# the stratum of an effect: "sp" when it contains a subplot factor, otherwise "wp"
ps_split_stratum_of <- function(st, roles) if (any(roles %in% st$sp)) "sp" else "wp"

ps_split_strata_codes <- function(st) {
  cd <- ps_fact_codes(length(st$ffacs)); lett <- LETTERS[seq_along(st$ffacs)]
  is_sp <- vapply(cd, function(x) any(strsplit(x, "")[[1]] %in% lett[st$ffacs %in% st$sp]), TRUE)
  list(wp = cd[!is_sp], sp = cd[is_sp])
}

# The effects that sit with the small ones in each stratum (Lenth's margin of error, stratum by stratum); at least one
# effect stays in the error of each stratum and at least one is tested.
ps_split_pool_suggest <- function(st) {
  sc <- ps_split_strata_codes(st); ce <- ps_fact_contrasts(st, NULL); out <- character(0)
  for (g in c("wp", "sp")) {
    cd <- sc[[g]]; if (!length(cd)) next
    e <- ce$effect[match(cd, ce$code)]; len <- ps_lenth(e); p <- cd[abs(e) <= len$me]
    if (length(p) < 1 || length(p) >= length(cd)) p <- cd[nchar(cd) >= if (g == "sp") 3 else 3]
    if (length(p) < 1 || length(p) >= length(cd)) p <- cd[which.min(abs(e))]
    out <- c(out, p)
  }
  out
}
ps_split_pool_rule <- function(st) { sc <- ps_split_strata_codes(st); c(sc$wp[nchar(sc$wp) >= 3], sc$sp[nchar(sc$sp) >= 3]) }

# the student's choice replaces the suggestion, stratum by stratum
ps_split_set_pool <- function(st, s) {
  sc <- ps_split_strata_codes(st); sel <- as.character(unlist(s[["pool_sel"]])); sug <- NULL; out <- character(0)
  for (g in c("wp", "sp")) {
    cd <- sc[[g]]; pick <- intersect(sel, cd)
    if (length(pick) < 1 || length(pick) >= length(cd)) { if (is.null(sug)) sug <- ps_split_pool_suggest(st); pick <- intersect(sug, cd) }
    out <- c(out, pick)
  }
  st$pool_codes <- out; st
}

# ---------- The model whose residuals are checked ----------
# Replicated: the whole plots as a factor plus the subplot effects, so the residuals are the subplot errors.
ps_split_formula <- function(st) {
  ef <- ps_fact_effects_all(st, NULL); tt <- vapply(Filter(function(e) identical(e$stratum, "sp"), ef), function(e) e$term, "")
  stats::as.formula(paste("y ~", paste(c(".wpu", tt), collapse = " + ")))
}

# ---------- The ANOVA ----------

ps_split_eff_row <- function(cd, e, df, df2, ss, ms, f, a)
  data.frame(code = cd, term = e$term, label = e$label, main = e$main, df = df, df2 = df2, ss = ss, ms = ms, stat = f,
             p = stats::pf(f, df, df2, lower.tail = FALSE), crit = stats::qf(1 - a, df, df2), alpha = a, stratum = e$stratum, stringsAsFactors = FALSE)

# the error of each factor, for the post-hoc comparisons (a whole-plot factor uses the whole-plot error)
ps_split_err_role <- function(st, wp_err, sp_err) {
  r <- c(stats::setNames(rep(list(wp_err), length(st$wp)), st$wp), stats::setNames(rep(list(sp_err), length(st$sp)), st$sp)); r
}

ps_split_anova <- function(out, kind, st, meta) {
  if (isTRUE(st$cpool)) return(ps_split_anova_contrasts(out, kind, st, meta))
  a <- out$alpha; d <- st$d; ef <- out$ef; N <- nrow(d)
  fm <- stats::as.formula(paste("y ~", paste(st$ffacs, collapse = " * "), "+ Error(block/.wpc)"))
  fit <- stats::aov(fm, data = d); sm <- summary(fit)
  nm <- names(sm); tabs <- lapply(seq_along(sm), function(i) { t <- as.data.frame(sm[[i]][[1]]); rownames(t) <- trimws(rownames(t)); t })
  i_b <- 1L; i_w <- grep("\\.wpc", nm)[1]; i_s <- length(sm)
  tb <- tabs[[i_b]]; tw <- tabs[[i_w]]; ts <- tabs[[i_s]]
  err <- function(t) list(ss = t["Residuals", "Sum Sq"], df = t["Residuals", "Df"], ms = t["Residuals", "Mean Sq"])
  e_w <- err(tw); e_s <- err(ts); e_b <- err(tb)
  rand <- isTRUE(meta$random_wp) && length(st$wp) == 1
  rows <- lapply(names(ef), function(cd) {
    e <- ef[[cd]]; t <- if (identical(e$stratum, "wp")) tw else ts; r <- t[e$term, ]
    ms_n <- r[["Mean Sq"]]; ss <- r[["Sum Sq"]]; df <- r[["Df"]]
    den <- if (identical(e$stratum, "wp")) e_w else e_s
    if (rand && identical(e$stratum, "sp") && !any(e$roles %in% st$wp)) {   # a random whole-plot factor: a subplot effect is tested against its interaction with it
      key <- paste(sort(match(c(e$roles, st$wp), st$ffacs)), collapse = ","); cand <- Filter(function(x) identical(paste(sort(match(x$roles, st$ffacs)), collapse = ","), key), ef)
      if (length(cand)) { rr <- ts[cand[[1]]$term, ]; den <- list(ss = rr[["Sum Sq"]], df = rr[["Df"]], ms = rr[["Mean Sq"]]) }
    }
    ps_split_eff_row(cd, e, df, den$df, ss, ms_n, ms_n / den$ms, a)
  })
  effects <- do.call(rbind, rows); rownames(effects) <- NULL
  sst <- sum((d$y - mean(d$y))^2)
  den_lab <- function(e) if (rand && identical(e$stratum, "sp") && !any(e$roles %in% st$wp)) "its interaction with the random whole-plot factor" else if (identical(e$stratum, "wp")) "the whole-plot error" else "the subplot error"
  blk_row <- c("Replicates (blocks)", e_b$df, ps_fmt(e_b$ss, 6), ps_fmt(e_b$ms, 6), "", "")
  mk <- function(sub) { x <- effects[effects$stratum == sub, , drop = FALSE]; if (!nrow(x)) return(NULL)
    data.frame(Source = sprintf("%s (%s)", x$label, x$code), df = x$df, `Sum of squares` = ps_fmt(x$ss, 6), `Mean square` = ps_fmt(x$ms, 6), F0 = ps_fmt(x$stat, 5), `P-value` = ps_p(x$p), check.names = FALSE, stringsAsFactors = FALSE) }
  line <- function(src, e) data.frame(Source = src, df = e$df, `Sum of squares` = ps_fmt(e$ss, 6), `Mean square` = ps_fmt(e$ms, 6), F0 = "", `P-value` = "", check.names = FALSE, stringsAsFactors = FALSE)
  tabdf <- rbind(data.frame(Source = "Replicates (blocks)", df = e_b$df, `Sum of squares` = ps_fmt(e_b$ss, 6), `Mean square` = ps_fmt(e_b$ms, 6), F0 = "", `P-value` = "", check.names = FALSE, stringsAsFactors = FALSE),
                 mk("wp"), line("Whole-plot error", e_w), mk("sp"), line("Subplot error", e_s),
                 data.frame(Source = "Total", df = N - 1, `Sum of squares` = ps_fmt(sst, 6), `Mean square` = "", F0 = "", `P-value` = "", check.names = FALSE, stringsAsFactors = FALSE))
  rownames(tabdf) <- NULL
  an <- list(table = effects, nuis = NULL, sse = e_s$ss, dfe = e_s$df, mse = e_s$ms, sst = sst, dft = N - 1, wp = e_w, sp = e_s, blocks = e_b)
  err_role <- ps_split_err_role(st, e_w, e_s)
  ph_main <- lapply(Filter(function(e) e$main, ef), function(e) { er <- err_role[[e$roles]]; posthoc_all(NULL, d, e$roles, er$ms, er$df, a, out$conf, dunnett = FALSE) })
  f <- function(x, k = 7) ps_fmt(x, k)
  lines <- c(sprintf("N = %d observations; %d whole plots (%d blocks × %d whole-plot treatments); ȳ... = %s", N, nlevels(d$.wpu), nlevels(d$block), nlevels(d$.wpc), f(mean(d$y), 6)),
             sprintf("SS_T = Σ y² − y...²/N = %s − %s = %s", f(sum(d$y^2), 8), f(sum(d$y)^2 / N, 8), f(sst)),
             "Two errors: the whole-plot error (blocks × whole-plot treatments) tests the whole-plot factors; the subplot error (the within-whole-plot residual) tests the subplot factors and their interactions with the whole-plot factors.",
             sprintf("Whole-plot error: SS = %s on %d df, MS = %s.   Subplot error: SS = %s on %d df, MS = %s.", f(e_w$ss), e_w$df, f(e_w$ms, 6), f(e_s$ss), e_s$df, f(e_s$ms, 6)),
             sprintf("F0 for %s = MS_effect / MS_error: %s", paste(effects$code, collapse = ", "), paste(sprintf("%s: %s / %s = %s", effects$code, f(effects$ms, 6), f(effects$ms / effects$stat, 6), f(effects$stat, 5)), collapse = ";  ")),
             if (rand) "The whole-plot factor is random: a subplot main effect is tested against its interaction with the whole-plot factor.")
  best <- effects[which.min(effects$p), ]
  t <- mk_test("anova", ps_param_name(kind), "param", "F", best$stat, best$p, NULL, NA, df = sprintf("%d, %s", best$df, format(best$df2)), dist = "F",
               about = "Each effect is tested against the error of its own stratum.")
  t$df1 <- best$df; t$df2 <- best$df2
  c(out, list(t = t, fit = fit, effects = effects, an = an, anova_df = tabdf, effects_df = ps_fact_effects_df(effects), ph_main = ph_main, formula = lines[!vapply(lines, is.null, TRUE)],
              varcomp = NULL, nonadd = NULL, nonadd_df = NULL, err_role = err_role, split = TRUE, den_lab = vapply(ef, den_lab, "")))
}

# Unreplicated 2^k split-plot: the effects are contrasts; in each stratum the pooled effects form that stratum's error.
ps_split_anova_contrasts <- function(out, kind, st, meta) {
  a <- out$alpha; d <- st$d; ef <- out$ef; N <- nrow(d); ce <- ps_fact_contrasts(st, meta)
  pool <- st$pool_codes; sc <- ps_split_strata_codes(st)
  err <- function(g) { cd <- intersect(pool, sc[[g]]); ss <- sum(ce$ss[match(cd, ce$code)]); list(ss = ss, df = length(cd), ms = ss / length(cd), codes = cd) }
  e_w <- err("wp"); e_s <- err("sp")
  rows <- lapply(names(ef), function(cd) { e <- ef[[cd]]; den <- if (identical(e$stratum, "wp")) e_w else e_s; ss <- ce$ss[match(cd, ce$code)]
    ps_split_eff_row(cd, e, 1, den$df, ss, ss, ss / den$ms, a) })
  effects <- do.call(rbind, rows); rownames(effects) <- NULL
  sst <- sum((d$y - mean(d$y))^2)
  mk <- function(sub) { x <- effects[effects$stratum == sub, , drop = FALSE]; if (!nrow(x)) return(NULL)
    data.frame(Source = sprintf("%s (%s)", x$label, x$code), df = x$df, `Sum of squares` = ps_fmt(x$ss, 6), `Mean square` = ps_fmt(x$ms, 6), F0 = ps_fmt(x$stat, 5), `P-value` = ps_p(x$p), check.names = FALSE, stringsAsFactors = FALSE) }
  line <- function(src, e) data.frame(Source = src, df = e$df, `Sum of squares` = ps_fmt(e$ss, 6), `Mean square` = ps_fmt(e$ms, 6), F0 = "", `P-value` = "", check.names = FALSE, stringsAsFactors = FALSE)
  tabdf <- rbind(mk("wp"), line(sprintf("Whole-plot error (pooled: %s)", paste(e_w$codes, collapse = ", ")), e_w), mk("sp"), line(sprintf("Subplot error (pooled: %s)", paste(e_s$codes, collapse = ", ")), e_s),
                 data.frame(Source = "Total", df = N - 1, `Sum of squares` = ps_fmt(sst, 6), `Mean square` = "", F0 = "", `P-value` = "", check.names = FALSE, stringsAsFactors = FALSE))
  rownames(tabdf) <- NULL
  fit <- stats::lm(ps_fact_formula(kind, st), data = d)
  an <- list(table = effects, nuis = NULL, sse = e_s$ss, dfe = e_s$df, mse = e_s$ms, sst = sst, dft = N - 1, wp = e_w, sp = e_s)
  err_role <- ps_split_err_role(st, e_w, e_s)
  ph_main <- lapply(Filter(function(e) e$main, ef), function(e) { er <- err_role[[e$roles]]; posthoc_all(NULL, d, e$roles, er$ms, er$df, a, out$conf, dunnett = FALSE) })
  f <- function(x, k = 7) ps_fmt(x, k)
  lines <- c(sprintf("N = %d observations; %d whole plots (the combinations of the whole-plot factors), each with %d subplot runs; ȳ... = %s", N, nlevels(d$.wpc), N / nlevels(d$.wpc), f(mean(d$y), 6)),
             sprintf("SS_T = Σ y² − y...²/N = %s − %s = %s", f(sum(d$y^2), 8), f(sum(d$y)^2 / N, 8), f(sst)),
             "Each effect is a contrast of the −1/+1 columns: effect = (mean at +) − (mean at −), SS = N × (effect/2)².",
             sprintf("Whole-plot error: the %d whole-plot effects put into the error (%s), SS = %s, MS = %s.", e_w$df, paste(e_w$codes, collapse = ", "), f(e_w$ss), f(e_w$ms, 6)),
             sprintf("Subplot error: the %d subplot effects put into the error (%s), SS = %s, MS = %s.", e_s$df, paste(e_s$codes, collapse = ", "), f(e_s$ss), f(e_s$ms, 6)),
             sprintf("F0 = MS_effect / MS_error of its stratum: %s", paste(sprintf("%s: %s / %s = %s", effects$code, f(effects$ms, 6), f(ifelse(effects$stratum == "wp", e_w$ms, e_s$ms), 6), f(effects$stat, 5)), collapse = ";  ")))
  best <- effects[which.min(effects$p), ]
  t <- mk_test("anova", ps_param_name(kind), "param", "F", best$stat, best$p, NULL, NA, df = sprintf("%d, %s", best$df, format(best$df2)), dist = "F",
               about = "Each effect is tested against the pooled error of its own stratum.")
  t$df1 <- best$df; t$df2 <- best$df2
  c(out, list(t = t, fit = fit, effects = effects, an = an, anova_df = tabdf, effects_df = ps_fact_effects_df(effects), ph_main = ph_main, formula = lines, varcomp = NULL, nonadd = NULL, nonadd_df = NULL,
              err_role = err_role, split = TRUE, pooled = pool))
}

# Rank-transform ANOVA (the non-parametric restart): the same analysis on the ranks of the response.
ps_split_rank <- function(out, kind, test, st, meta) {
  st2 <- st; st2$d$y <- rank(st$d$y)
  out2 <- out; out2$d <- st2$d
  r <- ps_split_anova(out2, kind, st2, meta)
  r$test <- test; r$t$name <- ps_np_name(kind); r$t$test <- test; r$t$family <- "nonpar"
  r$formula <- c("Rank all N observations together (ties get the average rank) and repeat the split-plot analysis on the ranks (the rank-transform ANOVA).", r$formula)
  r$stat_name <- "F"; r$d <- st$d; r$d_rank <- st2$d; r$rank_scores <- st2$d$y   # the data stay as measured; the ranks are used for the comparisons
  r
}

# The whole-plot residuals (the whole-plot error), one row per whole plot in the order the whole plots were run: the model used for the
# independence check of a split-plot.
ps_split_wp_fit <- function(st) {
  d <- st$d; key <- if ("block" %in% st$facs) d$.wpu else d$.wpc; first <- !duplicated(key)
  w <- d[first, , drop = FALSE]; w$y <- as.numeric(tapply(d$y, key, mean)[as.character(key[first])])
  if ("block" %in% st$facs) return(stats::lm(stats::as.formula(paste("y ~ block +", paste(st$wp, collapse = " * "))), data = w))
  tt <- vapply(Filter(function(e) identical(e$stratum, "wp"), ps_fact_effects(st, NULL)), function(e) e$term, "")
  stats::lm(stats::as.formula(paste("y ~", paste(tt, collapse = " + "))), data = w)
}

# ---------- The worked solution as an R script ----------
# Plain stats functions (aov with Error() for a replicated split-plot, contrasts of the -1/+1 columns for a 2^k run as a split-plot),
# the same five steps, and the rank-transform restart as the same function applied to the ranks.
ps_split_script <- function(m, st, s, test, ph, hyp) {
  d <- st$d; alpha <- s$alpha; info <- ps_info("split", st, m, s)
  rank_first <- test %in% PS_NONPAR_TESTS; rep_ <- "block" %in% st$facs
  out <- character(0); add <- function(...) out <<- c(out, ...)
  sec <- function(title) add("", "# ---", sprintf("# %s", title), "# ---")
  cmt <- function(x) for (l in strsplit(x, "\n", fixed = TRUE)[[1]]) add(paste0("# ", l))
  fs <- LETTERS[seq_along(st$ffacs)]; wpl <- fs[match(st$wp, st$ffacs)]; spl <- fs[match(st$sp, st$ffacs)]
  resp <- ps_lab(m, "response")
  dd <- d[, c("y", st$ffacs, if (rep_) "block"), drop = FALSE]; names(dd) <- c("y", fs, if (rep_) "block")
  add("# StatDesign Studio: worked solution as an R script",
      sprintf("# %s%s", if (identical(m$id, "custom")) (if (nzchar(m$title %||% "")) m$title else "Your problem") else sprintf("Problem %s: %s", m$id, m$title), if (isTRUE(m$edited)) " (edited data)" else ""),
      sprintf("# Layout: split-plot design. Test chosen in step 5: %s.", ps_test_choices("split", s)[[test]]),
      "# Run it from top to bottom. It uses base R (stats) and the lmtest package (install.packages(\"lmtest\") once).",
      "", "results <- list()   # the key numbers are collected here")
  sec("Step 1 - Problem and data")
  cmt(paste(c(m$statement, m$parts), collapse = "\n"))
  add(sprintf("# Columns: y = %s; %s%s", resp, paste(sprintf("%s = %s%s", fs, vapply(st$ffacs, function(r) ps_lab(m, r), ""), ifelse(fs %in% wpl, " (whole-plot factor)", " (subplot factor)")), collapse = "; "),
              if (rep_) sprintf("; block = %s (the replicate)", ps_lab(m, "block")) else ""), "dat <- data.frame(")
  for (j in seq_along(dd)) { v <- dd[[j]]; nm <- names(dd)[j]
    add(sprintf("  %s = %s%s", nm, if (is.numeric(v)) ps_rwrap(ps_rvec(v, TRUE)) else ps_rwrap(ps_rvec(as.character(v))), if (j < length(dd)) "," else "")) }
  add(")")
  for (j in seq_along(dd)[-1]) add(sprintf("dat$%s <- factor(dat$%s, levels = %s)", names(dd)[j], names(dd)[j], ps_rvec(levels(dd[[j]]))))
  add(sprintf("wp <- %s   # the whole-plot factors (a level is set once for a whole plot)", ps_rvec(wpl)), sprintf("sp <- %s   # the subplot factors (changed inside a whole plot)", ps_rvec(spl)), "str(dat)")
  sec("Step 2 - Hypotheses")
  fh <- ps_fact_hypotheses(st, m, if (rank_first) "nonpar" else "param")
  for (cd in names(fh)) cmt(sprintf("%s: H0: %s\n    H1: %s", cd, hyp$fh[[cd]]$h0 %||% fh[[cd]]$h0, hyp$fh[[cd]]$h1 %||% fh[[cd]]$h1))
  sec("Step 3 - Significance level"); add(sprintf("alpha <- %s", format(alpha)))
  sec("Step 4 - Information available")
  add("n <- nrow(dat); n", sprintf("ftable(table(dat[, c(%s)]))   # the treatment combinations", ps_rvec(fs)))
  if (rep_) {
    add("dat$wpc <- interaction(dat[wp], drop = TRUE)   # the whole-plot treatment", "# Whole-plot error = block x whole-plot treatment; subplot error = the within-whole-plot residual.")
  } else {
    add("# A 2^k design with one observation per combination, run as a split-plot: no pure error. In each stratum the effects put into its error",
        "# (chosen from the plots) form that stratum's error. Effects are contrasts of the coded columns (low = -1, high = +1).",
        sprintf("cd <- data.frame(y = dat$y, %s)", paste(sprintf("%s = ifelse(as.integer(dat$%s) == 2, 1, -1)", fs, fs), collapse = ", ")),
        sprintf("fit_all <- lm(y ~ %s, data = cd); effects <- 2 * coef(fit_all)[-1]; round(effects, 4)", paste(fs, collapse = " * ")),
        "qq <- qqnorm(abs(effects)); text(qq$x, qq$y, labels = gsub(\":\", \"\", names(effects)), pos = 4, cex = 0.7)   # the effects that stand away from the small ones are active",
        sprintf("pool <- %s   # the effects put into the error (step 4)", ps_rvec(st$pool_codes)))
  }
  # the analysis as a function of the response, so the rank-transform restart reuses it
  sec("Step 5 - The test: split-plot ANOVA (each effect against the error of its own stratum)")
  if (rep_) {
    rand <- isTRUE(m$random_wp) && length(st$wp) == 1
    add("split_anova <- function(y) {", "  dd <- dat; dd$y <- y",
        sprintf("  fit <- aov(y ~ %s + Error(block/wpc), data = dd); sm <- summary(fit)", paste(fs, collapse = " * ")),
        "  tab <- function(t, stratum) { r <- trimws(rownames(t)); k <- r != \"Residuals\"",
        "    data.frame(effect = gsub(\":\", \"\", r[k]), stratum = stratum, df = t[k, \"Df\"], ss = t[k, \"Sum Sq\"], ms = t[k, \"Mean Sq\"],",
        "               df_err = t[\"Residuals\", \"Df\"], ms_err = t[\"Residuals\", \"Mean Sq\"], stringsAsFactors = FALSE) }",
        "  ef <- rbind(tab(sm[[2]][[1]], \"whole plot\"), tab(sm[[length(sm)]][[1]], \"subplot\"))   # [[2]]: block x whole-plot stratum; last: within")
    if (rand) add("  # The whole-plot factor is random: a subplot main effect is tested against its interaction with the whole-plot factor.",
                  sprintf("  for (f in %s) { i <- which(ef$effect == f); j <- which(ef$effect == paste(sort(c(f, wp)), collapse = \"\")); ef$ms_err[i] <- ef$ms[j]; ef$df_err[i] <- ef$df[j] }", ps_rvec(spl)))
    add("  ef$F <- ef$ms / ef$ms_err; ef$p <- pf(ef$F, ef$df, ef$df_err, lower.tail = FALSE)", "  ef[match(effect_order, ef$effect), ]", "}",
        sprintf("effect_order <- %s   # A, B, ..., AB, AC, ...", ps_rvec(names(ps_fact_effects_all(st, NULL)))))
  } else {
    add(sprintf("effect_order <- setdiff(%s, pool)   # the effects that are tested", ps_rvec(names(ps_fact_effects_all(st, NULL)))),
        "split_anova <- function(y) {", sprintf("  cd2 <- cd; cd2$y <- y; fit <- lm(y ~ %s, data = cd2); b <- coef(fit)[-1]", paste(fs, collapse = " * ")),
        "  ef <- data.frame(effect = gsub(\":\", \"\", names(b)), df = 1, ss = nrow(cd2) * unname(b)^2, stringsAsFactors = FALSE); ef$ms <- ef$ss",
        sprintf("  ef$stratum <- ifelse(grepl(\"[%s]\", ef$effect), \"subplot\", \"whole plot\")", paste(spl, collapse = "")),
        "  err <- function(g) { p <- ef$effect %in% pool & ef$stratum == g; c(ms = sum(ef$ss[p]) / sum(p), df = sum(p)) }",
        "  ew <- err(\"whole plot\"); es <- err(\"subplot\")   # the two errors: the pooled effects of each stratum",
        "  ef <- ef[!ef$effect %in% pool, ]", "  ef$ms_err <- ifelse(ef$stratum == \"whole plot\", ew[\"ms\"], es[\"ms\"]); ef$df_err <- ifelse(ef$stratum == \"whole plot\", ew[\"df\"], es[\"df\"])",
        "  ef$F <- ef$ms / ef$ms_err; ef$p <- pf(ef$F, ef$df, ef$df_err, lower.tail = FALSE)", "  ef[match(effect_order, ef$effect), ]", "}")
  }
  if (!rank_first) {
    add("ef <- split_anova(dat$y); print(ef)", "results$p_param <- setNames(ef$p, ef$effect)")
    sec("Step 6 - Decision")
    add("for (i in seq_len(nrow(ef))) cat(sprintf(\"%-6s (%s) F(%g, %g) = %8.4f, p-value = %.4g -> %s H0\\n\", ef$effect[i], ef$stratum[i], ef$df[i], ef$df_err[i], ef$F[i], ef$p[i], if (ef$p[i] < alpha) \"reject\" else \"fail to reject\"))")
    sec("Step 7 - Post-hoc (only when H0 is rejected)")
    chs <- ps_posthoc_choices("anova", "split", m$control); mth <- if (!is.null(ph) && ph %in% names(chs)) ph else ps_default_ph(s, chs)
    add(sprintf("# method: %s (the error of the factor's stratum: whole-plot error for whole-plot factors, subplot error otherwise)", chs[[mth]]))
    add(PS_SCRIPT_HELPERS$tukey)
    add("err_of <- function(w, oth) { g <- if (w %in% wp && all(oth %in% wp)) \"whole plot\" else \"subplot\"; e <- ef[ef$stratum == g, ][1, ]; c(mse = e$ms_err, dfe = e$df_err) }",
        "if (any(ef$p < alpha)) {", "  for (e in ef$effect[ef$p < alpha]) { fsx <- strsplit(e, \"\")[[1]]; cat(\"\\n== \", e, \" ==\\n\")",
        "    for (w in fsx) { oth <- setdiff(fsx, w); er <- err_of(w, oth)",
        "      if (!length(oth)) print(tukey_pairs(dat$y, dat[[w]], er[[\"mse\"]], er[[\"dfe\"]], alpha))",
        "      else { key <- interaction(dat[oth]); for (lv in levels(key)) { sub <- key == lv; cat(\"  \", w, \"within\", paste(oth, collapse = \":\"), \"=\", lv, \"\\n\"); print(tukey_pairs(dat$y[sub], dat[[w]][sub], er[[\"mse\"]], er[[\"dfe\"]], alpha)) } } } }",
        "} else cat(\"H0 was not rejected for any effect: no post-hoc analysis is needed.\\n\")")
  } else add("# (the ANOVA is not run: the rank-transform ANOVA was chosen in step 5)")
  sec("Step 8 - Assumption checks")
  if (rep_) {
    sub_terms <- vapply(Filter(function(e) identical(e$stratum, "sp"), ps_fact_effects_all(st, NULL)), function(e) paste(vapply(e$roles, function(r) fs[match(r, st$ffacs)], ""), collapse = ":"), "")
    add("# The subplot residuals (whole plots as a factor) for normality and constant variance;",
        sprintf("fit_obs <- lm(y ~ interaction(block, wpc) + %s, data = dat); res_obs <- residuals(fit_obs)", paste(sub_terms, collapse = " + ")))
  } else {
    tt <- vapply(ps_fact_effects(st, NULL), function(e) paste(vapply(e$roles, function(r) fs[match(r, st$ffacs)], ""), collapse = ":"), "")
    add("# The residuals of the model with the tested effects (the pooled effects are the error);", sprintf("fit_obs <- lm(y ~ %s, data = cd); res_obs <- residuals(fit_obs)", paste(tt, collapse = " + ")))
  }
  add("sw <- shapiro.test(res_obs); sw                       # normality",
      "bp <- lmtest::bptest(fit_obs, ~ fv, data = data.frame(fv = fitted(fit_obs))); bp   # constant variance (on the fitted values)",
      "# The subplots of a whole plot share its error, so independence is checked on the whole-plot residuals, in the order the whole plots were run.")
  if (rep_) add("key <- interaction(dat$block, dat$wpc); first <- !duplicated(key)",
                "wm <- dat[first, ]; wm$y <- as.numeric(tapply(dat$y, key, mean)[as.character(key[first])])",
                sprintf("bg <- lmtest::bgtest(lm(y ~ block + %s, data = wm), order = 1); bg", paste(wpl, collapse = " * ")))
  else add("key <- interaction(dat[wp], drop = TRUE); first <- !duplicated(key)",
           "wm <- cd[first, ]; wm$y <- as.numeric(tapply(dat$y, key, mean)[as.character(key[first])])",
           sprintf("bg <- lmtest::bgtest(lm(y ~ %s, data = wm), order = 1); bg", paste(vapply(Filter(function(e) identical(e$stratum, "wp"), ps_fact_effects(st, NULL)), function(e) paste(vapply(e$roles, function(r) fs[match(r, st$ffacs)], ""), collapse = ":"), ""), collapse = " + ")))
  add("results$p_assumptions <- c(shapiro = sw$p.value, bp = bp$p.value, bg = bg$p.value)", "ok <- all(results$p_assumptions >= alpha, na.rm = TRUE)",
      "cat(if (ok) \"All assumptions hold: the analysis stops here.\\n\" else \"An assumption fails: continue with the rank-transform ANOVA below.\\n\")")
  sec("Steps 9 to 14 - The rank-transform ANOVA (used when an assumption fails)")
  add(if (rank_first) "# (chosen directly in step 5: it is run whatever the assumption checks say)" else "# Hypotheses about the medians; the same alpha. The same split-plot analysis on the ranks of y.",
      if (rank_first) "if (TRUE) {" else "if (!ok) {",
      "  efr <- split_anova(rank(dat$y)); print(efr)", "  results$p_rank <- setNames(efr$p, efr$effect)",
      "  for (i in seq_len(nrow(efr))) cat(sprintf(\"%-6s F = %8.4f, p-value = %.4g -> %s H0\\n\", efr$effect[i], efr$F[i], efr$p[i], if (efr$p[i] < alpha) \"reject\" else \"fail to reject\"))",
      "}", "", "# Conclusion: compare each p-value in `results` with alpha, as in steps 6 and 13.", "print(results)", "")
  out
}
