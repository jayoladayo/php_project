# Problem Sets engine: the step-by-step hypothesis-testing process taught in the course.
#   1 Problem and data  2 Hypotheses  3 Significance level  4 Information available
#   5 Test  6 Decision (rejection region, confidence interval, p-value)  7 Post-hoc
# plus the assumption checks. The tests themselves come from the course engine in
# R/samples.R (run_one, run_two, run_paired, run_k) and R/analysis.R (posthoc_all);
# the blocked designs of Chapter 4 are in R/problem_blocks.R.
# Plain R with no Shiny, so it can be tested on its own.

PS_ROLE_KEYS <- c("treatment", "block", "row", "column", "greek")
PS_ROLE_NAMES <- c(response = "Response", treatment = "Group", block = "Block or pair", row = "Row", column = "Column", greek = "Greek letter")

PS_KIND_LABEL <- c(one = "One sample", variance = "One sample (variance)", two = "Two independent samples",
                   paired = "Two dependent (paired) samples", k = "k independent samples",
                   rcbd = "Randomized complete block design", latin = "Latin square", glsd = "Graeco-Latin square",
                   bibd = "Balanced incomplete block design")
# Problem type, as used for the categories and filters of the list.
PS_TYPE <- c(one = "One sample", variance = "One sample", two = "Two sample · independent", paired = "Two sample · paired",
             k = "K sample (one-way)", rcbd = "RCBD", latin = "Latin square", glsd = "Graeco-Latin square", bibd = "BIBD")
PS_PH_SHORT <- c(tukey = "Tukey HSD", lsd = "Fisher LSD", bonferroni = "Bonferroni", scheffe = "Scheffé", dunnett = "Dunnett",
                 gh = "Games-Howell", dunn = "Dunn", nemenyi = "Nemenyi")

# ---------- Formatting ----------

ps_fmt <- function(x, sig = 5) {
  vapply(x, function(v) {
    if (is.na(v)) return("—")
    if (!is.finite(v)) return(if (v > 0) "∞" else "−∞")
    out <- format(signif(v, sig), big.mark = ",", scientific = FALSE, trim = TRUE)
    sub("^-", "−", out)
  }, "", USE.NAMES = FALSE)
}
ps_p <- function(p) vapply(p, function(x) if (is.na(x)) "—" else if (x < 1e-4) "< 0.0001" else formatC(x, format = "f", digits = 4), "", USE.NAMES = FALSE)
ps_pe <- function(p) vapply(p, function(x) if (is.na(x)) "p not available" else if (x < 1e-4) "p < 0.0001" else paste("p =", ps_p(x)), "", USE.NAMES = FALSE)
ps_list <- function(x, k = 6) if (length(x) <= k) paste(x, collapse = ", ") else paste0(paste(head(x, k), collapse = ", "), ", …")
pl_word <- function(w) if (grepl("(s|x|z|ch|sh)$", w)) paste0(w, "es") else if (grepl("[^aeiou]y$", w)) sub("y$", "ies", w) else paste0(w, "s")
ucfirst <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
lcfirst <- function(x) paste0(tolower(substring(x, 1, 1)), substring(x, 2))
plural <- function(n, w, pl = paste0(w, "s")) sprintf("%s %s", format(n), if (n == 1) w else pl)
ps_alt_sym <- function(alt) switch(alt %||% "two.sided", two.sided = "≠", greater = ">", less = "<")

# Names used in the text: the problem's own wording, else the column name.
ps_lab <- function(meta, key) {
  l <- meta$labels[[key]]
  if (!is.null(l) && nzchar(l)) return(l)
  meta$roles[[key]] %||% key
}
ps_unit <- function(meta) { u <- meta$labels$unit %||% ""; if (nzchar(u)) paste0(" ", u) else "" }

# ---------- Structure ----------

# Read the mapped columns into an analysis frame and describe its layout.
ps_structure <- function(df, roles) {
  st <- list(errors = character(0), warnings = character(0), roles = roles, facs = character(0))
  resp <- roles$response %||% ""
  if (is.null(df) || !nrow(df)) { st$errors <- "There is no data yet. Paste, type or upload the dataset."; return(st) }
  if (!nzchar(resp)) { st$errors <- "Choose the response: the numeric column you measured."; return(st) }
  if (!resp %in% names(df)) { st$errors <- sprintf("The Response role is set to \"%s\", which is not a column in the data.", resp); return(st) }
  used <- unlist(roles[c("response", PS_ROLE_KEYS)]); used <- used[nzchar(used)]
  if (any(duplicated(used))) { st$errors <- sprintf("The column \"%s\" has two roles. Give each column one role only.", used[duplicated(used)][1]); return(st) }
  raw <- df[[resp]]
  y <- suppressWarnings(as.numeric(as.character(raw)))
  bad <- which(is.na(y) & !is.na(raw) & trimws(as.character(raw)) != "")
  if (length(bad)) st$errors <- c(st$errors, sprintf("The response %s has %s that %s (row %d holds \"%s\"). Correct %s in the data editor.",
                                                     resp, plural(length(bad), "value"), if (length(bad) == 1) "is not a number" else "are not numbers",
                                                     bad[1], raw[bad[1]], if (length(bad) == 1) "it" else "them"))
  d <- data.frame(y = y, .order = seq_along(y))
  for (k in PS_ROLE_KEYS) {
    col <- roles[[k]] %||% ""
    if (!nzchar(col)) next
    if (!col %in% names(df)) { st$errors <- c(st$errors, sprintf("The %s role is set to \"%s\", which is not a column in the data. Choose another column for it, or (none).", PS_ROLE_NAMES[[k]], col)); next }
    v <- trimws(as.character(df[[col]])); v[v == ""] <- NA
    lev <- roles$levels[[k]] %||% unique(v[!is.na(v)])
    d[[k]] <- factor(v, levels = unique(c(lev, unique(v[!is.na(v)]))))
    st$facs <- c(st$facs, k)
  }
  if (length(st$errors)) return(st)
  keep <- !is.na(d$y)
  for (k in st$facs) keep <- keep & !is.na(d[[k]])
  # Cells whose factors are known but whose response is missing (Chapter 4 missing-value problems).
  lost <- is.na(d$y) & Reduce(`&`, lapply(st$facs, function(k) !is.na(d[[k]])), TRUE)
  if (length(st$facs) && any(lost)) st$missing <- data.frame(lapply(d[lost, st$facs, drop = FALSE], as.character), stringsAsFactors = FALSE)
  if (sum(!keep)) st$warnings <- c(st$warnings, sprintf("%s with a missing value %s left out.", plural(sum(!keep), "row"), if (sum(!keep) == 1) "was" else "were"))
  d <- d[keep, , drop = FALSE]
  for (k in st$facs) d[[k]] <- droplevels(d[[k]])
  rownames(d) <- NULL
  st$d <- d; st$n <- nrow(d)
  if (st$n < 2) st$errors <- c(st$errors, "At least 2 observations are needed.")
  if ("treatment" %in% st$facs) { st$ntrt <- nlevels(d$treatment); st$reps <- table(d$treatment) }
  if (all(c("treatment", "block") %in% st$facs)) {
    tab <- table(d$treatment, d$block); st$nblk <- nlevels(d$block)
    st$complete <- all(tab >= 1); st$one_per_cell <- all(tab == 1)
  }
  st
}

# Which sample test the layout calls for.
ps_kind <- function(st, meta) {
  if (length(st$errors)) return(NA_character_)
  has <- function(k) k %in% st$facs
  if (!has("treatment")) return(if (identical(meta$target, "variance")) "variance" else "one")
  if (st$ntrt < 2) return(NA_character_)
  if (has("row") && has("column")) return(if (has("greek")) "glsd" else "latin")
  if (has("block")) {
    if (st$ntrt == 2 && isTRUE(st$one_per_cell)) return("paired")
    if (!is.null(meta$design)) return(meta$design)
    if (isTRUE(st$complete)) return("rcbd")
    return(if (all(table(st$d$block) >= 2)) "bibd" else NA_character_)
  }
  if (any(st$reps < 2)) return(NA_character_)
  if (st$ntrt == 2) "two" else "k"
}

ps_kind_problem <- function(kind, st) {
  if (length(st$errors)) return(paste(st$errors, collapse = " "))
  if ("block" %in% st$facs) return("Every block needs at least two observations.")
  "The Group column needs at least two levels, each with at least two observations."
}

# The data as samples: list(x) for one sample, x1/x2 aligned by pair for paired data, groups otherwise.
ps_samples <- function(kind, st) {
  d <- st$d
  switch(kind,
    one = , variance = list(d$y),
    paired = {
      lv <- levels(d$treatment)
      w <- stats::reshape(d[, c("y", "treatment", "block")], idvar = "block", timevar = "treatment", direction = "wide")
      list(w[[paste0("y.", lv[1])]], w[[paste0("y.", lv[2])]], block = as.character(w$block))
    },
    split(d$y, d$treatment))
}

# The quantity whose normality matters for the test.
ps_norm_targets <- function(kind, st) {
  s <- ps_samples(kind, st)
  switch(kind,
    one = , variance = list(Data = s[[1]]),
    paired = stats::setNames(list(s[[1]] - s[[2]]), sprintf("Differences (%s − %s)", levels(st$d$treatment)[1], levels(st$d$treatment)[2])),
    two = s,
    stats::setNames(list(unname(stats::residuals(ps_model_fit(kind, st)))), sprintf("Residuals of the %s model", ps_model_name(kind))))
}

ps_stated_normal <- function(meta) grepl("normally distributed|assumed to be normal|normal distribution|are normal|is normal", paste(meta$statement, collapse = " "), ignore.case = TRUE)

ps_stated_alpha <- function(meta) {
  txt <- paste(c(meta$statement, meta$parts), collapse = " ")
  m <- regmatches(txt, regexpr("(α|alpha)\\s*=\\s*0?\\.[0-9]+", txt, ignore.case = TRUE))
  if (length(m)) as.numeric(sub(".*=\\s*", "", m)) else NA_real_
}

# ---------- Information available (step 4) ----------

ps_normality <- function(kind, st, alpha) {
  tg <- ps_norm_targets(kind, st)
  rows <- lapply(names(tg), function(nm) { w <- sw_test(tg[[nm]])
    data.frame(Sample = nm, n = sum(is.finite(tg[[nm]])), W = w$W, p = w$p, stringsAsFactors = FALSE) })
  tab <- do.call(rbind, rows)
  tab$Result <- ifelse(is.na(tab$p), "Not available", ifelse(tab$p < alpha, "Not normal", "Normal"))
  tab
}

# Breusch-Godfrey (lag 1) on the residuals in the order the data were recorded.
ps_independence <- function(kind, st) {
  d <- st$d
  fit <- switch(kind,
    one = , variance = stats::lm(y ~ 1, data = d),
    paired = { s <- ps_samples(kind, st); stats::lm(dd ~ 1, data = data.frame(dd = s[[1]] - s[[2]])) },
    ps_model_fit(kind, st))
  bg <- tryCatch(lmtest::bgtest(fit, order = 1), error = function(e) NULL)
  list(fit = fit, stat = if (!is.null(bg)) unname(bg$statistic) else NA, df = if (!is.null(bg)) unname(bg$parameter) else NA,
       p = if (!is.null(bg)) bg$p.value else NA)
}

# Equal variances of the groups: the F test for two samples; Bartlett or Levene (the
# student's choice) for k samples, which decides between the one-way and Welch ANOVA.
ps_equal_var <- function(kind, st, alpha, var_test = "levene") {
  if (!kind %in% c("two", "k")) return(NULL)
  d <- st$d; s <- split(d$y, d$treatment)
  lev <- quiet(car::leveneTest(y ~ treatment, data = d, center = stats::median))
  rows <- list()
  if (kind == "two") { ft <- quiet(stats::var.test(s[[1]], s[[2]]))
    if (!is.null(ft)) rows[[1]] <- data.frame(Test = "F test of two variances", Statistic = sprintf("F0 = %s", ps_fmt(unname(ft$statistic), 4)),
                                              df = paste(ft$parameter, collapse = ", "), p = ft$p.value, stringsAsFactors = FALSE) }
  if (kind == "k") { bt <- quiet(stats::bartlett.test(y ~ treatment, data = d))
    if (!is.null(bt)) rows[[1]] <- data.frame(Test = "Bartlett", Statistic = sprintf("K² = %s", ps_fmt(unname(bt$statistic), 4)),
                                              df = format(unname(bt$parameter)), p = bt$p.value, stringsAsFactors = FALSE) }
  if (!is.null(lev)) rows[[length(rows) + 1]] <- data.frame(Test = "Levene (median)", Statistic = sprintf("F = %s", ps_fmt(lev$`F value`[1], 4)),
                                                            df = paste(lev$Df, collapse = ", "), p = lev$`Pr(>F)`[1], stringsAsFactors = FALSE)
  tab <- do.call(rbind, rows)
  tab$Result <- ifelse(tab$p < alpha, "Unequal", "Equal")
  # The two-sample choice follows the F test; the k-sample choice follows the test the student picks.
  key <- if (kind == "two") 1 else if (identical(var_test, "bartlett")) 1 else nrow(tab)
  list(table = tab, equal = isTRUE(tab$p[key] >= alpha), basis = tab$Test[key], var_test = if (kind == "k") var_test else "f")
}

ps_test_choices <- function(kind, s) {
  sig1 <- !is.null(s[["sigma"]]); sig2 <- !is.null(s[["sigma"]]) && !is.null(s[["sigma2"]])
  switch(kind,
    one = c(t = "One-sample t-test", if (sig1) c(z = "One-sample z-test (σ known)"), wilcox = "Wilcoxon signed-rank test", sign = "Sign test"),
    variance = c(chisq = "χ² test on one variance"),
    two = c(pooled = "Pooled two-sample t-test (equal variances)", welch = "Welch two-sample t-test (unequal variances)",
            if (sig2) c(z = "Two-sample z-test (σ1, σ2 known)"), mw = "Mann-Whitney U test (Wilcoxon rank-sum)"),
    paired = c(paired = "Paired t-test", wsr = "Wilcoxon signed-rank test (paired)", sign = "Sign test (paired)"),
    k = c(anova = "One-way ANOVA (F test)", welch = "Welch ANOVA (unequal variances)", kw = "Kruskal-Wallis test"),
    rcbd = c(anova = ps_param_name("rcbd"), friedman = "Friedman test"),
    stats::setNames(c(ps_param_name(kind), ps_np_name(kind)), c("anova", "rankres")))
}
PS_NONPAR_TESTS <- c("wilcox", "sign", "mw", "wsr", "kw", "friedman", "rankres")

ps_info <- function(kind, st, meta, s) {
  if (kind %in% PS_BLOCK_KINDS) return(ps_info_block(kind, st, meta, s))
  a <- s$alpha; d <- st$d
  lab <- ps_lab(meta, "response"); u <- ps_unit(meta)
  lv <- if ("treatment" %in% st$facs) levels(d$treatment) else character(0)
  sizes <- if (length(lv)) table(d$treatment) else c(Sample = st$n)
  # k samples always start with a parametric test; their assumptions are checked after it.
  norm <- if (kind != "k") ps_normality(kind, st, a)
  ind <- if (kind %in% c("two", "paired")) ps_independence(kind, st)
  sig <- !is.null(s[["sigma"]]) && (kind != "two" || !is.null(s[["sigma2"]])) && kind %in% c("one", "two")
  # Equal variances only matter when σ1 and σ2 are unknown and have to be estimated.
  ev <- if ((kind == "two" && !sig) || kind == "k") ps_equal_var(kind, st, a, s$var_test %||% "levene")
  stated <- ps_stated_normal(meta); big <- min(sizes) >= 30; normal_ok <- is.null(norm) || all(norm$Result != "Not normal")
  fam_auto <- if (kind %in% c("variance", "k") || stated || normal_ok || big) "param" else "nonpar"
  fam <- if (isTRUE(s$family %in% c("param", "nonpar")) && !kind %in% c("variance", "k")) s$family else fam_auto
  samples_txt <- switch(kind,
    one = , variance = sprintf("One sample of %d independent observations.", st$n),
    paired = sprintf("Dependent (paired): each %s was measured under both %s, so the two measurements on a %s are related. The analysis uses the %d differences.",
                     ps_lab(meta, "block"), paste(lv, collapse = " and "), ps_lab(meta, "block"), length(ps_samples(kind, st)[[1]])),
    two = sprintf("Independent: different units received %s and %s.", lv[1], lv[2]),
    k = sprintf("Independent: %d groups of different units, one group for each %s.", length(lv), ps_lab(meta, "treatment")))
  facts <- data.frame(
    What = c("Response", "Number of samples", "Sample sizes", "Samples are", "Population standard deviation", "Population distribution"),
    Information = c(
      sprintf("%s (%s%s)", st$roles$response, lab, if (nzchar(u)) paste0(",", u) else ""),
      switch(kind, one = , variance = "1", paired = "2 (paired)", two = "2", k = as.character(length(lv))),
      if (length(lv)) paste(sprintf("%s: %d", names(sizes), as.integer(sizes)), collapse = "; ") else sprintf("n = %d", st$n),
      samples_txt,
      if (sig) sprintf("Known: %s (stated in the problem).", if (kind == "two") sprintf("σ1 = %s, σ2 = %s", format(s[["sigma"]]), format(s[["sigma2"]])) else sprintf("σ = %s", format(s[["sigma"]])))
      else "Unknown: estimated by the sample standard deviation.",
      if (stated) "Stated to be normal in the problem."
      else if (kind == "k") "Not stated. Normality, constant variance and independence are checked after the test (assumption checks)."
      else if (normal_ok) "Not stated; the Shapiro-Wilk test below finds no evidence against normality."
      else if (big) "Not normal by Shapiro-Wilk, but every sample has n ≥ 30, so the central limit theorem applies."
      else "Not stated, and the Shapiro-Wilk test below finds the data are not normal."),
    stringsAsFactors = FALSE)
  fam_reason <- if (kind == "variance") "The question is about a variance; the χ² test assumes a normal population."
    else if (kind == "k") "A parametric test (one-way ANOVA, or Welch ANOVA when the group variances differ) is always run first. If an assumption check fails afterwards, the Kruskal-Wallis test is used."
    else if (fam != fam_auto) sprintf("You chose a %s test.", if (fam == "param") "parametric" else "non-parametric")
    else if (stated) "The population is stated to be normal, so a parametric test is used."
    else if (normal_ok) "Normality is reasonable, so a parametric test is used."
    else if (big) "The samples are large (n ≥ 30), so a parametric test is used."
    else "Normality fails and the samples are small, so a non-parametric (rank) test is used."
  vm <- s$var_mode %||% "auto"
  sug <- switch(kind,
    one = if (fam == "nonpar") "wilcox" else if (sig) "z" else "t",
    variance = "chisq",
    two = if (fam == "nonpar") "mw" else if (sig) "z" else switch(vm, equal = "pooled", unequal = "welch", if (isTRUE(ev$equal)) "pooled" else "welch"),
    paired = if (fam == "nonpar") "wsr" else "paired",
    k = switch(vm, equal = "anova", unequal = "welch", if (isTRUE(ev$equal)) "anova" else "welch"))
  sug_reason <- switch(sug,
    z = "σ is known, so the z-test is used.",
    t = "σ is unknown and normality is reasonable, so the one-sample t-test is used.",
    wilcox = "The data are not normal, so the Wilcoxon signed-rank test compares the median with the target using ranks.",
    chisq = "One sample and a question about σ², so the χ² test on one variance is used.",
    pooled = sprintf("Normal populations, σ unknown and %s, so the pooled two-sample t-test is used.", if (vm == "equal") "equal variances assumed" else sprintf("equal variances by the %s", ev$basis)),
    welch = if (kind == "k") sprintf("The group variances are %s, so Welch ANOVA is used: it does not assume equal variances.",
                                     if (vm == "unequal") "assumed unequal" else sprintf("unequal by the %s", ev$basis))
            else sprintf("Normal populations, σ unknown and %s, so the Welch t-test is used.", if (vm == "unequal") "unequal variances assumed" else sprintf("unequal variances by the %s", ev$basis)),
    mw = "Independent samples that are not normal, so the Mann-Whitney U test is used.",
    paired = "Dependent samples with normal differences, so the paired t-test is used.",
    wsr = "Dependent samples whose differences are not normal, so the Wilcoxon signed-rank test is used.",
    anova = sprintf("%d independent groups compared on one factor with equal variances (%s), so the one-way ANOVA F test is used.", length(lv),
                    if (vm == "equal") "assumed" else sprintf("by the %s", ev$basis)),
    kw = "k independent samples that are not normal, so the Kruskal-Wallis test is used.")
  list(facts = facts, normality = norm, independence = ind, equal_var = ev, stated = stated, family_auto = fam_auto, family = fam,
       family_reason = fam_reason, suggest = sug, suggest_reason = sug_reason, sigma_known = sig)
}

# ---------- Hypotheses (step 2) ----------

ps_hypotheses <- function(kind, st, meta, s, family = "param") {
  lab <- ps_lab(meta, "response"); u <- ps_unit(meta); v <- s$value %||% 0; alt <- s$alt %||% "two.sided"
  lv <- if ("treatment" %in% st$facs) levels(st$d$treatment) else character(0)
  centre <- if (family == "nonpar") "median" else "mean"; sym <- if (family == "nonpar") "η" else "μ"
  word <- function(two_sided, greater, less) switch(alt, two.sided = two_sided, greater = greater, less = less)
  switch(kind,
    one = list(h0 = sprintf("The %s %s is %s%s.", centre, lab, format(v), u),
               h1 = sprintf("The %s %s is %s %s%s.", centre, lab, word("different from", "greater than", "less than"), format(v), u),
               h0s = sprintf("H0: %s = %s", sym, format(v)), h1s = sprintf("H1: %s %s %s", sym, ps_alt_sym(alt), format(v))),
    variance = list(h0 = sprintf("The variance of %s is %s.", lab, format(v)),
                    h1 = sprintf("The variance of %s is %s %s.", lab, word("different from", "greater than", "less than"), format(v)),
                    h0s = sprintf("H0: σ² = %s", format(v)), h1s = sprintf("H1: σ² %s %s", ps_alt_sym(alt), format(v))),
    two = list(h0 = if (v == 0) sprintf("The %s %s is the same for %s and %s.", centre, lab, lv[1], lv[2])
                    else sprintf("The %s %s for %s exceeds that for %s by %s%s.", centre, lab, lv[1], lv[2], format(v), u),
               h1 = if (v == 0) sprintf("The %s %s %s.", centre, lab, word(sprintf("differs between %s and %s", lv[1], lv[2]), sprintf("is greater for %s than for %s", lv[1], lv[2]), sprintf("is less for %s than for %s", lv[1], lv[2])))
                    else sprintf("The difference in %s %s (%s − %s) is %s %s%s.", centre, lab, lv[1], lv[2], word("different from", "greater than", "less than"), format(v), u),
               h0s = sprintf("H0: %s1 − %s2 = %s", sym, sym, format(v)), h1s = sprintf("H1: %s1 − %s2 %s %s", sym, sym, ps_alt_sym(alt), format(v))),
    paired = list(h0 = sprintf("There is no difference in %s %s between %s and %s.", centre, lab, lv[1], lv[2]),
                  h1 = sprintf("The %s %s %s.", centre, lab, word(sprintf("differs between %s and %s", lv[1], lv[2]), sprintf("is greater for %s than for %s", lv[1], lv[2]), sprintf("is less for %s than for %s", lv[1], lv[2]))),
                  h0s = sprintf("H0: %sd = %s", sym, format(v)), h1s = sprintf("H1: %sd %s %s", sym, ps_alt_sym(alt), format(v))),
    k = , rcbd = , latin = , glsd = , bibd = list(h0 = sprintf("The %s %s is the same for all %d %s.", centre, lab, length(lv), pl_word(ps_lab(meta, "treatment"))),
             h1 = sprintf("At least one %s has a different %s %s.", ps_lab(meta, "treatment"), centre, lab),
             h0s = sprintf("H0: %s", paste(sprintf("%s%d", sym, seq_along(lv)), collapse = " = ")),
             h1s = sprintf("H1: %sᵢ ≠ %sⱼ for at least one pair", sym, sym)))
}

# ---------- Test (step 5) ----------

ps_run_test <- function(kind, test, st, meta, s) {
  if (kind %in% PS_BLOCK_KINDS) return(ps_run_block(kind, test, st, meta, s))
  a <- s$alpha; conf <- 1 - a; alt <- if (kind == "k") "two.sided" else s$alt %||% "two.sided"; v <- s$value %||% 0
  smp <- ps_samples(kind, st); lv <- if ("treatment" %in% st$facs) levels(st$d$treatment) else "Sample"
  base <- list(mu = v, alt = alt, conf = conf, alpha = a, names = lv, var_mode = s$var_mode %||% "auto", method = "auto")
  # Pass known σ only when complete, so the engine never picks up a half-set pair.
  if (kind == "one" && !is.null(s[["sigma"]])) base$sigma <- s[["sigma"]]
  if (kind == "two" && !is.null(s[["sigma"]]) && !is.null(s[["sigma2"]])) { base$sigma <- s[["sigma"]]; base$sigma2 <- s[["sigma2"]] }
  out <- list(kind = kind, test = test, settings = s, alpha = a, conf = conf, alt = alt, ref = if (kind == "k") NA else v, levels = lv, control = meta$control)
  if (kind == "variance") {
    y <- smp[[1]]; n <- length(y); s2 <- stats::var(y); dfree <- n - 1; chi <- dfree * s2 / v
    p <- p_from(chi, alt, function(x) stats::pchisq(x, dfree))
    ci <- switch(alt, two.sided = c(dfree * s2 / stats::qchisq(1 - a / 2, dfree), dfree * s2 / stats::qchisq(a / 2, dfree)),
                 greater = c(dfree * s2 / stats::qchisq(1 - a, dfree), Inf), less = c(0, dfree * s2 / stats::qchisq(a, dfree)))
    out$t <- mk_test("chisq", "χ² test on one variance", "param", "χ0²", chi, p, "Sample variance S²", s2, ci[1], ci[2], df = dfree, dist = "chisq")
    out$desc <- cbind(sample = "Sample", describe_vec(y, conf))
    out$formula <- c(sprintf("n = %d,   S² = Σ(yᵢ − ȳ)² / (n − 1) = %s", n, ps_fmt(s2)),
                     sprintf("χ0² = (n − 1) S² / σ0² = %d × %s / %s = %s,   df = %d", dfree, ps_fmt(s2), format(v), ps_fmt(chi, 4), dfree))
  } else {
    r <- switch(kind, one = run_one(smp[[1]], base), two = run_two(smp[[1]], smp[[2]], base), paired = run_paired(smp[[1]], smp[[2]], base),
                k = run_k(st$d$y, st$d$treatment, base))
    t <- r$tests[[test]]
    if (is.null(t)) stop(sprintf("%s is not available for these data.", test))
    out$r <- r; out$t <- t; out$desc <- r$desc
    if (kind == "k") {
      out$anova <- r$anova; out$fit <- r$fit
      if (test == "anova") {
        out$ph_all <- posthoc_all(r$fit, data.frame(y = r$data$y, treatment = r$data$g), "treatment", r$anova$mse, r$anova$dfe, a, conf, control = meta$control, dunnett = FALSE)
        out$ci_tab <- r$posthoc$tukey; out$ci_name <- "Tukey"; out$anova_df <- ps_anova_table(out)
      } else if (test == "welch") { out$ci_tab <- r$posthoc$gh; out$ci_name <- "Games-Howell" }
      else {
        d <- st$d; N <- nrow(d); rk <- rank(d$y); Ri <- tapply(rk, d$treatment, sum); ni <- tapply(rk, d$treatment, length)
        H0 <- 12 / (N * (N + 1)) * sum(Ri^2 / ni) - 3 * (N + 1); ties <- table(d$y); cf <- 1 - sum(ties^3 - ties) / (N^3 - N)
        out$np_formula <- c(sprintf("Rank all N = %d observations together, then add the ranks in each group (Rᵢ).", N),
                            sprintf("H = 12 / (N(N + 1)) × Σ Rᵢ²/nᵢ − 3(N + 1) = %s", ps_fmt(H0, 5)),
                            if (cf < 1) sprintf("Correction for ties: H / %s = %s", ps_fmt(cf, 5), ps_fmt(H0 / cf, 5)),
                            sprintf("df = k − 1 = %d;   H is compared with the χ² distribution", nlevels(d$treatment) - 1))
        out$rank_table <- ps_rank_table(st); out$rank_title <- "Ranks by group"
      }
    }
    out$formula <- out$np_formula %||% ps_formula(kind, test, r, smp, base)
  }
  t <- out$t
  out$dist <- t$dist
  out$df1 <- if (!is.null(t$df1)) t$df1 else if (is.numeric(t$df)) t$df else NA
  out$df2 <- t$df2 %||% NA
  out$p <- t$p; out$reject <- !is.na(t$p) && t$p < a
  out
}

# The test statistic worked out with the numbers, for the parametric tests.
ps_formula <- function(kind, test, r, smp, s) {
  t <- r$tests[[test]]; f <- function(x, d = 5) ps_fmt(x, d); v <- s$mu %||% 0
  switch(paste(kind, test),
    "one t" = { y <- smp[[1]]; n <- length(y)
      c(sprintf("n = %d,   ȳ = %s,   s = %s", n, f(mean(y)), f(stats::sd(y))),
        sprintf("t0 = (ȳ − μ0) / (s / √n) = (%s − %s) / (%s / √%d) = %s,   df = n − 1 = %d", f(mean(y)), format(v), f(stats::sd(y)), n, f(t$stat, 4), n - 1)) },
    "one z" = { y <- smp[[1]]; n <- length(y)
      c(sprintf("n = %d,   ȳ = %s,   σ = %s (known)", n, f(mean(y)), format(s$sigma)),
        sprintf("Z0 = (ȳ − μ0) / (σ / √n) = (%s − %s) / (%s / √%d) = %s", f(mean(y)), format(v), format(s$sigma), n, f(t$stat, 4))) },
    "two pooled" = { x1 <- smp[[1]]; x2 <- smp[[2]]; n1 <- length(x1); n2 <- length(x2)
      sp <- sqrt(((n1 - 1) * stats::var(x1) + (n2 - 1) * stats::var(x2)) / (n1 + n2 - 2))
      c(sprintf("ȳ1 = %s,  S1² = %s,  n1 = %d;    ȳ2 = %s,  S2² = %s,  n2 = %d", f(mean(x1)), f(stats::var(x1)), n1, f(mean(x2)), f(stats::var(x2)), n2),
        sprintf("Sp² = [(n1 − 1)S1² + (n2 − 1)S2²] / (n1 + n2 − 2) = %s,   Sp = %s", f(sp^2), f(sp)),
        sprintf("t0 = (ȳ1 − ȳ2 − %s) / (Sp √(1/n1 + 1/n2)) = %s,   df = n1 + n2 − 2 = %d", format(v), f(t$stat, 4), n1 + n2 - 2)) },
    "two welch" = { x1 <- smp[[1]]; x2 <- smp[[2]]
      c(sprintf("ȳ1 = %s,  S1² = %s,  n1 = %d;    ȳ2 = %s,  S2² = %s,  n2 = %d", f(mean(x1)), f(stats::var(x1)), length(x1), f(mean(x2)), f(stats::var(x2)), length(x2)),
        sprintf("t0 = (ȳ1 − ȳ2 − %s) / √(S1²/n1 + S2²/n2) = %s,   Welch df = %s", format(v), f(t$stat, 4), f(t$df, 4))) },
    "two z" = { x1 <- smp[[1]]; x2 <- smp[[2]]
      c(sprintf("ȳ1 = %s,  n1 = %d,  σ1 = %s;    ȳ2 = %s,  n2 = %d,  σ2 = %s", f(mean(x1)), length(x1), format(s$sigma), f(mean(x2)), length(x2), format(s$sigma2)),
        sprintf("Z0 = (ȳ1 − ȳ2 − %s) / √(σ1²/n1 + σ2²/n2) = %s", format(v), f(t$stat, 4))) },
    "paired paired" = { dd <- smp[[1]] - smp[[2]]; n <- length(dd)
      c(sprintf("dⱼ = %s − %s for each pair:  %s", s$names[1], s$names[2], ps_list(f(dd, 4), 12)),
        sprintf("n = %d,   d̄ = %s,   Sd = %s", n, f(mean(dd)), f(stats::sd(dd))),
        sprintf("t0 = (d̄ − %s) / (Sd / √n) = %s,   df = n − 1 = %d", format(v), f(t$stat, 4), n - 1)) },
    "k welch" = { g <- r$data$g; y <- r$data$y; n <- tapply(y, g, length); v <- tapply(y, g, stats::var); w <- n / v
      c(sprintf("Weights wᵢ = nᵢ / Sᵢ²:  %s", paste(sprintf("%s: %s", names(w), f(w, 4)), collapse = ",  ")),
        sprintf("F* = Σwᵢ(ȳᵢ − ȳw)² / (k − 1), divided by 1 + 2(k − 2)/(k² − 1) × Σ(1 − wᵢ/Σw)²/(nᵢ − 1);   F* = %s", f(t$stat, 5)),
        sprintf("df = (%s, %s) — the second df is the Welch approximation", f(t$df1, 4), f(t$df2, 4))) },
    "k anova" = { an <- r$anova; tr <- an$table[1, ]
      c(sprintf("SS_Treatments = %s on %d df,   SS_Error = %s on %d df", f(tr$ss, 7), tr$df, f(an$sse, 7), an$dfe),
        sprintf("MS_Treatments = SS / df = %s,   MS_Error = %s", f(tr$ss / tr$df, 6), f(an$mse, 6)),
        sprintf("F0 = MS_Treatments / MS_Error = %s,   df = (%d, %d)", f(tr$F, 5), tr$df, an$dfe)) },
    c(t$about, sprintf("%s = %s%s", t$stat_name, f(t$stat, 5),
                       if (length(t$df) && !is.na(t$df[1]) && nzchar(as.character(t$df[1]))) sprintf(",   df = %s", if (is.numeric(t$df)) f(t$df, 4) else t$df) else "")))
}

ps_test_table <- function(res) {
  t <- res$t
  dfx <- if (is.character(t$df)) t$df else if (is.na(t$df %||% NA)) "—" else ps_fmt(t$df, 4)
  ci <- if (is.finite(t$lwr %||% NA) || is.finite(t$upr %||% NA)) sprintf("[%s, %s]", ps_fmt(t$lwr), ps_fmt(t$upr)) else "—"
  stats::setNames(data.frame(t$name, sprintf("%s = %s", t$stat_name, ps_fmt(t$stat, 5)), dfx, ps_p(t$p),
                             if (!is.null(t$est_name)) sprintf("%s = %s", t$est_name, ps_fmt(t$est)) else "—", ci, stringsAsFactors = FALSE),
                  c("Test", "Statistic", "df", "P-value", "Estimate", sprintf("%g%% confidence interval", 100 * res$conf)))
}

ps_desc_table <- function(res) {
  d <- res$desc
  data.frame(Sample = d$sample, n = d$n, Mean = ps_fmt(d$mean), SD = ps_fmt(d$sd, 4), Median = ps_fmt(d$median), Min = ps_fmt(d$min), Max = ps_fmt(d$max), check.names = FALSE)
}

ps_anova_table <- function(res) {
  an <- res$anova; tr <- an$table[1, ]
  data.frame(Source = c(tr$source, "Error", "Total"), df = c(tr$df, an$dfe, an$dft),
             `Sum of squares` = ps_fmt(c(tr$ss, an$sse, an$sst), 6), `Mean square` = c(ps_fmt(tr$ss / tr$df, 6), ps_fmt(an$mse, 6), ""),
             F0 = c(ps_fmt(tr$F, 5), "", ""), `P-value` = c(ps_p(tr$p), "", ""), check.names = FALSE)
}

# ---------- Decision (step 6) ----------

# Critical values and the rejection rule for the test's reference distribution. Rank tests
# with no tabled distribution here are shown through their standard normal equivalent.
ps_reference <- function(res) {
  t <- res$t; a <- res$alpha; alt <- res$alt
  if (res$dist %in% c("t", "z")) {
    q <- if (res$dist == "z") stats::qnorm else function(p) stats::qt(p, res$df1)
    crit <- switch(alt, two.sided = c(q(a / 2), q(1 - a / 2)), greater = q(1 - a), less = q(a))
    sym <- if (res$dist == "z") "Z0" else "t0"
    return(list(dist = res$dist, stat = t$stat, df1 = res$df1, df2 = NA, crit = crit, sym = sym, approx = FALSE, alt = alt,
                rule = switch(alt, two.sided = sprintf("|%s| > %s", sym, ps_fmt(crit[2], 4)), greater = sprintf("%s > %s", sym, ps_fmt(crit, 4)), less = sprintf("%s < %s", sym, ps_fmt(crit, 4)))))
  }
  if (res$dist == "F") {
    crit <- stats::qf(1 - a, res$df1, res$df2)
    return(list(dist = "F", stat = t$stat, df1 = res$df1, df2 = res$df2, crit = crit, sym = "F0", approx = FALSE, alt = "greater",
                rule = sprintf("F0 > F(%g; %s, %s) = %s", a, ps_fmt(res$df1, 4), ps_fmt(res$df2, 4), ps_fmt(crit, 4))))
  }
  if (res$dist == "chisq") {
    alt2 <- if (ps_multi(res$kind)) "greater" else alt
    crit <- switch(alt2, two.sided = c(stats::qchisq(a / 2, res$df1), stats::qchisq(1 - a / 2, res$df1)), greater = stats::qchisq(1 - a, res$df1), less = stats::qchisq(a, res$df1))
    return(list(dist = "chisq", stat = t$stat, df1 = res$df1, df2 = NA, crit = crit, sym = "χ0²", approx = FALSE, alt = alt2,
                rule = switch(alt2, two.sided = sprintf("χ0² < %s or χ0² > %s", ps_fmt(crit[1], 4), ps_fmt(crit[2], 4)), greater = sprintf("χ0² > %s", ps_fmt(crit, 4)), less = sprintf("χ0² < %s", ps_fmt(crit, 4)))))
  }
  # Rank tests: the standard normal statistic with the same p-value.
  p <- min(max(t$p, 1e-12), 1 - 1e-12)
  side <- if (is.finite(t$est %||% NA) && is.finite(res$ref %||% NA)) sign(t$est - res$ref) else 1
  z <- switch(alt, two.sided = (if (side == 0) 1 else side) * stats::qnorm(1 - p / 2), greater = stats::qnorm(1 - p), less = stats::qnorm(p))
  crit <- switch(alt, two.sided = c(stats::qnorm(a / 2), stats::qnorm(1 - a / 2)), greater = stats::qnorm(1 - a), less = stats::qnorm(a))
  list(dist = "z", stat = z, df1 = NA, df2 = NA, crit = crit, sym = "z", approx = TRUE, alt = alt,
       rule = switch(alt, two.sided = sprintf("|z| > %s", ps_fmt(crit[2], 4)), greater = sprintf("z > %s", ps_fmt(crit, 4)), less = sprintf("z < %s", ps_fmt(crit, 4))))
}

ref_value <- function(res) if (res$kind == "variance") res$settings$value else res$ref

ps_decision <- function(res, h1_text) {
  t <- res$t; a <- res$alpha; ref <- ps_reference(res); rej <- res$reject
  in_rr <- switch(ref$alt, two.sided = ref$stat < ref$crit[1] || ref$stat > ref$crit[length(ref$crit)], greater = ref$stat > max(ref$crit), less = ref$stat < min(ref$crit))
  rr_txt <- sprintf("Reject H0 if %s. %s %s = %s, which %s the rejection region.", ref$rule,
                    if (ref$approx) sprintf("The %s gives the standard normal equivalent", t$name) else "The test statistic is",
                    ref$sym, ps_fmt(ref$stat, 4), if (in_rr) "falls in" else "is not in")
  ci <- NULL
  if (ps_multi(res$kind)) {
    ph <- res$ci_tab
    if (!is.null(ph) && !is.null(ph$lwr)) {
      excl <- ph$lwr > 0 | ph$upr < 0
      ci <- list(available = TRUE, reject = any(excl), table = ph,
                 text = sprintf("%g%% %s intervals for every difference of two %s: %d of %d exclude 0. %s", 100 * res$conf, res$ci_name %||% "Tukey",
                                if (identical(res$kind, "bibd")) "adjusted means" else "means",
                                sum(excl), length(excl),
                                if (any(excl) == rej) { if (rej) "At least one pair of means differs, which agrees with rejecting H0." else "Every interval contains 0, which agrees with not rejecting H0." }
                                else if (rej) "No single pair is clearly different even though the F test rejects H0; near the boundary the two methods can disagree. The F test decides H0."
                                else "One interval excludes 0 although the F test does not reject H0; near the boundary the two methods can disagree. The F test decides H0, and the post-hoc step is not carried out."))
    } else ci <- list(available = FALSE, text = sprintf("The %s has no confidence interval, so decide with the p-value or the rejection region.", t$name))
  } else if (is.finite(t$lwr %||% NA) || is.finite(t$upr %||% NA)) {
    lo <- t$lwr; hi <- t$upr; h0 <- ref_value(res)
    excl <- (is.finite(lo) && lo > h0) || (is.finite(hi) && hi < h0)
    side <- switch(res$alt, two.sided = "two-sided interval", greater = "one-sided interval (lower bound)", less = "one-sided interval (upper bound)")
    ci <- list(available = TRUE, reject = excl, lo = lo, hi = hi,
               text = sprintf("The %g%% %s for %s is [%s, %s]. The H0 value %s is %s the interval, so %s H0.", 100 * res$conf, side, lcfirst(t$est_name %||% "the parameter"),
                              ps_fmt(lo), ps_fmt(hi), format(h0), if (excl) "outside" else "inside", if (excl) "reject" else "do not reject"))
  } else ci <- list(available = FALSE, text = "This test does not give a confidence interval, so decide with the p-value or the rejection region.")
  pv_txt <- sprintf("P-value = %s %s α = %s, so %s H0.", ps_p(t$p), if (rej) "<" else "≥", format(a), if (rej) "reject" else "do not reject")
  h1w <- lcfirst(sub("\\.$", "", sub("^\\s*H1\\s*:\\s*", "", trimws(h1_text))))
  concl <- if (rej) sprintf("Reject H0. At α = %s there is sufficient evidence to conclude that %s (%s, %s).", format(a), h1w, t$name, ps_pe(t$p))
           else sprintf("Fail to reject H0. At α = %s there is not sufficient evidence to conclude that %s (%s, %s).", format(a), h1w, t$name, ps_pe(t$p))
  list(reject = rej, ref = ref, rr = list(reject = in_rr, text = rr_txt), ci = ci, pv = list(reject = rej, text = pv_txt), conclusion = concl)
}

# ---------- Post-hoc (step 7) ----------

# Dunnett is offered only when the problem names a control group.
ps_posthoc_choices <- function(test, kind = "k", control = NULL) switch(test,
  anova = c(tukey = "Tukey HSD", lsd = "Fisher LSD", bonferroni = "Bonferroni", scheffe = "Scheffé",
            if (!is.null(control) && !identical(kind, "bibd")) c(dunnett = sprintf("Dunnett (each treatment vs the control, %s)", control))),
  welch = c(gh = "Games-Howell"), kw = c(dunn = "Dunn test (Holm adjustment)"), friedman = c(nemenyi = "Nemenyi test"),
  rankres = c(dunn = "Dunn test on the ranked residuals (Holm adjustment)"), character(0))

# The method suggested first: the one the problem asks for, Dunnett when it names a control, else Tukey.
ps_default_ph <- function(s, choices) {
  if (!length(choices)) return(NA_character_)
  if (!is.null(s$posthoc) && s$posthoc %in% names(choices)) s$posthoc else names(choices)[1]
}

ps_posthoc <- function(res, method) {
  a <- res$alpha; rank_based <- res$test %in% c("kw", "friedman", "rankres")
  if (res$test == "anova") {
    ph <- res$ph_all
    # Dunnett's simultaneous intervals are simulated, so they are computed only when asked for.
    if (method == "dunnett" && is.null(ph$methods$dunnett) && !is.null(res$control) && !identical(res$kind, "bibd")) {
      an <- res$an %||% res$anova; dd <- res$d %||% data.frame(y = res$r$data$y, treatment = res$r$data$g)
      ph <- posthoc_all(res$fit, dd, "treatment", an$mse, an$dfe, res$alpha, res$conf, control = res$control)
    }
    if (!method %in% names(ph$methods) || is.null(ph$methods[[method]])) method <- "tukey"
    tab <- ph$methods[[method]]; lt <- ph$letters[[method]]
    centre <- stats::setNames(ph$means$mean, ph$means$level)
  } else if (res$test == "welch") {
    r <- res$r; tab <- r$posthoc$gh; lt <- r$letters$gh; centre <- tapply(r$data$y, r$data$g, mean)
  } else if (res$test == "kw") {
    r <- res$r; tab <- r$posthoc$dunn; lt <- r$letters$dunn; centre <- tapply(rank(r$data$y), r$data$g, mean)
  } else {
    tab <- res$np_ph; centre <- res$centre
    lt <- cld_letters(names(centre), as.numeric(centre), tab$i[tab$p < a], tab$j[tab$p < a])
  }
  tab$signif <- tab$p < a
  ord <- names(sort(centre, decreasing = TRUE))
  df <- data.frame(Comparison = tab$pair, Difference = if (!is.null(tab$diff)) ps_fmt(tab$diff) else "—",
                   CI = if (!is.null(tab$lwr)) sprintf("[%s, %s]", ps_fmt(tab$lwr), ps_fmt(tab$upr)) else "—",
                   `Adjusted p` = ps_p(tab$p), Result = ifelse(tab$signif, "Different", "Not different"), check.names = FALSE)
  if (method == "lsd") names(df)[4] <- "p-value"
  if (rank_based) { names(df)[2] <- "Mean rank difference"; df$CI <- NULL } else names(df)[3] <- sprintf("%g%% CI", 100 * res$conf)
  if (identical(res$kind, "bibd") && res$test == "anova") names(df)[2] <- "Difference (adjusted means)"
  cn <- if (rank_based) "Mean rank" else if (identical(res$kind, "bibd")) "Adjusted mean" else "Mean"
  groups <- if (method == "dunnett") {
    ctrl <- res$control
    data.frame(Level = ord, Centre = ps_fmt(centre[ord]),
               `Compared with the control` = ifelse(ord == ctrl, "Control", ifelse(ord %in% tab$j[tab$signif], "Differs from the control", "Same as the control")), check.names = FALSE)
  } else data.frame(Level = ord, Centre = ps_fmt(centre[ord]), Group = unname(lt[ord]), check.names = FALSE)
  names(groups)[2] <- cn
  list(table = df, raw = tab, groups = groups)
}

# ---------- Assumptions (after the test, for ANOVA-type designs) ----------

# Normality, constant variance and independence of the residuals of the fitted model.
# Constant variance is the Breusch-Pagan test on the residuals; Welch ANOVA does not need it.
ps_assumptions <- function(kind, st, s, test = "anova") {
  a <- s$alpha; fit <- ps_model_fit(kind, st); res <- unname(stats::residuals(fit))
  sw <- sw_test(res); ad <- if (length(res) >= 8) quiet(nortest::ad.test(res)) else NULL
  nm <- sprintf("Residuals of the %s model", ps_model_name(kind))
  normal_ok <- is.na(sw$p) || sw$p >= a
  ntab <- data.frame(Sample = nm, n = length(res), `Shapiro-Wilk W` = ps_fmt(sw$W, 4), `Shapiro-Wilk p` = ps_p(sw$p),
                     `Anderson-Darling p` = if (is.null(ad)) "n < 8" else ps_p(ad$p.value), Result = if (normal_ok) "Normal" else "Not normal", check.names = FALSE)
  bg <- tryCatch(lmtest::bgtest(fit, order = 1), error = function(e) NULL)
  bg_p <- if (is.null(bg)) NA else bg$p.value
  itab <- data.frame(Test = "Breusch-Godfrey (lag 1)", Statistic = sprintf("LM = %s", if (is.null(bg)) "—" else ps_fmt(unname(bg$statistic), 4)),
                     df = if (is.null(bg)) "—" else format(unname(bg$parameter)), `p-value` = ps_p(bg_p),
                     Result = if (is.na(bg_p)) "Not available" else if (bg_p < a) "Dependent" else "Independent", check.names = FALSE)
  welch <- identical(kind, "k") && identical(test, "welch")
  bp <- if (!welch) tryCatch(lmtest::bptest(fit), error = function(e) NULL)
  bp_p <- if (is.null(bp)) NA else bp$p.value
  vtab <- if (!welch) data.frame(Test = "Breusch-Pagan", Statistic = sprintf("BP = %s", if (is.null(bp)) "—" else ps_fmt(unname(bp$statistic), 4)),
                                 df = if (is.null(bp)) "—" else format(unname(bp$parameter)), `p-value` = ps_p(bp_p),
                                 Result = if (is.na(bp_p)) "Not available" else if (bp_p < a) "Not constant" else "Constant", check.names = FALSE)
  var_ok <- if (welch || is.na(bp_p)) NA else bp_p >= a
  indep_ok <- is.na(bg_p) || bg_p >= a
  failed <- c(Normality = !normal_ok, `Constant variance` = isFALSE(var_ok), Independence = !indep_ok)
  list(normality = ntab, normal_ok = normal_ok, independence = itab, indep_ok = indep_ok, variance = vtab, var_ok = var_ok, welch = welch,
       all_ok = !any(failed), failed = names(failed)[failed], targets = stats::setNames(list(res), nm), ind_fit = fit,
       p = c(normality = sw$p, variance = bp_p, independence = bg_p))
}

# One row per assumption, for the non-parametric restart.
ps_asm_summary <- function(a) {
  data.frame(Assumption = c("Normality", "Constant variance", "Independence"),
             Test = c("Shapiro-Wilk", "Breusch-Pagan", "Breusch-Godfrey (lag 1)"),
             `p-value` = ps_p(unname(a$p[c("normality", "variance", "independence")])),
             Result = ifelse(c(a$normal_ok, !isFALSE(a$var_ok), a$indep_ok), ifelse(c(FALSE, isTRUE(a$welch), FALSE), "Not required (Welch)", "Satisfied"), "Not satisfied"),
             check.names = FALSE)
}

# Group sizes, medians and mean ranks for the Kruskal-Wallis test.
ps_rank_table <- function(st) {
  d <- st$d; r <- rank(d$y)
  data.frame(Group = levels(d$treatment), n = as.integer(table(d$treatment)), Median = ps_fmt(tapply(d$y, d$treatment, stats::median)),
             `Rank sum` = ps_fmt(tapply(r, d$treatment, sum), 6), `Mean rank` = ps_fmt(tapply(r, d$treatment, mean), 4), check.names = FALSE)
}

# ---------- Plots ----------

ps_plot_pvalue <- function(ref, p, alpha) {
  if (ref$dist %in% c("t", "z")) {
    dn <- if (ref$dist == "z") stats::dnorm else function(x) stats::dt(x, ref$df1)
    lim <- min(max(4, abs(ref$stat) * 1.15), 12); x <- seq(-lim, lim, length.out = 500); d <- data.frame(x = x, y = dn(x))
    sh <- switch(ref$alt, two.sided = abs(d$x) >= abs(ref$stat), greater = d$x >= ref$stat, less = d$x <= ref$stat)
  } else {
    dn <- if (ref$dist == "F") function(x) stats::df(x, ref$df1, ref$df2) else function(x) stats::dchisq(x, ref$df1)
    cr <- max(ref$crit); lim <- max(cr * 1.8, min(ref$stat * 1.15, cr * 4)); x <- seq(0.001, lim, length.out = 500)
    yv <- dn(x); yv[!is.finite(yv)] <- NA; yv <- pmin(yv, stats::quantile(yv, .98, na.rm = TRUE) * 1.2); d <- data.frame(x = x, y = yv)
    sh <- switch(ref$alt, greater = d$x >= ref$stat, less = d$x <= ref$stat,
                 two.sided = if (ref$stat > stats::qchisq(.5, ref$df1)) d$x >= ref$stat else d$x <= ref$stat)
  }
  shown <- max(min(ref$stat, max(d$x)), min(d$x)); top <- max(d$y, na.rm = TRUE)
  ggplot(d, aes(x, y)) + geom_area(data = transform(d, y = ifelse(sh, y, NA)), fill = PETROL, alpha = .35, na.rm = TRUE) +
    geom_line(colour = INK, linewidth = .8, na.rm = TRUE) + geom_vline(xintercept = shown, colour = GREEN, linewidth = 1.1) +
    annotate("text", x = shown, y = top * .8, label = sprintf("p = %s", ps_p(p)), hjust = if (shown > mean(range(d$x))) 1.08 else -0.08, colour = PETROL, size = 4, fontface = "bold") +
    labs(x = "Shaded area = p-value", y = NULL) + theme_sds() + theme(axis.text.y = element_blank())
}

ps_plot_rr <- function(ref, alpha) {
  if (ref$dist %in% c("t", "z")) return(plot_dist_alt(ref$dist, ref$stat, ref$df1, NA, alpha, ref$alt))
  if (ref$dist == "F" || (ref$dist == "chisq" && ref$alt == "greater")) return(plot_dist(ref$dist, ref$stat, ref$df1, ref$df2, alpha))
  # Two-sided or lower-tail chi-squared (the variance test).
  cr <- ref$crit; lim <- max(max(cr) * 1.6, ref$stat * 1.15); x <- seq(0.001, lim, length.out = 500); d <- data.frame(x = x, y = stats::dchisq(x, ref$df1))
  rej <- if (length(cr) == 2) d$x <= cr[1] | d$x >= cr[2] else d$x <= cr
  ggplot(d, aes(x, y)) + geom_area(data = transform(d, y = ifelse(rej, y, NA)), fill = RED, alpha = .25, na.rm = TRUE) +
    geom_line(colour = INK, linewidth = .8) + geom_vline(xintercept = cr, colour = RED, linetype = "dashed") +
    geom_vline(xintercept = min(ref$stat, lim), colour = GREEN, linewidth = 1.1) +
    annotate("text", x = cr, y = max(d$y) * .95, label = sprintf("%.2f", cr), hjust = -0.1, colour = RED, size = 3.6) +
    annotate("text", x = min(ref$stat, lim), y = max(d$y) * .7, label = sprintf("χ² = %.2f", ref$stat), hjust = -0.08, colour = GREEN, size = 3.8, fontface = "bold") +
    labs(x = sprintf("χ²(%s)", format(ref$df1)), y = NULL) + theme_sds() + theme(axis.text.y = element_blank())
}

ps_plot_pbar <- function(p, alpha) {
  d <- data.frame(k = factor(c("p-value", "α"), levels = c("α", "p-value")), v = c(p, alpha))
  ggplot(d, aes(v, k, fill = k)) + geom_col(width = .5) + geom_text(aes(label = ps_p(v)), hjust = -0.15, colour = INK, size = 3.8) +
    scale_fill_manual(values = c(`p-value` = PETROL, `α` = RED)) + scale_x_continuous(limits = c(0, max(1, p) * 1.15)) +
    labs(x = NULL, y = NULL) + theme_sds()
}

ps_plot_ci_k <- function(tab, conf) {
  tab$pair <- factor(tab$pair, levels = rev(tab$pair)); tab$excl <- ifelse(tab$lwr > 0 | tab$upr < 0, "Excludes 0", "Contains 0")
  ggplot(tab, aes(diff, pair, colour = excl)) + geom_vline(xintercept = 0, colour = RED, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lwr, xmax = upr), height = .18, linewidth = 1) + geom_point(size = 3) +
    scale_colour_manual(values = c("Excludes 0" = GREEN, "Contains 0" = "#8A8F9C")) +
    labs(x = sprintf("Difference in means (%g%%)", 100 * conf), y = NULL) + theme_sds()
}

ps_plot_data <- function(kind, st, meta) {
  ylab <- paste0(st$roles$response, if (nzchar(meta$labels$unit %||% "")) paste0(" (", meta$labels$unit, ")") else "")
  d <- st$d
  if (kind == "paired") {
    s <- ps_samples(kind, st); lv <- levels(d$treatment); n <- length(s[[1]])
    dd <- data.frame(pair = rep(seq_len(n), 2), g = factor(rep(lv, each = n), levels = lv), y = c(s[[1]], s[[2]]))
    return(ggplot(dd, aes(g, y, group = pair)) + geom_line(colour = "#8A8F9C", linewidth = .6) + geom_point(colour = PETROL, size = 2.6) +
             stat_summary(aes(group = 1), fun = mean, geom = "line", colour = GREEN, linewidth = 1.3) +
             stat_summary(aes(group = 1), fun = mean, geom = "point", colour = GREEN, size = 4, shape = 15) +
             labs(x = NULL, y = ylab, subtitle = "Grey: one line per pair · Green: means") + theme_sds() + theme(plot.subtitle = element_text(colour = INK, size = 10.5)))
  }
  if (!"treatment" %in% st$facs) return(plot_box_by(d$y, factor(rep("Sample", nrow(d))), ylab))
  plot_box_by(d$y, d$treatment, ylab)
}

ps_plot_order <- function(fit) {
  r <- stats::residuals(fit)
  ggplot(data.frame(i = seq_along(r), r = r), aes(i, r)) + geom_hline(yintercept = 0, colour = RED, linetype = "dashed") +
    geom_line(colour = GREEN, linewidth = .7) + geom_point(colour = GREEN, size = 2.4) +
    labs(x = "Observation order (as recorded)", y = "Residuals") + theme_sds()
}

ps_plot_rvf <- function(fit) {
  ggplot(data.frame(f = stats::fitted(fit), r = stats::residuals(fit)), aes(f, r)) + geom_hline(yintercept = 0, colour = RED, linetype = "dashed") +
    geom_point(colour = PETROL, size = 2.6, alpha = .85) + labs(x = "Fitted values", y = "Residuals") + theme_sds()
}

# ---------- Defaults and classification for the list ----------

ps_default_settings <- function(meta, kind) {
  s <- meta$settings
  list(alpha = s$alpha %||% 0.05, alt = s$alt %||% "two.sided",
       value = if (identical(kind, "variance")) s$var0 %||% 1 else s$mu0 %||% 0,
       sigma = s[["sigma"]], sigma2 = s[["sigma2"]], var_mode = s$var_mode %||% "auto", var_test = s$var_test %||% "levene", family = "auto",
       posthoc = s$posthoc %||% (if (!is.null(meta$control)) "dunnett" else "tukey"))
}

# Problem type, test type, test and post-hoc of a problem as the steps would solve it, cached
# per problem: used for the categories and filters of the list.
.ps_class_cache <- new.env()
ps_classify <- function(p) {
  if (!is.null(.ps_class_cache[[p$id]])) return(.ps_class_cache[[p$id]])
  st <- ps_structure(p$data, p$roles); kind <- ps_kind(st, p)
  out <- list(kind = kind, type = if (is.na(kind)) "Other" else unname(PS_TYPE[kind]), family = "param", test = "", posthoc = "Not applicable")
  if (is.na(kind)) return(out)
  s <- ps_default_settings(p, kind)
  info <- tryCatch(ps_info(kind, st, p, s), error = function(e) NULL)
  if (!ps_multi(kind)) {
    out$family <- info$family %||% "param"
    out$test <- if (!is.null(info)) unname(ps_test_choices(kind, s)[info$suggest]) else ""
  } else {
    test <- info$suggest %||% "anova"
    res <- tryCatch(ps_run_test(kind, test, st, p, s), error = function(e) NULL)
    asm <- tryCatch(ps_assumptions(kind, st, s, test), error = function(e) NULL)
    pname <- sub(" \\(F test\\)$", "", ps_param_name(kind, test))
    if (!is.null(asm) && !asm$all_ok) {
      npt <- ps_np_test(kind); rn <- tryCatch(ps_run_test(kind, npt, st, p, s), error = function(e) NULL)
      out$family <- "nonpar"; out$test <- sprintf("%s, then %s", pname, ps_np_short(kind))
      out$posthoc <- if (isTRUE(rn$reject)) unname(PS_PH_SHORT[names(ps_posthoc_choices(npt, kind))[1]]) else "None"
    } else {
      out$test <- pname
      out$posthoc <- if (isTRUE(res$reject)) unname(PS_PH_SHORT[ps_default_ph(s, ps_posthoc_choices(test, kind, p$control))]) else "None"
    }
  }
  .ps_class_cache[[p$id]] <- out
  out
}
