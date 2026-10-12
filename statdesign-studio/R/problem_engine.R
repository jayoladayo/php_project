# Problem Sets engine: read the structure of a dataset, recommend an analysis,
# check that a chosen analysis fits the data, run it, and write the step-by-step
# Answer Model. Plain R with no Shiny, so it can be tested on its own.

PS_ANALYSES <- list(
  one_sample = list(label = "One-sample t-test (z-test when σ is known)", short = "One-sample test"),
  variance1 = list(label = "χ² test on one variance", short = "Variance test"),
  two_sample = list(label = "Two-sample t-test (independent samples)", short = "Two-sample t-test"),
  paired = list(label = "Paired t-test (RCBD with two treatments)", short = "Paired t-test"),
  crd = list(label = "One-way ANOVA (CRD)", short = "One-way ANOVA"),
  random = list(label = "Random-effects one-way ANOVA", short = "Random-effects ANOVA"),
  rcbd = list(label = "RCBD ANOVA", short = "RCBD ANOVA"),
  lsd = list(label = "Latin Square ANOVA", short = "Latin Square ANOVA"),
  glm = list(label = "General linear model", short = "General linear model"),
  nonpar = list(label = "Non-parametric rank test", short = "Rank test"),
  desc = list(label = "Descriptive statistics", short = "Descriptive statistics"),
  diag = list(label = "Residual and assumption diagnostics", short = "Diagnostics")
)

PS_ROLE_KEYS <- c("treatment", "block", "row", "column")
PS_ROLE_NAMES <- c(response = "Response", treatment = "Treatment", block = "Block", row = "Row", column = "Column")

ps_fmt <- function(x, sig = 5) {
  vapply(x, function(v) {
    if (is.na(v)) return("—")
    if (!is.finite(v)) return(if (v > 0) "∞" else "−∞")
    out <- format(signif(v, sig), big.mark = ",", scientific = FALSE, trim = TRUE)
    sub("^-", "−", out)
  }, "")
}
ps_p <- function(p) vapply(p, function(x) if (is.na(x)) "—" else if (x < 1e-4) "< 0.0001" else formatC(x, format = "f", digits = 4), "", USE.NAMES = FALSE)
ps_pe <- function(p) vapply(p, function(x) if (is.na(x)) "p not available" else if (x < 1e-4) "p < 0.0001" else paste("p =", ps_p(x)), "", USE.NAMES = FALSE)
ps_list <- function(x, k = 6) if (length(x) <= k) paste(x, collapse = ", ") else paste0(paste(head(x, k), collapse = ", "), ", …")
pl_word <- function(w) if (grepl("(s|x|z|ch|sh)$", w)) paste0(w, "es") else if (grepl("[^aeiou]y$", w)) sub("y$", "ies", w) else paste0(w, "s")
ucfirst <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
plural <- function(n, w, pl = paste0(w, "s")) sprintf("%s %s", format(n), if (n == 1) w else pl)

# Names used in the text: the problem's own wording, else the column name.
ps_lab <- function(meta, key) {
  l <- meta$labels[[key]]
  if (!is.null(l) && nzchar(l)) return(l)
  meta$roles[[key]] %||% key
}
ps_unit <- function(meta) { u <- meta$labels$unit %||% ""; if (nzchar(u)) paste0(" ", u) else "" }

# ---------- Structure ----------

# Read the mapped columns into an analysis frame and describe its layout.
ps_structure <- function(df, roles, transform = "none") {
  st <- list(errors = character(0), warnings = character(0), roles = roles, transform = transform, facs = character(0))
  resp <- roles$response %||% ""
  if (is.null(df) || !nrow(df)) { st$errors <- "There is no data yet. Paste, type or upload the dataset."; return(st) }
  if (!nzchar(resp)) { st$errors <- "Choose the response: the numeric column you measured."; return(st) }
  if (!resp %in% names(df)) { st$errors <- sprintf("The response column \"%s\" is not in the dataset.", resp); return(st) }
  used <- unlist(roles[c("response", PS_ROLE_KEYS)])
  used <- used[nzchar(used)]
  if (any(duplicated(used))) { st$errors <- sprintf("The column \"%s\" has two roles. Give each column one role only.", used[duplicated(used)][1]); return(st) }
  raw <- df[[resp]]
  y <- suppressWarnings(as.numeric(as.character(raw)))
  bad <- which(is.na(y) & !is.na(raw) & trimws(as.character(raw)) != "")
  if (length(bad)) st$errors <- c(st$errors, sprintf("The response %s has %s that %s not numbers (row %d holds \"%s\"). Correct %s in the data editor.",
                                                     resp, plural(length(bad), "value"), if (length(bad) == 1) "is" else "are", bad[1], raw[bad[1]],
                                                     if (length(bad) == 1) "it" else "them"))
  d <- data.frame(y = y)
  for (k in PS_ROLE_KEYS) {
    col <- roles[[k]] %||% ""
    if (!nzchar(col)) next
    if (!col %in% names(df)) { st$errors <- c(st$errors, sprintf("The %s column \"%s\" is not in the dataset.", tolower(PS_ROLE_NAMES[[k]]), col)); next }
    v <- trimws(as.character(df[[col]])); v[v == ""] <- NA
    lev <- roles$levels[[k]] %||% unique(v[!is.na(v)])
    d[[k]] <- factor(v, levels = unique(c(lev, unique(v[!is.na(v)]))))
    st$facs <- c(st$facs, k)
  }
  if (length(st$errors)) return(st)
  keep <- !is.na(d$y)
  for (k in st$facs) keep <- keep & !is.na(d[[k]])
  if (sum(!keep)) st$warnings <- c(st$warnings, sprintf("%s with a missing response or factor value %s left out.", plural(sum(!keep), "row"), if (sum(!keep) == 1) "was" else "were"))
  d <- d[keep, , drop = FALSE]
  for (k in st$facs) d[[k]] <- droplevels(d[[k]])
  if (transform == "log") {
    if (any(d$y <= 0)) st$errors <- c(st$errors, "The log transformation needs every response value to be above 0.") else d$y <- log(d$y)
  } else if (transform == "sqrt") {
    if (any(d$y < 0)) st$errors <- c(st$errors, "The square-root transformation needs every response value to be 0 or more.") else d$y <- sqrt(d$y)
  }
  rownames(d) <- NULL
  st$d <- d; st$n <- nrow(d)
  if (st$n < 2) st$errors <- c(st$errors, "At least 2 observations are needed.")
  st$nl <- vapply(st$facs, function(k) nlevels(d[[k]]), 0L)
  has <- function(k) k %in% st$facs
  if (has("treatment")) {
    st$ntrt <- nlevels(d$treatment); st$reps <- table(d$treatment)
    st$balanced <- length(unique(st$reps)) == 1
  }
  if (has("treatment") && has("block")) {
    tab <- table(d$treatment, d$block); st$nblk <- nlevels(d$block)
    st$complete <- all(tab >= 1); st$one_per_cell <- all(tab == 1); st$cell_equal <- length(unique(c(tab))) == 1
  }
  if (has("treatment") && has("row") && has("column")) {
    p <- st$ntrt; st$psize <- p
    st$latin <- nlevels(d$row) == p && nlevels(d$column) == p && st$n == p * p && all(table(d$row, d$column) == 1) &&
      all(table(d$treatment, d$row) == 1) && all(table(d$treatment, d$column) == 1)
  }
  st$integer_y <- all(abs(d$y - round(d$y)) < 1e-9)
  st
}

# A short description of the layout, used in the recommendation.
ps_layout_text <- function(st, meta) {
  r <- st$roles
  out <- sprintf("One numeric response, %s, with %s.", r$response, plural(st$n, "observation"))
  if (!is.null(st$ntrt)) out <- c(out, sprintf("A treatment factor, %s, with %d levels (%s), replicated %s.", r$treatment, st$ntrt, ps_list(levels(st$d$treatment)),
                                            if (st$balanced) sprintf("%d times each", st$reps[[1]]) else sprintf("unequally (%s)", paste(st$reps, collapse = ", "))))
  if ("block" %in% st$facs) out <- c(out, sprintf("A blocking factor, %s, with %d levels.", r$block, nlevels(st$d$block)))
  if ("row" %in% st$facs) out <- c(out, sprintf("A row factor, %s, with %d levels.", r$row, nlevels(st$d$row)))
  if ("column" %in% st$facs) out <- c(out, sprintf("A column factor, %s, with %d levels.", r$column, nlevels(st$d$column)))
  out
}

# ---------- Validation ----------

# Errors stop the analysis; warnings are shown but do not.
ps_validate <- function(kind, st, settings = list()) {
  e <- character(0); w <- character(0)
  if (length(st$errors)) return(list(errors = st$errors, warnings = st$warnings))
  d <- st$d; r <- st$roles; has <- function(k) k %in% st$facs
  need_trt <- function(min_levels = 2, min_reps = 2) {
    if (!has("treatment")) { e <<- c(e, sprintf("%s needs a treatment (group) column. Map the factor you are comparing to the Treatment role.", PS_ANALYSES[[kind]]$short)); return(FALSE) }
    if (st$ntrt < min_levels) { e <<- c(e, sprintf("%s has only %d level; at least %d are needed.", r$treatment, st$ntrt, min_levels)); return(FALSE) }
    if (min_reps > 1 && any(st$reps < min_reps)) { e <<- c(e, sprintf("Every level of %s needs at least %d observations; %s has %d.", r$treatment, min_reps, names(st$reps)[which.min(st$reps)], min(st$reps))); return(FALSE) }
    TRUE
  }
  num <- function(x) !is.null(x) && length(x) == 1 && !is.na(suppressWarnings(as.numeric(x)))
  switch(kind,
    one_sample = {
      if (!num(settings$mu0)) e <- c(e, "Enter the hypothesised mean μ0 in the analysis settings.")
      if (st$n < 2) e <- c(e, "A one-sample test needs at least 2 values.")
      if (has("treatment")) w <- c(w, sprintf("The data have a treatment column (%s). A one-sample test ignores it and treats all %d values as one sample.", r$treatment, st$n))
      if (num(settings[["sigma"]]) && as.numeric(settings[["sigma"]]) <= 0) e <- c(e, "The known σ must be above 0, or left empty if σ is unknown.")
    },
    variance1 = {
      if (!num(settings$var0) || as.numeric(settings$var0) <= 0) e <- c(e, "Enter the hypothesised variance σ0² (above 0) in the analysis settings.")
      if (st$n < 3) e <- c(e, "A test on a variance needs at least 3 values.")
      if (has("treatment")) w <- c(w, sprintf("The data have a treatment column (%s). This test pools all values into one sample.", r$treatment))
    },
    two_sample = {
      if (need_trt(2, 2) && st$ntrt != 2) e <- c(e, sprintf("A two-sample test compares exactly 2 groups, but %s has %d levels (%s). Choose One-way ANOVA (CRD) for 3 or more groups.", r$treatment, st$ntrt, ps_list(levels(d$treatment))))
      if (has("block")) w <- c(w, sprintf("The observations are paired by %s. An independent two-sample test ignores the pairing and usually loses power; the paired t-test is the matching analysis.", r$block))
    },
    paired = {
      if (need_trt(2, 1)) {
        if (st$ntrt != 2) e <- c(e, sprintf("A paired t-test compares exactly 2 treatments, but %s has %d levels. With 3 or more treatments in blocks, choose RCBD ANOVA.", r$treatment, st$ntrt))
        else if (!has("block")) e <- c(e, "A paired t-test needs a column that identifies the pairs (for example Inspector, Pair or Subject). Map it to the Block role. If the two samples are independent, choose the two-sample t-test.")
        else if (!isTRUE(st$one_per_cell)) {
          tab <- table(d$block, d$treatment); bad <- rownames(tab)[apply(tab, 1, function(z) any(z != 1))]
          e <- c(e, sprintf("Each pair must have exactly one value for each treatment. Check %s %s.", r$block, ps_list(bad, 4)))
        }
      }
    },
    crd = , random = {
      need_trt(2, if (kind == "crd") 2 else 1)
      if (kind == "random" && has("treatment") && any(st$reps < 1)) e <- c(e, "Every level needs at least one observation.")
      if (has("treatment") && st$n - st$ntrt < 1) e <- c(e, "There are no error degrees of freedom: some levels need more than one observation.")
      if (has("block")) w <- c(w, sprintf("The data also have a block column (%s). A one-way ANOVA leaves the block-to-block variation in the error term; RCBD ANOVA removes it.", r$block))
      if (has("row") || has("column")) w <- c(w, "The row and column factors are ignored by a one-way ANOVA.")
    },
    rcbd = {
      if (need_trt(2, 1)) {
        if (!has("block")) e <- c(e, "RCBD ANOVA needs a block column (the nuisance factor in which every treatment appears once). Map it to the Block role, or choose One-way ANOVA (CRD) if there is no blocking.")
        else if (st$nblk < 2) e <- c(e, "An RCBD needs at least 2 blocks.")
        else if (!isTRUE(st$complete)) {
          tab <- table(d$treatment, d$block); miss <- which(tab == 0, arr.ind = TRUE)
          e <- c(e, sprintf("The blocks are incomplete: %s %s has no observation in %s %s. Every treatment must appear in every block. Use the General linear model for incomplete blocks.",
                            r$treatment, rownames(tab)[miss[1, 1]], r$block, colnames(tab)[miss[1, 2]]))
        } else if (!isTRUE(st$one_per_cell)) w <- c(w, "Some treatment–block cells hold more than one observation. The additive RCBD model is still fitted.")
        if (has("block") && isTRUE(st$complete) && st$n - st$ntrt - st$nblk + 1 < 1) e <- c(e, "The design leaves no error degrees of freedom.")
      }
    },
    lsd = {
      miss <- c(treatment = "Treatment", row = "Row", column = "Column")[!c("treatment", "row", "column") %in% st$facs]
      if (length(miss)) e <- c(e, sprintf("Latin Square ANOVA needs row, column and treatment variables. Map a column to the %s role%s. If the data have only one blocking factor, choose RCBD ANOVA.", paste(miss, collapse = " and "), if (length(miss) > 1) "s" else ""))
      else {
        p <- st$ntrt
        if (nlevels(d$row) != p || nlevels(d$column) != p) e <- c(e, sprintf("A Latin square with %d treatments needs %d rows and %d columns; the data have %d rows and %d columns.", p, p, p, nlevels(d$row), nlevels(d$column)))
        else if (st$n != p * p) e <- c(e, sprintf("A %d × %d Latin square needs %d observations, one per row–column cell; the data have %d.", p, p, p * p, st$n))
        else if (any(table(d$row, d$column) != 1)) e <- c(e, "Each row–column cell must hold exactly one observation.")
        else {
          if (any(table(d$treatment, d$row) != 1)) e <- c(e, sprintf("Each treatment must appear exactly once in every row (%s). This arrangement is not a Latin square.", r$row))
          if (any(table(d$treatment, d$column) != 1)) e <- c(e, sprintf("Each treatment must appear exactly once in every column (%s). This arrangement is not a Latin square.", r$column))
        }
        if (p < 3) e <- c(e, "A Latin square needs at least 3 treatments to leave error degrees of freedom.")
      }
    },
    glm = {
      if (!length(st$facs)) e <- c(e, "A general linear model needs at least one factor. Map a treatment, block, row or column.")
      else {
        X <- stats::model.matrix(stats::as.formula(paste("y ~", paste(st$facs, collapse = " + "))), d)
        rk <- qr(X)$rank
        if (rk < ncol(X)) e <- c(e, "Some factors are completely confounded with each other, so their effects cannot be separated. Remove one of them.")
        else if (st$n - rk < 1) e <- c(e, "The model leaves no error degrees of freedom.")
      }
    },
    nonpar = {
      if (!length(st$facs) && !num(settings$mu0)) e <- c(e, "With one sample, the signed-rank test needs a hypothesised centre μ0. Enter it in the settings.")
      if (has("treatment") && st$ntrt < 2) e <- c(e, "The treatment needs at least 2 levels.")
      if (has("block") && has("treatment") && !isTRUE(st$complete)) e <- c(e, "The rank test for blocked data needs every treatment in every block.")
      if (!has("treatment") && length(st$facs)) e <- c(e, "Map the factor you are comparing to the Treatment role.")
    },
    desc = {},
    diag = { if (st$n < 4) e <- c(e, "Diagnostics need at least 4 observations.") }
  )
  list(errors = c(st$errors, e), warnings = c(st$warnings, w))
}

# ---------- Recommendation ----------

ps_recommend <- function(st, meta = list(), statement = "") {
  if (length(st$errors)) return(list(kind = NA, reasons = st$errors, computes = character(0), notes = character(0)))
  has <- function(k) k %in% st$facs; d <- st$d; r <- st$roles
  txt <- tolower(paste(statement, collapse = " "))
  random_txt <- grepl("selected at random|randomly selected|chosen at random|random sample of (batches|looms|ovens|positions|operators|machines)|random effect|large (number|population) of", txt)
  reasons <- ps_layout_text(st, meta); notes <- character(0)
  if (has("treatment") && has("row") && has("column")) {
    if (isTRUE(st$latin)) {
      kind <- "lsd"
      reasons <- c(reasons, sprintf("The %d × %d arrangement has one observation per row–column cell and every treatment appears once in each row and once in each column: a Latin square.", st$psize, st$psize),
                   "Rows and columns are two blocking factors, so the Latin Square ANOVA removes both before testing the treatments.")
    } else {
      kind <- "glm"
      reasons <- c(reasons, "Row, column and treatment variables are present, but the layout is not a valid Latin square (see the data checks), so the additive general linear model is used instead.")
    }
  } else if (has("treatment") && has("block")) {
    if (!isTRUE(st$complete)) {
      kind <- "glm"; reasons <- c(reasons, "Not every treatment appears in every block (incomplete blocks), so the general linear model, which handles missing cells, is used.")
    } else if (st$ntrt == 2 && isTRUE(st$one_per_cell)) {
      kind <- "paired"
      reasons <- c(reasons, sprintf("Every %s has exactly one observation under each of the 2 treatments (%d = 2 × %d), so the observations are paired within %s.", r$block, st$n, st$nblk, r$block),
                   sprintf("Pairs are blocks. The paired t-test works on the differences within each %s, so %s-to-%s variation cancels out.", r$block, r$block, r$block),
                   "With two treatments in complete blocks, the paired t-test and the RCBD ANOVA are the same test (F = t²); both are shown.")
    } else {
      kind <- "rcbd"
      reasons <- c(reasons, sprintf("Every treatment appears in every %s (%d treatments × %d blocks), so this is a randomized complete block design.", r$block, st$ntrt, st$nblk),
                   "The RCBD ANOVA separates the block-to-block variation from the error before testing the treatments.")
    }
  } else if (has("treatment")) {
    if (st$ntrt == 2 && !isTRUE(meta$random)) {
      kind <- "two_sample"
      reasons <- c(reasons, sprintf("There are exactly two independent groups of %s and no pairing variable, so the two samples are compared with a two-sample t-test.", r$treatment),
                   "The F test of the two variances is run first; it decides between the pooled and the Welch t-test.")
      if (!is.null(meta$settings[["sigma"]]) && !is.null(meta$settings[["sigma2"]])) reasons <- c(reasons, "The problem gives both population standard deviations, so the two-sample z-test is used.")
    } else if (isTRUE(meta$random) || (is.null(meta$random) && random_txt)) {
      kind <- "random"
      reasons <- c(reasons, sprintf("The levels of %s were selected at random from a larger population, so %s is a random factor.", r$treatment, r$treatment),
                   "The question is about the variability between levels in general, not about these particular levels, so the random-effects ANOVA and its variance components are used.")
    } else {
      kind <- "crd"
      reasons <- c(reasons, sprintf("There are %d groups with no blocking variable, and the units were assigned to the groups at random, so this is a completely randomized design (CRD).", st$ntrt),
                   "The one-way ANOVA F test compares all group means at once, and post-hoc tests then find which means differ.")
    }
  } else {
    if (identical(meta$target, "variance")) {
      kind <- "variance1"
      reasons <- c(reasons, "There is one sample and the question is about its variance (σ²), so the χ² test and interval for one variance are used.")
    } else {
      kind <- "one_sample"
      reasons <- c(reasons, "There is one sample and no grouping factor, so its mean is compared with the target value μ0.")
      if (!is.null(meta$settings[["sigma"]])) reasons <- c(reasons, sprintf("The population standard deviation is known (σ = %s), so the z-test is used.", format(meta$settings[["sigma"]])))
      else reasons <- c(reasons, "σ is not known, so it is estimated by the sample standard deviation and the t-test is used.")
    }
  }
  # Data checks that do not change the design but matter for the analysis.
  if (has("treatment") && kind %in% c("crd", "random", "two_sample", "rcbd", "lsd", "glm")) {
    sds <- tapply(d$y, d$treatment, stats::sd); mns <- tapply(d$y, d$treatment, mean)
    if (!st$balanced) notes <- c(notes, sprintf("Unequal replication (%s). The ANOVA still works; Tukey's test becomes Tukey-Kramer.", paste(st$reps, collapse = ", ")))
    if (all(is.finite(sds)) && min(sds) > 0 && max(sds) / min(sds) > 3 && st$transform == "none") {
      grows <- length(sds) >= 3 && isTRUE(stats::cor(mns, sds) > 0.5)
      notes <- c(notes, sprintf("The largest group SD is %.1f times the smallest%s, so the equal-variance assumption is in doubt. After running, check the residual plots; %s",
                                max(sds) / min(sds), if (grows) " and the SD grows with the mean" else "",
                                if (grows) "a log or square-root transformation, or the rank test, may be needed." else "the rank test or a Welch-type analysis may be safer."))
    }
    if (st$integer_y && min(d$y) >= 0 && max(d$y) <= 30 && st$transform == "none") notes <- c(notes, "The response is a small count. Counts often have variance that grows with the mean; the square-root transformation is the usual remedy if the residual plots show it.")
  }
  if (st$transform != "none") notes <- c(notes, sprintf("The analysis uses the %s of the response.", if (st$transform == "log") "natural log" else "square root"))
  if (isTRUE(meta$random) && kind != "random") notes <- c(notes, "The problem states that the levels were chosen at random; consider the random-effects ANOVA.")
  computes <- switch(kind,
    one_sample = c("Sample mean, standard deviation and standard error", "Test statistic, P-value and critical value", "Confidence interval on the mean", "Normality check (Shapiro-Wilk and QQ plot)"),
    variance1 = c("Sample variance", "χ² statistic, P-value and critical values", "Confidence interval on σ²", "Normality check (the χ² test depends on it)"),
    two_sample = c("Group means and standard deviations", "F test of equal variances", "Pooled or Welch t-test (z-test when σ is known)", "Confidence interval on the difference in means", "Normality check for each group"),
    paired = c("Differences within each pair", "Paired t-test", "Confidence interval on the mean difference", "Equivalent RCBD ANOVA (treatment and block effects)", "Normality check of the differences"),
    crd = c("ANOVA table: treatment effect", "F test, degrees of freedom and P-value", "Treatment mean comparisons (if the F test is significant)", "Assumption checks on the residuals"),
    random = c("ANOVA table and F test of H0: στ² = 0", "Variance components στ² and σ²", "Intraclass correlation and its confidence interval", "Assumption checks on the residuals"),
    rcbd = c("ANOVA table: treatment and block effects", "F test for treatments (and the block effect)", "Treatment mean comparisons (if significant)", "Relative efficiency of blocking", "Assumption checks on the residuals"),
    lsd = c("ANOVA table: treatment, row and column effects", "F test for treatments", "Treatment mean comparisons (if significant)", "Assumption checks on the residuals"),
    glm = c("Least-squares fit of the additive model", "Type II ANOVA table for every factor", "Adjusted (least-squares) treatment means and comparisons", "Assumption checks on the residuals"),
    nonpar = c("Rank-based test that does not assume normality"),
    desc = c("Summary statistics"), diag = c("Residual diagnostics"))
  list(kind = kind, reasons = reasons, computes = computes, notes = notes)
}

# Which analyses fit this dataset, for the selector.
ps_compatible <- function(st, settings = list()) {
  vapply(names(PS_ANALYSES), function(k) !length(ps_validate(k, st, settings)$errors), TRUE)
}

# ---------- Assumption checks ----------

ps_assump <- function(res_vec, alpha, group = NULL, what = "residuals", random_order = TRUE) {
  rows <- list(); txt <- character(0)
  verdict <- function(p) if (is.na(p)) "Not available" else if (p < alpha) "Fail" else "Pass"
  sw <- sw_test(res_vec)
  rows[[1]] <- data.frame(Assumption = "Normality", Test = sprintf("Shapiro-Wilk on the %s", what), Statistic = if (is.na(sw$W)) "—" else sprintf("W = %.4f", sw$W),
                          `p-value` = ps_p(sw$p), Result = verdict(sw$p), check.names = FALSE)
  txt <- c(txt, if (is.na(sw$p)) "Normality could not be tested (too few values)." else
    sprintf("Normality: Shapiro-Wilk W = %.4f, %s. %s", sw$W, ps_pe(sw$p),
            if (sw$p < alpha) sprintf("Since p < %s the %s depart from normality; look at the QQ plot and consider a transformation or a rank test.", format(alpha), what)
            else sprintf("Since p ≥ %s there is no evidence against normality; the points in the QQ plot should lie close to the line.", format(alpha))))
  if (!is.null(group) && nlevels(droplevels(group)) >= 2 && all(table(group) >= 2)) {
    g <- droplevels(group)
    lev <- quiet(car::leveneTest(res_vec ~ g, center = stats::median))
    bt <- quiet(stats::bartlett.test(res_vec ~ g))
    if (!is.null(lev)) rows[[length(rows) + 1]] <- data.frame(Assumption = "Equal variance", Test = "Levene (median, Brown-Forsythe)",
                                                               Statistic = sprintf("F = %.4f", lev$`F value`[1]), `p-value` = ps_p(lev$`Pr(>F)`[1]), Result = verdict(lev$`Pr(>F)`[1]), check.names = FALSE)
    if (!is.null(bt)) rows[[length(rows) + 1]] <- data.frame(Assumption = "Equal variance", Test = "Bartlett", Statistic = sprintf("K² = %.4f", unname(bt$statistic)),
                                                              `p-value` = ps_p(bt$p.value), Result = verdict(bt$p.value), check.names = FALSE)
    pv <- if (!is.null(lev)) lev$`Pr(>F)`[1] else if (!is.null(bt)) bt$p.value else NA
    txt <- c(txt, if (is.na(pv)) "Equal variance could not be tested." else
      sprintf("Equal variance: Levene's test gives %s%s. %s", ps_pe(pv), if (!is.null(bt)) sprintf(" and Bartlett's test %s", ps_pe(bt$p.value)) else "",
              if (pv < alpha) "The groups do not have the same spread; the residuals-versus-fitted plot will fan out. A transformation or a test that allows unequal variances is safer."
              else "The spread is similar in every group; the residuals-versus-fitted plot should show a band of even width."))
  }
  rows[[length(rows) + 1]] <- data.frame(Assumption = "Independence", Test = "Design: randomised run order", Statistic = "—", `p-value` = "—",
                                         Result = if (random_order) "By design" else "Check", check.names = FALSE)
  txt <- c(txt, if (random_order) "Independence: the run order is not recorded, so it cannot be tested from the data; it is ensured by randomising the runs, as the problem states." else
    "Independence: check how the data were collected; values from the same unit or time period may be correlated.")
  list(table = do.call(rbind, rows), text = txt, sw = sw)
}

# ---------- Plots ----------

ps_plot_groups <- function(d, ylab, xlab, term = "treatment") {
  if (!term %in% names(d)) {
    return(ggplot(d, aes(x = "", y = y)) + geom_boxplot(width = .35, fill = GREEN, alpha = .15, colour = GREEN, outlier.shape = NA) +
             geom_jitter(width = .06, height = 0, colour = PETROL, size = 2.6, alpha = .85) +
             stat_summary(fun = mean, geom = "point", shape = 23, size = 3.4, fill = "white", colour = GREEN) +
             labs(x = NULL, y = ylab) + theme_sds())
  }
  d$g <- d[[term]]
  ggplot(d, aes(g, y)) + geom_boxplot(width = .5, fill = GREEN, alpha = .15, colour = GREEN, outlier.shape = NA) +
    geom_jitter(width = .07, height = 0, colour = PETROL, size = 2.4, alpha = .85) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3.4, fill = "white", colour = GREEN) +
    labs(x = xlab, y = ylab) + theme_sds()
}

ps_plot_profile <- function(d, ylab, xlab, blab) {
  ggplot(d, aes(treatment, y, group = block)) + geom_line(colour = "#8A8F9C", linewidth = .6) +
    geom_point(colour = PETROL, size = 2.6) +
    stat_summary(aes(group = 1), fun = mean, geom = "line", colour = GREEN, linewidth = 1.4) +
    stat_summary(aes(group = 1), fun = mean, geom = "point", colour = GREEN, size = 4, shape = 15) +
    labs(x = xlab, y = ylab, subtitle = sprintf("Grey: one line per %s · Green: treatment means", blab)) + theme_sds() +
    theme(plot.subtitle = element_text(colour = INK, size = 10.5))
}

ps_plot_qq <- function(r, lab = "Residuals") {
  r <- r[is.finite(r)]; n <- length(r)
  q <- stats::qnorm(stats::ppoints(n)); s <- sort((r - mean(r)) / stats::sd(r))
  p <- stats::ppoints(n); se <- sqrt(p * (1 - p) / n) / stats::dnorm(q)
  band <- data.frame(q = q, lo = q - 1.96 * se, hi = q + 1.96 * se)
  ggplot() + geom_ribbon(data = band, aes(q, ymin = lo, ymax = hi), fill = GREEN, alpha = .10) +
    geom_abline(slope = 1, intercept = 0, colour = GREEN, linewidth = .9) +
    geom_point(aes(q, s), colour = PETROL, size = 2.6) +
    labs(x = "Theoretical normal quantiles", y = sprintf("Standardised %s", tolower(lab))) + theme_sds()
}

ps_plot_rvf <- function(fitted, resid) {
  ggplot(data.frame(f = fitted, r = resid), aes(f, r)) + geom_hline(yintercept = 0, colour = RED, linetype = "dashed") +
    geom_jitter(width = 0, height = 0, colour = PETROL, size = 2.6, alpha = .85) +
    labs(x = "Fitted values (predicted response)", y = "Residuals") + theme_sds()
}

ps_plot_ci <- function(tab, conf) {
  tab$pair <- factor(tab$pair, levels = rev(tab$pair))
  tab$sig <- ifelse(tab$signif, "Different", "Not different")
  ggplot(tab, aes(diff, pair, colour = sig)) + geom_vline(xintercept = 0, colour = RED, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lwr, xmax = upr), height = .18, linewidth = 1) + geom_point(size = 3) +
    scale_colour_manual(values = c("Different" = GREEN, "Not different" = "#8A8F9C")) +
    labs(x = sprintf("Difference in means, %g%% simultaneous interval", 100 * conf), y = NULL) + theme_sds()
}

# ---------- Shared pieces ----------

ps_crit <- function(alt, alpha, q) switch(alt, two.sided = q(1 - alpha / 2), greater = q(1 - alpha), less = q(alpha))
ps_alt_word <- function(alt) switch(alt, two.sided = "≠", greater = ">", less = "<")
ps_reject_rule <- function(alt, stat_sym, crit) switch(alt,
  two.sided = sprintf("|%s| > %s", stat_sym, ps_fmt(abs(crit), 4)),
  greater = sprintf("%s > %s", stat_sym, ps_fmt(crit, 4)),
  less = sprintf("%s < %s", stat_sym, ps_fmt(crit, 4)))

ps_decide <- function(p, alpha) !is.na(p) && p < alpha

ps_decision_txt <- function(p, alpha, stat_txt, crit_txt) {
  if (ps_decide(p, alpha)) sprintf("Since %s < α = %s (equivalently %s), we reject H0.", ps_pe(p), format(alpha), crit_txt)
  else sprintf("Since %s ≥ α = %s (equivalently %s is not in the rejection region), we fail to reject H0.", ps_pe(p), format(alpha), stat_txt)
}

ps_vars_table <- function(st, meta) {
  rows <- list(data.frame(Role = "Response", Variable = st$roles$response,
                          Description = sprintf("%s%s; %s", ps_lab(meta, "response"), if (nzchar(meta$labels$unit %||% "")) sprintf(" (%s)", meta$labels$unit) else "",
                                                if (st$transform == "none") "numeric" else sprintf("analysed on the %s scale", if (st$transform == "log") "log" else "square-root")),
                          stringsAsFactors = FALSE))
  desc <- c(treatment = "Treatment (the factor being compared)", block = "Block (nuisance factor)", row = "Row (first blocking factor)", column = "Column (second blocking factor)")
  for (k in st$facs) rows[[length(rows) + 1]] <- data.frame(Role = PS_ROLE_NAMES[[k]], Variable = st$roles[[k]],
                                                             Description = sprintf("%s: %d levels (%s)", desc[[k]], nlevels(st$d[[k]]), ps_list(levels(st$d[[k]]))), stringsAsFactors = FALSE)
  do.call(rbind, rows)
}

ps_means_df <- function(d, term, conf, mse = NULL, dfe = NULL) {
  f <- d[[term]]; n <- as.integer(table(f)); m <- as.numeric(tapply(d$y, f, mean)); s <- as.numeric(tapply(d$y, f, stats::sd))
  se <- if (!is.null(mse)) sqrt(mse / n) else s / sqrt(n)
  tc <- stats::qt(1 - (1 - conf) / 2, if (!is.null(dfe)) dfe else n - 1)
  data.frame(Level = levels(f), n = n, Total = ps_fmt(as.numeric(tapply(d$y, f, sum)), 6), Mean = ps_fmt(m), SD = ps_fmt(s, 4),
             CI = sprintf("[%s, %s]", ps_fmt(m - tc * se), ps_fmt(m + tc * se)), check.names = FALSE, stringsAsFactors = FALSE) |>
    stats::setNames(c("Level", "n", "Total", "Mean", "SD", sprintf("%g%% CI for the mean", 100 * conf)))
}

ps_anova_df <- function(rows, dfe, sse, dft, sst) {
  out <- data.frame(Source = c(rows$source, "Error", "Total"), df = c(rows$df, dfe, dft),
                    `Sum of squares` = ps_fmt(c(rows$ss, sse, sst), 6), `Mean square` = c(ps_fmt(rows$ss / rows$df, 6), ps_fmt(sse / dfe, 6), ""),
                    F0 = c(ps_fmt(rows$F, 5), "", ""), `P-value` = c(vapply(rows$p, ps_p, ""), "", ""), check.names = FALSE, stringsAsFactors = FALSE)
  out
}

# Pairwise comparisons after a significant ANOVA.
ps_compare <- function(fit, d, mse, dfe, alpha, conf, method, meta, sig) {
  lab_t <- ps_lab(meta, "treatment"); k <- nlevels(d$treatment)
  if (k < 3) {
    m <- tapply(d$y, d$treatment, mean)
    return(list(text = sprintf("With only two levels of %s, the F test itself compares the two means (%s: %s; %s: %s). No further comparisons are needed.",
                               lab_t, names(m)[1], ps_fmt(m[1]), names(m)[2], ps_fmt(m[2]))))
  }
  if (!sig) return(list(text = sprintf("The F test did not find a significant %s effect, so pairwise comparisons are not carried out: any differences between the sample means can be explained by chance.", lab_t)))
  ph <- posthoc_all(fit, d, "treatment", mse, dfe, alpha, conf)
  tab <- ph$methods[[method]]; lt <- ph$letters[[method]]
  mname <- c(tukey = "Tukey's HSD", lsd = "Fisher's LSD")[[method]]
  balanced <- length(unique(table(d$treatment))) == 1
  crit_txt <- if (method == "tukey") {
    if (balanced) sprintf("Tukey's critical difference is T = q(%s; %d, %d) × √(MSE / n) = %s × √(%s / %d) = %s.", format(alpha), k, dfe, ps_fmt(stats::qtukey(1 - alpha, k, dfe), 4), ps_fmt(mse), table(d$treatment)[[1]],
                          ps_fmt(stats::qtukey(1 - alpha, k, dfe) * sqrt(mse / table(d$treatment)[[1]]), 4))
    else "The group sizes differ, so the Tukey-Kramer version is used: each pair has its own critical difference q × √(MSE/2 × (1/nᵢ + 1/nⱼ))."
  } else {
    if (balanced) sprintf("Fisher's least significant difference is LSD = t(%s/2, %d) × √(2 MSE / n) = %s × √(2 × %s / %d) = %s.", format(alpha), dfe, ps_fmt(stats::qt(1 - alpha / 2, dfe), 4), ps_fmt(mse), table(d$treatment)[[1]],
                          ps_fmt(stats::qt(1 - alpha / 2, dfe) * sqrt(2 * mse / table(d$treatment)[[1]]), 4))
    else "The group sizes differ, so each pair uses LSD = t × √(MSE × (1/nᵢ + 1/nⱼ))."
  }
  means <- tapply(d$y, d$treatment, mean); ord <- names(sort(means, decreasing = TRUE))
  df <- data.frame(Comparison = tab$pair, Difference = ps_fmt(tab$diff), CI = sprintf("[%s, %s]", ps_fmt(tab$lwr), ps_fmt(tab$upr)),
                   `Adjusted p` = vapply(tab$p, ps_p, ""), Result = ifelse(tab$p < alpha, "Different", "Not different"), check.names = FALSE)
  if (method == "lsd") names(df)[4] <- "p-value"
  names(df)[3] <- sprintf("%g%% CI", 100 * conf)
  grp <- data.frame(Level = ord, Mean = ps_fmt(means[ord]), Group = unname(lt[ord]), check.names = FALSE)
  nsig <- sum(tab$p < alpha)
  list(method = mname, crit = crit_txt, table = df, groups = grp, ph = tab,
       text = c(sprintf("%s compares every pair of %s means while keeping the overall error rate at α = %s%s.", mname, lab_t, format(alpha),
                        if (method == "lsd") " for each comparison (it is the least conservative method)" else ""),
                crit_txt,
                sprintf("%d of %d pairs differ significantly. Levels that share a letter in the grouping table are not significantly different.", nsig, nrow(tab))))
}

# ---------- Analyses ----------

ps_run <- function(kind, st, settings, meta) {
  v <- ps_validate(kind, st, settings)
  if (length(v$errors)) stop(paste(v$errors, collapse = " "))
  s <- modifyList(list(alpha = 0.05, conf = 0.95, alt = "two.sided", posthoc = "tukey", var_mode = "auto"), Filter(Negate(is.null), settings))
  s$alpha <- as.numeric(s$alpha); s$conf <- as.numeric(s$conf)
  numset <- function(x) { x <- suppressWarnings(as.numeric(x)); if (length(x) == 1 && is.finite(x)) x else NULL }
  s$mu0 <- numset(s$mu0); s[["sigma"]] <- numset(s[["sigma"]]); s[["sigma2"]] <- numset(s[["sigma2"]]); s$var0 <- numset(s$var0)
  fn <- switch(kind, one_sample = ps_one, variance1 = ps_var1, two_sample = ps_two, paired = ps_paired, crd = ps_anova, random = ps_anova,
               rcbd = ps_anova, lsd = ps_anova, glm = ps_glm, nonpar = ps_nonpar, desc = ps_desc, diag = ps_diag)
  res <- fn(kind, st, s, meta)
  res$kind <- kind; res$label <- PS_ANALYSES[[kind]]$label; res$settings <- s; res$warnings <- v$warnings
  res
}

ps_ylab <- function(meta, st) {
  base <- paste0(st$roles$response, if (nzchar(meta$labels$unit %||% "")) paste0(" (", meta$labels$unit, ")") else "")
  switch(st$transform, log = sprintf("log(%s)", base), sqrt = sprintf("√(%s)", base), base)
}

# One sample: t or z test of the mean.
ps_one <- function(kind, st, s, meta) {
  y <- st$d$y; n <- length(y); a <- s$alpha; alt <- s$alt; mu0 <- s$mu0
  ybar <- mean(y); sd <- stats::sd(y); lab <- ps_lab(meta, "response"); u <- ps_unit(meta)
  z <- !is.null(s[["sigma"]])
  se <- if (z) s[["sigma"]] / sqrt(n) else sd / sqrt(n)
  stat <- (ybar - mu0) / se; dfree <- n - 1
  cdf <- if (z) stats::pnorm else function(x) stats::pt(x, dfree)
  q <- if (z) stats::qnorm else function(p) stats::qt(p, dfree)
  p <- p_from(stat, alt, cdf); crit <- ps_crit(alt, a, q)
  sym <- if (z) "Z0" else "t0"
  h <- (1 - s$conf) / 2; qc <- q(1 - h)
  ci <- c(ybar - qc * se, ybar + qc * se)
  sig <- ps_decide(p, a)
  chk <- ps_assump(y, a, what = "data")
  list(
    design = sprintf("A single sample of %s %s. There is no treatment comparison: the question is whether the population mean equals the target μ0 = %s.", plural(n, "measurement"), paste0("of ", lab), format(mu0)),
    why = if (z) "The population standard deviation σ is known, so the standardised mean follows a normal distribution and the one-sample Z-test is used." else
      "σ is unknown and is estimated by the sample standard deviation s, so the standardised mean follows a t distribution with n − 1 degrees of freedom.",
    vars = ps_vars_table(st, meta),
    hyp = c(sprintf("H0: μ = %s", format(mu0)), sprintf("H1: μ %s %s", ps_alt_word(alt), format(mu0)),
            sprintf("where μ is the true mean %s. Significance level α = %s.", lab, format(a))),
    model = list(eq = "yᵢ = μ + εᵢ,   εᵢ ~ N(0, σ²),   i = 1, …, n",
                 notes = c("μ is the population mean and εᵢ the random error of observation i.",
                           if (z) sprintf("Test statistic: Z0 = (ȳ − μ0) / (σ / √n), which is N(0, 1) when H0 is true.") else "Test statistic: t0 = (ȳ − μ0) / (s / √n), which has a t distribution with n − 1 df when H0 is true.")),
    calc = list(
      list(t = "Sample summaries", x = c(sprintf("n = %d,   ȳ = Σyᵢ / n = %s / %d = %s", n, ps_fmt(sum(y), 7), n, ps_fmt(ybar)),
                                        sprintf("s = √[Σ(yᵢ − ȳ)² / (n − 1)] = √(%s / %d) = %s", ps_fmt(sum((y - ybar)^2)), n - 1, ps_fmt(sd)))),
      list(t = "Standard error", x = if (z) sprintf("σ / √n = %s / √%d = %s", format(s[["sigma"]]), n, ps_fmt(se)) else sprintf("s / √n = %s / √%d = %s", ps_fmt(sd), n, ps_fmt(se))),
      list(t = "Test statistic", x = sprintf("%s = (ȳ − μ0) / SE = (%s − %s) / %s = %s", sym, ps_fmt(ybar), format(mu0), ps_fmt(se), ps_fmt(stat, 4))),
      list(t = "Critical value and P-value", x = c(sprintf("Reject H0 if %s%s", ps_reject_rule(alt, sym, crit), if (z) "" else sprintf("   (t distribution with %d df)", dfree)),
                                                  sprintf("P-value = %s = %s", switch(alt, two.sided = sprintf("2 × P(%s > |%s|)", if (z) "Z" else "T", ps_fmt(stat, 4)),
                                                                                       greater = sprintf("P(%s > %s)", if (z) "Z" else "T", ps_fmt(stat, 4)), less = sprintf("P(%s < %s)", if (z) "Z" else "T", ps_fmt(stat, 4))), ps_p(p)))),
      list(t = sprintf("%g%% confidence interval on μ", 100 * s$conf), x = sprintf("ȳ ± %s × SE = %s ± %s × %s = [%s, %s]%s", if (z) sprintf("z(%g)", h) else sprintf("t(%g, %d)", h, dfree),
                                                                                  ps_fmt(ybar), ps_fmt(qc, 4), ps_fmt(se), ps_fmt(ci[1]), ps_fmt(ci[2]), u))),
    table = list(title = "Test results", df = data.frame(Test = if (z) "One-sample Z-test" else "One-sample t-test", n = n, Mean = ps_fmt(ybar),
                                                         `SD` = ps_fmt(sd), SE = ps_fmt(se), Statistic = sprintf("%s = %s", sym, ps_fmt(stat, 4)),
                                                         df = if (z) "—" else as.character(dfree), `P-value` = ps_p(p), CI = sprintf("[%s, %s]", ps_fmt(ci[1]), ps_fmt(ci[2])), check.names = FALSE) |>
                   stats::setNames(c("Test", "n", "Mean", "SD", "SE", "Statistic", "df", "P-value", sprintf("%g%% CI", 100 * s$conf)))),
    stats = data.frame(Effect = "Mean vs μ0", Statistic = sprintf("%s = %s", sym, ps_fmt(stat, 4)), df = if (z) "—" else as.character(dfree),
                       `P-value` = ps_p(p), Critical = sprintf("%s", ps_fmt(crit, 4)), Decision = if (sig) "Reject H0" else "Fail to reject H0", check.names = FALSE),
    decision = ps_decision_txt(p, a, sprintf("%s = %s", sym, ps_fmt(stat, 4)), sprintf("%s = %s, %s", sym, ps_fmt(stat, 4), ps_reject_rule(alt, sym, crit))),
    comp = list(text = c(sprintf("There are no treatments to compare. The %g%% confidence interval [%s, %s]%s gives the plausible values of μ.", 100 * s$conf, ps_fmt(ci[1]), ps_fmt(ci[2]), u),
                         if (alt == "two.sided") sprintf("μ0 = %s is %s the interval, which agrees with the test decision.", format(mu0), if (mu0 >= ci[1] && mu0 <= ci[2]) "inside" else "outside") else
                           "The interval is two-sided; the one-sided test can still disagree near the boundary.")),
    assump = chk,
    conclusion = sprintf("At α = %s, there is %s evidence that the mean %s is %s %s (%s = %s, %s).", format(a), if (sig) "sufficient" else "insufficient", lab,
                         switch(alt, two.sided = "different from", greater = "greater than", less = "less than"), format(mu0), sym, ps_fmt(stat, 4), ps_pe(p)),
    plain = sprintf("The sample average is %s%s. %s The true mean is likely to be between %s and %s%s.", ps_fmt(ybar), u,
                    if (sig) sprintf("This is far enough from %s that chance alone is an unlikely explanation, so we conclude the mean %s is %s %s.", format(mu0), lab,
                                     switch(alt, two.sided = "differs from", greater = "is above", less = "is below"), format(mu0))
                    else sprintf("The difference from %s is small compared with the sampling variation, so the data do not show that the mean %s %s %s.", format(mu0), lab,
                                 switch(alt, two.sided = "differs from", greater = "is above", less = "is below"), format(mu0)),
                    ps_fmt(ci[1]), ps_fmt(ci[2]), u),
    plots = list(list(t = "Data with the target value", p = ps_plot_groups(st$d, ps_ylab(meta, st), NULL) + geom_hline(yintercept = mu0, colour = RED, linetype = "dashed")),
                 list(t = "Normal QQ plot of the data", p = ps_plot_qq(y, "Data"))))
}

# One sample: chi-square test and interval for a variance.
ps_var1 <- function(kind, st, s, meta) {
  y <- st$d$y; n <- length(y); a <- s$alpha; alt <- s$alt; v0 <- s$var0; lab <- ps_lab(meta, "response")
  s2 <- stats::var(y); chi <- (n - 1) * s2 / v0; dfree <- n - 1
  p <- p_from(chi, alt, function(x) stats::pchisq(x, dfree))
  lo <- stats::qchisq(a / 2, dfree); hi <- stats::qchisq(1 - a / 2, dfree)
  rule <- switch(alt, two.sided = sprintf("χ0² > %s or χ0² < %s", ps_fmt(hi, 4), ps_fmt(lo, 4)),
                 greater = sprintf("χ0² > %s", ps_fmt(stats::qchisq(1 - a, dfree), 4)), less = sprintf("χ0² < %s", ps_fmt(stats::qchisq(a, dfree), 4)))
  h <- (1 - s$conf) / 2
  ci <- c((n - 1) * s2 / stats::qchisq(1 - h, dfree), (n - 1) * s2 / stats::qchisq(h, dfree))
  sig <- ps_decide(p, a); chk <- ps_assump(y, a, what = "data")
  list(
    design = sprintf("A single sample of %s. The question is about the variability of %s, so the parameter of interest is the variance σ², not the mean.", plural(n, "measurement"), lab),
    why = "For normal data, (n − 1)S²/σ² follows a χ² distribution with n − 1 degrees of freedom; this gives both the test and the confidence interval.",
    vars = ps_vars_table(st, meta),
    hyp = c(sprintf("H0: σ² = %s", format(v0)), sprintf("H1: σ² %s %s", ps_alt_word(alt), format(v0)), sprintf("Significance level α = %s.", format(a))),
    model = list(eq = "yᵢ = μ + εᵢ,   εᵢ ~ N(0, σ²)", notes = c("Test statistic: χ0² = (n − 1)S² / σ0², which is χ² with n − 1 df when H0 is true.",
                                                             "This test is very sensitive to non-normality, unlike the t-test.")),
    calc = list(
      list(t = "Sample variance", x = c(sprintf("n = %d,   ȳ = %s", n, ps_fmt(mean(y))), sprintf("S² = Σ(yᵢ − ȳ)² / (n − 1) = %s / %d = %s", ps_fmt(sum((y - mean(y))^2)), dfree, ps_fmt(s2)))),
      list(t = "Test statistic", x = sprintf("χ0² = (n − 1)S² / σ0² = %d × %s / %s = %s", dfree, ps_fmt(s2), format(v0), ps_fmt(chi, 4))),
      list(t = "Rejection region and P-value", x = c(sprintf("Reject H0 if %s   (χ² with %d df)", rule, dfree), sprintf("P-value = %s", ps_p(p)))),
      list(t = sprintf("%g%% confidence interval on σ²", 100 * s$conf), x = c(sprintf("[(n − 1)S² / χ²(%g, %d), (n − 1)S² / χ²(%g, %d)]", h, dfree, 1 - h, dfree),
                                                                              sprintf("= [%d × %s / %s, %d × %s / %s] = [%s, %s]", dfree, ps_fmt(s2), ps_fmt(stats::qchisq(1 - h, dfree), 5), dfree, ps_fmt(s2), ps_fmt(stats::qchisq(h, dfree), 5), ps_fmt(ci[1]), ps_fmt(ci[2])),
                                                                              sprintf("For σ: [%s, %s]", ps_fmt(sqrt(ci[1])), ps_fmt(sqrt(ci[2])))))),
    table = list(title = "Test results", df = data.frame(Test = "χ² test on one variance", n = n, `S²` = ps_fmt(s2), Statistic = sprintf("χ0² = %s", ps_fmt(chi, 4)), df = dfree,
                                                         `P-value` = ps_p(p), CI = sprintf("[%s, %s]", ps_fmt(ci[1]), ps_fmt(ci[2])), check.names = FALSE) |>
                   stats::setNames(c("Test", "n", "S²", "Statistic", "df", "P-value", sprintf("%g%% CI for σ²", 100 * s$conf)))),
    stats = data.frame(Effect = "Variance vs σ0²", Statistic = sprintf("χ0² = %s", ps_fmt(chi, 4)), df = as.character(dfree), `P-value` = ps_p(p), Critical = rule,
                       Decision = if (sig) "Reject H0" else "Fail to reject H0", check.names = FALSE),
    decision = ps_decision_txt(p, a, sprintf("χ0² = %s", ps_fmt(chi, 4)), sprintf("χ0² = %s satisfies %s", ps_fmt(chi, 4), rule)),
    comp = list(text = sprintf("There are no treatments to compare. The %g%% interval for σ² is [%s, %s]; σ0² = %s is %s it.", 100 * s$conf, ps_fmt(ci[1]), ps_fmt(ci[2]), format(v0),
                               if (v0 >= ci[1] && v0 <= ci[2]) "inside" else "outside")),
    assump = chk,
    conclusion = sprintf("At α = %s, there is %s evidence that the variance of %s differs from %s (χ0² = %s with %d df, %s).", format(a), if (sig) "sufficient" else "insufficient", lab, format(v0), ps_fmt(chi, 4), dfree, ps_pe(p)),
    plain = sprintf("The sample variance is %s (standard deviation %s). %s The true variance is plausibly between %s and %s. Because this test relies heavily on normality, the QQ plot should be checked before trusting it.",
                    ps_fmt(s2), ps_fmt(sqrt(s2)), if (sig) sprintf("That is too far from %s to be chance, so the process variability is not %s.", format(v0), format(v0))
                    else sprintf("That is consistent with a true variance of %s.", format(v0)), ps_fmt(ci[1]), ps_fmt(ci[2])),
    plots = list(list(t = "The data", p = ps_plot_groups(st$d, ps_ylab(meta, st), NULL)), list(t = "Normal QQ plot of the data", p = ps_plot_qq(y, "Data"))))
}

# Two independent samples: F test of variances, then pooled/Welch t or z.
ps_two <- function(kind, st, s, meta) {
  d <- st$d; lv <- levels(d$treatment); a <- s$alpha; alt <- s$alt
  y1 <- d$y[d$treatment == lv[1]]; y2 <- d$y[d$treatment == lv[2]]; n1 <- length(y1); n2 <- length(y2)
  m1 <- mean(y1); m2 <- mean(y2); v1 <- stats::var(y1); v2 <- stats::var(y2)
  lab <- ps_lab(meta, "response"); lt <- ps_lab(meta, "treatment"); u <- ps_unit(meta)
  Fv <- v1 / v2; pF <- 2 * min(stats::pf(Fv, n1 - 1, n2 - 1), 1 - stats::pf(Fv, n1 - 1, n2 - 1))
  z <- !is.null(s[["sigma"]]) && !is.null(s[["sigma2"]])
  equal <- switch(s$var_mode, equal = TRUE, unequal = FALSE, pF >= a)
  if (z) {
    se <- sqrt(s[["sigma"]]^2 / n1 + s[["sigma2"]]^2 / n2); dfree <- Inf; sym <- "Z0"; tname <- "Two-sample Z-test (σ1, σ2 known)"
    q <- stats::qnorm; cdf <- stats::pnorm
  } else if (equal) {
    sp <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2)); se <- sp * sqrt(1 / n1 + 1 / n2); dfree <- n1 + n2 - 2; sym <- "t0"; tname <- "Pooled two-sample t-test"
  } else {
    se <- sqrt(v1 / n1 + v2 / n2); dfree <- (v1 / n1 + v2 / n2)^2 / ((v1 / n1)^2 / (n1 - 1) + (v2 / n2)^2 / (n2 - 1)); sym <- "t0"; tname <- "Welch two-sample t-test"
  }
  if (!z) { q <- function(p) stats::qt(p, dfree); cdf <- function(x) stats::pt(x, dfree) }
  stat <- (m1 - m2) / se; p <- p_from(stat, alt, cdf); crit <- ps_crit(alt, a, q)
  h <- (1 - s$conf) / 2; ci <- (m1 - m2) + c(-1, 1) * q(1 - h) * se
  sig <- ps_decide(p, a)
  fit <- stats::lm(y ~ treatment, data = d)
  chk <- ps_assump(stats::residuals(fit), a, d$treatment)
  sw1 <- sw_test(y1); sw2 <- sw_test(y2)
  chk$table <- rbind(chk$table[1, ], data.frame(Assumption = "Normality", Test = sprintf("Shapiro-Wilk, %s / %s", lv[1], lv[2]),
                                                 Statistic = sprintf("W = %.3f / %.3f", sw1$W, sw2$W), `p-value` = sprintf("%s / %s", ps_p(sw1$p), ps_p(sw2$p)),
                                                 Result = if (min(sw1$p, sw2$p, na.rm = TRUE) < a) "Fail" else "Pass", check.names = FALSE), chk$table[-1, ])
  chk$table <- rbind(chk$table, data.frame(Assumption = "Equal variance", Test = "F test (two-sided)", Statistic = sprintf("F0 = %.4f", Fv), `p-value` = ps_p(pF),
                                           Result = if (pF < a) "Fail" else "Pass", check.names = FALSE))
  mu_txt <- sprintf("μ1 − μ2, where μ1 is the mean %s for %s and μ2 for %s", lab, lv[1], lv[2])
  dir_word <- switch(alt, two.sided = "differs between", greater = sprintf("is higher for %s than for", lv[1]), less = sprintf("is lower for %s than for", lv[1]))
  list(
    design = sprintf("Two independent samples: %d units received %s and %d different units received %s, in random order. This is a completely randomized design with one factor (%s) at two levels.", n1, lv[1], n2, lv[2], lt),
    why = if (z) "Both population standard deviations are known, so the difference in sample means is standardised with them (two-sample Z-test)." else
      sprintf("σ1 and σ2 are unknown. The F test of equal variances gives %s, so %s.", ps_pe(pF),
              if (s$var_mode == "equal") "the pooled t-test is used, as the problem says to assume equal variances" else if (s$var_mode == "unequal") "the Welch t-test is used, as you chose"
              else if (equal) "the variances are pooled (pooled t-test)" else "the Welch t-test, which does not pool the variances, is used"),
    vars = ps_vars_table(st, meta),
    hyp = c(sprintf("Variances: H0: σ1² = σ2² vs H1: σ1² ≠ σ2²"), sprintf("Means: H0: μ1 = μ2 vs H1: μ1 %s μ2", ps_alt_word(alt)),
            sprintf("μ1 = mean %s for %s, μ2 = mean for %s. Significance level α = %s.", lab, lv[1], lv[2], format(a))),
    model = list(eq = "yᵢⱼ = μᵢ + εᵢⱼ,   i = 1, 2;  j = 1, …, nᵢ,   εᵢⱼ ~ N(0, σᵢ²)",
                 notes = c("μᵢ is the mean of group i; the errors are independent.", if (z) "Z0 = (ȳ1 − ȳ2) / √(σ1²/n1 + σ2²/n2)" else if (equal)
                   "Pooled: t0 = (ȳ1 − ȳ2) / (Sp √(1/n1 + 1/n2)),  Sp² = [(n1 − 1)S1² + (n2 − 1)S2²] / (n1 + n2 − 2),  df = n1 + n2 − 2" else
                     "Welch: t0 = (ȳ1 − ȳ2) / √(S1²/n1 + S2²/n2), with Welch-Satterthwaite degrees of freedom")),
    calc = c(list(
      list(t = "Sample summaries", x = c(sprintf("%s: n1 = %d,  ȳ1 = %s,  S1² = %s  (S1 = %s)", lv[1], n1, ps_fmt(m1), ps_fmt(v1), ps_fmt(sqrt(v1))),
                                        sprintf("%s: n2 = %d,  ȳ2 = %s,  S2² = %s  (S2 = %s)", lv[2], n2, ps_fmt(m2), ps_fmt(v2), ps_fmt(sqrt(v2))))),
      list(t = "Step 1: F test of equal variances", x = c(sprintf("F0 = S1² / S2² = %s / %s = %s,  df = (%d, %d)", ps_fmt(v1), ps_fmt(v2), ps_fmt(Fv, 4), n1 - 1, n2 - 1),
                                                          sprintf("Critical values F(%g) = %s and F(%g) = %s;  P-value = %s", 1 - a / 2, ps_fmt(stats::qf(a / 2, n1 - 1, n2 - 1), 4), a / 2, ps_fmt(stats::qf(1 - a / 2, n1 - 1, n2 - 1), 4), ps_p(pF)),
                                                          sprintf("%s", if (pF < a) "Reject H0: the variances differ." else "Fail to reject H0: the variances can be treated as equal.")))),
      list(list(t = "Step 2: standard error of ȳ1 − ȳ2", x = if (z) sprintf("√(σ1²/n1 + σ2²/n2) = √(%s²/%d + %s²/%d) = %s", format(s[["sigma"]]), n1, format(s[["sigma2"]]), n2, ps_fmt(se))
                else if (equal) c(sprintf("Sp² = [(%d)(%s) + (%d)(%s)] / %d = %s,  Sp = %s", n1 - 1, ps_fmt(v1), n2 - 1, ps_fmt(v2), n1 + n2 - 2, ps_fmt(sp^2), ps_fmt(sp)),
                                  sprintf("SE = Sp √(1/%d + 1/%d) = %s", n1, n2, ps_fmt(se)))
                else c(sprintf("SE = √(S1²/n1 + S2²/n2) = √(%s/%d + %s/%d) = %s", ps_fmt(v1), n1, ps_fmt(v2), n2, ps_fmt(se)), sprintf("Welch df = %s", ps_fmt(dfree, 4)))),
           list(t = "Step 3: test statistic", x = sprintf("%s = (ȳ1 − ȳ2) / SE = (%s − %s) / %s = %s", sym, ps_fmt(m1), ps_fmt(m2), ps_fmt(se), ps_fmt(stat, 4))),
           list(t = "Step 4: critical value and P-value", x = c(sprintf("Reject H0 if %s%s", ps_reject_rule(alt, sym, crit), if (z) "" else sprintf("   (t with %s df)", ps_fmt(dfree, 4))), sprintf("P-value = %s", ps_p(p)))),
           list(t = sprintf("%g%% confidence interval on μ1 − μ2", 100 * s$conf), x = sprintf("(ȳ1 − ȳ2) ± %s × SE = %s ± %s × %s = [%s, %s]%s", if (z) sprintf("z(%g)", h) else sprintf("t(%g, %s)", h, ps_fmt(dfree, 4)),
                                                                                         ps_fmt(m1 - m2), ps_fmt(q(1 - h), 4), ps_fmt(se), ps_fmt(ci[1]), ps_fmt(ci[2]), u)))),
    table = list(title = "Test results", df = data.frame(Test = c("F test of equal variances", tname),
                                                         Statistic = c(sprintf("F0 = %s", ps_fmt(Fv, 4)), sprintf("%s = %s", sym, ps_fmt(stat, 4))),
                                                         df = c(sprintf("%d, %d", n1 - 1, n2 - 1), if (z) "—" else ps_fmt(dfree, 4)),
                                                         `P-value` = c(ps_p(pF), ps_p(p)), Estimate = c(sprintf("S1²/S2² = %s", ps_fmt(Fv, 4)), sprintf("ȳ1 − ȳ2 = %s", ps_fmt(m1 - m2))),
                                                         CI = c(sprintf("[%s, %s]", ps_fmt(Fv / stats::qf(1 - h, n1 - 1, n2 - 1), 4), ps_fmt(Fv / stats::qf(h, n1 - 1, n2 - 1), 4)), sprintf("[%s, %s]", ps_fmt(ci[1]), ps_fmt(ci[2]))),
                                                         check.names = FALSE) |> stats::setNames(c("Test", "Statistic", "df", "P-value", "Estimate", sprintf("%g%% CI", 100 * s$conf)))),
    stats = data.frame(Effect = c("Equal variances", "Equal means"), Statistic = c(sprintf("F0 = %s", ps_fmt(Fv, 4)), sprintf("%s = %s", sym, ps_fmt(stat, 4))),
                       df = c(sprintf("%d, %d", n1 - 1, n2 - 1), if (z) "—" else ps_fmt(dfree, 4)), `P-value` = c(ps_p(pF), ps_p(p)),
                       Critical = c(sprintf("%s, %s", ps_fmt(stats::qf(a / 2, n1 - 1, n2 - 1), 4), ps_fmt(stats::qf(1 - a / 2, n1 - 1, n2 - 1), 4)), ps_fmt(crit, 4)),
                       Decision = c(if (pF < a) "Reject H0" else "Fail to reject H0", if (sig) "Reject H0" else "Fail to reject H0"), check.names = FALSE),
    decision = c(sprintf("Variances: %s", if (pF < a) sprintf("%s < %s, so the variances differ.", ps_pe(pF), format(a)) else sprintf("%s ≥ %s, so there is no evidence the variances differ.", ps_pe(pF), format(a))),
                 sprintf("Means: %s", ps_decision_txt(p, a, sprintf("%s = %s", sym, ps_fmt(stat, 4)), sprintf("%s = %s, %s", sym, ps_fmt(stat, 4), ps_reject_rule(alt, sym, crit))))),
    comp = list(text = c(sprintf("With two groups the test itself is the comparison. The estimated difference is ȳ1 − ȳ2 = %s%s, with %g%% confidence interval [%s, %s]%s.", ps_fmt(m1 - m2), u, 100 * s$conf, ps_fmt(ci[1]), ps_fmt(ci[2]), u),
                         if (ci[1] > 0 || ci[2] < 0) "The interval does not contain 0, so the two means differ at this confidence level." else "The interval contains 0, so a zero difference is plausible.")),
    assump = chk,
    conclusion = sprintf("At α = %s, there is %s evidence that the mean %s %s %s (%s = %s, %s).", format(a), if (sig) "sufficient" else "insufficient", lab, dir_word,
                         if (alt == "two.sided") paste(lv[1], "and", lv[2]) else lv[2], sym, ps_fmt(stat, 4), ps_pe(p)),
    plain = sprintf("On average %s gave %s%s and %s gave %s%s, a difference of %s%s. %s",
                    lv[1], ps_fmt(m1), u, lv[2], ps_fmt(m2), u, ps_fmt(m1 - m2), u,
                    if (sig) sprintf("This difference is larger than chance variation would produce, so the %s matters: the mean %s %s %s.", lt, lab, dir_word, if (alt == "two.sided") "the two groups" else lv[2])
                    else sprintf("This difference is within what random variation could produce, so the data do not show that the %s affects the mean %s.", lt, lab)),
    plots = list(list(t = sprintf("%s by %s", st$roles$response, st$roles$treatment), p = ps_plot_groups(d, ps_ylab(meta, st), st$roles$treatment)),
                 list(t = "Normal QQ plot of the residuals", p = ps_plot_qq(stats::residuals(fit))),
                 list(t = "Residuals versus fitted values", p = ps_plot_rvf(stats::fitted(fit), stats::residuals(fit)))))
}

# Paired comparison, with the equivalent RCBD ANOVA.
ps_paired <- function(kind, st, s, meta) {
  d <- st$d; lv <- levels(d$treatment); a <- s$alpha; alt <- s$alt
  lab <- ps_lab(meta, "response"); lt <- ps_lab(meta, "treatment"); lb <- ps_lab(meta, "block"); u <- ps_unit(meta)
  w <- stats::reshape(d[, c("y", "treatment", "block")], idvar = "block", timevar = "treatment", direction = "wide")
  y1 <- w[[paste0("y.", lv[1])]]; y2 <- w[[paste0("y.", lv[2])]]; dd <- y1 - y2; n <- length(dd)
  dbar <- mean(dd); sd <- stats::sd(dd); se <- sd / sqrt(n); dfree <- n - 1
  stat <- dbar / se; p <- p_from(stat, alt, function(x) stats::pt(x, dfree)); crit <- ps_crit(alt, a, function(q) stats::qt(q, dfree))
  h <- (1 - s$conf) / 2; ci <- dbar + c(-1, 1) * stats::qt(1 - h, dfree) * se
  sig <- ps_decide(p, a)
  fit <- stats::aov(y ~ treatment + block, data = d); at <- stats::anova(fit)
  Ft <- at["treatment", "F value"]; pt <- at["treatment", "Pr(>F)"]; Fb <- at["block", "F value"]; pb <- at["block", "Pr(>F)"]
  chk <- ps_assump(dd, a, what = "differences")
  diffs <- data.frame(w$block, ps_fmt(y1), ps_fmt(y2), ps_fmt(dd)); names(diffs) <- c(st$roles$block, lv[1], lv[2], sprintf("d = %s − %s", lv[1], lv[2]))
  list(
    design = sprintf("A paired comparison. Each %s was measured under both %s (%s and %s), so the %d %s are blocks and %s is the treatment. This is a randomized complete block design with two treatments.",
                     lb, pl_word(lt), lv[1], lv[2], n, pl_word(lb), lt),
    why = sprintf("Measurements on the same %s are related. Taking the difference within each %s removes the %s-to-%s variation, which would otherwise inflate the error and hide a real treatment difference.", lb, lb, lb, lb),
    vars = ps_vars_table(st, meta),
    hyp = c(sprintf("H0: μd = 0   (no difference between %s and %s)", lv[1], lv[2]), sprintf("H1: μd %s 0", ps_alt_word(alt)),
            sprintf("μd = mean of %s − %s over the population of %s. Significance level α = %s.", lv[1], lv[2], pl_word(lb), format(a)),
            "In RCBD form: H0: τ1 = τ2 = 0 (treatments), and H0: β1 = … = βb = 0 (blocks)."),
    model = list(eq = "yᵢⱼ = μ + τᵢ + βⱼ + εᵢⱼ,   i = 1, 2 (treatments);  j = 1, …, b (blocks)",
                 notes = c(sprintf("τᵢ is the effect of %s i, βⱼ the effect of %s j, εᵢⱼ ~ N(0, σ²).", lt, lb),
                           "Differencing within a block removes βⱼ: dⱼ = (τ1 − τ2) + (ε1ⱼ − ε2ⱼ), so the paired t-test is t0 = d̄ / (Sd / √n) with n − 1 df.")),
    calc = list(
      list(t = "Differences within each pair", x = c(sprintf("dⱼ = y1ⱼ − y2ⱼ for the %d %s (table below)", n, pl_word(lb))), df = diffs),
      list(t = "Mean and standard deviation of the differences", x = c(sprintf("d̄ = Σdⱼ / n = %s / %d = %s", ps_fmt(sum(dd)), n, ps_fmt(dbar)),
                                                                       sprintf("Sd = √[Σ(dⱼ − d̄)² / (n − 1)] = %s,  SE = Sd / √n = %s", ps_fmt(sd), ps_fmt(se)))),
      list(t = "Test statistic", x = sprintf("t0 = d̄ / (Sd / √n) = %s / %s = %s,  df = %d", ps_fmt(dbar), ps_fmt(se), ps_fmt(stat, 4), dfree)),
      list(t = "Critical value and P-value", x = c(sprintf("Reject H0 if %s", ps_reject_rule(alt, "t0", crit)), sprintf("P-value = %s", ps_p(p)))),
      list(t = sprintf("%g%% confidence interval on μd", 100 * s$conf), x = sprintf("d̄ ± t(%g, %d) × SE = %s ± %s × %s = [%s, %s]%s", h, dfree, ps_fmt(dbar), ps_fmt(stats::qt(1 - h, dfree), 4), ps_fmt(se), ps_fmt(ci[1]), ps_fmt(ci[2]), u)),
      list(t = "The same test as an RCBD ANOVA", x = c(sprintf("Fitting yᵢⱼ = μ + τᵢ + βⱼ + εᵢⱼ gives F(treatment) = %s with (1, %d) df, %s.", ps_fmt(Ft, 5), dfree, ps_pe(pt)),
                                                     sprintf("F = t0² = %s² = %s: the two analyses agree%s.", ps_fmt(stat, 5), ps_fmt(stat^2, 5), if (alt != "two.sided") " (the F test is always two-sided)" else "")))),
    table = list(title = "RCBD ANOVA table (equivalent to the paired t-test)",
                 df = ps_anova_df(data.frame(source = c(st$roles$treatment, paste(st$roles$block, "(blocks)")), df = at$Df[1:2], ss = at$`Sum Sq`[1:2], F = at$`F value`[1:2], p = at$`Pr(>F)`[1:2]),
                                  at$Df[3], at$`Sum Sq`[3], nrow(d) - 1, sum((d$y - mean(d$y))^2))),
    stats = data.frame(Effect = c("Paired t-test", sprintf("%s (RCBD F)", st$roles$treatment), sprintf("%s (blocks)", st$roles$block)),
                       Statistic = c(sprintf("t0 = %s", ps_fmt(stat, 4)), sprintf("F0 = %s", ps_fmt(Ft, 4)), sprintf("F0 = %s", ps_fmt(Fb, 4))),
                       df = c(as.character(dfree), sprintf("1, %d", dfree), sprintf("%d, %d", n - 1, dfree)), `P-value` = c(ps_p(p), ps_p(pt), ps_p(pb)),
                       Critical = c(ps_fmt(crit, 4), ps_fmt(stats::qf(1 - a, 1, dfree), 4), ps_fmt(stats::qf(1 - a, n - 1, dfree), 4)),
                       Decision = c(if (sig) "Reject H0" else "Fail to reject H0", if (pt < a) "Reject H0" else "Fail to reject H0", if (pb < a) "Reject H0" else "Fail to reject H0"), check.names = FALSE),
    decision = c(ps_decision_txt(p, a, sprintf("t0 = %s", ps_fmt(stat, 4)), sprintf("t0 = %s, %s", ps_fmt(stat, 4), ps_reject_rule(alt, "t0", crit))),
                 sprintf("Blocks: F0 = %s, %s. %s", ps_fmt(Fb, 4), ps_pe(pb), if (pb < a) sprintf("The %s differ, so pairing was worthwhile: it removed real %s-to-%s variation.", pl_word(lb), lb, lb)
                         else sprintf("The %s do not differ much, so pairing removed little variation here.", pl_word(lb)))),
    comp = list(text = c(sprintf("Two treatments, so the comparison is the mean difference d̄ = %s%s with %g%% confidence interval [%s, %s]%s.", ps_fmt(dbar), u, 100 * s$conf, ps_fmt(ci[1]), ps_fmt(ci[2]), u),
                         if (ci[1] > 0 || ci[2] < 0) "The interval excludes 0: the two treatments differ." else "The interval contains 0: no difference between the treatments is detected.")),
    assump = chk,
    conclusion = sprintf("At α = %s, there is %s evidence of a difference in mean %s between %s and %s (t0 = %s with %d df, %s; equivalently F0 = %s in the RCBD).", format(a),
                         if (sig) "sufficient" else "insufficient", lab, lv[1], lv[2], ps_fmt(stat, 4), dfree, ps_pe(p), ps_fmt(Ft, 4)),
    plain = sprintf("Within the same %s, %s gave on average %s%s %s than %s. %s", lb, lv[1], ps_fmt(abs(dbar)), u, if (dbar >= 0) "more" else "less", lv[2],
                    if (sig) sprintf("This difference is consistent across %s and too large to be chance, so the %s does affect the %s.", pl_word(lb), lt, lab)
                    else sprintf("This difference is small compared with how much the differences vary from %s to %s, so the data do not show that the %s affects the %s.", lb, lb, lt, lab)),
    plots = list(list(t = sprintf("One line per %s", st$roles$block), p = ps_plot_profile(d, ps_ylab(meta, st), st$roles$treatment, st$roles$block)),
                 list(t = "Normal QQ plot of the differences", p = ps_plot_qq(dd, "Differences"))))
}

# CRD, random-effects, RCBD and Latin square ANOVA.
ps_anova <- function(kind, st, s, meta) {
  d <- st$d; a <- s$alpha; lab <- ps_lab(meta, "response"); lt <- ps_lab(meta, "treatment"); u <- ps_unit(meta)
  terms <- switch(kind, crd = "treatment", random = "treatment", rcbd = c("treatment", "block"), lsd = c("treatment", "row", "column"))
  fit <- stats::aov(stats::as.formula(paste("y ~", paste(terms, collapse = " + "))), data = d)
  at <- stats::anova(fit)
  N <- nrow(d); tl <- nlevels(d$treatment); G <- sum(d$y); CF <- G^2 / N; SST <- sum(d$y^2) - CF
  dfe <- at["Residuals", "Df"]; sse <- at["Residuals", "Sum Sq"]; mse <- sse / dfe
  src <- c(treatment = st$roles$treatment, block = paste(st$roles$block %||% "", "(blocks)"), row = paste(st$roles$row %||% "", "(rows)"), column = paste(st$roles$column %||% "", "(columns)"))
  rows <- data.frame(term = terms, source = unname(src[terms]), df = at[terms, "Df"], ss = at[terms, "Sum Sq"], F = at[terms, "F value"], p = at[terms, "Pr(>F)"], stringsAsFactors = FALSE)
  rows$crit <- stats::qf(1 - a, rows$df, dfe)
  Ft <- rows$F[1]; pt <- rows$p[1]; sig <- ps_decide(pt, a)
  ss_line <- function(term, nm) {
    tot <- tapply(d$y, d[[term]], sum); nn <- tapply(d$y, d[[term]], length)
    shown <- if (length(tot) <= 4) paste(sprintf("%s²/%d", ps_fmt(tot, 7), nn), collapse = " + ") else paste(c(sprintf("%s²/%d", ps_fmt(tot[1:3], 7), nn[1:3]), "…"), collapse = " + ")
    sprintf("SS_%s = Σ yᵢ.²/nᵢ − CF = (%s) − %s = %s", nm, shown, ps_fmt(CF, 8), ps_fmt(sum(tot^2 / nn) - CF, 7))
  }
  balanced <- kind %in% c("crd", "random") || (kind == "rcbd" && isTRUE(st$cell_equal)) || kind == "lsd"
  calc <- list(
    list(t = "Totals and correction factor", x = c(sprintf("N = %d observations, grand total y.. = %s, grand mean ȳ.. = %s", N, ps_fmt(G, 8), ps_fmt(G / N)),
                                                   sprintf("CF = y..² / N = %s² / %d = %s", ps_fmt(G, 8), N, ps_fmt(CF, 8)))),
    list(t = "Total sum of squares", x = sprintf("SS_T = Σ y² − CF = %s − %s = %s,  df = N − 1 = %d", ps_fmt(sum(d$y^2), 9), ps_fmt(CF, 8), ps_fmt(SST, 7), N - 1)))
  if (balanced) {
    lines <- c(ss_line("treatment", "Treatments"))
    if (kind == "rcbd") lines <- c(lines, ss_line("block", "Blocks"))
    if (kind == "lsd") lines <- c(lines, ss_line("row", "Rows"), ss_line("column", "Columns"))
    calc[[3]] <- list(t = "Sums of squares for each factor", x = c(lines, sprintf("SS_E = SS_T − %s = %s", paste(c("SS_Treatments", if (kind == "rcbd") "SS_Blocks", if (kind == "lsd") c("SS_Rows", "SS_Columns")), collapse = " − "), ps_fmt(sse, 7))))
  } else calc[[3]] <- list(t = "Sums of squares", x = "The blocks hold unequal numbers of observations, so the sums of squares come from least squares (sequential, treatment first).")
  calc[[4]] <- list(t = "Degrees of freedom", x = c(paste(sprintf("%s: %d", rows$source, rows$df), collapse = ";  "),
                                                     sprintf("Error: %d%s", dfe, switch(kind, crd = , random = sprintf(" = N − a = %d − %d", N, tl), rcbd = sprintf(" = (a − 1)(b − 1) = %d × %d", tl - 1, nlevels(d$block) - 1),
                                                                                         lsd = sprintf(" = (p − 1)(p − 2) = %d × %d", tl - 1, tl - 2), ""))))
  calc[[5]] <- list(t = "Mean squares and F statistics", x = c(sprintf("MS_E = SS_E / df_E = %s / %d = %s", ps_fmt(sse, 7), dfe, ps_fmt(mse, 6)),
                                                             sprintf("MS_%s = %s / %d = %s,  F0 = MS / MS_E = %s", rows$source, ps_fmt(rows$ss, 7), rows$df, ps_fmt(rows$ss / rows$df, 6), ps_fmt(rows$F, 5))))
  calc[[6]] <- list(t = "Critical values and P-values", x = sprintf("%s: F(%g; %d, %d) = %s, %s", rows$source, a, rows$df, dfe, ps_fmt(rows$crit, 4), ps_pe(rows$p)))
  hyp <- c(sprintf("Treatments: H0: τ1 = τ2 = … = τ%d = 0   (all %s means are equal)", tl, lt), sprintf("H1: τᵢ ≠ 0 for at least one i   (at least one mean differs)"))
  if (kind == "random") hyp <- c(sprintf("H0: στ² = 0   (no variability between %s)", pl_word(lt)), sprintf("H1: στ² > 0   (the %s vary)", pl_word(lt)))
  if (kind == "rcbd") hyp <- c(hyp, sprintf("Blocks (secondary): H0: β1 = … = β%d = 0", nlevels(d$block)))
  if (kind == "lsd") hyp <- c(hyp, sprintf("Rows (secondary): H0: α1 = … = α%d = 0;  Columns: H0: β1 = … = β%d = 0", tl, tl))
  hyp <- c(hyp, sprintf("Significance level α = %s.", format(a)))
  model <- switch(kind,
    crd = list(eq = "yᵢⱼ = μ + τᵢ + εᵢⱼ,   i = 1, …, a;  j = 1, …, nᵢ", notes = c(sprintf("μ: overall mean; τᵢ: effect of %s i (Στᵢ = 0); εᵢⱼ ~ N(0, σ²) independent.", lt))),
    random = list(eq = "yᵢⱼ = μ + τᵢ + εᵢⱼ,   τᵢ ~ N(0, στ²),  εᵢⱼ ~ N(0, σ²)", notes = c(sprintf("τᵢ is a random effect: the %s are a random sample from a large population.", pl_word(lt)), "Var(yᵢⱼ) = στ² + σ²; the two parts are the variance components.")),
    rcbd = list(eq = "yᵢⱼ = μ + τᵢ + βⱼ + εᵢⱼ,   i = 1, …, a;  j = 1, …, b", notes = c(sprintf("τᵢ: effect of %s i; βⱼ: effect of block j (%s); εᵢⱼ ~ N(0, σ²).", lt, ps_lab(meta, "block")), "Treatments and blocks are assumed not to interact (additive model).")),
    lsd = list(eq = "yᵢⱼₖ = μ + αᵢ + τⱼ + βₖ + εᵢⱼₖ,   i, j, k = 1, …, p", notes = c(sprintf("αᵢ: row effect (%s); τⱼ: treatment effect; βₖ: column effect (%s); εᵢⱼₖ ~ N(0, σ²).", st$roles$row, st$roles$column), "No interactions between rows, columns and treatments.")))
  design <- switch(kind,
    crd = sprintf("A completely randomized design (CRD) with one factor, %s, at %d levels%s. Units were assigned to the levels at random and there is no blocking.", lt, tl,
                  if (st$balanced) sprintf(" and %d replicates each", st$reps[[1]]) else sprintf(" with unequal replication (%s)", paste(st$reps, collapse = ", "))),
    random = sprintf("A single-factor experiment in which the %d levels of %s were selected at random from a large population. %s is therefore a random factor, so the question is about the variance between levels, not about these particular levels.", tl, lt, ucfirst(lt)),
    rcbd = sprintf("A randomized complete block design (RCBD): %d treatments (%s) each appear in all %d blocks (%s).", tl, lt, nlevels(d$block), ps_lab(meta, "block")),
    lsd = sprintf("A %d × %d Latin square: %d treatments arranged so that each appears once in every row (%s) and once in every column (%s).", tl, tl, tl, st$roles$row, st$roles$column))
  why <- switch(kind,
    crd = "With one factor and no restriction on randomization, the one-way ANOVA splits the total variation into a between-treatment part and a within-treatment (error) part.",
    random = "Because the levels are a random sample, the conclusions are about the whole population of levels, and the ANOVA mean squares estimate the variance components.",
    rcbd = "Blocking removes the variation between blocks from the error, so the treatment F test is more sensitive than in a CRD.",
    lsd = "The Latin square removes two sources of nuisance variation (rows and columns) with only p² runs.")
  comp <- if (kind == "random") list(text = "The levels were chosen at random, so comparing these particular levels is not the goal; the variance components below describe the population of levels.")
          else ps_compare(fit, d, mse, dfe, a, s$conf, s$posthoc %||% "tukey", meta, sig)
  extra <- list(list(t = sprintf("Treatment means with %g%% confidence intervals", 100 * s$conf), df = ps_means_df(d, "treatment", s$conf, mse, dfe),
                     x = sprintf("Each interval is ȳᵢ. ± t(%g, %d) √(MS_E / nᵢ).", (1 - s$conf) / 2, dfe)))
  if (kind %in% c("crd", "random")) {
    eff <- tapply(d$y, d$treatment, mean) - mean(d$y)
    extra[[length(extra) + 1]] <- list(t = "Estimated model parameters", x = c(sprintf("μ̂ = ȳ.. = %s", ps_fmt(mean(d$y))),
                                                                             paste(sprintf("τ̂%s = ȳ%s. − ȳ.. = %s", seq_along(eff), seq_along(eff), ps_fmt(eff)), collapse = ";  "), sprintf("σ̂² = MS_E = %s", ps_fmt(mse))))
  }
  var_comp <- NULL
  if (kind == "random") {
    ni <- as.numeric(table(d$treatment)); n0 <- (N - sum(ni^2) / N) / (tl - 1)
    mstr <- rows$ss[1] / rows$df[1]; s2t <- (mstr - mse) / n0; s2t_use <- max(s2t, 0); icc <- s2t_use / (s2t_use + mse)
    lines <- c(sprintf("n%s = %s", if (st$balanced) "" else "₀", if (st$balanced) format(ni[1]) else sprintf("(N − Σnᵢ²/N)/(a − 1) = %s", ps_fmt(n0))),
               sprintf("σ̂² = MS_E = %s", ps_fmt(mse)), sprintf("σ̂τ² = (MS_Treatments − MS_E) / n = (%s − %s) / %s = %s", ps_fmt(mstr), ps_fmt(mse), ps_fmt(n0), ps_fmt(s2t)),
               if (s2t < 0) "The estimate is negative, which happens when MS_Treatments < MS_E; it is taken as 0 (no detectable variation between levels)." else NULL,
               sprintf("Total variance σ̂y² = σ̂τ² + σ̂² = %s;  share due to %s: σ̂τ² / (σ̂τ² + σ̂²) = %s (%.1f%%)", ps_fmt(s2t_use + mse), lt, ps_fmt(icc, 4), 100 * icc))
    if (st$balanced) {
      n <- ni[1]; L <- (Ft / stats::qf(1 - a / 2, tl - 1, N - tl) - 1) / n; U <- (Ft / stats::qf(a / 2, tl - 1, N - tl) - 1) / n
      lines <- c(lines, sprintf("%g%% CI for στ²/(στ² + σ²): L = [F0/F(α/2) − 1]/n = %s, U = [F0/F(1 − α/2) − 1]/n = %s, interval [L/(1+L), U/(1+U)] = [%s, %s]",
                                100 * (1 - a), ps_fmt(L, 4), ps_fmt(U, 4), ps_fmt(max(L, 0) / (1 + max(L, 0)), 4), ps_fmt(U / (1 + U), 4)))
    }
    ci_s2 <- c(dfe * mse / stats::qchisq(1 - a / 2, dfe), dfe * mse / stats::qchisq(a / 2, dfe))
    lines <- c(lines, sprintf("%g%% CI for σ²: [df_E × MS_E / χ²(α/2, df_E), df_E × MS_E / χ²(1 − α/2, df_E)] = [%s, %s]", 100 * (1 - a), ps_fmt(ci_s2[1]), ps_fmt(ci_s2[2])))
    extra <- c(list(list(t = "Variance components", x = lines)), extra)
    var_comp <- list(s2t = s2t_use, s2 = mse, icc = icc)
  }
  if (kind %in% c("rcbd", "lsd")) {
    re <- if (kind == "rcbd") { b <- nlevels(d$block); msb <- rows$ss[2] / rows$df[2]; ((b - 1) * msb + b * (tl - 1) * mse) / ((b * tl - 1) * mse) }
          else { msr <- rows$ss[2] / rows$df[2]; msc <- rows$ss[3] / rows$df[3]; (msr + msc + (tl - 1) * mse) / ((tl + 1) * mse) }
    extra[[length(extra) + 1]] <- list(t = "Relative efficiency of blocking", x = sprintf("RE = %s: a CRD would need about %s times as many replicates to estimate the treatment effects as precisely.", ps_fmt(re, 4), ps_fmt(re, 3)))
  }
  chk <- ps_assump(stats::residuals(fit), a, d$treatment)
  sec <- rows[-1, , drop = FALSE]
  dec <- ps_decision_txt(pt, a, sprintf("F0 = %s", ps_fmt(Ft, 4)), sprintf("F0 = %s > F(%g; %d, %d) = %s", ps_fmt(Ft, 4), a, rows$df[1], dfe, ps_fmt(rows$crit[1], 4)))
  if (nrow(sec)) dec <- c(dec, sprintf("%s: F0 = %s, %s, %s.", sec$source, ps_fmt(sec$F, 4), vapply(sec$p, ps_pe, ""),
                                       ifelse(sec$p < a, "a significant nuisance effect, so blocking on it was worthwhile", "not significant; this blocking factor removed little variation")))
  m <- tapply(d$y, d$treatment, mean); hi <- names(which.max(m)); lo <- names(which.min(m))
  scale_txt <- if (st$transform != "none") sprintf(" (on the %s scale)", if (st$transform == "log") "log" else "square-root") else ""
  plain <- if (kind == "random") {
    sprintf("%s The variation between %s accounts for about %.0f%% of the total variance of %s; the rest is measurement-to-measurement variation within a %s.",
            if (sig) sprintf("The %s really do differ in %s.", pl_word(lt), lab) else sprintf("The data do not show real differences between %s.", pl_word(lt)), pl_word(lt), 100 * var_comp$icc, lab, lt)
  } else {
    sprintf("%s The highest mean %s was for %s (%s%s) and the lowest for %s (%s%s)%s. %s",
            if (sig) sprintf("The %s affects the %s: the differences between the level means are too large to be explained by chance.", lt, lab)
            else sprintf("The data do not show that the %s affects the %s; the differences between the level means are within ordinary random variation.", lt, lab),
            lab, hi, ps_fmt(m[hi]), if (st$transform == "none") u else "", lo, ps_fmt(m[lo]), if (st$transform == "none") u else "", scale_txt,
            if (sig && !is.null(comp$groups)) sprintf("The grouping letters show which levels can be told apart: %s.", paste(sprintf("%s (%s)", comp$groups$Level, comp$groups$Group), collapse = ", ")) else "")
  }
  list(design = design, why = why, vars = ps_vars_table(st, meta), hyp = hyp, model = model, calc = calc,
       table = list(title = "ANOVA table", df = ps_anova_df(rows, dfe, sse, N - 1, SST)),
       stats = data.frame(Effect = rows$source, Statistic = sprintf("F0 = %s", ps_fmt(rows$F, 4)), df = sprintf("%d, %d", rows$df, dfe), `P-value` = vapply(rows$p, ps_p, ""),
                          Critical = ps_fmt(rows$crit, 4), Decision = ifelse(rows$p < a, "Reject H0", "Fail to reject H0"), check.names = FALSE),
       decision = dec, comp = comp, assump = chk, extra = extra, var_comp = var_comp,
       conclusion = sprintf("At α = %s, %s (F0 = %s with %d and %d df, %s).", format(a),
                            if (kind == "random") { if (sig) sprintf("there is significant variability between %s: στ² > 0", pl_word(lt)) else sprintf("there is no significant variability between %s", pl_word(lt)) }
                            else if (sig) sprintf("%s has a significant effect on the mean %s%s", lt, lab, scale_txt) else sprintf("there is no significant difference in mean %s between the %s levels%s", lab, lt, scale_txt),
                            ps_fmt(Ft, 4), rows$df[1], dfe, ps_pe(pt)),
       plain = plain, fit = fit, mse = mse, dfe = dfe,
       plots = list(list(t = sprintf("%s by %s", st$roles$response, st$roles$treatment), p = ps_plot_groups(d, ps_ylab(meta, st), st$roles$treatment)),
                    if (!is.null(comp$ph)) list(t = sprintf("%s intervals for the differences", comp$method), p = ps_plot_ci(comp$ph, s$conf)),
                    list(t = "Normal QQ plot of the residuals", p = ps_plot_qq(stats::residuals(fit))),
                    list(t = "Residuals versus fitted values", p = ps_plot_rvf(stats::fitted(fit), stats::residuals(fit)))) |> Filter(f = Negate(is.null)))
}

# General linear model: additive model in every mapped factor, Type II tests.
ps_glm <- function(kind, st, s, meta) {
  d <- st$d; a <- s$alpha; facs <- st$facs; lab <- ps_lab(meta, "response")
  fit <- stats::lm(stats::as.formula(paste("y ~", paste(facs, collapse = " + "))), data = d)
  av <- suppressWarnings(car::Anova(fit, type = 2))
  dfe <- av["Residuals", "Df"]; sse <- av["Residuals", "Sum Sq"]; mse <- sse / dfe
  src <- vapply(facs, function(k) st$roles[[k]], "")
  rows <- data.frame(term = facs, source = src, df = av[facs, "Df"], ss = av[facs, "Sum Sq"], F = av[facs, "F value"], p = av[facs, "Pr(>F)"], stringsAsFactors = FALSE)
  rows$crit <- stats::qf(1 - a, rows$df, dfe)
  sm <- summary(fit); N <- nrow(d); SST <- sum((d$y - mean(d$y))^2)
  has_t <- "treatment" %in% facs
  key <- if (has_t) "treatment" else facs[1]; kr <- rows[rows$term == key, ]
  sig <- ps_decide(kr$p, a)
  comp <- list(text = "There is no treatment factor to compare.")
  if (has_t) {
    em <- quiet(emmeans::emmeans(fit, "treatment"))
    if (!is.null(em) && sig && nlevels(d$treatment) > 2) {
      pr <- as.data.frame(summary(graphics::pairs(em, adjust = "tukey"), infer = TRUE, level = s$conf))
      comp <- list(text = c("Pairwise comparisons of the adjusted (least-squares) means with Tukey's adjustment. Adjusted means correct for the other factors in the model, which matters when the design is unbalanced."),
                   table = data.frame(Comparison = pr$contrast, Difference = ps_fmt(pr$estimate), CI = sprintf("[%s, %s]", ps_fmt(pr$lower.CL), ps_fmt(pr$upper.CL)),
                                      `Adjusted p` = vapply(pr$p.value, ps_p, ""), Result = ifelse(pr$p.value < a, "Different", "Not different"), check.names = FALSE))
    } else comp <- list(text = if (!sig) "The treatment effect is not significant, so pairwise comparisons are not carried out." else "Two levels: the F test is the comparison.")
    if (!is.null(em)) { e <- as.data.frame(summary(em, level = s$conf))
      comp$means <- data.frame(Level = e$treatment, `Adjusted mean` = ps_fmt(e$emmean), SE = ps_fmt(e$SE, 4), CI = sprintf("[%s, %s]", ps_fmt(e$lower.CL), ps_fmt(e$upper.CL)), check.names = FALSE) }
  }
  chk <- ps_assump(stats::residuals(fit), a, if (has_t) d$treatment else NULL)
  list(design = sprintf("An additive model with %s: %s. The general linear model fits it by least squares, so it works even when the layout is unbalanced or incomplete.", plural(length(facs), "factor"), paste(src, collapse = ", ")),
       why = "Type II sums of squares test each factor after adjusting for all the others, so the result does not depend on the order of the terms.",
       vars = ps_vars_table(st, meta),
       hyp = c(sprintf("For each factor: H0: all of its effects are 0, H1: at least one is not 0."), sprintf("The main question is about %s. Significance level α = %s.", src[[key]], format(a))),
       model = list(eq = sprintf("y = μ + %s + ε,   ε ~ N(0, σ²)", paste(sprintf("%s effect", src), collapse = " + ")), notes = "All factors enter additively; no interactions."),
       calc = list(list(t = "Least-squares fit", x = c(sprintf("The model has %d parameters estimated from N = %d observations, leaving %d error df.", N - dfe, N, dfe),
                                                        sprintf("SS_E = %s,  MS_E = SS_E / df_E = %s", ps_fmt(sse, 7), ps_fmt(mse, 6)),
                                                        sprintf("R² = %s, adjusted R² = %s", ps_fmt(sm$r.squared, 4), ps_fmt(sm$adj.r.squared, 4)))),
                   list(t = "Type II F tests", x = sprintf("%s: SS = %s on %d df, F0 = MS / MS_E = %s, %s", rows$source, ps_fmt(rows$ss, 7), rows$df, ps_fmt(rows$F, 5), vapply(rows$p, ps_pe, "")))),
       table = list(title = "Type II ANOVA table", df = ps_anova_df(rows, dfe, sse, N - 1, SST)),
       stats = data.frame(Effect = rows$source, Statistic = sprintf("F0 = %s", ps_fmt(rows$F, 4)), df = sprintf("%d, %d", rows$df, dfe), `P-value` = vapply(rows$p, ps_p, ""),
                          Critical = ps_fmt(rows$crit, 4), Decision = ifelse(rows$p < a, "Reject H0", "Fail to reject H0"), check.names = FALSE),
       decision = sprintf("%s: %s", rows$source, vapply(seq_len(nrow(rows)), function(i) ps_decision_txt(rows$p[i], a, sprintf("F0 = %s", ps_fmt(rows$F[i], 4)), sprintf("F0 = %s > %s", ps_fmt(rows$F[i], 4), ps_fmt(rows$crit[i], 4))), "")),
       comp = comp, assump = chk,
       extra = if (!is.null(comp$means)) list(list(t = sprintf("Adjusted means with %g%% confidence intervals", 100 * s$conf), df = comp$means)) else list(),
       conclusion = sprintf("At α = %s, %s %s a significant effect on %s (F0 = %s, %s).", format(a), src[[key]], if (sig) "has" else "does not have", lab, ps_fmt(kr$F, 4), ps_pe(kr$p)),
       plain = sprintf("After allowing for the other factors, %s.", if (sig) sprintf("%s changes the %s by more than chance would explain", src[[key]], lab) else sprintf("the data do not show that %s changes the %s", src[[key]], lab)),
       plots = list(list(t = sprintf("%s by %s", st$roles$response, src[[key]]), p = ps_plot_groups(d, ps_ylab(meta, st), src[[key]], key)),
                    list(t = "Normal QQ plot of the residuals", p = ps_plot_qq(stats::residuals(fit))),
                    list(t = "Residuals versus fitted values", p = ps_plot_rvf(stats::fitted(fit), stats::residuals(fit)))))
}

# Rank tests matched to the layout.
ps_nonpar <- function(kind, st, s, meta) {
  d <- st$d; a <- s$alpha; alt <- s$alt; has <- function(k) k %in% st$facs; lab <- ps_lab(meta, "response")
  out <- list(vars = ps_vars_table(st, meta), assump = list(table = data.frame(Assumption = "Distribution", Test = "None needed (rank-based)", Statistic = "—", `p-value` = "—", Result = "—", check.names = FALSE),
                                                          text = c("Rank tests do not assume normality. They still assume independent observations and, for comparing locations, groups of similar shape and spread.")))
  if (!length(st$facs)) {
    w <- quiet(stats::wilcox.test(d$y, mu = s$mu0, alternative = alt, exact = FALSE))
    name <- "Wilcoxon signed-rank test"; stat <- sprintf("V = %s", ps_fmt(unname(w$statistic))); p <- w$p.value; dfx <- "—"
    hyp <- c(sprintf("H0: the median of %s is %s", lab, format(s$mu0)), sprintf("H1: the median is %s %s", ps_alt_word(alt), format(s$mu0)))
    design <- "One sample compared with a target value."; how <- "Ranks the absolute distances from μ0 and adds the ranks of the values above it (V)."
    pl <- ps_plot_groups(d, ps_ylab(meta, st), NULL)
  } else if (has("row") && has("column")) {
    bm <- stats::lm(y ~ row + column, data = d); rr <- rank(stats::residuals(bm)); kw <- stats::kruskal.test(rr ~ d$treatment)
    name <- "Kruskal-Wallis test on ranked residuals (rows and columns removed)"; stat <- sprintf("H = %s", ps_fmt(unname(kw$statistic), 5)); p <- kw$p.value; dfx <- format(kw$parameter)
    hyp <- c("H0: the treatments have the same distribution after removing row and column effects", "H1: at least one treatment differs")
    design <- "A Latin square analysed without assuming normality."; how <- "Removes the row and column effects with a linear model, ranks what is left and compares the treatments' mean ranks (the class approach)."
    pl <- ps_plot_groups(d, ps_ylab(meta, st), st$roles$treatment)
  } else if (has("block")) {
    if (st$ntrt == 2) {
      w <- stats::reshape(d[, c("y", "treatment", "block")], idvar = "block", timevar = "treatment", direction = "wide"); lv <- levels(d$treatment)
      wt <- quiet(stats::wilcox.test(w[[paste0("y.", lv[1])]], w[[paste0("y.", lv[2])]], paired = TRUE, alternative = alt, exact = FALSE))
      name <- "Wilcoxon signed-rank test (paired)"; stat <- sprintf("V = %s", ps_fmt(unname(wt$statistic))); p <- wt$p.value; dfx <- "—"
      hyp <- c(sprintf("H0: the median difference %s − %s is 0", lv[1], lv[2]), sprintf("H1: the median difference is %s 0", ps_alt_word(alt)))
      how <- "Ranks the absolute within-pair differences and sums the ranks of the positive ones."
    } else {
      m <- stats::aggregate(y ~ treatment + block, data = d, FUN = mean); fr <- stats::friedman.test(y ~ treatment | block, data = m)
      name <- "Friedman test"; stat <- sprintf("χ² = %s", ps_fmt(unname(fr$statistic), 5)); p <- fr$p.value; dfx <- format(fr$parameter)
      hyp <- c("H0: the treatments have the same distribution within blocks", "H1: at least one treatment tends to give larger values")
      how <- "Ranks the treatments within each block, so block differences cannot affect the result."
    }
    design <- "A blocked comparison analysed without assuming normality."
    pl <- ps_plot_profile(d, ps_ylab(meta, st), st$roles$treatment, st$roles$block)
  } else if (st$ntrt == 2) {
    lv <- levels(d$treatment)
    wt <- quiet(stats::wilcox.test(d$y[d$treatment == lv[1]], d$y[d$treatment == lv[2]], alternative = alt, exact = FALSE))
    name <- "Mann-Whitney U (Wilcoxon rank-sum) test"; stat <- sprintf("W = %s", ps_fmt(unname(wt$statistic))); p <- wt$p.value; dfx <- "—"
    hyp <- c(sprintf("H0: %s and %s have the same distribution", lv[1], lv[2]), sprintf("H1: %s tends to give %s values than %s", lv[1], switch(alt, two.sided = "different", greater = "larger", less = "smaller"), lv[2]))
    design <- "Two independent groups compared without assuming normality."; how <- "Ranks all observations together and compares the rank sums of the two groups."
    pl <- ps_plot_groups(d, ps_ylab(meta, st), st$roles$treatment)
  } else {
    kw <- stats::kruskal.test(y ~ treatment, data = d)
    name <- "Kruskal-Wallis test"; stat <- sprintf("H = %s", ps_fmt(unname(kw$statistic), 5)); p <- kw$p.value; dfx <- format(kw$parameter)
    hyp <- c(sprintf("H0: all %d groups have the same distribution", st$ntrt), "H1: at least one group tends to give larger values")
    design <- "Several independent groups compared without assuming normality."; how <- "Ranks all observations together and compares the mean rank of each group (H is approximately χ² with a − 1 df)."
    pl <- ps_plot_groups(d, ps_ylab(meta, st), st$roles$treatment)
    if (p < a) { dn <- dunn_test(d$y, d$treatment, "holm")
      out$comp <- list(text = "Dunn's test compares the mean ranks of each pair, with Holm's adjustment for multiple comparisons.",
                       table = data.frame(Comparison = dn$pair, `Mean rank difference` = ps_fmt(dn$diff, 4), z = ps_fmt(dn$stat, 4), `Adjusted p` = vapply(dn$p, ps_p, ""),
                                          Result = ifelse(dn$p < a, "Different", "Not different"), check.names = FALSE)) }
  }
  sig <- ps_decide(p, a)
  c(out, list(design = design, why = "A rank test replaces the values by their ranks, so it is not affected by non-normal errors or outliers. It is the usual alternative when the normality check fails.",
              hyp = c(hyp, sprintf("Significance level α = %s.", format(a))), model = list(eq = "No distributional model is assumed; the test uses ranks.", notes = how),
              calc = list(list(t = "Rank test", x = c(how, sprintf("%s: %s%s, %s", name, stat, if (dfx != "—") sprintf(" with %s df", dfx) else "", ps_pe(p))))),
              table = list(title = "Test results", df = data.frame(Test = name, Statistic = stat, df = dfx, `P-value` = ps_p(p), check.names = FALSE)),
              stats = data.frame(Effect = if (length(st$facs)) st$roles$treatment else "Median vs μ0", Statistic = stat, df = dfx, `P-value` = ps_p(p), Critical = "—",
                                 Decision = if (sig) "Reject H0" else "Fail to reject H0", check.names = FALSE),
              decision = if (sig) sprintf("Since %s < α = %s, we reject H0.", ps_pe(p), format(a)) else sprintf("Since %s ≥ α = %s, we fail to reject H0.", ps_pe(p), format(a)),
              comp = out$comp %||% list(text = if (sig) "See the test above; with two groups or blocked data the test is the comparison." else "The test is not significant, so no pairwise comparisons are made."),
              conclusion = sprintf("At α = %s, the %s %s (%s, %s).", format(a), name, if (sig) "finds a significant difference" else "finds no significant difference", stat, ps_pe(p)),
              plain = if (sig) sprintf("Even without assuming normal data, the %s values differ more than chance would explain.", lab) else sprintf("Without assuming normal data, the differences in %s are within ordinary random variation.", lab),
              plots = list(list(t = "The data", p = pl))))
}

# Descriptive statistics only.
ps_desc <- function(kind, st, s, meta) {
  d <- st$d; g <- if ("treatment" %in% st$facs) d$treatment else factor(rep("All data", nrow(d)))
  rows <- lapply(levels(g), function(l) { y <- d$y[g == l]; sw <- sw_test(y)
    data.frame(Group = l, n = length(y), Mean = ps_fmt(mean(y)), SD = ps_fmt(stats::sd(y), 4), SE = ps_fmt(stats::sd(y) / sqrt(length(y)), 4), Median = ps_fmt(stats::median(y)),
               Min = ps_fmt(min(y)), Max = ps_fmt(max(y)), IQR = ps_fmt(stats::IQR(y), 4), CV = sprintf("%.1f%%", 100 * stats::sd(y) / mean(y)),
               `Shapiro-Wilk p` = ps_p(sw$p), check.names = FALSE) })
  tab <- do.call(rbind, rows)
  list(design = sprintf("Descriptive summary of %s%s.", st$roles$response, if ("treatment" %in% st$facs) sprintf(" for each level of %s", st$roles$treatment) else ""),
       why = "Descriptive statistics describe the centre, spread and shape of the data. They do not test a hypothesis, but they show what any test will be working with.",
       vars = ps_vars_table(st, meta), hyp = "Descriptive statistics do not test hypotheses. Choose an inferential analysis to test one.",
       model = list(eq = "x̄ = Σxᵢ / n,   s² = Σ(xᵢ − x̄)² / (n − 1),   SE = s / √n", notes = "CV = s / x̄ × 100% is the spread relative to the mean."),
       calc = list(list(t = "Summary statistics", x = "Computed for each group in the table below.")),
       table = list(title = "Summary statistics", df = tab), stats = NULL, decision = "No hypothesis is tested.", comp = list(text = "Compare the means and standard deviations in the table; a formal test is needed before claiming a difference."),
       assump = list(table = NULL, text = "The Shapiro-Wilk p-value for each group is in the table: p < α suggests that group is not normal."),
       conclusion = "Descriptive results only; no statistical conclusion about the population is drawn.",
       plain = sprintf("The table and the plot summarise the %s values. Look for differences in centre between groups, differences in spread, and unusual values.", ps_lab(meta, "response")),
       plots = list(list(t = "The data", p = ps_plot_groups(d, ps_ylab(meta, st), if ("treatment" %in% st$facs) st$roles$treatment else NULL))))
}

# Residual diagnostics for the model the layout implies.
ps_diag <- function(kind, st, s, meta) {
  d <- st$d; a <- s$alpha
  facs <- st$facs
  form <- if (length(facs)) paste("y ~", paste(facs, collapse = " + ")) else "y ~ 1"
  fit <- stats::lm(stats::as.formula(form), data = d)
  form_disp <- paste(st$roles$response, "~", if (length(facs)) paste(vapply(facs, function(k) st$roles[[k]], ""), collapse = " + ") else "1")
  r <- stats::residuals(fit); rs <- stats::rstandard(fit); fv <- stats::fitted(fit)
  ok <- is.finite(rs)
  chk <- ps_assump(r, a, if ("treatment" %in% facs) d$treatment else NULL)
  bp <- if (length(facs)) quiet(lmtest::bptest(fit)) else NULL
  if (!is.null(bp)) chk$table <- rbind(chk$table, data.frame(Assumption = "Equal variance", Test = "Breusch-Pagan", Statistic = sprintf("BP = %.4f", unname(bp$statistic)),
                                                             `p-value` = ps_p(bp$p.value), Result = if (bp$p.value < a) "Fail" else "Pass", check.names = FALSE))
  out <- which(ok & abs(rs) > 3)
  list(design = sprintf("Residual diagnostics for the model %s, the model implied by the mapped variables.", form_disp),
       why = "The F and t tests assume normal, independent errors with constant variance. The residuals eᵢ = yᵢ − ŷᵢ estimate those errors, so their behaviour shows whether the assumptions hold.",
       vars = ps_vars_table(st, meta),
       hyp = c("Normality: H0: the errors are normally distributed.", "Equal variance: H0: the error variance is the same for every group or fitted value.", "A diagnostic test passes when p ≥ α."),
       model = list(eq = "eᵢ = yᵢ − ŷᵢ,   standardised dᵢ = eᵢ / √MS_E", notes = "About 95% of standardised residuals should lie within ±2, and values beyond ±3 are possible outliers."),
       calc = list(list(t = "Residuals", x = c(sprintf("Model fitted: %s", form_disp), sprintf("MS_E = %s on %d df", ps_fmt(sum(r^2) / fit$df.residual), fit$df.residual),
                                               sprintf("Standardised residuals range from %s to %s.", ps_fmt(min(rs[ok]), 3), ps_fmt(max(rs[ok]), 3))))),
       table = list(title = "Diagnostic tests", df = chk$table), stats = NULL,
       decision = sprintf("%d check%s fail at α = %s.", sum(chk$table$Result == "Fail"), if (sum(chk$table$Result == "Fail") == 1) "" else "s", format(a)),
       comp = list(text = if (length(out)) sprintf("Possible outliers (|standardised residual| > 3): observation%s %s.", if (length(out) > 1) "s" else "", paste(out, collapse = ", ")) else "No standardised residual is beyond ±3, so there are no clear outliers."),
       assump = chk,
       conclusion = if (any(chk$table$Result == "Fail")) "At least one assumption fails; consider a transformation of the response or a rank-based test before relying on the ANOVA." else "The residuals are consistent with the model assumptions, so the usual F and t tests can be trusted.",
       plain = "The QQ plot should be close to a straight line, and the residuals-versus-fitted plot should be a shapeless band of even width around 0. Curves, funnels or isolated points signal a problem.",
       plots = list(list(t = "Normal QQ plot of the residuals", p = ps_plot_qq(r)), list(t = "Residuals versus fitted values", p = ps_plot_rvf(fv, r))))
}
