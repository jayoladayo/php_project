# Designed experiments: roles each design needs and the checks run before analysis.

ROLE_RESPONSE <- list(key = "response", label = "Response", icon = "trend", kind = "number", required = TRUE,
                      desc = "The numeric outcome you measured.", pattern = "yield|response|y$|value|score|time|weight|mpg|output")
ROLE_TREAT <- list(key = "treatment", label = "Treatment", icon = "flask", kind = "factor", required = TRUE,
                   desc = "The factor you want to compare.", pattern = "treat|trt|variety|fert|method|diet|drug|group|type")
ROLE_BLOCK <- list(key = "block", label = "Block", icon = "rows", kind = "factor", required = TRUE,
                   desc = "The nuisance factor each treatment appears in once.", pattern = "block|farm|rep|batch|field|day|site")
ROLE_ROW <- list(key = "row", label = "Row", icon = "rows", kind = "factor", required = TRUE,
                 desc = "First blocking factor of the square.", pattern = "^row|soil|operator|day")
ROLE_COL <- list(key = "column", label = "Column", icon = "columns", kind = "factor", required = TRUE,
                 desc = "Second blocking factor of the square.", pattern = "^col|irrig|machine|position|order")
ROLE_GREEK <- list(key = "greek", label = "Greek factor", icon = "layers", kind = "factor", required = TRUE,
                   desc = "Second treatment factor, laid over the Latin square.", pattern = "greek|second|fertilizer|method|type")
ROLE_RUN <- list(key = "run", label = "Run order", icon = "clock", kind = "any", required = FALSE,
                 desc = "Used for the independence check. If empty, file order is used.", pattern = "run|order|sequence|time")

DESIGNS <- list(
  CRD = list(key = "CRD", page = "crd", name = "Completely Randomized Design", short = "CRD", icon = "shuffle",
             blurb = "Treatments assigned to units completely at random.",
             roles = list(ROLE_RESPONSE, ROLE_TREAT, ROLE_RUN), example = "crd",
             nonpar = "Kruskal-Wallis"),
  RCBD = list(key = "RCBD", page = "rcbd", name = "Randomized Complete Block Design", short = "RCBD", icon = "rows",
              blurb = "Each block holds every treatment once.",
              roles = list(ROLE_RESPONSE, ROLE_TREAT, ROLE_BLOCK, ROLE_RUN), example = "rcbd_coded",
              nonpar = "Friedman"),
  LSD = list(key = "LSD", page = "lsd", name = "Latin Square Design", short = "Latin Square", icon = "grid",
             blurb = "Two blocking factors; each treatment once per row and column.",
             roles = list(ROLE_RESPONSE, ROLE_TREAT, ROLE_ROW, ROLE_COL, ROLE_RUN), example = "lsd",
             nonpar = "Friedman on treatments within rows"),
  GLSD = list(key = "GLSD", page = "glsd", name = "Graeco-Latin Square Design", short = "Graeco-Latin Square", icon = "grid4",
              blurb = "Two treatment factors over two blocking factors.",
              roles = list(ROLE_RESPONSE, modifyList(ROLE_TREAT, list(label = "Latin treatment")), ROLE_GREEK, ROLE_ROW, ROLE_COL, ROLE_RUN),
              example = "glsd", nonpar = "Kruskal-Wallis per factor (approximate)")
)

role_keys <- function(design) vapply(design$roles, `[[`, "", "key")
factor_roles <- function(design) Filter(function(r) r$kind == "factor", design$roles)

# Guess a mapping from column names and types.
guess_mapping <- function(design, df, types) {
  used <- character(0); out <- list()
  cols <- names(df)
  for (r in design$roles) {
    cand <- setdiff(cols, used)
    if (r$kind == "number") cand <- cand[types[cand] == "number"]
    if (r$kind == "factor") cand <- cand[types[cand] != "id" & types[cand] != "number" | grepl(r$pattern, tolower(cand))]
    if (r$kind == "factor") cand <- cand[types[cand] != "number" | vapply(cand, function(cn) {
      v <- suppressWarnings(as.numeric(df[[cn]])); u <- unique(v[!is.na(v)]); length(u) <= 12 && all(u == round(u))
    }, TRUE)]
    alts <- strsplit(r$pattern, "|", fixed = TRUE)[[1]]
    hit <- unique(unlist(lapply(alts, function(a) cand[grepl(a, tolower(gsub("[^A-Za-z]", "", cand)))])))
    pick <- if (length(hit)) hit[1] else if (r$required && length(cand)) cand[1] else ""
    if (r$kind == "any" && !length(hit)) pick <- ""
    out[[r$key]] <- pick
    if (nzchar(pick)) used <- c(used, pick)
  }
  out
}

fmt_levels <- function(x, k = 4) {
  u <- unique(as.character(x[!is.na(x)]))
  if (length(u) <= k) paste(u, collapse = ", ") else paste0(paste(head(u, k), collapse = ", "), ", …")
}

# Check the mapped data against the design. Returns items to display,
# problems that block analysis, and warnings that do not.
design_check <- function(design, df, types, map) {
  res <- list(valid = FALSE, items = list(), errors = character(0), warnings = character(0),
              formula = NULL, df_error = NA, n = nrow(df))
  req <- Filter(function(r) r$required, design$roles)
  missing_roles <- vapply(req, function(r) !nzchar(map[[r$key]] %||% ""), TRUE)
  if (any(missing_roles)) {
    res$errors <- sprintf("Choose a column for %s.", paste(vapply(req[missing_roles], `[[`, "", "label"), collapse = ", "))
    return(res)
  }
  chosen <- unlist(map[nzchar(unlist(map))])
  if (any(duplicated(chosen))) {
    res$errors <- "Each column can have only one role."
    return(res)
  }
  y <- suppressWarnings(as.numeric(as.character(df[[map$response]])))
  bad_y <- sum(is.na(y) & !is.na(df[[map$response]]))
  if (bad_y) res$errors <- c(res$errors, sprintf("%s has %d cell%s that are not numbers. Fix them in Data Studio.", map$response, bad_y, if (bad_y > 1) "s" else ""))
  keep <- !is.na(y)
  fk <- vapply(factor_roles(design), `[[`, "", "key")
  for (k in fk) keep <- keep & !is.na(df[[map[[k]]]]) & trimws(as.character(df[[map[[k]]]])) != ""
  d <- df[keep, , drop = FALSE]; y <- y[keep]
  n <- nrow(d); res$n <- n
  f <- lapply(setNames(fk, fk), function(k) factor(as.character(d[[map[[k]]]])))
  nl <- vapply(f, nlevels, 0L)
  lab <- function(k) { r <- Find(function(r) r$key == k, design$roles); r$label }
  item <- function(l, v) res$items[[length(res$items) + 1]] <<- list(label = l, value = v)

  t <- nl[["treatment"]]
  item(if (design$key == "GLSD") "Latin treatments" else "Treatments", sprintf("%d (%s)", t, fmt_levels(f$treatment)))
  if (t < 2) res$errors <- c(res$errors, sprintf("%s needs at least 2 levels.", map$treatment))

  if (design$key == "CRD") {
    reps <- table(f$treatment)
    item("Replicates per treatment", if (length(unique(reps)) == 1) as.character(reps[1]) else paste(range(reps), collapse = " to "))
    item("Observations", as.character(n))
    if (any(reps < 2)) res$errors <- c(res$errors, "Every treatment needs at least 2 observations.")
    if (length(unique(reps)) > 1) res$warnings <- c(res$warnings, "Unequal replication. The analysis still works; Tukey uses the Tukey-Kramer adjustment.")
    res$df_error <- n - t
    res$formula <- sprintf("%s ~ %s", bt(map$response), bt(map$treatment))
  }

  if (design$key == "RCBD") {
    b <- nl[["block"]]
    item("Blocks", sprintf("%d (%s)", b, fmt_levels(f$block)))
    tab <- table(f$treatment, f$block)
    item("Observations", sprintf("%d of %d expected", n, t * b))
    complete <- all(tab >= 1); one <- all(tab == 1)
    item("Complete and balanced", if (one) "Yes, 1 per cell" else if (complete && length(unique(c(tab))) == 1) sprintf("Yes, %d per cell", tab[1]) else "No")
    if (b < 2) res$errors <- c(res$errors, "An RCBD needs at least 2 blocks.")
    if (!complete) {
      miss <- which(tab == 0, arr.ind = TRUE)
      res$errors <- c(res$errors, sprintf("Not every treatment appears in every block (for example %s %s is missing in %s %s).",
                                          map$treatment, rownames(tab)[miss[1, 1]], map$block, colnames(tab)[miss[1, 2]]))
    } else if (!one && length(unique(c(tab))) > 1) {
      res$warnings <- c(res$warnings, "Some treatment and block combinations have more observations than others.")
    }
    res$df_error <- n - t - b + 1
    res$formula <- sprintf("%s ~ %s + %s", bt(map$response), bt(map$treatment), bt(map$block))
  }

  if (design$key %in% c("LSD", "GLSD")) {
    p <- t; r <- nl[["row"]]; cc <- nl[["column"]]
    item("Rows × columns", sprintf("%d × %d", r, cc))
    item("Observations", sprintf("%d of %d expected", n, p * p))
    ok_sq <- r == p && cc == p && n == p * p
    if (!ok_sq) res$errors <- c(res$errors, sprintf("A %d-treatment square needs %d rows, %d columns and %d observations; found %d rows, %d columns and %d observations.",
                                                    p, p, p, p * p, r, cc, n))
    cell <- table(f$row, f$column)
    if (ok_sq && any(cell != 1)) res$errors <- c(res$errors, "Each row and column combination must hold exactly one plot.")
    check_latin <- function(fac, name) {
      okr <- all(table(fac, f$row) == 1); okc <- all(table(fac, f$column) == 1)
      if (!okr) res$warnings <<- c(res$warnings, sprintf("%s does not appear exactly once in every %s, so this is not a true square in rows.", name, map$row))
      if (!okc) res$warnings <<- c(res$warnings, sprintf("%s does not appear exactly once in every %s, so this is not a true square in columns.", name, map$column))
      okr && okc
    }
    lat <- if (ok_sq) check_latin(f$treatment, map$treatment) else FALSE
    item("Each treatment once per row and column", if (lat) "Yes" else "No")
    if (design$key == "GLSD") {
      g <- nl[["greek"]]
      item("Greek treatments", sprintf("%d (%s)", g, fmt_levels(f$greek)))
      if (g != p) res$errors <- c(res$errors, sprintf("%s has %d levels but %s has %d; both must match the square size.", map$greek, g, map$treatment, p))
      gr <- if (ok_sq && g == p) check_latin(f$greek, map$greek) else FALSE
      orth <- ok_sq && g == p && all(table(f$treatment, f$greek) == 1)
      if (ok_sq && g == p && !orth) res$warnings <- c(res$warnings, sprintf("Each %s and %s pair must occur exactly once (the two squares must be orthogonal).", map$treatment, map$greek))
      item("Greek factor once per row and column", if (gr) "Yes" else "No")
      item("Squares orthogonal", if (orth) "Yes" else "No")
      res$df_error <- (p - 1) * (p - 3)
      res$formula <- sprintf("%s ~ %s + %s + %s + %s", bt(map$response), bt(map$treatment), bt(map$row), bt(map$column), bt(map$greek))
      if (p == 3) res$errors <- c(res$errors, "A 3 × 3 Graeco-Latin square leaves 0 error degrees of freedom, so no F-test is possible. Use a 4 × 4 or larger square, or drop one factor and analyse it as a Latin square.")
      if (p == 6) res$errors <- c(res$errors, "No 6 × 6 Graeco-Latin square exists (Euler's 36 officers problem).")
    } else {
      res$df_error <- (p - 1) * (p - 2)
      res$formula <- sprintf("%s ~ %s + %s + %s", bt(map$response), bt(map$treatment), bt(map$row), bt(map$column))
    }
  }
  # A broken square can still be analysed as an additive model if no effect is aliased.
  if (length(res$errors) == 0 && !is.null(res$formula)) {
    md <- as.data.frame(f); md$.y <- y
    rk <- tryCatch({
      X <- stats::model.matrix(stats::as.formula(paste(".y ~", paste(fk, collapse = " + "))), md)
      c(qr(X)$rank, ncol(X))
    }, error = function(e) c(0, 1))
    if (rk[1] < rk[2]) res$errors <- c(res$errors, "Some effects cannot be separated from each other (they are confounded), so the model cannot be estimated. Check the layout.")
    else if (length(res$warnings) && design$key %in% c("LSD", "GLSD") && any(grepl("not a true", res$warnings)))
      res$warnings <- c(res$warnings, "The analysis can still run as an additive model, but effects may be partly mixed together. Treat the results with care.")
    res$df_error <- n - rk[1]
  }
  item("Error degrees of freedom", as.character(res$df_error))
  if (!is.na(res$df_error) && res$df_error <= 0 && !any(grepl("error degrees", res$errors)))
    res$errors <- c(res$errors, "The design leaves no error degrees of freedom, so the F-test cannot be computed.")
  else if (!is.na(res$df_error) && res$df_error > 0 && res$df_error < 4)
    res$warnings <- c(res$warnings, sprintf("Only %d error degrees of freedom. Tests will have low power and assumption checks are rough.", res$df_error))
  res$valid <- length(res$errors) == 0
  res
}

# Backtick names with spaces so formulas stay valid R.
bt <- function(x) if (make.names(x) == x) x else paste0("`", x, "`")
