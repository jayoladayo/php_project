# Sharing an analysis: everything needed to open the same analysis on another computer (the data as the student
# has them, the roles, the settings, the wording of the hypotheses, the test and post-hoc chosen, the step reached),
# plus a short summary of the results so the person who opens it can check that they get the same numbers.
# A file (.sdsa, JSON) or a text code (the same JSON, compressed) carries it. Plain R, no Shiny.

SHARE_PREFIX <- "SDS1:"

share_encode <- function(x) {
  json <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA, dataframe = "columns")
  paste0(SHARE_PREFIX, base64enc::base64encode(memCompress(charToRaw(as.character(json)), "gzip")))
}
share_decode <- function(code) {
  code <- gsub("[[:space:]]+", "", code)
  if (!startsWith(code, SHARE_PREFIX)) stop("This is not a StatDesign Studio share code.")
  json <- rawToChar(memDecompress(base64enc::base64decode(sub(SHARE_PREFIX, "", code, fixed = TRUE)), "gzip"))
  share_parse(json)
}
share_parse <- function(json) {
  x <- jsonlite::fromJSON(json, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  if (!identical(x$app, "StatDesign Studio") || !identical(x$kind, "analysis")) stop("This file is not a shared StatDesign Studio analysis.")
  x$data <- as.data.frame(lapply(x$data, function(v) if (is.list(v)) unlist(lapply(v, function(e) if (is.null(e)) NA else e)) else v), stringsAsFactors = FALSE, check.names = FALSE)
  x
}
share_read_file <- function(path) share_parse(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))

# The numbers a lecturer can check: test, p-value(s), decision, and for designs with three or more treatments the
# assumption verdict and the rank test when it is needed.
ps_share_summary <- function(m, s, test) {
  st <- ps_structure(m$data, m$roles); kind <- ps_kind(st, m)
  if (is.na(kind)) return(NULL)
  res <- ps_run_test(kind, test, st, m, s)
  pv <- if (ps_is_fact(kind)) stats::setNames(res$effects$p, res$effects$code) else res$p
  out <- list(layout = PS_KIND_LABEL[[kind]], test = res$t$name %||% test, p = pv, reject = isTRUE(res$reject))
  if (ps_multi(kind)) {
    a <- ps_assumptions(kind, st, s, if (test %in% PS_NONPAR_TESTS) ps_info(kind, st, m, s)$suggest else test)
    out$assumptions_ok <- isTRUE(a$all_ok); out$failed <- a$failed
    if (!a$all_ok || test %in% PS_NONPAR_TESTS) {
      rn <- if (test %in% PS_NONPAR_TESTS) res else ps_run_test(kind, ps_np_test(kind), st, m, s)
      out$rank_test <- rn$t$name; out$rank_p <- if (ps_is_fact(kind)) stats::setNames(rn$effects$p, rn$effects$code) else rn$p
    }
  }
  out
}
share_same <- function(a, b) {
  if (is.null(a) || is.null(b)) return(NA)
  f <- function(x, y) length(x) == length(y) && all(abs(unname(unlist(x)) - unname(unlist(y))) <= 1e-8 * pmax(1, abs(unname(unlist(y)))))
  f(a$p, b$p) && (is.null(a$rank_p) || is.null(b$rank_p) || f(a$rank_p, b$rank_p))
}
