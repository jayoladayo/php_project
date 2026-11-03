# English / Arabic interface. One dictionary serves the browser (www/i18n.js translates the screen as it is
# built, so menus, steps, results and help follow the language switch) and R (the worked-solution PDF and the
# exported report). Exact sentences are looked up first; sentences with numbers or names in them are matched by
# patterns whose captured parts are translated again (names and numbers stay as they are).
#
# AR_EXACT: English text (whitespace collapsed) = Arabic text.
# AR_RX:    c(pattern, replacement); $1, $2 ... stand for the captured parts.
# Statistical terms follow the usual Arabic textbook usage: null hypothesis = الفرضية الصفرية, p-value =
# القيمة الاحتمالية, ANOVA = تحليل التباين, post-hoc = الاختبارات البعدية.

AR_EXACT <- character(0)
AR_RX <- list()
ar_add <- function(...) { x <- c(...); AR_EXACT[ar_norm(names(x))] <<- unname(x); invisible(NULL) }
ar_rx <- function(pattern, replacement) { AR_RX[[length(AR_RX) + 1]] <<- c(pattern, replacement); invisible(NULL) }
# A sprintf template as a pattern: %s matches any text, %d an integer, %g / %.2f a number, %% a percent sign.
# The Arabic text uses $1, $2 ... for the pieces in the order they appear in the English template.
ar_tpl <- function(..., variants = TRUE) {
  x <- c(...)
  if (variants) {   # the sentence also appears with a lower-case start and no final full stop (inside a longer sentence)
    nm <- names(x); ar <- unname(x); keys <- character(0); vals <- character(0)
    for (i in seq_along(nm)) {
      keys <- c(keys, nm[i]); vals <- c(vals, ar[i])
      lc <- sub("^([A-Z])([a-z])", "\\L\\1\\2", nm[i], perl = TRUE)
      if (!identical(lc, nm[i]) && !grepl("^[A-Z]{2}", nm[i])) { keys <- c(keys, lc); vals <- c(vals, ar[i]) }
      if (grepl("\\.$", nm[i])) { keys <- c(keys, sub("\\.$", "", nm[i])); vals <- c(vals, sub("\\.$", "", ar[i]))
        if (!identical(lc, nm[i]) && !grepl("^[A-Z]{2}", nm[i])) { keys <- c(keys, sub("\\.$", "", lc)); vals <- c(vals, sub("\\.$", "", ar[i])) } }
    }
    ok <- !duplicated(keys); x <- stats::setNames(vals[ok], keys[ok])
  }
  for (i in seq_along(x)) {
    t <- ar_norm(names(x)[i])
    t <- gsub("%%", "\001", t, fixed = TRUE)
    t <- gsub("%[0-9.]*[gf]", "\002G", t)
    t <- gsub("%d", "\002D", t, fixed = TRUE)
    t <- gsub("%s", "\002S", t, fixed = TRUE)
    rx <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", t)
    rx <- gsub("\002G", "(-?[0-9.]+(?:e-?[0-9]+)?)", rx, fixed = TRUE)
    rx <- gsub("\002D", "(-?[0-9]+)", rx, fixed = TRUE)
    rx <- gsub("\002S", "(.*?)", rx, fixed = TRUE)
    rx <- gsub("\001", "%", rx, fixed = TRUE)
    ar_rx(rx, unname(x[i]))
  }
  invisible(NULL)
}
# Patterns with the most fixed text first, so the most specific sentence wins.
ar_rx_all <- local({
  cache <- NULL; n <- -1
  function() {
    if (is.null(cache) || n != length(AR_RX)) {
      fixed <- vapply(AR_RX, function(r) nchar(gsub("\\([^)]*\\)", "", gsub("\\(\\?:[^)]*\\)", "", r[[1]]))), 0)
      cache <<- AR_RX[order(-fixed, seq_along(AR_RX))]; n <<- length(AR_RX)
    }
    cache
  }
})

# The dictionary files each define a function ar_part_*(); they are run once, on first use, so the order in which
# the files are loaded does not matter.
ar_ready <- FALSE
ar_init <- function() {
  if (ar_ready) return(invisible(NULL))
  env <- environment(ar_init)
  AR_EXACT <<- character(0); AR_RX <<- list()
  for (n in sort(ls(envir = env, pattern = "^ar_part_"))) get(n, envir = env)()
  ar_ready <<- TRUE; invisible(NULL)
}

ar_norm <- function(x) trimws(gsub("[[:space:]]+", " ", x))

# Translate one text. Returns NA when nothing in the dictionary matches.
ar_lookup <- function(x) {
  ar_init()
  t <- ar_norm(x)
  if (!nzchar(t)) return(NA_character_)
  hit <- AR_EXACT[t]
  if (!is.na(hit)) return(unname(hit))
  for (r in ar_rx_all()) {
    m <- regexec(paste0("^", r[[1]], "$"), t, perl = TRUE)
    g <- regmatches(t, m)[[1]]
    if (length(g)) {
      out <- r[[2]]
      for (i in rev(seq_len(length(g) - 1))) out <- gsub(paste0("$", i), ar_text(g[i + 1], keep = TRUE), out, fixed = TRUE)
      return(out)
    }
  }
  NA_character_
}
# The text in Arabic, or unchanged when it has no translation (keep = TRUE) or NA (keep = FALSE).
ar_text <- function(x, keep = TRUE) {
  vapply(x, function(s) { if (is.na(s)) return(NA_character_); r <- ar_lookup(s); if (is.na(r)) (if (keep) s else NA_character_) else r }, "", USE.NAMES = FALSE)
}
# A translator for a language code: "ar" translates, anything else leaves the text alone.
i18n_tr <- function(lang) if (identical(lang, "ar")) function(x) ar_text(x) else function(x) x

# The dictionary as JSON for the browser.
i18n_json <- function() {
  ar_init()
  jsonlite::toJSON(list(exact = as.list(AR_EXACT), rx = lapply(ar_rx_all(), function(r) list(r[[1]], r[[2]]))), auto_unbox = TRUE)
}

# A chart with its words in Arabic: titles, axis labels and the text drawn on it. Numbers, level names and the data stay
# as they are.
ar_plot <- function(p) {
  if (!inherits(p, "ggplot")) return(p)
  tr1 <- function(x) if (is.character(x) && length(x) == 1) ar_text(x) else x
  p$labels <- lapply(p$labels, tr1)
  p$layers <- lapply(p$layers, function(l) {
    if (!is.null(l$aes_params$label)) l$aes_params$label <- ar_text(as.character(l$aes_params$label))
    if (is.data.frame(l$data) && "label" %in% names(l$data) && is.character(l$data$label)) l$data$label <- ar_text(l$data$label)
    if (is.list(l$geom_params$label)) NULL
    l
  })
  p   # (no font family is named: a named font that the null device does not know can crash R; the default sans font has the Arabic letters)
}
