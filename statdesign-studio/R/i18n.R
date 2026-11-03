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
ar_add <- function(...) { x <- c(...); AR_EXACT[names(x)] <<- unname(x); invisible(NULL) }
ar_rx <- function(pattern, replacement) { AR_RX[[length(AR_RX) + 1]] <<- c(pattern, replacement); invisible(NULL) }

ar_norm <- function(x) trimws(gsub("[[:space:]]+", " ", x))

# Translate one text. Returns NA when nothing in the dictionary matches.
ar_lookup <- function(x) {
  t <- ar_norm(x)
  if (!nzchar(t)) return(NA_character_)
  hit <- AR_EXACT[t]
  if (!is.na(hit)) return(unname(hit))
  for (r in AR_RX) {
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
  jsonlite::toJSON(list(exact = as.list(AR_EXACT), rx = lapply(AR_RX, function(r) list(r[[1]], r[[2]]))), auto_unbox = TRUE)
}
