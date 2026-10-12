# Reading files, detecting column types and checking data health.

TYPE_CHOICES <- c("Number" = "number", "Category" = "category", "Text (ID)" = "id")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# Turn text such as "49 kg" or "1,5" into a number; NA when it is not one.
parse_number_loose <- function(x, dec = ".") {
  x <- trimws(as.character(x))
  if (dec == ",") x <- gsub(",", ".", gsub(".", "", x, fixed = TRUE), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

is_missing_code <- function(x, na_codes) {
  x <- trimws(as.character(x))
  is.na(x) | x %in% na_codes
}

# Clean column names so they are unique and readable, but keep the user's wording.
clean_names <- function(nms) {
  nms <- trimws(as.character(nms))
  nms[is.na(nms) | nms == ""] <- paste0("Column ", which(is.na(nms) | nms == ""))
  make.unique(nms, sep = " ")
}

# Guess the role of each column. Numbers that are small sets of whole numbers
# look like codes (e.g. 1, 2, 3 for treatments) and are suggested as categories.
guess_type <- function(x, name = "") {
  v <- x[!is.na(x) & trimws(as.character(x)) != ""]
  if (length(v) == 0) return("category")
  num <- suppressWarnings(as.numeric(as.character(v)))
  if (all(!is.na(num))) {
    u <- unique(num)
    whole <- all(abs(num - round(num)) < 1e-9)
    looks_id <- grepl("(^id$|\\bid\\b|plot|unit|subject|run|order)", tolower(name))
    if (looks_id && length(u) == length(num)) return("number")
    if (whole && length(u) <= 8 && length(u) < length(num) / 1.5) return("category")
    return("number")
  }
  # Mostly numbers with a few bad cells: still a number column with a problem.
  if (mean(!is.na(num)) >= 0.6) return("number")
  if (length(unique(v)) == length(v) && length(v) > 8) return("id")
  if (grepl("(^id$|\\bid\\b|plot)", tolower(name)) && length(unique(v)) == length(v)) return("id")
  "category"
}

guess_types <- function(df) {
  setNames(vapply(names(df), function(n) guess_type(df[[n]], n), ""), names(df))
}

# Apply user types: numbers become numeric (bad cells become NA, reported by health checks).
coerce_types <- function(df, types) {
  for (n in names(df)) {
    t <- types[[n]] %||% "category"
    if (t == "number") {
      if (!is.numeric(df[[n]])) df[[n]] <- parse_number_loose(df[[n]])
    } else {
      df[[n]] <- as.character(df[[n]])
    }
  }
  df
}

drop_empty_rows <- function(df) {
  if (nrow(df) == 0) return(df)
  keep <- apply(df, 1, function(r) any(!is.na(r) & trimws(as.character(r)) != ""))
  df[keep, , drop = FALSE]
}
drop_empty_cols <- function(df) {
  if (ncol(df) == 0) return(df)
  keep <- vapply(df, function(c) any(!is.na(c) & trimws(as.character(c)) != ""), TRUE)
  df[, keep, drop = FALSE]
}

# ---------- Reading ----------

sniff_delim <- function(lines) {
  lines <- head(lines[nzchar(trimws(lines))], 20)
  cands <- c("," = ",", ";" = ";", "\t" = "\t", "|" = "|")
  counts <- vapply(cands, function(d) {
    k <- lengths(regmatches(lines, gregexpr(d, lines, fixed = TRUE)))
    if (length(k) == 0 || k[1] == 0) return(0)
    # consistent count across lines wins
    sum(k == k[1]) * k[1]
  }, 0)
  if (all(counts == 0)) return(",")
  cands[[which.max(counts)]]
}

read_text_table <- function(path_or_text, header = TRUE, skip = 0, na_codes = c("", "NA"), is_text = FALSE) {
  lines <- if (is_text) strsplit(path_or_text, "\r?\n")[[1]] else readLines(path_or_text, warn = FALSE, encoding = "UTF-8")
  lines <- sub("^﻿", "", lines)
  if (skip > 0) lines <- lines[-seq_len(min(skip, length(lines)))]
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) stop("The file has no rows.")
  d <- sniff_delim(lines)
  dec <- "."
  if (d == ";" && any(grepl("[0-9],[0-9]", lines))) dec <- ","
  df <- utils::read.table(text = lines, sep = d, header = FALSE, quote = "\"'", dec = ".",
                          colClasses = "character", fill = TRUE, comment.char = "",
                          strip.white = TRUE, na.strings = character(0), check.names = FALSE)
  if (header) {
    nms <- as.character(unlist(df[1, ])); df <- df[-1, , drop = FALSE]
  } else nms <- paste0("Column ", seq_len(ncol(df)))
  names(df) <- clean_names(nms)
  df[] <- lapply(df, function(c) { c[is_missing_code(c, na_codes)] <- NA; c })
  if (dec == ",") df[] <- lapply(df, function(c) {
    p <- parse_number_loose(c, ",")
    if (all(is.na(c) | !is.na(p))) as.character(p) else c
  })
  df <- drop_empty_cols(drop_empty_rows(df))
  rownames(df) <- NULL
  list(data = df, delim = d, dec = dec)
}

read_excel_sheet <- function(path, sheet, header = TRUE, skip = 0, na_codes = c("", "NA")) {
  df <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, skip = skip,
                           col_types = "text", na = na_codes, .name_repair = "minimal")
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(data.frame())
  if (header) {
    nms <- as.character(unlist(df[1, ])); df <- df[-1, , drop = FALSE]
  } else nms <- paste0("Column ", seq_len(ncol(df)))
  names(df) <- clean_names(nms)
  df[] <- lapply(df, function(c) { c[is_missing_code(c, na_codes)] <- NA; c })
  df <- drop_empty_cols(drop_empty_rows(df))
  # Excel stores 50 as "50" but sometimes as "50.000000000001"; tidy numbers.
  df[] <- lapply(df, function(c) {
    p <- suppressWarnings(as.numeric(c))
    if (all(is.na(c) | !is.na(p))) { out <- c; out[!is.na(p)] <- format(signif(p[!is.na(p)], 12), trim = TRUE, scientific = FALSE); out } else c
  })
  rownames(df) <- NULL
  df
}

# Summary of each sheet for the sheet picker.
sheet_summaries <- function(path, header = TRUE, skip = 0, na_codes = c("", "NA")) {
  sheets <- readxl::excel_sheets(path)
  lapply(sheets, function(s) {
    df <- tryCatch(read_excel_sheet(path, s, header, skip, na_codes), error = function(e) data.frame())
    nnum <- if (ncol(df)) sum(vapply(df, function(c) {
      p <- suppressWarnings(as.numeric(c)); v <- !is.na(c); any(v) && mean(!is.na(p[v])) >= 0.6
    }, TRUE)) else 0
    list(name = s, rows = nrow(df), cols = ncol(df), numeric = nnum,
         status = if (nrow(df) == 0) "Empty" else if (nnum == 0) "No numbers" else "Ready")
  })
}

# ---------- Health checks ----------

# Returns a list of findings. Each has level (ok, warn, info, error), text and,
# when we can fix it, a fix list(col, label).
data_health <- function(df, types, empty_rows = 0) {
  out <- list()
  add <- function(level, text, fix = NULL) out[[length(out) + 1]] <<- list(level = level, text = text, fix = fix)
  n <- nrow(df)
  complete <- sum(stats::complete.cases(df))
  add(if (complete == n) "ok" else "info",
      sprintf("%d of %d rows complete%s", complete, n,
              if (empty_rows > 0) sprintf(", %d empty rows ignored", empty_rows) else ""))
  for (nm in names(df)) {
    t <- types[[nm]] %||% "category"
    x <- df[[nm]]
    if (t == "number") {
      raw <- as.character(x)
      bad <- which(!is.na(raw) & trimws(raw) != "" & is.na(suppressWarnings(as.numeric(raw))))
      if (length(bad)) {
        fixable <- bad[!is.na(parse_number_loose(gsub("[^0-9eE+.,-]", "", raw[bad])))]
        ex <- raw[bad[1]]
        add("warn", sprintf("%s row %d contains text: “%s”%s", nm, bad[1], ex,
                            if (length(bad) > 1) sprintf(" (and %d more)", length(bad) - 1) else ""),
            if (length(fixable)) list(col = nm, rows = fixable,
                                      label = sprintf("Keep the number in %s and drop the text (for example “%s” becomes %s).",
                                                      nm, ex, format(parse_number_loose(gsub("[^0-9eE+.,-]", "", ex))))))
      }
      nmiss <- sum(is.na(x) | trimws(as.character(x)) == "")
      if (nmiss) add("info", sprintf("%s has %d missing value%s", nm, nmiss, if (nmiss > 1) "s" else ""))
      v <- suppressWarnings(as.numeric(as.character(x)))
      if (sum(!is.na(v)) > 1 && length(unique(v[!is.na(v)])) == 1) add("warn", sprintf("%s has the same value in every row", nm))
    } else if (t == "id") {
      d <- sum(duplicated(x[!is.na(x)]))
      if (d) add("warn", sprintf("%s has %d duplicated ID%s", nm, d, if (d > 1) "s" else ""))
      else add("ok", sprintf("No duplicate %s values", nm))
    } else {
      v <- x[!is.na(x)]
      p <- suppressWarnings(as.numeric(as.character(v)))
      if (length(v) && all(!is.na(p))) {
        u <- sort(unique(p))
        add("info", sprintf("%s looks like codes (%s). They will be treated as categories, and you can name each code later.",
                            nm, paste(head(u, 6), collapse = ", ")))
      }
      if (length(unique(v)) == 1 && length(v) > 1) add("warn", sprintf("%s has only one level", nm))
      # Same label typed two ways, e.g. "Urea" and "urea ".
      lv <- unique(as.character(v))
      key <- tolower(trimws(lv))
      if (any(duplicated(key))) {
        dup <- lv[key %in% key[duplicated(key)]]
        add("warn", sprintf("%s has labels that differ only by case or spaces: %s", nm, paste(dQuote(dup, FALSE), collapse = ", ")),
            list(col = nm, rows = NULL, kind = "case",
                 label = sprintf("Merge the %s labels that differ only by case or spaces.", nm)))
      }
    }
  }
  out
}

apply_fix <- function(df, fix) {
  x <- as.character(df[[fix$col]])
  if (identical(fix$kind, "case")) {
    key <- tolower(trimws(x))
    first <- x[match(key, key)]
    df[[fix$col]] <- trimws(first)
  } else {
    x[fix$rows] <- as.character(parse_number_loose(gsub("[^0-9eE+.,-]", "", x[fix$rows])))
    df[[fix$col]] <- x
  }
  df
}

# Parse text pasted from Excel or Sheets (tab separated).
parse_pasted <- function(txt, header = TRUE) {
  if (!nzchar(trimws(txt))) return(NULL)
  read_text_table(txt, header = header, is_text = TRUE)$data
}

new_dataset <- function(name, df, source = "Typed", sheet = NULL, file = NULL, notes = character(0)) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  df[] <- lapply(df, as.character)
  list(id = paste0("d", as.integer(Sys.time()), sample(1000:9999, 1)),
       name = name, data = df, types = guess_types(df), source = source,
       sheet = sheet, file = file, notes = notes, created = Sys.time())
}

# Data typed as numbers for analysis.
typed_data <- function(ds) coerce_types(ds$data, ds$types)

# ---------- Project files (JSON) ----------

json_chr <- function(col) vapply(col, function(v) if (is.null(v)) NA_character_ else as.character(v), "")

records_to_df <- function(recs) {
  if (!length(recs)) return(NULL)
  keys <- unique(unlist(lapply(recs, names)))
  as.data.frame(setNames(lapply(keys, function(k) {
    v <- lapply(recs, function(r) r[[k]] %||% NA)
    if (all(vapply(v, is.numeric, TRUE))) unlist(v) else vapply(v, function(x) as.character(x), "")
  }), keys), stringsAsFactors = FALSE)
}

json_state <- function(s) {
  s$map <- lapply(s$map, function(v) v %||% "")
  s$labels <- lapply(s$labels, records_to_df)
  s$inputs$posthoc <- unlist(s$inputs$posthoc)
  s
}
