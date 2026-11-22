# Paper Replications: published experiments whose results the group re-created with StatDesign Studio.
#
# Each paper is a folder in papers/ (the replication pack as the group assembled it):
#   paper/     the open-access PDF, or a link file when the PDF is large
#   data/      the labelled CSV files used in the app (source_supplement/ keeps the authors' own file)
#   analysis/  the saved analyses (.sdsa, made with Share analysis), the app's text output and the printed output of the check
#   scripts/   the independent re-computation (Python for papers 01 to 09, base R for 10 to 15)
#   README.md  citation, licence, what the paper did, how it was replicated, the tally table and the insights
#
# The curated facts below (design, field, verdict, short title) drive the cards and the filters; everything else is
# read from the folder, so adding a paper means adding its folder and one entry to PAPER_META. Plain R, no Shiny,
# except the resource path that lets the browser download the files.

PAPERS_DIR <- "papers"
if (dir.exists(PAPERS_DIR)) shiny::addResourcePath("paperfiles", normalizePath(PAPERS_DIR))

# The designs, in the order of the course, and the analysis page each one opens.
PAPER_DESIGNS <- list(
  one  = list(label = "One sample", icon = "target", page = "one", desc = "One sample compared with a target value: the mean against a limit or a standard."),
  crd  = list(label = "CRD / k samples", icon = "layers", page = "crd", desc = "Three or more independent groups, completely randomized: one-way ANOVA."),
  rcbd = list(label = "RCBD", icon = "rows", page = "rcbd", desc = "Randomized complete block designs: treatments compared within blocks."),
  lsd  = list(label = "Latin Square", icon = "grid", page = "lsd", desc = "Two blocking factors (rows and columns); here L9 orthogonal arrays read as 3 × 3 Latin squares."),
  glsd = list(label = "Graeco-Latin Square", icon = "grid4", page = "glsd", desc = "Two treatment factors laid over two blocking factors."),
  fact = list(label = "Factorial", icon = "cross", page = "fact", desc = "Crossed factors with interactions, in blocks or in a row-column layout."),
  twok = list(label = "2^k Factorial", icon = "cross", page = "twok", desc = "Two-level designs, unreplicated (pooled error) or replicated (pure error)."))

# Do the paper's printed numbers come out of the app?
PAPER_TALLY <- c(yes = "Values tally", partly = "Values partly tally", no = "Discrepancies found")
PAPER_TALLY_KIND <- c(yes = "pass", partly = "warn", no = "fail")
PAPER_TALLY_DESC <- c(yes = "The app reproduces the paper's ANOVA (to rounding).",
                      partly = "The main numbers reproduce; some printed values only hold on another scale or contain a slip.",
                      no = "Part of the paper's analysis cannot be reproduced from its own data, and the conclusion changes.")
PAPER_FIELDS <- c("Agriculture", "Civil & materials", "Manufacturing & electronics", "Health & biology", "Environment")

PAPER_META <- list(
  list(num = "01", folder = "01_RCBD_Potato_Varieties_Tessema2022_PeerJ", design = "rcbd", field = "Agriculture", tally = "yes",
       title = "Genetic variability of Ethiopian potato varieties", who = "Tessema et al.", year = 2022, journal = "PeerJ",
       tally_note = "Yes, exactly (MS, σ²g, GCV, H²)", finding = "The total-yield row of the combined ANOVA does not match the supplement."),
  list(num = "02", folder = "02_RCBD_SweetPotato_Genotypes_Maulana2020_DataInBrief", design = "rcbd", field = "Agriculture", tally = "partly",
       title = "Yield stability of orange-fleshed sweet potato", who = "Maulana et al.", year = 2020, journal = "Data in Brief",
       tally_note = "Means yes; the ANOVA only on √yield", finding = "An undisclosed square-root transform; the check varieties are stored as exact square roots."),
  list(num = "03", folder = "03_LatinSquare_LVT_Milling_Zhu2019_Materials", design = "lsd", field = "Manufacturing & electronics", tally = "partly",
       title = "Milling luxury vinyl tiles with a helical cutter", who = "Zhu et al.", year = 2019, journal = "Materials",
       tally_note = "Force exact; roughness F values use an error of 0.008, not the printed 0.010", finding = "Depth of cut drives the cutting force."),
  list(num = "04", folder = "04_LatinSquare_Pervious_Concrete_Lu2025_Materials", design = "lsd", field = "Civil & materials", tally = "no",
       title = "Optimum mix design of pervious concrete", who = "Lu et al.", year = 2025, journal = "Materials",
       tally_note = "Permeability and 7-day strength yes; 28-day and tensile strength no", finding = "The paper's error SS is wrong (copied, or zero), and the significance reverses."),
  list(num = "05", folder = "05_GraecoLatin_SCC_Air_Voids_Lazniewska2026_Materials", design = "glsd", field = "Civil & materials", tally = "yes",
       title = "Air-void stability in self-compacting concrete", who = "Łaźniewska-Piekarczyk et al.", year = 2026, journal = "Materials",
       tally_note = "Yes, all six ANOVAs exactly", finding = "Only the water-to-binder ratio is significant (air loss, micropores)."),
  list(num = "06", folder = "06_Factorial_Cowpea_Zn_Fe_Carlos2026_PLOSONE", design = "fact", field = "Agriculture", tally = "yes",
       title = "Zinc and iron biofortification of cowpea", who = "Carlos et al.", year = 2026, journal = "PLOS ONE",
       tally_note = "Yes, exactly", finding = "A CV of 0.23 % is implausible for field data; the variance and independence checks fail."),
  list(num = "07", folder = "07_Factorial_Onion_Variety_Nitrogen_Yeshiwas2024_PLOSONE", design = "fact", field = "Agriculture", tally = "yes",
       title = "Nitrogen and hybrid onion varieties", who = "Yeshiwas et al.", year = 2024, journal = "PLOS ONE",
       tally_note = "Yes (means, CV 20.77 %, LSD 2.25)", finding = "The pooled model hides significant location × variety and location × N interactions."),
  list(num = "08", folder = "08_2k_Laser_Kerf_PTFE_PCB_Pellei2026_Micromachines", design = "twok", field = "Manufacturing & electronics", tally = "yes",
       title = "CO₂ laser kerf depth in PTFE circuit boards", who = "Pellei et al.", year = 2026, journal = "Micromachines",
       tally_note = "Yes (SS and % contributions)", finding = "Interactions are negligible on the log scale; the tables carry copy errors."),
  list(num = "09", folder = "09_2k_ALD_Al2O3_Growth_Pan2025_Materials", design = "twok", field = "Manufacturing & electronics", tally = "yes",
       title = "Growth rate in Al₂O₃ atomic layer deposition", who = "Pan & Lei", year = 2025, journal = "Materials",
       tally_note = "Yes (to rounding)", finding = "Lack of fit F = 82: against the pure error every effect is significant."),
  list(num = "10", folder = "10_OneSample_Groundwater_pH_Uddin2023_Heliyon", design = "one", field = "Environment", tally = "yes",
       title = "Groundwater pH against the 8.5 guideline limit", who = "Uddin et al.", year = 2023, journal = "Heliyon",
       tally_note = "Yes, exactly (t = −9.52, df = 17)", finding = "The mean pH, 7.67, is significantly below the 8.5 limit; the 18 values are 9 sites measured in two seasons."),
  list(num = "11", folder = "11_kSample_Bond_Strength_Aging_Adawi2024_MedSciMonit", design = "crd", field = "Health & biology", tally = "partly",
       title = "Artificial aging and the bond strength of a resin composite", who = "Adawi et al.", year = 2024, journal = "Medical Science Monitor",
       tally_note = "Table 2's means, SDs and N give F = 9.57; the printed F = 13.55 does not follow", finding = "Highly significant either way. The F in Table 2 disagrees with its own SDs, while the Tukey table agrees with the F (data rebuilt from means and SDs)."),
  list(num = "12", folder = "12_CRD_Broiler_Banana_Peel_Chacrabati2013_BJAS", design = "crd", field = "Agriculture", tally = "yes",
       title = "Banana peel meal with probiotics in broiler diets", who = "Chacrabati et al.", year = 2013, journal = "Bangladesh Journal of Animal Science",
       tally_note = "Yes: everything printed agrees (means, SE, ** significance); no F is printed. Data rebuilt from the means and SEs", finding = "Only the plantain 0.5 % diet differs: about 170 to 200 g lighter birds (data rebuilt from means and SEs)."),
  list(num = "13", folder = "13_RCBD_Vetch_Oat_Mixtures_Yadeta2024_JSciAgric", design = "rcbd", field = "Agriculture", tally = "yes",
       title = "Seed yield of vetch and oat variety mixtures", who = "Yadeta", year = 2024, journal = "Journal of Scientific Agriculture",
       tally_note = "Yes: means, grand mean, CV 18.68 % and P agree; no F is printed. Data rebuilt, block effect assumed", finding = "Sole Bonsa oats and the Bonsa + vetch mixture yield most; the block structure is an assumption (data rebuilt from Table 5)."),
  list(num = "14", folder = "14_LatinSquare_Diffusing_Factor_Bacharach1940_BiochemJ", design = "lsd", field = "Health & biology", tally = "yes",
       title = "Bioassay of the testicular diffusing factor", who = "Bacharach et al.", year = 1940, journal = "Biochemical Journal",
       tally_note = "Yes, all three F ratios (0.172, 1.168, 3.910)", finding = "Order of injection has no effect; rabbits differ (F = 3.91, p = 0.012). The paper's own raw data."),
  list(num = "15", folder = "15_Factorial_Fruit_Shelf_Life_ChanAsrah2026_EKST", design = "fact", field = "Agriculture", tally = "yes",
       title = "Preservative, moisture control and storage on fruit shelf life", who = "Chan & Asrah", year = 2026, journal = "EKST",
       tally_note = "Yes (total SS 2522.1 and the ANOVA table)", finding = "Preservative and storage dominate; Preservative × Storage and Moisture × Storage interact. The paper's own raw data."))

# ---------- Reading a paper's folder ----------

# Inline Markdown of the READMEs (bold, italics, code, links) as safe HTML.
paper_md_inline <- function(x) {
  x <- htmltools::htmlEscape(x)
  x <- gsub("`([^`]+)`", "<code>\\1</code>", x)
  x <- gsub("\\*\\*([^*]+)\\*\\*", "<b>\\1</b>", x)
  x <- gsub("(^|[^*])\\*([^* ][^*]*)\\*", "\\1<i>\\2</i>", x)
  x <- gsub("(https?://[^ )<]+[^ )<.,;])", "<a href=\"\\1\" target=\"_blank\" rel=\"noopener\">\\1</a>", x)
  htmltools::HTML(x)
}

paper_readme <- function(folder) {
  f <- file.path(PAPERS_DIR, folder, "README.md")
  x <- if (file.exists(f)) readLines(f, warn = FALSE, encoding = "UTF-8") else character(0)
  lead <- function(lab) { l <- grep(sprintf("^\\*\\*%s\\.\\*\\*", lab), x, value = TRUE)[1]; if (is.na(l)) "" else trimws(sub(sprintf("^\\*\\*%s\\.\\*\\*", lab), "", l)) }
  section <- function(h) {
    i <- grep(paste0("^## ", h), x)[1]; if (is.na(i)) return(character(0))
    j <- grep("^## ", x); j <- j[j > i][1]; if (is.na(j)) j <- length(x) + 1
    if (j - 1 < i + 1) character(0) else x[(i + 1):(j - 1)]
  }
  first_par <- function(l) { l <- trimws(l); s <- which(nzchar(l))[1]; if (is.na(s)) return(""); e <- which(!nzchar(l) & seq_along(l) > s)[1]; paste(l[s:(if (is.na(e)) length(l) else e - 1)], collapse = " ") }
  cite <- lead("Paper")
  tl <- section("Do the values tally\\?"); tl <- trimws(tl[grepl("^\\s*\\|", tl)])
  tally <- NULL
  if (length(tl) >= 3) {
    cells <- lapply(tl[-2], function(r) trimws(strsplit(sub("^\\|", "", sub("\\|$", "", r)), "|", fixed = TRUE)[[1]]))
    tally <- as.data.frame(do.call(rbind, cells[-1]), stringsAsFactors = FALSE); names(tally) <- cells[[1]]
  }
  ins <- section("Insights"); ins <- sub("^-\\s*", "", trimws(ins[grepl("^\\s*-\\s", ins)]))
  list(cite = cite, doi = regmatches(cite, regexpr("https://doi.org/[^ ]+[^ .]", cite)),
       licence = sub("\\s*Paper:.*$", "", lead("Licence")), design = lead("Design"),
       did = first_par(section("What the paper did")), how = first_par(section("How it was replicated")),
       tally = tally, insights = ins)
}

# The open-access link given in a paper's link file (for the papers whose PDF is too large to include).
paper_link <- function(folder) {
  f <- file.path(PAPERS_DIR, folder, "paper", "PAPER_LINK.txt"); if (!file.exists(f)) return(NULL)
  l <- readLines(f, warn = FALSE, encoding = "UTF-8"); h <- grep("^Full text", l, value = TRUE)[1]
  if (is.na(h)) NULL else regmatches(h, regexpr("https?://[^ ]+", h))
}

# Every file of a paper, by sub-folder, with the address the browser downloads it from.
paper_files <- function(folder) {
  root <- file.path(PAPERS_DIR, folder)
  fs <- list.files(root, recursive = TRUE)
  if (!length(fs)) return(data.frame(path = character(0), dir = character(0), name = character(0), size = numeric(0), url = character(0)))
  data.frame(path = fs, dir = ifelse(dirname(fs) == ".", "", dirname(fs)), name = basename(fs), size = file.size(file.path(root, fs)),
             url = paste0("paperfiles/", folder, "/", vapply(strsplit(fs, "/", fixed = TRUE), function(p) paste(utils::URLencode(p, reserved = TRUE), collapse = "/"), "")),
             stringsAsFactors = FALSE)
}

paper_size <- function(b) ifelse(b >= 1024^2, sprintf("%.1f MB", b / 1024^2), ifelse(b >= 1024, sprintf("%.0f KB", b / 1024), sprintf("%d B", as.integer(b))))

# A saved analysis as the problem it describes (the same construction as opening a shared analysis).
paper_problem <- function(x) {
  mt <- x$meta; df <- x$data; roles <- as.list(mt$roles)
  for (n in names(df)) if (!identical(n, roles$response)) df[[n]] <- as.character(df[[n]])
  m <- list(id = "custom", chapter = 0, num = "Custom", title = mt$title %||% "", statement = as.character(mt$statement), parts = as.character(mt$parts %||% character(0)),
            data = df, display = df, roles = roles, labels = as.list(mt$labels), settings = as.list(mt$settings), target = mt$target %||% "mean",
            source = "Paper replication", control = mt$control)
  if (!is.null(mt$random_blocks)) m$random_blocks <- isTRUE(mt$random_blocks)
  if (!is.null(mt$random_wp)) m$random_wp <- isTRUE(mt$random_wp)
  if (!is.null(mt$design)) m$design <- mt$design
  m
}

paper_analyses <- function(folder) {
  fs <- sort(list.files(file.path(PAPERS_DIR, folder, "analysis"), "\\.sdsa$", full.names = TRUE))
  out <- lapply(fs, function(f) {
    x <- tryCatch(share_read_file(f), error = function(e) NULL); if (is.null(x)) return(NULL)
    x$file <- f; x$key <- sub("\\.sdsa$", "", basename(f)); x
  })
  Filter(Negate(is.null), out)
}

# What each column of an analysis does, in the words of its page.
paper_roles <- function(x) {
  r <- as.list(x$meta$roles); md <- PS_MODES[[x$page]]
  lab <- c(response = "Response", treatment = "Treatment", factorB = "Factor B", factorC = "Factor C", factorD = "Factor D", factorE = "Factor E",
           block = "Block", row = "Row", column = "Column", greek = "Greek letter")
  keep <- names(r)[vapply(r, function(v) length(v) == 1 && !is.na(v) && nzchar(v), TRUE)]
  keep <- intersect(names(lab), keep)
  data.frame(Role = vapply(keep, function(k) sub(" \\(.*$", "", md$labels[[k]] %||% lab[[k]]), ""), Column = unname(unlist(r[keep])), stringsAsFactors = FALSE, row.names = NULL)
}

# The papers, read once (the registry is small: fifteen folders).
papers_all <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    ok <- vapply(PAPER_META, function(p) dir.exists(file.path(PAPERS_DIR, p$folder)), TRUE)
    cache <<- stats::setNames(lapply(PAPER_META[ok], function(p) {
      p$readme <- paper_readme(p$folder); p$link <- paper_link(p$folder)
      p$analyses <- paper_analyses(p$folder); p$files <- paper_files(p$folder)
      p$pdf <- p$files$url[p$files$dir == "paper" & grepl("\\.pdf$", p$files$name, ignore.case = TRUE)][1]
      p$cite_short <- sprintf("%s (%d)", p$who, p$year)
      p
    }), vapply(PAPER_META[ok], `[[`, "", "num"))
    cache
  }
})

# Re-run a saved analysis on this version of the app: the ANOVA, the assumption verdict and whether the numbers are
# identical to those saved in the file. Kept per file, because the result never changes while the app runs.
paper_check <- local({
  memo <- new.env()
  function(x) {
    k <- x$file
    if (!is.null(memo[[k]])) return(memo[[k]])
    out <- tryCatch({
      m <- paper_problem(x); s <- Filter(Negate(is.null), as.list(x$state$s)); test <- x$state$test %||% "anova"
      st <- ps_structure(m$data, m$roles); kind <- ps_kind(st, m)
      res <- ps_run_test(kind, test, st, m, s)
      now <- ps_share_summary(m, s, test)
      list(ok = TRUE, kind = kind, res = res, now = now, same = share_same(now, x$summary), n = nrow(drop_empty_rows(m$data)))
    }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    memo[[k]] <- out
    out
  }
})
