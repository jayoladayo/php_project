# Builds the replication packs of papers 10 to 15 (the confirmation papers) in papers/:
# data (labelled CSV), the saved analyses (.sdsa, made with the app's own engine), the app's text output,
# the independent base-R check and its printed output, a link file where a DOI is known, and the README.
# Run from the app folder after putting the raw confirmation files in $PAPER_RAW (default ../confirmation_raw):
#   PAPER_RAW=/path/to/raw Rscript tools/build_papers_10_15.R
suppressMessages({ library(shiny); library(bslib); library(ggplot2) })
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, local = TRUE)
RAW <- Sys.getenv("PAPER_RAW", "../confirmation_raw")
rd <- function(f, ...) utils::read.csv(file.path(RAW, f), na.strings = character(0), stringsAsFactors = FALSE, check.names = FALSE, ...)
wr <- function(path, lines) { dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); writeLines(lines, path, useBytes = TRUE) }
wcsv <- function(df, path) { dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); utils::write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8") }
SAVED <- "2026-11-22 12:00"

# ---------- the data, with full column names ----------
d10 <- rd("onesample_ttest_uddin2023.csv"); d10 <- data.frame(Site = d10$Site, Season = d10$Season, `Groundwater pH` = d10$pH, check.names = FALSE)
a11 <- rd("ksample_anova_adawi2024.csv"); s11 <- rd("ksample_synthetic_adawi2024.csv")
g11 <- c(TC_control = "TC (control)", T1_1month = "T1 (1 month)", T3_3months = "T3 (3 months)", T6_6months = "T6 (6 months)", T12_12months = "T12 (12 months)")
d11 <- data.frame(`Aging group` = unname(g11[s11$Group]), `Shear bond strength (MPa)` = s11$BondStrength_MPa, check.names = FALSE)
a11 <- data.frame(`Aging group` = unname(g11[a11$Group]), n = a11$N, `Mean (MPa)` = a11$Mean, `SD (MPa)` = a11$SD, check.names = FALSE)
a12 <- rd("crd_chacrabati2013.csv"); s12 <- utils::read.delim(file.path(RAW, "crd_synthetic_chacrabati2013.csv"), stringsAsFactors = FALSE)
g12 <- c(Control = "Control", Plantain0.5pct = "Plantain 0.5 %", Bio_Sel_E = "Bio-Sel E", Commercial = "Commercial")
d12 <- data.frame(Treatment = unname(g12[s12$Treatment]), `Live weight (g)` = s12$LiveWeight_g, check.names = FALSE)
a12 <- data.frame(Treatment = unname(g12[a12$Treatment]), n = a12$N, `Mean live weight (g)` = a12$Mean_LiveWeight_g, SE = a12$SE, `Significance letter` = a12$SigLetter, check.names = FALSE)
a13 <- rd("rcbd_yadeta2024.csv"); s13 <- rd("rcbd_synthetic_yadeta2024.csv")
g13 <- c(T1_OatBonabas = "T1 Oat Bona-bas (sole)", T2_OatBonsa = "T2 Oat Bonsa (sole)", T3_VetchGebisa = "T3 Vetch Gebisa (sole)", T4_Bonsa_Gebisa_mix = "T4 Vetch Gebisa + Oat Bonsa", T5_Bonabas_Gebisa_mix = "T5 Vetch Gebisa + Oat Bona-bas")
d13 <- data.frame(Treatment = unname(g13[s13$Treatment]), Block = s13$Block, `Seed yield (t/ha)` = s13$SeedYield_t_ha, check.names = FALSE)
a13 <- data.frame(Treatment = paste(a13$Treatment, a13$Description, sep = " "), `Seed yield (t/ha)` = a13$SeedYield_t_ha, `Significance letter` = a13$SigLetter, check.names = FALSE)
b14 <- rd("latin_square_bacharach1940.csv")
d14 <- data.frame(Position = b14$Position, Rabbit = b14$Rabbit, `Order of injection` = b14$Order, `Blister area (cm2)` = b14$BlisterArea_cm2, check.names = FALSE)
k15 <- rd("kfactorial_chan_asrah2026.csv")
d15 <- data.frame(Preservative = ifelse(k15$Preservative == "NoPreservative", "No preservative", k15$Preservative), Replicate = k15$Rep,
                  `Moisture control` = k15$Moisture, `Storage condition` = k15$Storage, `Shelf life (days)` = k15$Days, check.names = FALSE)

# ---------- the saved analyses ----------
sdsa <- function(path, page, from, title, statement, roles, labels, test, note, data, settings = list(), ph = "tukey", target = "mean") {
  full <- list(response = "", treatment = "", factorB = "", factorC = "", factorD = "", factorE = "", block = "", row = "", column = "", greek = "")
  roles <- modifyList(full, roles)
  m <- list(id = "custom", chapter = 0, num = "Custom", title = title, statement = statement, parts = character(0), data = data, display = data, roles = roles,
            labels = labels, settings = modifyList(list(alpha = 0.05, alt = "two.sided"), settings), target = target, source = "Paper replication", control = NULL)
  st <- ps_structure(m$data, m$roles); kind <- ps_kind(st, m); stopifnot(!is.na(kind))
  s <- modifyList(ps_default_settings(m, kind), list(posthoc = ph)); s <- Filter(Negate(is.null), s)
  hyp <- function(fam) if (ps_is_fact(kind)) lapply(ps_fact_hypotheses(st, m, fam), function(h) list(h0 = h$h0, h1 = h$h1)) else ps_hypotheses(kind, st, m, s, fam)
  h <- if (ps_is_fact(kind)) NULL else hyp("param"); hn <- if (ps_is_fact(kind)) NULL else hyp("nonpar")
  sv <- list(s = s, h0 = h$h0, h1 = h$h1, hs0 = h$h0, hs1 = h$h1, h0n = hn$h0, h1n = hn$h1, hs0n = hn$h0, hs1n = hn$h1,
             fh = if (ps_is_fact(kind)) hyp("param"), fhn = if (ps_is_fact(kind)) hyp("nonpar"), test = test, test_user = FALSE, ph = ph, step = 5, seen = 5)
  sm <- ps_share_summary(m, s, test)
  meta <- m[c("title", "statement", "parts", "roles", "labels", "settings", "target", "control", "random_blocks", "design", "random_wp")]
  meta$random_wp <- FALSE; meta$random_blocks <- NULL; meta$design <- NULL
  x <- list(app = "StatDesign Studio", kind = "analysis", version = 1, saved = SAVED, from = from, note = note, page = page, problem_id = "custom", title = title, edited = FALSE,
            meta = meta, data = data, state = sv, summary = sm)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA, dataframe = "columns", pretty = TRUE)), path, useBytes = TRUE)
  # the app's text output: the ANOVA (or test) and the three assumption checks
  res <- ps_run_test(kind, test, st, m, s); out <- c(sprintf("StatDesign Studio output - %s", title), sprintf("Layout: %s. Test: %s. alpha = %s", PS_KIND_LABEL[[kind]], res$t$name, format(s$alpha)), "")
  if (!is.null(res$anova_df)) out <- c(out, "ANOVA table", utils::capture.output(print(res$anova_df, row.names = FALSE)), "")
  else out <- c(out, utils::capture.output(print(ps_test_table(res), row.names = FALSE)), "")
  if (ps_multi(kind)) { a <- ps_assumptions(kind, st, s, test)
    out <- c(out, "Assumption checks on the residuals", utils::capture.output(print(a$normality, row.names = FALSE)), utils::capture.output(print(a$variance, row.names = FALSE)), utils::capture.output(print(a$independence, row.names = FALSE)),
             sprintf("All assumptions hold: %s%s", a$all_ok, if (!a$all_ok) paste0(" (not satisfied: ", paste(a$failed, collapse = ", "), ")") else "")) }
  list(file = path, out = out, summary = sm, res = res, kind = kind)
}
save_out <- function(r, path) wr(path, r$out)

P <- "papers"
dirs <- c(p10 = "10_OneSample_Groundwater_pH_Uddin2023_Heliyon", p11 = "11_kSample_Bond_Strength_Aging_Adawi2024_MedSciMonit", p12 = "12_CRD_Broiler_Banana_Peel_Chacrabati2013_BJAS",
          p13 = "13_RCBD_Vetch_Oat_Mixtures_Yadeta2024_JSciAgric", p14 = "14_LatinSquare_Diffusing_Factor_Bacharach1940_BiochemJ", p15 = "15_Factorial_Fruit_Shelf_Life_ChanAsrah2026_EKST")
for (d in dirs) unlink(file.path(P, d), recursive = TRUE)

# 10 Uddin: one sample
wcsv(d10, file.path(P, dirs[["p10"]], "data", "Uddin2023_groundwater_pH_9_sites_2_seasons.csv"))
r10 <- sdsa(file.path(P, dirs[["p10"]], "analysis", "Uddin2023_groundwater_pH_one_sample_t.sdsa"), "one", "Uddin2023_groundwater_pH_one_sample_t",
  "Uddin et al. (2023) – groundwater pH against the 8.5 guideline limit, one-sample t-test",
  "Groundwater pH was measured at 9 sites (GW1 to GW9) in the dry and in the wet season, 18 values in all. The upper guideline limit for pH (WHO and Bangladesh standards) is 8.5. Is the mean pH of the groundwater different from 8.5? Test at α = 0.05. Source: Heliyon 9:e19668.",
  list(response = "Groundwater pH"), list(response = "groundwater pH", unit = ""), "t",
  "Replication of the paper's one-sample t-test: t = −9.52, df = 17. The app gives t = −9.5167, p < 0.0001, mean 7.666, 95 % CI [7.481, 7.851]. H0 is rejected: the mean pH is below 8.5.",
  d10, settings = list(mu0 = 8.5, alt = "two.sided"))
save_out(r10, file.path(P, dirs[["p10"]], "analysis", "StatDesign_app_output_groundwater_pH.txt"))

# 11 Adawi: k samples (data rebuilt from the published means and SDs)
wcsv(d11, file.path(P, dirs[["p11"]], "data", "Adawi2024_bond_strength_5_aging_groups_reconstructed.csv"))
wcsv(a11, file.path(P, dirs[["p11"]], "data", "source_supplement", "Adawi2024_Table2_published_means_SD.csv"))
r11 <- sdsa(file.path(P, dirs[["p11"]], "analysis", "Adawi2024_bond_strength_k_samples.sdsa"), "k", "Adawi2024_bond_strength_k_samples",
  "Adawi et al. (2024) – shear bond strength after thermocycling aging, 5 groups (data rebuilt from Table 2)",
  "Shear bond strength (MPa) of a veneer ceramic bonded to direct resin nano-hybrid composite was measured in 5 groups of 16 specimens: a control (TC) and four groups aged by thermocycling for 1, 3, 6 and 12 months. The paper prints only each group's mean and SD, so the 16 values of each group were rebuilt to have exactly that mean and SD. Does the mean bond strength differ between the groups? Test at α = 0.05. Source: Medical Science Monitor 30:e945243, Table 2.",
  list(response = "Shear bond strength (MPa)", treatment = "Aging group"), list(response = "shear bond strength", unit = "MPa", treatment = "aging group"), "anova",
  "The data are rebuilt from the published means and SDs. From Table 2 the app (and an independent calculation) gives F = 9.571 on 4 and 75 df; the F = 13.55 printed in the paper's text does not follow from its own Table 2. Both are highly significant.",
  d11)
save_out(r11, file.path(P, dirs[["p11"]], "analysis", "StatDesign_app_output_bond_strength.txt"))

# 12 Chacrabati: CRD (data rebuilt from means and SE)
wcsv(d12, file.path(P, dirs[["p12"]], "data", "Chacrabati2013_broiler_live_weight_4_treatments_reconstructed.csv"))
wcsv(a12, file.path(P, dirs[["p12"]], "data", "source_supplement", "Chacrabati2013_published_means_SE.csv"))
r12 <- sdsa(file.path(P, dirs[["p12"]], "analysis", "Chacrabati2013_broiler_live_weight_CRD.sdsa"), "crd", "Chacrabati2013_broiler_live_weight_CRD",
  "Chacrabati et al. (2013) – broiler live weight, completely randomized design with 4 diets (data rebuilt from means and SE)",
  "Four diets (control, banana peel meal at 0.5 %, Bio-Sel E and a commercial additive) were each given to 4 replicate groups of broilers in a completely randomized design, and the live weight (g) was recorded. The paper prints only each diet's mean and standard error (with significance letters), so the four values of each diet were rebuilt to have exactly that mean and SD = SE × √4. Does the mean live weight differ between the diets? Test at α = 0.05. Source: Bangladesh Journal of Animal Science (2013).",
  list(response = "Live weight (g)", treatment = "Treatment"), list(response = "live weight", unit = "g", treatment = "diet"), "anova",
  "The data are rebuilt from the published means and SEs. The app gives F = 30.351 on 3 and 12 df, p < 0.0001 (η² = 0.884), equal to the independent calculation (30.3512) and consistent with the paper's '**' (p < 0.01). The paper prints no F value to compare with.",
  d12)
save_out(r12, file.path(P, dirs[["p12"]], "analysis", "StatDesign_app_output_live_weight.txt"))

# 13 Yadeta: RCBD (data rebuilt from treatment means and CV)
wcsv(d13, file.path(P, dirs[["p13"]], "data", "Yadeta2024_seed_yield_5_treatments_5_blocks_reconstructed.csv"))
wcsv(a13, file.path(P, dirs[["p13"]], "data", "source_supplement", "Yadeta2024_Table5_published_means.csv"))
r13 <- sdsa(file.path(P, dirs[["p13"]], "analysis", "Yadeta2024_seed_yield_RCBD.sdsa"), "rcbd", "Yadeta2024_seed_yield_RCBD",
  "Yadeta (2024) – seed yield of vetch and oat mixtures, RCBD with 5 treatments in 5 blocks (data rebuilt from Table 5)",
  "Five treatments (sole Oat Bona-bas, sole Oat Bonsa, sole Vetch Gebisa, and two vetch-oat mixtures) were grown in a randomized complete block design with 5 replications, and the seed yield (t/ha) was measured. The paper prints only the treatment means, the grand mean (1.8) and CV = 18.68 %, so the 25 yields were rebuilt to reproduce those exactly, with no block effect (no block values were published). Does the mean seed yield differ between the treatments? Test at α = 0.05. Source: Journal of Scientific Agriculture 8:51-58, Table 5.",
  list(response = "Seed yield (t/ha)", treatment = "Treatment", block = "Block"), list(response = "seed yield", unit = "t/ha", treatment = "treatment", block = "block"), "anova",
  "The 25 values are a reconstruction: treatment means and CV (18.68 %) are exact, the block structure is an assumption (no block effect). The app gives F = 34.718 on 4 and 16 df, p < 0.0001, equal to the independent calculation (34.7176); the paper prints p = 0.001 and no F.",
  d13)
save_out(r13, file.path(P, dirs[["p13"]], "analysis", "StatDesign_app_output_seed_yield.txt"))

# 14 Bacharach: Latin square (the paper's own raw data)
wcsv(d14, file.path(P, dirs[["p14"]], "data", "Bacharach1940_blister_area_6x6_latin_square.csv"))
r14 <- sdsa(file.path(P, dirs[["p14"]], "analysis", "Bacharach1940_blister_area_latin_square.sdsa"), "lsd", "Bacharach1940_blister_area_latin_square",
  "Bacharach, Chance & Middleton (1940) – blister area in a 6 × 6 Latin square (6 rabbits, 6 positions, 6 injection orders)",
  "In the bioassay of the testicular diffusing factor, six injections were made into each of six rabbits, one injection at each of six skin positions (rows a to f), and the blister area (cm²) was measured. The order of injection (i to vi) is the treatment, rabbits are the columns and positions the rows of a 6 × 6 Latin square. Test the order of injection, the positions and the rabbits at α = 0.05. Source: Biochemical Journal 34:1464-1471.",
  list(response = "Blister area (cm2)", treatment = "Order of injection", row = "Position", column = "Rabbit"), list(response = "blister area", unit = "cm²", treatment = "order of injection", row = "position", column = "rabbit"), "anova",
  "The paper's own raw data. All three F ratios of the paper are reproduced: order F = 0.172 (p = 0.970), positions F = 1.168 (p = 0.359), rabbits F = 3.910 (p = 0.012).",
  d14)
save_out(r14, file.path(P, dirs[["p14"]], "analysis", "StatDesign_app_output_blister_area.txt"))

# 15 Chan & Asrah: 3 x 3 x 3 factorial (the paper's own raw data)
wcsv(d15, file.path(P, dirs[["p15"]], "data", "ChanAsrah2026_shelf_life_3x3x3_factorial.csv"))
r15 <- sdsa(file.path(P, dirs[["p15"]], "analysis", "ChanAsrah2026_shelf_life_factorial.sdsa"), "fact", "ChanAsrah2026_shelf_life_factorial",
  "Chan & Asrah (2026) – fruit shelf life, 3 × 3 × 3 factorial in a CRD with 3 replicates",
  "The shelf life (days) of fruit was recorded for 3 preservatives (natural, chemical, none) × 3 moisture-control schedules (no sprinkling, every 3 days, every 5 days) × 3 storage conditions (open, partial, full), with 3 replicates in each of the 27 combinations (81 observations). Test the three main effects and all the interactions at α = 0.05. Source: EKST (2026), the paper's data table.",
  list(response = "Shelf life (days)", treatment = "Preservative", factorB = "Moisture control", factorC = "Storage condition"),
  list(response = "shelf life", unit = "days", treatment = "preservative", factorB = "moisture control", factorC = "storage condition"), "anova",
  "The paper's own raw data. Total SS = 2522.1 and the whole ANOVA table agree with the paper's. The three main effects, Preservative × Storage and Moisture × Storage are significant; Preservative × Moisture and the three-factor interaction are not.",
  d15)
save_out(r15, file.path(P, dirs[["p15"]], "analysis", "StatDesign_app_output_shelf_life.txt"))

# ---------- independent checks in base R, with their printed output ----------
checks <- list(
 p10 = c("# Independent check of paper 10 (Uddin et al. 2023): one-sample t-test of the 18 groundwater pH values against 8.5. Base R only.",
         "d <- read.csv('../data/Uddin2023_groundwater_pH_9_sites_2_seasons.csv', check.names = FALSE)",
         "x <- d[['Groundwater pH']]; cat('n =', length(x), ' mean =', round(mean(x), 4), ' sd =', round(sd(x), 4), '\\n')",
         "tt <- t.test(x, mu = 8.5); print(tt)",
         "cat(sprintf('t = %.4f, df = %d, p = %.3g (paper: t = -9.52, df = 17, p < 0.05)\\n', tt$statistic, tt$parameter, tt$p.value))"),
 p11 = c("# Independent check of paper 11 (Adawi et al. 2024): one-way ANOVA from the published Table 2 (means, SDs, n = 16), and from the rebuilt data. Base R only.",
         "s <- read.csv('../data/source_supplement/Adawi2024_Table2_published_means_SD.csv', check.names = FALSE)",
         "n <- s$n; m <- s[['Mean (MPa)']]; sd <- s[['SD (MPa)']]; k <- length(n); N <- sum(n); gm <- sum(n * m) / N",
         "ssb <- sum(n * (m - gm)^2); sse <- sum((n - 1) * sd^2); F <- (ssb / (k - 1)) / (sse / (N - k))",
         "cat(sprintf('From Table 2: SS between = %.4f, SS error = %.4f, F(%d, %d) = %.4f, p = %.3g, eta^2 = %.4f\\n', ssb, sse, k - 1, N - k, F, pf(F, k - 1, N - k, lower.tail = FALSE), ssb / (ssb + sse)))",
         "cat('The paper prints F = 13.55 (df 4), p = 0.00001 in its text; it does not follow from Table 2.\\n\\n')",
         "d <- read.csv('../data/Adawi2024_bond_strength_5_aging_groups_reconstructed.csv', check.names = FALSE)",
         "d[['Aging group']] <- factor(d[['Aging group']], levels = unique(d[['Aging group']]))",
         "print(aggregate(`Shear bond strength (MPa)` ~ `Aging group`, d, function(v) c(n = length(v), mean = mean(v), sd = sd(v))))",
         "print(summary(aov(`Shear bond strength (MPa)` ~ `Aging group`, d)))"),
 p12 = c("# Independent check of paper 12 (Chacrabati et al. 2013): one-way ANOVA of the rebuilt live weights, and the same F from the published means and SEs. Base R only.",
         "s <- read.csv('../data/source_supplement/Chacrabati2013_published_means_SE.csv', check.names = FALSE)",
         "n <- s$n; m <- s[['Mean live weight (g)']]; sd <- s$SE * sqrt(n); k <- length(n); N <- sum(n); gm <- sum(n * m) / N",
         "ssb <- sum(n * (m - gm)^2); sse <- sum((n - 1) * sd^2); F <- (ssb / (k - 1)) / (sse / (N - k))",
         "cat(sprintf('From the published means and SE: F(%d, %d) = %.4f, p = %.3g\\n\\n', k - 1, N - k, F, pf(F, k - 1, N - k, lower.tail = FALSE)))",
         "d <- read.csv('../data/Chacrabati2013_broiler_live_weight_4_treatments_reconstructed.csv', check.names = FALSE)",
         "d$Treatment <- factor(d$Treatment, levels = unique(d$Treatment))",
         "print(aggregate(`Live weight (g)` ~ Treatment, d, mean)); print(summary(aov(`Live weight (g)` ~ Treatment, d)))"),
 p13 = c("# Independent check of paper 13 (Yadeta 2024): RCBD ANOVA of the rebuilt seed yields, and the CV against the printed 18.68 %. Base R only.",
         "d <- read.csv('../data/Yadeta2024_seed_yield_5_treatments_5_blocks_reconstructed.csv', check.names = FALSE)",
         "d$Treatment <- factor(d$Treatment, levels = unique(d$Treatment)); d$Block <- factor(d$Block)",
         "y <- d[['Seed yield (t/ha)']]; print(aggregate(y ~ Treatment, d, mean)); cat('grand mean =', round(mean(y), 4), '\\n')",
         "fit <- aov(y ~ Treatment + Block, d); print(summary(fit)); a <- anova(fit)",
         "cat(sprintf('CV = %.4f %% (paper: 18.68 %%)\\n', 100 * sqrt(a['Residuals', 'Mean Sq']) / mean(y)))"),
 p14 = c("# Independent check of paper 14 (Bacharach, Chance & Middleton 1940): the 6 x 6 Latin square ANOVA. Base R only.",
         "d <- read.csv('../data/Bacharach1940_blister_area_6x6_latin_square.csv', check.names = FALSE)",
         "for (v in c('Position', 'Rabbit', 'Order of injection')) d[[v]] <- factor(d[[v]])",
         "print(table(d[['Order of injection']], d$Position))   # every order once in every position",
         "print(summary(aov(`Blister area (cm2)` ~ `Order of injection` + Position + Rabbit, d)))",
         "cat('Paper: F(order) = 0.172, F(positions) = 1.168, F(rabbits) = 3.910\\n')"),
 p15 = c("# Independent check of paper 15 (Chan & Asrah 2026): 3 x 3 x 3 factorial ANOVA. Base R only.",
         "d <- read.csv('../data/ChanAsrah2026_shelf_life_3x3x3_factorial.csv', check.names = FALSE, na.strings = character(0))   # na.strings: a level called None must not become NA",
         "for (v in c('Preservative', 'Moisture control', 'Storage condition')) d[[v]] <- factor(d[[v]])",
         "print(table(d$Preservative)); fit <- aov(`Shelf life (days)` ~ Preservative * `Moisture control` * `Storage condition`, d)",
         "a <- anova(fit); print(a); cat('Total SS =', round(sum(a[['Sum Sq']]), 3), '(paper: 2522.10)\\n')"))
names(checks) <- names(dirs)
vname <- c(p10 = "Uddin2023_verify_one_sample", p11 = "Adawi2024_verify_k_samples", p12 = "Chacrabati2013_verify_CRD", p13 = "Yadeta2024_verify_RCBD", p14 = "Bacharach1940_verify_latin_square", p15 = "ChanAsrah2026_verify_factorial")
for (k in names(dirs)) {
  sc <- file.path(P, dirs[[k]], "scripts", paste0(vname[[k]], ".R")); wr(sc, checks[[k]])
  out <- system2("Rscript", c("-e", shQuote(sprintf("setwd('%s'); source('%s', echo = FALSE, print.eval = TRUE)", dirname(sc), basename(sc)))), stdout = TRUE, stderr = TRUE)
  wr(file.path(P, dirs[[k]], "analysis", paste0(vname[[k]], "_output.txt")), out)
}

# ---------- the project file with every dataset ----------
mk_ds <- function(name, df) list(id = paste0("d", sprintf("%.0f", runif(1, 1e13, 9e13))), name = name, columns = names(df),
  data = lapply(df, function(v) as.character(v)), types = vapply(df, function(v) if (is.numeric(v)) "number" else "category", ""), source = "Pasted", sheet = NULL, file = NULL, notes = list(), ignored = list())
old <- jsonlite::fromJSON(file.path(P, "StatDesign_9_papers_all_datasets.sdproj"), simplifyVector = FALSE)
new <- list(mk_ds("10 Uddin et al. (2023) – groundwater pH, 9 sites × 2 seasons", d10), mk_ds("11 Adawi et al. (2024) – bond strength, 5 aging groups (rebuilt from Table 2)", d11),
            mk_ds("12 Chacrabati et al. (2013) – broiler live weight, CRD (rebuilt from means and SE)", d12), mk_ds("13 Yadeta (2024) – vetch and oats, RCBD (rebuilt from Table 5)", d13),
            mk_ds("14 Bacharach et al. (1940) – blister area, 6×6 Latin square", d14), mk_ds("15 Chan & Asrah (2026) – fruit shelf life, 3×3×3 factorial", d15))
old$datasets <- c(old$datasets, new); old$saved <- SAVED
writeLines(as.character(jsonlite::toJSON(old, auto_unbox = TRUE, null = "null", na = "null", digits = NA)), file.path(P, "StatDesign_15_papers_all_datasets.sdproj"), useBytes = TRUE)
unlink(file.path(P, "StatDesign_9_papers_all_datasets.sdproj"))

saveRDS(lapply(list(p10 = r10, p11 = r11, p12 = r12, p13 = r13, p14 = r14, p15 = r15), function(r) list(summary = r$summary, kind = r$kind)), file.path(Sys.getenv("TMPDIR", tempdir()), "papers_10_15_summaries.rds"))
cat("Built papers 10 to 15.\n"); for (n in c("p10", "p11", "p12", "p13", "p14", "p15")) { r <- get(sub("p", "r", n)); cat(n, ":", r$kind, "| p =", paste(signif(unlist(r$summary$p), 4), collapse = ", "), "| assumptions ok:", r$summary$assumptions_ok, "\n") }
