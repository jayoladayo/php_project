# Problem Sets: exercises copied from the course problem sheets (Montgomery,
# Design and Analysis of Experiments, Sections 2.7, 3.12 and 4.5).
# Each problem keeps its full statement and its complete dataset. Data are stored
# in long format (one row per observation) for analysis, with the textbook layout
# kept for display.

PS_CHAPTERS <- list(
  ch2 = list(key = "ch2", num = 2, label = "Chapter 2 problems", icon = "columns",
             title = "Chapter 2: Simple Comparative Experiments",
             blurb = "One-sample, two-sample (independent and paired) comparisons."),
  ch3 = list(key = "ch3", num = 3, label = "Chapter 3 problems", icon = "layers",
             title = "Chapter 3: Experiments with a Single Factor (ANOVA)",
             blurb = "Single-factor completely randomized designs: one-way ANOVA and the Kruskal-Wallis test."),
  ch4 = list(key = "ch4", num = 4, label = "Chapter 4 problems", icon = "grid",
             title = "Chapter 4: Randomized Blocks, Latin Squares, and Related Designs",
             blurb = "Randomized complete block, Latin square, Graeco-Latin square and balanced incomplete block designs.")
)

# ---------- Builders for the three textbook layouts ----------

# Groups shown as rows (Chapter 3 style): one row per level, observations across.
ps_rows <- function(factor_name, resp_name, groups) {
  lev <- names(groups)
  long <- data.frame(factor = rep(lev, lengths(groups)), Replicate = unlist(lapply(groups, seq_along)),
                     resp = unlist(groups, use.names = FALSE), stringsAsFactors = FALSE)
  names(long) <- c(factor_name, "Replicate", resp_name)
  k <- max(lengths(groups))
  wide <- data.frame(lev, stringsAsFactors = FALSE)
  for (j in seq_len(k)) wide[[paste("Obs", j)]] <- vapply(groups, function(g) if (j <= length(g)) format(g[j]) else "", "")
  names(wide)[1] <- factor_name
  list(long = long, wide = wide)
}

# Groups shown as columns (Chapter 2 style): one column per sample.
ps_cols <- function(factor_name, resp_name, groups) {
  lev <- names(groups)
  long <- data.frame(factor = rep(lev, lengths(groups)), Replicate = unlist(lapply(groups, seq_along)),
                     resp = unlist(groups, use.names = FALSE), stringsAsFactors = FALSE)
  names(long) <- c(factor_name, "Replicate", resp_name)
  k <- max(lengths(groups))
  wide <- as.data.frame(lapply(groups, function(g) c(format(g), rep("", k - length(g)))), stringsAsFactors = FALSE, check.names = FALSE)
  names(wide) <- lev
  list(long = long, wide = wide)
}

# Paired data: one row per pair (block), one column per treatment.
ps_pairs <- function(block_name, blocks, treat_name, levels, resp_name, x1, x2) {
  long <- data.frame(block = rep(blocks, 2), treat = rep(levels, each = length(blocks)), resp = c(x1, x2), stringsAsFactors = FALSE)
  names(long) <- c(block_name, treat_name, resp_name)
  wide <- data.frame(blocks, format(x1), format(x2), stringsAsFactors = FALSE)
  names(wide) <- c(block_name, levels)
  list(long = long, wide = wide)
}

# One sample, shown in the same grid as the book.
ps_single <- function(resp_name, x, ncol, byrow = FALSE) {
  long <- data.frame(Observation = seq_along(x), resp = x)
  names(long)[2] <- resp_name
  m <- matrix("", ceiling(length(x) / ncol), ncol)
  if (byrow) m <- t(m)
  m[seq_along(x)] <- format(x)
  if (byrow) m <- t(m)
  wide <- as.data.frame(m, stringsAsFactors = FALSE)
  names(wide) <- c(resp_name, rep("", ncol - 1))
  list(long = long, wide = wide)
}

# A grid with treatments as rows and blocks as columns (Chapter 4). NA cells are not in the
# design (incomplete blocks); cells listed in `missing` are in the design but were lost.
ps_grid <- function(trt_name, blk_name, resp_name, trt, blk, m, missing = NULL) {
  m <- matrix(m, nrow = length(trt), byrow = TRUE)
  lost <- matrix(FALSE, nrow(m), ncol(m)); for (x in missing) lost[x[1], x[2]] <- TRUE
  idx <- which(!is.na(m) | lost, arr.ind = TRUE); idx <- idx[order(idx[, 1], idx[, 2]), , drop = FALSE]
  long <- data.frame(trt = trt[idx[, 1]], blk = blk[idx[, 2]], resp = ifelse(lost[idx], NA, m[idx]), stringsAsFactors = FALSE)
  names(long) <- c(trt_name, blk_name, resp_name)
  wide <- data.frame(trt, stringsAsFactors = FALSE)
  for (j in seq_along(blk)) wide[[blk[j]]] <- ifelse(lost[, j], "missing", ifelse(is.na(m[, j]), "", format(m[, j])))
  names(wide)[1] <- trt_name
  list(long = long, wide = wide)
}

# A Latin (or Graeco-Latin) square: rows and columns as printed, a letter and a value in each cell.
ps_square <- function(row_name, col_name, trt_name, resp_name, rows, cols, L, V, G = NULL, greek_name = NULL, missing = NULL) {
  p <- length(rows); L <- matrix(L, p, byrow = TRUE); V <- matrix(V, p, byrow = TRUE); if (!is.null(G)) G <- matrix(G, p, byrow = TRUE)
  if (!is.null(missing)) V[missing[1], missing[2]] <- NA
  ij <- expand.grid(j = seq_len(p), i = seq_len(p))[, 2:1]
  long <- data.frame(r = rows[ij$i], c = cols[ij$j], t = L[cbind(ij$i, ij$j)], stringsAsFactors = FALSE)
  if (!is.null(G)) long$g <- G[cbind(ij$i, ij$j)]
  long$y <- V[cbind(ij$i, ij$j)]
  names(long) <- c(row_name, col_name, trt_name, if (!is.null(G)) greek_name, resp_name)
  cell <- matrix(paste0(L, if (!is.null(G)) G else "", " = ", ifelse(is.na(V), "missing", format(V, trim = TRUE))), p)
  wide <- data.frame(rows, cell, stringsAsFactors = FALSE, check.names = FALSE)
  names(wide) <- c(row_name, cols)
  list(long = long, wide = wide)
}

prob <- function(id, chapter, title, statement, parts, data, roles, labels, settings = list(), target = "mean",
                 random = FALSE, tags = character(0), covers = NULL, note = NULL, source = NULL,
                 control = NULL, design = NULL, random_blocks = FALSE, scaled_t = FALSE) {
  list(id = id, chapter = chapter, num = id, title = title, statement = statement, parts = parts,
       data = data$long, display = data$wide, roles = roles, labels = labels,
       settings = modifyList(list(alpha = 0.05, conf = 0.95, alt = "two.sided"), settings),
       target = target, random = random, tags = tags, covers = covers, note = note,
       control = control, design = design, random_blocks = random_blocks, scaled_t = scaled_t,
       source = source %||% sprintf("Problem %s, Montgomery, Design and Analysis of Experiments, Chapter %d.", id, chapter))
}

# ---------- Chapter 2 ----------

PROBLEMS_CH2 <- list(
  prob("2.8", 2, "Is the mean 10? Ten sample values",
       "Consider the following sample data: 9.37, 13.04, 11.69, 8.21, 11.18, 10.41, 13.15, 11.51, 13.21, and 7.75.",
       c("Is it reasonable to assume that this data is a sample from a normal distribution?",
         "Is there evidence to support a claim that the mean of the population is 10?"),
       ps_single("Value", c(9.37, 13.04, 11.69, 8.21, 11.18, 10.41, 13.15, 11.51, 13.21, 7.75), 5, byrow = TRUE),
       roles = list(response = "Value"), labels = list(response = "value"),
       settings = list(mu0 = 10), tags = c("one sample", "normality")),
  prob("2.18", 2, "Fiber breaking strength (σ known)",
       "The breaking strength of a fiber is required to be at least 150 psi. Past experience has indicated that the standard deviation of breaking strength is σ = 3 psi. A random sample of four specimens is tested, and the results are y1 = 145, y2 = 153, y3 = 150, and y4 = 147.",
       c("(a) State the hypotheses that you think should be tested in this experiment.",
         "(b) Test these hypotheses using α = 0.05. What are your conclusions?",
         "(c) Find the P-value for the test in part (b).",
         "(d) Construct a 95 percent confidence interval on the mean breaking strength."),
       ps_single("Strength", c(145, 153, 150, 147), 4),
       roles = list(response = "Strength"), labels = list(response = "breaking strength", unit = "psi"),
       settings = list(mu0 = 150, sigma = 3, alt = "greater"), tags = c("one sample", "z-test")),
  prob("2.22", 2, "Shelf life of a carbonated beverage",
       "The shelf life of a carbonated beverage is of interest. Ten bottles are randomly selected and tested, and the following results (days) are obtained.",
       c("(a) We would like to demonstrate that the mean shelf life exceeds 120 days. Set up appropriate hypotheses for investigating this claim.",
         "(b) Test these hypotheses using α = 0.01. What are your conclusions?",
         "(c) Find the P-value for the test in part (b).",
         "(d) Construct a 99 percent confidence interval on the mean shelf life."),
       ps_single("Days", c(108, 124, 124, 106, 115, 138, 163, 159, 134, 139), 2),
       roles = list(response = "Days"), labels = list(response = "shelf life", unit = "days"),
       settings = list(mu0 = 120, alt = "greater", alpha = 0.01, conf = 0.99), tags = c("one sample", "one-sided")),
  prob("2.24", 2, "Repair time of an electronic instrument",
       "The time to repair an electronic instrument is a normally distributed random variable measured in hours. The repair times for 16 such instruments chosen at random are as follows.",
       c("(a) You wish to know if the mean repair time exceeds 225 hours. Set up appropriate hypotheses for investigating this issue.",
         "(b) Test the hypotheses you formulated in part (a). What are your conclusions? Use α = 0.05.",
         "(c) Find the P-value for the test.",
         "(d) Construct a 95 percent confidence interval on mean repair time."),
       ps_single("Hours", c(159, 224, 222, 149, 280, 379, 362, 260, 101, 179, 168, 485, 212, 264, 250, 170), 4),
       roles = list(response = "Hours"), labels = list(response = "repair time", unit = "hours"),
       settings = list(mu0 = 225, alt = "greater"), tags = c("one sample", "one-sided")),
  prob("2.26", 2, "Bottle filling: two machines (σ known)",
       "Two machines are used for filling plastic bottles with a net volume of 16.0 ounces. The filling processes can be assumed to be normal, with standard deviations of σ1 = 0.015 and σ2 = 0.018. The quality engineering department suspects that both machines fill to the same net volume, whether or not this volume is 16.0 ounces. An experiment is performed by taking a random sample from the output of each machine.",
       c("(a) State the hypotheses that should be tested in this experiment.",
         "(b) Test these hypotheses using α = 0.05. What are your conclusions?",
         "(c) Find the P-value for this test.",
         "(d) Find a 95 percent confidence interval on the difference in mean fill volume for the two machines."),
       ps_cols("Machine", "Volume", list(`Machine 1` = c(16.03, 16.04, 16.05, 16.05, 16.02, 16.01, 15.96, 15.98, 16.02, 15.99),
                                         `Machine 2` = c(16.02, 15.97, 15.96, 16.01, 15.99, 16.03, 16.04, 16.02, 16.01, 16.00))),
       roles = list(response = "Volume", treatment = "Machine"), labels = list(response = "fill volume", unit = "oz", treatment = "machine"),
       settings = list(sigma = 0.015, sigma2 = 0.018), tags = c("two samples", "z-test")),
  prob("2.28", 2, "Burning time of two flare formulations",
       "The following are the burning times (in minutes) of chemical flares of two different formulations. The design engineers are interested in both the mean and variance of the burning times.",
       c("(a) Test the hypothesis that the two variances are equal. Use α = 0.05.",
         "(b) Using the results of (a), test the hypothesis that the mean burning times are equal. Use α = 0.05. What is the P-value for this test?",
         "(c) Discuss the role of the normality assumption in this problem. Check the assumption of normality for both types of flares."),
       ps_cols("Type", "Minutes", list(`Type 1` = c(65, 81, 57, 66, 82, 82, 67, 59, 75, 70),
                                       `Type 2` = c(64, 71, 83, 59, 65, 56, 69, 74, 82, 79))),
       roles = list(response = "Minutes", treatment = "Type"), labels = list(response = "burning time", unit = "min", treatment = "flare type"),
       tags = c("two samples", "variances")),
  prob("2.29", 2, "Plasma etching: C2F6 flow rate and uniformity",
       "An article in Solid State Technology, \"Orthogonal Design for Process Optimization and Its Application to Plasma Etching\" by G. Z. Yin and D. W. Jillie (May 1987), describes an experiment to determine the effect of the C2F6 flow rate on the uniformity of the etch on a silicon wafer used in integrated circuit manufacturing. All of the runs were made in random order. Data for two flow rates are as follows.",
       c("(a) Does the C2F6 flow rate affect average etch uniformity? Use α = 0.05.",
         "(b) What is the P-value for the test in part (a)?",
         "(c) Does the C2F6 flow rate affect the wafer-to-wafer variability in etch uniformity? Use α = 0.05.",
         "(d) Draw box plots to assist in the interpretation of the data from this experiment."),
       ps_rows("Flow (SCCM)", "Uniformity", list(`125` = c(2.7, 4.6, 2.6, 3.0, 3.2, 3.8), `200` = c(4.6, 3.4, 2.9, 3.5, 4.1, 5.1))),
       roles = list(response = "Uniformity", treatment = "Flow (SCCM)"), labels = list(response = "etch uniformity", treatment = "C2F6 flow rate"),
       tags = c("two samples", "variances")),
  prob("2.31", 2, "Photoresist thickness at two baking temperatures",
       "Photoresist is a light-sensitive material applied to semiconductor wafers so that the circuit pattern can be imaged on to the wafer. After application, the coated wafers are baked to remove the solvent in the photoresist mixture and to harden the resist. Here are measurements of photoresist thickness (in kA) for eight wafers baked at two different temperatures. Assume that all of the runs were made in random order.",
       c("(a) Is there evidence to support the claim that the higher baking temperature results in wafers with a lower mean photoresist thickness? Use α = 0.05.",
         "(b) What is the P-value for the test conducted in part (a)?",
         "(c) Find a 95 percent confidence interval on the difference in means. Provide a practical interpretation of this interval.",
         "(d) Draw dot diagrams to assist in interpreting the results from this experiment.",
         "(e) Check the assumption of normality of the photoresist thickness.",
         "(f) Find the power of this test for detecting an actual difference in means of 2.5 kA.",
         "(g) What sample size would be necessary to detect an actual difference in means of 1.5 kA with a power of at least 0.9?"),
       ps_cols("Temperature", "Thickness", list(`95 °C` = c(11.176, 7.089, 8.097, 11.739, 11.291, 10.759, 6.467, 8.315),
                                                `100 °C` = c(5.263, 6.748, 7.461, 7.015, 8.133, 7.418, 3.772, 8.963))),
       roles = list(response = "Thickness", treatment = "Temperature"), labels = list(response = "photoresist thickness", unit = "kA", treatment = "baking temperature"),
       settings = list(alt = "greater"), tags = c("two samples", "one-sided")),
  prob("2.32", 2, "Cool-down time and cosmetic defects",
       "Front housings for cell phones are manufactured in an injection molding process. The time the part is allowed to cool in the mold before removal is thought to influence the occurrence of a particularly troublesome cosmetic defect, flow lines, in the finished housing. After manufacturing, the housings are inspected visually and assigned a score between 1 and 10 based on their appearance, with 10 corresponding to a perfect part and 1 corresponding to a completely defective part. An experiment was conducted using two cool-down times, 10 and 20 seconds, and 20 housings were evaluated at each level of cool-down time. All 40 observations in this experiment were run in random order.",
       c("(a) Is there evidence to support the claim that the longer cool-down time results in fewer appearance defects? Use α = 0.05.",
         "(b) What is the P-value for the test conducted in part (a)?",
         "(c) Find a 95 percent confidence interval on the difference in means. Provide a practical interpretation of this interval.",
         "(d) Draw dot diagrams to assist in interpreting the results from this experiment.",
         "(e) Check the assumption of normality for the data from this experiment."),
       ps_cols("Cool-down", "Score", list(`10 seconds` = c(1, 2, 1, 3, 5, 1, 5, 2, 3, 5, 3, 6, 5, 3, 2, 1, 6, 8, 2, 3),
                                          `20 seconds` = c(7, 8, 5, 9, 5, 8, 6, 4, 6, 7, 6, 9, 5, 7, 4, 6, 8, 5, 8, 7))),
       roles = list(response = "Score", treatment = "Cool-down"), labels = list(response = "appearance score", treatment = "cool-down time"),
       settings = list(alt = "less"), tags = c("two samples", "one-sided")),
  prob("2.33", 2, "Etch uniformity: a test on the variance",
       "Twenty observations on etch uniformity on silicon wafers are taken during a qualification experiment for a plasma etcher. The data are as follows.",
       c("(a) Construct a 95 percent confidence interval estimate of σ².",
         "(b) Test the hypothesis that σ² = 1.0. Use α = 0.05. What are your conclusions?",
         "(c) Discuss the normality assumption and its role in this problem.",
         "(d) Check normality by constructing a normal probability plot. What are your conclusions?"),
       ps_single("Uniformity", c(5.34, 6.00, 5.97, 5.25, 6.65, 7.55, 7.35, 6.35, 4.76, 5.54, 5.44, 4.61, 5.98, 5.62, 4.39, 6.00, 7.25, 6.21, 4.98, 5.32), 5),
       roles = list(response = "Uniformity"), labels = list(response = "etch uniformity"),
       settings = list(var0 = 1), target = "variance", tags = c("one sample", "variance")),
  prob("2.34", 2, "Ball bearing diameter: two calipers, 12 inspectors",
       "The diameter of a ball bearing was measured by 12 inspectors, each using two different kinds of calipers. The results are as follows.",
       c("(a) Is there a significant difference between the means of the population of measurements from which the two samples were selected? Use α = 0.05.",
         "(b) Find the P-value for the test in part (a).",
         "(c) Construct a 95 percent confidence interval on the difference in mean diameter measurements for the two types of calipers."),
       ps_pairs("Inspector", as.character(1:12), "Caliper", c("Caliper 1", "Caliper 2"), "Diameter",
                c(0.265, 0.265, 0.266, 0.267, 0.267, 0.265, 0.267, 0.267, 0.265, 0.268, 0.268, 0.265),
                c(0.264, 0.265, 0.264, 0.266, 0.267, 0.268, 0.264, 0.265, 0.265, 0.267, 0.268, 0.269)),
       roles = list(response = "Diameter", treatment = "Caliper", block = "Inspector"),
       labels = list(response = "measured diameter", unit = "in", treatment = "caliper", block = "inspector"),
       tags = c("paired", "RCBD", "blocking")),
  prob("2.35", 2, "Intelligence scores of monozygotic twins",
       "An article in the journal Neurology (1998, Vol. 50, pp. 1246–1252) observed that monozygotic twins share numerous physical, psychological, and pathological traits. The investigators measured an intelligence score of 10 pairs of twins. The data obtained are as follows.",
       c("(a) Is the assumption that the difference in score is normally distributed reasonable?",
         "(b) Find a 95% confidence interval on the difference in mean score. Is there any evidence that mean score depends on birth order?",
         "(c) Test an appropriate set of hypotheses indicating that the mean score does not depend on birth order."),
       ps_pairs("Pair", as.character(1:10), "Birth order", c("Birth order 1", "Birth order 2"), "Score",
                c(6.08, 6.22, 7.99, 7.44, 6.48, 7.99, 6.32, 7.60, 6.03, 7.52),
                c(5.73, 5.80, 8.42, 6.84, 6.43, 8.76, 6.32, 7.62, 6.59, 7.67)),
       roles = list(response = "Score", treatment = "Birth order", block = "Pair"),
       labels = list(response = "intelligence score", treatment = "birth order", block = "twin pair"),
       tags = c("paired", "RCBD", "blocking")),
  prob("2.36", 2, "Steel plate girders: Karlsruhe vs Lehigh method",
       "An article in the Journal of Strain Analysis (Vol. 18, no. 2, 1983) compares several procedures for predicting the shear strength for steel plate girders. Data for nine girders in the form of the ratio of predicted to observed load for two of these procedures, the Karlsruhe and Lehigh methods, are as follows.",
       c("(a) Is there any evidence to support a claim that there is a difference in mean performance between the two methods? Use α = 0.05.",
         "(b) What is the P-value for the test in part (a)?",
         "(c) Construct a 95 percent confidence interval for the difference in mean predicted to observed load.",
         "(d) Investigate the normality assumption for both samples.",
         "(e) Investigate the normality assumption for the difference in ratios for the two methods.",
         "(f) Discuss the role of the normality assumption in the paired t-test."),
       ps_pairs("Girder", c("S1/1", "S2/1", "S3/1", "S4/1", "S5/1", "S2/1b", "S2/2", "S2/3", "S2/4"), "Method", c("Karlsruhe", "Lehigh"), "Ratio",
                c(1.186, 1.151, 1.322, 1.339, 1.200, 1.402, 1.365, 1.537, 1.559),
                c(1.061, 0.992, 1.063, 1.062, 1.065, 1.178, 1.037, 1.086, 1.052)),
       roles = list(response = "Ratio", treatment = "Method", block = "Girder"),
       labels = list(response = "predicted-to-observed load ratio", treatment = "method", block = "girder"),
       tags = c("paired", "RCBD", "blocking"),
       note = "The source lists girder S2/1 twice. The second one is labelled S2/1b here so that each of the nine girders is its own pair."),
  prob("2.37", 2, "Deflection temperature of two ABS formulations",
       "The deflection temperature under load for two different formulations of ABS plastic pipe is being studied. Two samples of 12 observations each are prepared using each formulation and the deflection temperatures (in °F) are reported below.",
       c("(a) Construct normal probability plots for both samples. Do these plots support assumptions of normality and equal variance for both samples?",
         "(b) Do the data support the claim that the mean deflection temperature under load for formulation 1 exceeds that of formulation 2? Use α = 0.05.",
         "(c) What is the P-value for the test in part (a)?"),
       ps_cols("Formulation", "Temperature", list(`Formulation 1` = c(206, 188, 205, 187, 193, 207, 185, 189, 192, 210, 194, 178),
                                                  `Formulation 2` = c(177, 197, 206, 201, 176, 185, 200, 197, 198, 188, 189, 203))),
       roles = list(response = "Temperature", treatment = "Formulation"), labels = list(response = "deflection temperature", unit = "°F", treatment = "formulation"),
       settings = list(alt = "greater"), tags = c("two samples", "one-sided")),
  prob("2.39", 2, "Wet chemical etching: two solutions",
       "In semiconductor manufacturing, wet chemical etching is often used to remove silicon from the backs of wafers prior to metalization. The etch rate is an important characteristic of this process. Two different etching solutions are being evaluated. Eight randomly selected wafers have been etched in each solution, and the observed etch rates (in mils/min) are as follows.",
       c("(a) Do the data indicate that the claim that both solutions have the same mean etch rate is valid? Use α = 0.05 and assume equal variances.",
         "(b) Find a 95 percent confidence interval on the difference in mean etch rates.",
         "(c) Use normal probability plots to investigate the adequacy of the assumptions of normality and equal variances."),
       ps_cols("Solution", "Etch rate", list(`Solution 1` = c(9.9, 9.4, 10.0, 10.3, 10.6, 10.3, 9.3, 9.8),
                                             `Solution 2` = c(10.2, 10.0, 10.7, 10.5, 10.6, 10.2, 10.4, 10.3))),
       roles = list(response = "Etch rate", treatment = "Solution"), labels = list(response = "etch rate", unit = "mils/min", treatment = "etching solution"),
       settings = list(var_mode = "equal"), tags = c("two samples", "equal variances"))
)

# ---------- Chapter 3 ----------

PROBLEMS_CH3 <- list(
  prob("3.10", 3, "PEMF treatment and bone mineral density loss",
       "An article in Bioelectromagnetics (\"Electromagnetic Effects on Forearm Disuse Osteopenia: A Randomized, Double-Blind, Sham-Controlled Study,\" Vol. 32, 2011, pp. 273–282) described a randomized, double-blind, sham-controlled, feasibility and dosing study to determine if a common pulsing electromagnetic field (PEMF) treatment could moderate the substantial osteopenia that occurs after forearm disuse. Subjects were randomized into four groups after a distal radius fracture, or carpal surgery requiring immobilization in a cast. Active or identical sham PEMF transducers were worn on the distal forearm for 1, 2, or 4 h/day for 8 weeks starting after cast removal (\"baseline\") when bone density continues to decline. Bone mineral density (BMD) and bone geometry were measured in the distal forearm by dual energy X-ray absorptiometry (DXA) and peripheral quantitative computed tomography (pQCT). The data below are the percent losses in BMD measurements on the radius after 16 weeks for patients wearing the active or sham PEMF transducers for 1, 2, or 4 h/day (data were constructed to match the means and standard deviations read from a graph in the paper).",
       c("(a) Is there evidence to support a claim that PEMF usage affects BMD loss? If so, analyze the data to determine which specific treatments produce the differences.",
         "(b) Analyze the residuals from this experiment and comment on the underlying assumptions and model adequacy."),
       ps_cols("Treatment", "BMD loss", list(
         Sham = c(4.51, 7.95, 4.97, 3.00, 7.97, 2.23, 3.95, 5.64, 9.35, 6.52, 4.96, 6.10, 7.19, 4.03, 2.72, 9.19, 5.17, 5.70, 5.85, 6.45),
         `PEMF 1 h/day` = c(5.32, 6.00, 5.12, 7.08, 5.48, 6.52, 4.09, 6.28, 7.77, 5.68, 8.47, 4.58, 4.11, 5.72, 5.91, 6.89, 6.99, 4.98, 9.94, 6.38),
         `PEMF 2 h/day` = c(4.73, 5.81, 5.69, 3.86, 4.06, 6.56, 8.34, 3.01, 6.71, 6.51, 1.70, 5.89, 6.55, 5.34, 5.88, 7.50, 3.28, 5.38, 7.30, 5.46),
         `PEMF 4 h/day` = c(7.03, 4.65, 6.65, 5.49, 6.98, 4.85, 7.26, 5.92, 5.58, 7.91, 4.90, 4.54, 8.18, 5.42, 6.03, 7.04, 5.17, 7.60, 7.90, 7.91))),
       roles = list(response = "BMD loss", treatment = "Treatment"), labels = list(response = "BMD loss", unit = "%", treatment = "PEMF treatment"),
       control = "Sham", tags = c("one-way ANOVA", "CRD", "control group")),
  prob("3.11", 3, "Tensile strength of Portland cement",
       "The tensile strength of Portland cement is being studied. Four different mixing techniques can be used economically. A completely randomized experiment was conducted and the following data (lb/in²) were collected.",
       c("(a) Test the hypothesis that mixing techniques affect the strength of the cement. Use α = 0.05.",
         "(b) Construct a graphical display as described in Section 3.5.3 to compare the mean tensile strengths for the four mixing techniques. What are your conclusions?",
         "(c) Use the Fisher LSD method with α = 0.05 to make comparisons between pairs of means.",
         "(d) Construct a normal probability plot of the residuals. What conclusion would you draw about the validity of the normality assumption?",
         "(e) Plot the residuals versus the predicted tensile strength. Comment on the plot.",
         "(f) Prepare a scatter plot of the results to aid the interpretation of the results of this experiment."),
       ps_rows("Mixing technique", "Strength", list(`1` = c(3129, 3000, 2865, 2890), `2` = c(3200, 3300, 2975, 3150),
                                                   `3` = c(2800, 2900, 2985, 3050), `4` = c(2600, 2700, 2600, 2765))),
       roles = list(response = "Strength", treatment = "Mixing technique"), labels = list(response = "tensile strength", unit = "lb/in²", treatment = "mixing technique"),
       settings = list(posthoc = "lsd"), tags = c("one-way ANOVA", "CRD", "Fisher LSD")),
  prob("3.14", 3, "Synthetic fiber strength and cotton content",
       "A product developer is investigating the tensile strength of a new synthetic fiber that will be used to make cloth for men's shirts. Strength is usually affected by the percentage of cotton used in the blend of materials for the fiber. The engineer conducts a completely randomized experiment with five levels of cotton content and replicates the experiment five times. The data are shown in the following table.",
       c("(a) Is there evidence to support the claim that cotton content affects the mean tensile strength? Use α = 0.05.",
         "(b) Use the Fisher LSD method to make comparisons between the pairs of means. What conclusions can you draw?",
         "(c) Analyze the residuals from this experiment and comment on model adequacy."),
       ps_rows("Cotton %", "Strength", list(`15` = c(7, 7, 15, 11, 9), `20` = c(12, 17, 12, 18, 18), `25` = c(14, 19, 19, 18, 18),
                                            `30` = c(19, 25, 22, 19, 23), `35` = c(7, 10, 11, 15, 11))),
       roles = list(response = "Strength", treatment = "Cotton %"), labels = list(response = "tensile strength", treatment = "cotton weight percent"),
       settings = list(posthoc = "lsd"), tags = c("one-way ANOVA", "CRD", "Fisher LSD")),
  prob("3.16", 3, "Bioactivity of a new drug at three dosages",
       "A pharmaceutical manufacturer wants to investigate the bioactivity of a new drug. A completely randomized single-factor experiment was conducted with three dosage levels, and the following results were obtained.",
       c("(a) Is there evidence to indicate that dosage level affects bioactivity? Use α = 0.05.",
         "(b) If it is appropriate to do so, make comparisons between the pairs of means. What conclusions can you draw?",
         "(c) Analyze the residuals from this experiment and comment on model adequacy."),
       ps_rows("Dosage", "Bioactivity", list(`20 g` = c(24, 28, 37, 30), `30 g` = c(37, 44, 31, 35), `40 g` = c(42, 47, 52, 38))),
       roles = list(response = "Bioactivity", treatment = "Dosage"), labels = list(response = "bioactivity", treatment = "dosage"),
       tags = c("one-way ANOVA", "CRD")),
  prob("3.17", 3, "Rental car type and length of rental",
       "A rental car company wants to investigate whether the type of car rented affects the length of the rental period. An experiment is run for one week at a particular location, and 10 rental contracts are selected at random for each car type. The results (days) are shown in the following table.",
       c("(a) Is there evidence to support a claim that the type of car rented affects the length of the rental contract? Use α = 0.05. If so, which types of cars are responsible for the difference?",
         "(b) Analyze the residuals from this experiment and comment on model adequacy.",
         "(c) Notice that the response variable in this experiment is a count. Should this cause any potential concerns about the validity of the analysis of variance?"),
       ps_rows("Car type", "Days", list(Subcompact = c(3, 5, 3, 7, 6, 5, 3, 2, 1, 6), Compact = c(1, 3, 4, 7, 5, 6, 3, 2, 1, 7),
                                        Midsize = c(4, 1, 3, 5, 7, 1, 2, 4, 2, 7), `Full size` = c(3, 5, 7, 5, 10, 3, 4, 7, 2, 7))),
       roles = list(response = "Days", treatment = "Car type"), labels = list(response = "rental length", unit = "days", treatment = "car type"),
       tags = c("one-way ANOVA", "count data")),
  prob("3.18", 3, "Golf scores in three seasons (unequal sample sizes)",
       "I belong to a golf club in my neighborhood. I divide the year into three golf seasons: summer (June–September), winter (November–March), and shoulder (October, April, and May). I believe that I play my best golf during the summer (because I have more time and the course isn't crowded) and shoulder (because the course isn't crowded) seasons, and my worst golf is during the winter (because when all of the part-year residents show up, the course is crowded, play is slow, and I get frustrated). Data from the last year are shown in the following table.",
       c("(a) Do the data indicate that my opinion is correct? Use α = 0.05.",
         "(b) Analyze the residuals from this experiment and comment on model adequacy."),
       ps_rows("Season", "Score", list(Summer = c(83, 85, 85, 87, 90, 88, 88, 84, 91, 90), Shoulder = c(91, 87, 84, 87, 85, 86, 83),
                                       Winter = c(94, 91, 87, 85, 87, 91, 92, 86))),
       roles = list(response = "Score", treatment = "Season"), labels = list(response = "golf score", treatment = "season"),
       tags = c("one-way ANOVA", "unbalanced")),
  prob("3.19", 3, "Opera company: three approaches to soliciting donations",
       "A regional opera company has tried three approaches to solicit donations from 24 potential sponsors. The 24 potential sponsors were randomly divided into three groups of eight, and one approach was used for each group. The dollar amounts of the resulting contributions are shown in the following table.",
       c("(a) Do the data indicate that there is a difference in results obtained from the three different approaches? Use α = 0.05.",
         "(b) Analyze the residuals from this experiment and comment on model adequacy."),
       ps_rows("Approach", "Contribution", list(`1` = c(1000, 1500, 1200, 1800, 1600, 1100, 1000, 1250),
                                               `2` = c(1500, 1800, 2000, 1200, 2000, 1700, 1800, 1900),
                                               `3` = c(900, 1000, 1200, 1500, 1200, 1550, 1000, 1100))),
       roles = list(response = "Contribution", treatment = "Approach"), labels = list(response = "contribution", unit = "$", treatment = "approach"),
       tags = c("one-way ANOVA", "CRD")),
  prob("3.20", 3, "Firing temperature and brick density (unequal sample sizes)",
       "An experiment was run to determine whether four specific firing temperatures affect the density of a certain type of brick. A completely randomized experiment led to the following data.",
       c("(a) Does the firing temperature affect the density of the bricks? Use α = 0.05.",
         "(b) Is it appropriate to compare the means using the Fisher LSD method (for example) in this experiment?",
         "(c) Analyze the residuals from this experiment. Are the analysis of variance assumptions satisfied?",
         "(d) Construct a graphical display of the treatment as described in Section 3.5.3. Does this graph adequately summarize the results of the analysis of variance in part (a)?"),
       ps_rows("Temperature", "Density", list(`100` = c(21.8, 21.9, 21.7, 21.6, 21.7), `125` = c(21.7, 21.4, 21.5, 21.4),
                                              `150` = c(21.9, 21.8, 21.8, 21.6, 21.5), `175` = c(21.9, 21.7, 21.8, 21.4))),
       roles = list(response = "Density", treatment = "Temperature"), labels = list(response = "brick density", treatment = "firing temperature"),
       tags = c("one-way ANOVA", "unbalanced")),
  prob("3.22", 3, "Picture tube coating and conductivity",
       "A manufacturer of television sets is interested in the effect on tube conductivity of four different types of coating for color picture tubes. A completely randomized experiment is conducted and the following conductivity data are obtained.",
       c("(a) Is there a difference in conductivity due to coating type? Use α = 0.05.",
         "(b) Estimate the overall mean and the treatment effects.",
         "(c) Compute a 95 percent confidence interval estimate of the mean of coating type 4. Compute a 99 percent confidence interval estimate of the mean difference between coating types 1 and 4.",
         "(d) Test all pairs of means using the Fisher LSD method with α = 0.05.",
         "(e) Use the graphical method discussed in Section 3.5.3 to compare the means. Which coating type produces the highest conductivity?",
         "(f) Assuming that coating type 4 is currently in use, what are your recommendations to the manufacturer? We wish to minimize conductivity."),
       ps_rows("Coating type", "Conductivity", list(`1` = c(143, 141, 150, 146), `2` = c(152, 149, 137, 143),
                                                   `3` = c(134, 136, 132, 127), `4` = c(129, 127, 132, 129))),
       roles = list(response = "Conductivity", treatment = "Coating type"), labels = list(response = "conductivity", treatment = "coating type"),
       settings = list(posthoc = "lsd"), tags = c("one-way ANOVA", "CRD", "Fisher LSD")),
  prob("3.26", 3, "Response time of three circuit types",
       "The response time in milliseconds was determined for three different types of circuits that could be used in an automatic valve shutoff mechanism. The results from a completely randomized experiment are shown in the following table.",
       c("(a) Test the hypothesis that the three circuit types have the same response time. Use α = 0.01.",
         "(b) Use Tukey's test to compare pairs of treatment means. Use α = 0.01.",
         "(c) Use the graphical procedure in Section 3.5.3 to compare the treatment means. What conclusions can you draw? How do they compare with the conclusions from part (b)?",
         "(d) Construct a set of orthogonal contrasts, assuming that at the outset of the experiment you suspected the response time of circuit type 2 to be different from the other two.",
         "(e) If you were the design engineer and you wished to minimize the response time, which circuit type would you select?",
         "(f) Analyze the residuals from this experiment. Are the basic analysis of variance assumptions satisfied?"),
       ps_rows("Circuit type", "Response time", list(`1` = c(9, 12, 10, 8, 15), `2` = c(20, 21, 23, 17, 30), `3` = c(6, 5, 8, 16, 7))),
       roles = list(response = "Response time", treatment = "Circuit type"), labels = list(response = "response time", unit = "ms", treatment = "circuit type"),
       settings = list(alpha = 0.01, conf = 0.99), tags = c("one-way ANOVA", "Tukey", "α = 0.01")),
  prob("3.27", 3, "Effective life of four insulating fluids",
       "The effective life of insulating fluids at an accelerated load of 35 kV is being studied. Test data have been obtained for four types of fluids. The results from a completely randomized experiment are as follows (life in hours at 35 kV load).",
       c("(a) Is there any indication that the fluids differ? Use α = 0.05.",
         "(b) Which fluid would you select, given that the objective is long life?",
         "(c) Analyze the residuals from this experiment. Are the basic analysis of variance assumptions satisfied?"),
       ps_rows("Fluid type", "Life", list(`1` = c(17.6, 18.9, 16.3, 17.4, 20.1, 21.6), `2` = c(16.9, 15.3, 18.6, 17.1, 19.5, 20.3),
                                          `3` = c(21.4, 23.6, 19.4, 18.5, 20.5, 22.3), `4` = c(19.3, 21.1, 16.9, 17.5, 18.3, 19.8))),
       roles = list(response = "Life", treatment = "Fluid type"), labels = list(response = "effective life", unit = "h", treatment = "fluid type"),
       tags = c("one-way ANOVA", "CRD")),
  prob("3.28", 3, "Noise in four digital circuit designs",
       "Four different designs for a digital computer circuit are being studied to compare the amount of noise present. The following data have been obtained.",
       c("(a) Is the same amount of noise present for all four designs? Use α = 0.05.",
         "(b) Analyze the residuals from this experiment. Are the analysis of variance assumptions satisfied?",
         "(c) Which circuit design would you select for use? Low noise is best."),
       ps_rows("Circuit design", "Noise", list(`1` = c(19, 20, 19, 30, 8), `2` = c(80, 61, 73, 56, 80), `3` = c(47, 26, 25, 35, 50), `4` = c(95, 46, 83, 78, 97))),
       roles = list(response = "Noise", treatment = "Circuit design"), labels = list(response = "noise", treatment = "circuit design"),
       tags = c("one-way ANOVA", "CRD")),
  prob("3.30", 3, "Battery life of three brands",
       "Three brands of batteries are under study. It is suspected that the lives (in weeks) of the three brands are different. Five randomly selected batteries of each brand are tested with the following results.",
       c("(a) Are the lives of these brands of batteries different?",
         "(b) Analyze the residuals from this experiment.",
         "(c) Construct a 95 percent confidence interval estimate on the mean life of battery brand 2. Construct a 99 percent confidence interval estimate on the mean difference between the lives of battery brands 2 and 3.",
         "(d) Which brand would you select for use? If the manufacturer will replace without charge any battery that fails in less than 85 weeks, what percentage would the company expect to replace?"),
       ps_cols("Brand", "Weeks", list(`Brand 1` = c(100, 96, 92, 96, 92), `Brand 2` = c(76, 80, 75, 84, 82), `Brand 3` = c(108, 100, 96, 98, 100))),
       roles = list(response = "Weeks", treatment = "Brand"), labels = list(response = "battery life", unit = "weeks", treatment = "brand"),
       tags = c("one-way ANOVA", "CRD")),
  prob("3.32", 3, "Failure time of five insulating materials",
       "An experiment was performed to investigate the effectiveness of five insulating materials. Four samples of each material were tested at an elevated voltage level to accelerate the time to failure. The failure times (in minutes) are shown below.",
       c("(a) Do all five materials have the same effect on mean failure time?",
         "(b) Plot the residuals versus the predicted response. Construct a normal probability plot of the residuals. What information is conveyed by these plots?",
         "(c) Based on your answer to part (b), conduct another analysis of the failure time data and draw appropriate conclusions."),
       ps_rows("Material", "Failure time", list(`1` = c(110, 157, 194, 178), `2` = c(1, 2, 4, 18), `3` = c(880, 1256, 5276, 4355),
                                               `4` = c(495, 7040, 5307, 10050), `5` = c(7, 5, 29, 2))),
       roles = list(response = "Failure time", treatment = "Material"), labels = list(response = "failure time", unit = "min", treatment = "material"),
       tags = c("one-way ANOVA", "transformation")),
  prob("3.34", 3, "Calcium content of raw material batches (random effects)",
       "A manufacturer suspects that the batches of raw material furnished by his supplier differ significantly in calcium content. There are a large number of batches currently in the warehouse. Five of these are randomly selected for study. A chemist makes five determinations on each batch and obtains the following data.",
       c("(a) Is there significant variation in calcium content from batch to batch? Use α = 0.05.",
         "(b) Estimate the components of variance.",
         "(c) Find a 95 percent confidence interval for στ² / (στ² + σ²).",
         "(d) Analyze the residuals from this experiment. Are the analysis of variance assumptions satisfied?",
         "(e) Use the REML method to analyze this data. Compare the 95 percent confidence interval on the error variance from REML with the exact chi-square confidence interval."),
       ps_cols("Batch", "Calcium", list(`Batch 1` = c(23.46, 23.48, 23.56, 23.39, 23.40), `Batch 2` = c(23.59, 23.46, 23.42, 23.49, 23.50),
                                        `Batch 3` = c(23.51, 23.64, 23.46, 23.52, 23.49), `Batch 4` = c(23.28, 23.40, 23.37, 23.46, 23.39),
                                        `Batch 5` = c(23.29, 23.46, 23.37, 23.32, 23.38))),
       roles = list(response = "Calcium", treatment = "Batch"), labels = list(response = "calcium content", treatment = "batch"), tags = c("random effects", "variance components"))
)

# ---------- Chapter 4 ----------

CH4_TXT <- list(
  p48 = "A chemist wishes to test the effect of four chemical agents on the strength of a particular type of cloth. Because there might be variability from one bolt to another, the chemist decides to use a randomized block design, with the bolts of cloth considered as blocks. She selects five bolts and applies all four chemicals in random order to each bolt. The resulting tensile strengths follow. Analyze the data from this experiment (use α = 0.05) and draw appropriate conclusions.",
  p49 = "Three different washing solutions are being compared to study their effectiveness in retarding bacteria growth in 5-gallon milk containers. The analysis is done in a laboratory, and only three trials can be run on any day. Because days could represent a potential source of variability, the experimenter decides to use a randomized block design. Observations are taken for four days, and the data are shown here. Analyze the data from this experiment (use α = 0.05) and draw conclusions.",
  p412 = "Consider the hardness testing experiment described in Section 4.1. Suppose that the experiment was conducted as described and that the following Rockwell C-scale data (coded by subtracting 40 units) obtained:",
  p413 = "A consumer products company relies on direct mail marketing pieces as a major component of its advertising campaigns. The company has three different designs for a new brochure and wants to evaluate their effectiveness, as there are substantial differences in costs between the three designs. The company decides to test the three designs by mailing 5000 samples of each to potential customers in four different regions of the country. Since there are known regional differences in the customer base, regions are considered as blocks. The number of responses to each mailing is as follows.",
  p414 = "The effect of three different lubricating oils on fuel economy in diesel truck engines is being studied. Fuel economy is measured using brake-specific fuel consumption after the engine has been running for 15 minutes. Five different truck engines are available for the study, and the experimenters conduct the following RCBD.",
  p427 = "The effect of five different ingredients (A, B, C, D, E) on the reaction time of a chemical process is being studied. Each batch of new material is only large enough to permit five runs to be made. Furthermore, each run requires approximately 1½ hours, so only five runs can be made in one day. The experimenter decides to run the experiment as a Latin square so that day and batch effects may be systematically controlled. She obtains the data that follow. Analyze the data from this experiment (use α = 0.05) and draw conclusions.",
  p428 = "An industrial engineer is investigating the effect of four assembly methods (A, B, C, D) on the assembly time for a color television component. Four operators are selected for the study. Furthermore, the engineer knows that each assembly method produces such fatigue that the time required for the last assembly may be greater than the time required for the first, regardless of the method. That is, a trend develops in the required assembly time. To account for this source of variability, the engineer uses the Latin square design that follows. Analyze the data from this experiment (α = 0.05) and draw appropriate conclusions.")
ctx <- function(id, key) sprintf("Problem %s: %s", id, CH4_TXT[[key]])

D48 <- c(73, 68, 74, 71, 67,  73, 67, 75, 72, 70,  75, 68, 78, 73, 68,  73, 71, 75, 75, 69)
D49 <- c(13, 22, 18, 39,  16, 24, 17, 44,  5, 4, 1, 22)
D412 <- c(9.3, 9.4, 9.6, 10.0,  9.4, 9.3, 9.8, 9.9,  9.2, 9.4, 9.5, 9.7,  9.7, 9.6, 10.0, 10.2)
D413 <- c(250, 350, 219, 375,  400, 525, 390, 580,  275, 340, 200, 310)
D414 <- c(0.500, 0.634, 0.487, 0.329, 0.512,  0.535, 0.675, 0.520, 0.435, 0.540,  0.513, 0.595, 0.488, 0.400, 0.510)
L427 <- c("A", "B", "D", "C", "E",  "C", "E", "A", "D", "B",  "B", "A", "C", "E", "D",  "D", "C", "E", "B", "A",  "E", "D", "B", "A", "C")
V427 <- c(8, 7, 1, 7, 3,  11, 2, 7, 3, 8,  4, 9, 10, 1, 5,  6, 8, 6, 6, 10,  4, 2, 3, 8, 8)
L428 <- c("C", "D", "A", "B",  "B", "C", "D", "A",  "A", "B", "C", "D",  "D", "A", "B", "C")
V428 <- c(10, 14, 7, 8,  7, 18, 11, 8,  5, 10, 11, 9,  10, 10, 12, 14)

g48 <- function(missing = NULL) ps_grid("Chemical", "Bolt", "Strength", as.character(1:4), as.character(1:5), D48, missing)
g49 <- function() ps_grid("Solution", "Day", "Bacteria", as.character(1:3), as.character(1:4), D49)
g412 <- function(missing = NULL) ps_grid("Tip", "Coupon", "Hardness", as.character(1:4), as.character(1:4), D412, missing)
g413 <- function() ps_grid("Design", "Region", "Responses", as.character(1:3), c("NE", "NW", "SE", "SW"), D413)
g414 <- function() ps_grid("Oil", "Truck", "BSFC", as.character(1:3), as.character(1:5), D414)
s427 <- function(missing = NULL) ps_square("Batch", "Day", "Ingredient", "Time", as.character(1:5), as.character(1:5), L427, V427, missing = missing)
s428 <- function() ps_square("Order", "Operator", "Method", "Time", as.character(1:4), as.character(1:4), L428, V428)

R_RCBD <- function(trt, blk, resp) list(response = resp, treatment = trt, block = blk)
LAB48 <- list(response = "tensile strength", treatment = "chemical", block = "bolt")
LAB49 <- list(response = "bacteria count", treatment = "solution", block = "day")
LAB412 <- list(response = "hardness", unit = "Rockwell C − 40", treatment = "tip", block = "coupon")
LAB413 <- list(response = "number of responses", treatment = "design", block = "region")
LAB414 <- list(response = "brake-specific fuel consumption", treatment = "oil", block = "truck")
LAB427 <- list(response = "reaction time", treatment = "ingredient", row = "batch", column = "day")
LAB428 <- list(response = "assembly time", treatment = "assembly method", row = "order of assembly", column = "operator")

PROBLEMS_CH4 <- list(
  prob("4.8", 4, "Four chemical agents and cloth strength (bolts as blocks)", CH4_TXT$p48, character(0), g48(),
       roles = R_RCBD("Chemical", "Bolt", "Strength"), labels = LAB48, design = "rcbd", tags = c("RCBD")),
  prob("4.9", 4, "Washing solutions and bacteria growth (days as blocks)", CH4_TXT$p49, character(0), g49(),
       roles = R_RCBD("Solution", "Day", "Bacteria"), labels = LAB49, design = "rcbd", tags = c("RCBD")),
  prob("4.10", 4, "Chemical means against a scaled t distribution",
       c("Plot the mean tensile strengths observed for each chemical type in Problem 4.8 and compare them to an appropriately scaled t distribution. What conclusions would you draw from this display?", ctx("4.8", "p48")),
       character(0), g48(), roles = R_RCBD("Chemical", "Bolt", "Strength"), labels = LAB48, design = "rcbd", scaled_t = TRUE,
       note = "The data are those of Problem 4.8. The scaled t display is in the Decision step.", tags = c("RCBD", "scaled t")),
  prob("4.11", 4, "Solution means against a scaled t distribution",
       c("Plot the average bacteria counts for each solution in Problem 4.9 and compare them to a scaled t distribution. What conclusions can you draw?", ctx("4.9", "p49")),
       character(0), g49(), roles = R_RCBD("Solution", "Day", "Bacteria"), labels = LAB49, design = "rcbd", scaled_t = TRUE,
       note = "The data are those of Problem 4.9. The scaled t display is in the Decision step.", tags = c("RCBD", "scaled t")),
  prob("4.12", 4, "Hardness testing: four tips on four coupons", CH4_TXT$p412,
       c("(a) Analyze the data from this experiment.",
         "(b) Use the Fisher LSD method to make comparisons among the four tips to determine specifically which tips differ in mean hardness readings.",
         "(c) Analyze the residuals from this experiment."),
       g412(), roles = R_RCBD("Tip", "Coupon", "Hardness"), labels = LAB412, design = "rcbd", settings = list(posthoc = "lsd"), tags = c("RCBD", "Fisher LSD")),
  prob("4.13", 4, "Direct mail brochure designs (regions as blocks)", CH4_TXT$p413,
       c("(a) Analyze the data from this experiment.",
         "(b) Use the Fisher LSD method to make comparisons among the three designs to determine specifically which designs differ in the mean response rate.",
         "(c) Analyze the residuals from this experiment."),
       g413(), roles = R_RCBD("Design", "Region", "Responses"), labels = LAB413, design = "rcbd", settings = list(posthoc = "lsd"), tags = c("RCBD", "Fisher LSD")),
  prob("4.14", 4, "Lubricating oils and fuel economy (trucks as blocks)", CH4_TXT$p414,
       c("(a) Analyze the data from this experiment.",
         "(b) Use the Fisher LSD method to make comparisons among the three lubricating oils to determine specifically which oils differ in brake-specific fuel consumption.",
         "(c) Analyze the residuals from this experiment."),
       g414(), roles = R_RCBD("Oil", "Truck", "BSFC"), labels = LAB414, design = "rcbd", settings = list(posthoc = "lsd"), tags = c("RCBD", "Fisher LSD")),
  prob("4.15", 4, "Nozzle design and the shape factor of water jets",
       "An article in the Fire Safety Journal (“The Effect of Nozzle Design on the Stability and Performance of Turbulent Water Jets,” Vol. 4, August 1981) describes an experiment in which a shape factor was determined for several different nozzle designs at six levels of jet efflux velocity. Interest focused on potential differences between nozzle designs, with velocity considered as a nuisance variable. The data are shown below:",
       c("(a) Does nozzle design affect the shape factor? Compare the nozzles with a scatter plot and with an analysis of variance, using α = 0.05.",
         "(b) Analyze the residuals from this experiment.",
         "(c) Which nozzle designs are different with respect to shape factor? Draw a graph of the average shape factor for each nozzle type and compare this to a scaled t distribution. Compare the conclusions that you draw from this plot to those from Duncan’s multiple range test."),
       ps_grid("Nozzle", "Velocity", "Shape", as.character(1:5), c("11.73", "14.37", "16.59", "20.43", "23.46", "28.74"),
               c(0.78, 0.80, 0.81, 0.75, 0.77, 0.78,  0.85, 0.85, 0.92, 0.86, 0.81, 0.83,  0.93, 0.92, 0.95, 0.89, 0.89, 0.83,
                 1.14, 0.97, 0.98, 0.88, 0.86, 0.83,  0.97, 0.86, 0.78, 0.76, 0.76, 0.75)),
       roles = R_RCBD("Nozzle", "Velocity", "Shape"), labels = list(response = "shape factor", treatment = "nozzle design", block = "jet efflux velocity", unit = ""),
       design = "rcbd", scaled_t = TRUE, note = "Duncan's multiple range test is not part of the course; the post-hoc step offers Tukey HSD, Fisher LSD, Bonferroni and Scheffé. Velocity (m/s) is the blocking factor.",
       tags = c("RCBD", "scaled t")),
  prob("4.16", 4, "Software cost estimation algorithms (projects as blocks)",
       "An article in Communications of the ACM (Vol. 30, No. 5, 1987) studied different algorithms for estimating software development costs. Six algorithms were applied to several different software development projects and the percent error in estimating the development cost was observed. Some of the data from this experiment is shown in the table below.",
       c("(a) Do the algorithms differ in their mean cost estimation accuracy?", "(b) Analyze the residuals from this experiment.",
         "(c) Which algorithm would you recommend for use in practice?"),
       ps_grid("Algorithm", "Project", "Error", c("1 (SLIM)", "2 (COCOMO-A)", "3 (COCOMO-R)", "4 (COCONO-C)", "5 (Function points)", "6 (ESTIMALS)"), as.character(1:6),
               c(1244, 21, 82, 2221, 905, 839,  281, 129, 396, 1306, 336, 910,  220, 84, 458, 543, 300, 794,
                 225, 83, 425, 552, 291, 826,  19, 11, -34, 121, 15, 103,  -20, 35, -53, 170, 104, 199)),
       roles = R_RCBD("Algorithm", "Project", "Error"), labels = list(response = "percent error", unit = "%", treatment = "algorithm", block = "project"),
       design = "rcbd", tags = c("RCBD")),
  prob("4.17", 4, "Gene expression under three leukemia treatments",
       "An article in Nature Genetics (2003, Vol. 34, pp. 85–90) “Treatment-Specific Changes in Gene Expression Discriminate in vivo Drug Response in Human Leukemia Cells” studied gene expression as a function of different treatments for leukemia. Three treatment groups are as follows: mercaptopurine (MP) only; low-dose methotrexate (LDMTX) and MP; and high-dose methotrexate (HDMTX) and MP. Each group contained ten subjects. The responses from a specific gene are shown in the table below.",
       c("(a) Is there evidence to support the claim that the treatment means differ?",
         "(b) Check the normality assumption. Can we assume these samples are from normal populations?",
         "(c) Take the logarithm of the raw data. Is there evidence to support the claim that the treatment means differ for the transformed data?",
         "(d) Analyze the residuals from the transformed data and comment on model adequacy."),
       ps_grid("Treatment", "Subject", "Expression", c("MP only", "MP + HDMTX", "MP + LDMTX"), as.character(1:10),
               c(334.5, 31.6, 701, 41.2, 61.2, 69.6, 67.5, 66.6, 120.7, 881.9,
                 919.4, 404.2, 1024.8, 54.1, 62.8, 671.6, 882.1, 354.2, 321.9, 91.1,
                 108.4, 26.1, 240.8, 191.1, 69.7, 242.8, 62.7, 396.9, 23.6, 290.4)),
       roles = R_RCBD("Treatment", "Subject", "Expression"), labels = list(response = "gene expression", treatment = "treatment", block = "subject"),
       design = "rcbd", note = "Problem 4.33 treats the ten subjects (the columns) as blocks, so the data are analysed as a randomized block design. For part (c), use Copy to Custom Problem and replace the values by their logarithms.",
       tags = c("RCBD", "normality", "transformation")),
  prob("4.18", 4, "Ratio control algorithms and cell voltage (time periods as blocks)",
       "Consider the ratio control algorithm experiment described in Section 3.8. The experiment was actually conducted as a randomized block design, where six time periods were selected as the blocks, and all four ratio control algorithms were tested in each time period. The average cell voltage and the standard deviation of voltage (shown in parentheses) for each cell are as follows:",
       c("(a) Analyze the average cell voltage data. (Use α = 0.05.) Does the choice of ratio control algorithm affect the average cell voltage?",
         "(b) Perform an appropriate analysis on the standard deviation of voltage. (Recall that this is called “pot noise.”) Does the choice of ratio control algorithm affect the pot noise?",
         "(c) Conduct any residual analyses that seem appropriate.",
         "(d) Which ratio control algorithm would you select if your objective is to reduce both the average cell voltage and the pot noise?"),
       local({
         v <- matrix(c(4.93, 4.86, 4.75, 4.95, 4.79, 4.88,  4.85, 4.91, 4.79, 4.85, 4.75, 4.85,  4.83, 4.88, 4.90, 4.75, 4.82, 4.90,  4.89, 4.77, 4.94, 4.86, 4.79, 4.76), 4, byrow = TRUE)
         sdv <- matrix(c(0.05, 0.04, 0.05, 0.06, 0.03, 0.05,  0.04, 0.02, 0.03, 0.05, 0.03, 0.02,  0.09, 0.13, 0.11, 0.15, 0.08, 0.12,  0.03, 0.04, 0.05, 0.05, 0.03, 0.02), 4, byrow = TRUE)
         ij <- expand.grid(j = 1:6, i = 1:4)
         wide <- data.frame(Algorithm = as.character(1:4), stringsAsFactors = FALSE)
         for (j in 1:6) wide[[as.character(j)]] <- sprintf("%.2f (%.2f)", v[, j], sdv[, j])
         list(long = data.frame(Algorithm = as.character(ij$i), Period = as.character(ij$j), Voltage = v[cbind(ij$i, ij$j)], SD = sdv[cbind(ij$i, ij$j)], stringsAsFactors = FALSE),
              wide = wide)
       }),
       roles = R_RCBD("Algorithm", "Period", "Voltage"), labels = list(response = "average cell voltage", unit = "V", treatment = "ratio control algorithm", block = "time period"),
       design = "rcbd", note = "The steps analyse the average cell voltage, part (a). For part (b), use Copy to Custom Problem and choose the SD column as the response.",
       tags = c("RCBD")),
  prob("4.19", 4, "Stirring rate and grain size (furnaces as blocks)",
       "An aluminum master alloy manufacturer produces grain refiners in ingot form. The company produces the product in four furnaces. Each furnace is known to have its own unique operating characteristics, so any experiment run in the foundry that involves more than one furnace will consider furnaces as a nuisance variable. The process engineers suspect that stirring rate affects the grain size of the product. Each furnace can be run at four different stirring rates. A randomized block design is run for a particular refiner, and the resulting grain size data is as follows.",
       c("(a) Is there any evidence that stirring rate affects grain size?", "(b) Graph the residuals from this experiment on a normal probability plot. Interpret this plot.",
         "(c) Plot the residuals versus furnace and stirring rate. Does this plot convey any useful information?",
         "(d) What should the process engineers recommend concerning the choice of stirring rate and furnace for this particular grain refiner if small grain size is desirable?"),
       ps_grid("Stirring rate", "Furnace", "Grain size", c("5", "10", "15", "20"), as.character(1:4), c(8, 4, 5, 6,  14, 5, 6, 9,  14, 6, 9, 2,  17, 9, 3, 6)),
       roles = R_RCBD("Stirring rate", "Furnace", "Grain size"), labels = list(response = "grain size", treatment = "stirring rate", block = "furnace"),
       design = "rcbd", tags = c("RCBD")),
  prob("4.23", 4, "Problem 4.8 with one missing value",
       c("Suppose that the observation for chemical type 2 and bolt 3 is missing in Problem 4.8. Analyze the problem by estimating the missing value. Perform the exact analysis and compare the results.", ctx("4.8", "p48")),
       character(0), g48(list(c(2, 3))), roles = R_RCBD("Chemical", "Bolt", "Strength"), labels = LAB48, design = "rcbd",
       note = "The missing value is estimated in step 5, which also shows the exact analysis (general regression significance test).", tags = c("RCBD", "missing value")),
  prob("4.24", 4, "Problem 4.12 with one missing value",
       c("Consider the hardness testing experiment in Problem 4.12. Suppose that the observation for tip 2 in coupon 3 is missing. Analyze the problem by estimating the missing value.", ctx("4.12", "p412")),
       character(0), g412(list(c(2, 3))), roles = R_RCBD("Tip", "Coupon", "Hardness"), labels = LAB412, design = "rcbd",
       note = "The missing value is estimated in step 5.", tags = c("RCBD", "missing value")),
  prob("4.26", 4, "Eye focus time at four distances (subjects as blocks)",
       "An industrial engineer is conducting an experiment on eye focus time. He is interested in the effect of the distance of the object from the eye on the focus time. Four different distances are of interest. He has five subjects available for the experiment. Because there may be differences among individuals, he decides to conduct the experiment in a randomized block design. The data obtained follow. Analyze the data from this experiment (use α = 0.05) and draw appropriate conclusions.",
       character(0),
       ps_grid("Distance (ft)", "Subject", "Focus time", c("4", "6", "8", "10"), as.character(1:5), c(10, 6, 6, 6, 6,  7, 6, 6, 1, 6,  5, 3, 3, 2, 5,  6, 4, 4, 2, 3)),
       roles = R_RCBD("Distance (ft)", "Subject", "Focus time"), labels = list(response = "focus time", treatment = "distance", block = "subject"),
       design = "rcbd", tags = c("RCBD")),
  prob("4.27", 4, "Five ingredients and reaction time (Latin square)", CH4_TXT$p427, character(0), s427(),
       roles = list(response = "Time", treatment = "Ingredient", row = "Batch", column = "Day"), labels = LAB427, tags = c("Latin square")),
  prob("4.28", 4, "Four assembly methods and assembly time (Latin square)", CH4_TXT$p428, character(0), s428(),
       roles = list(response = "Time", treatment = "Method", row = "Order", column = "Operator"), labels = LAB428, tags = c("Latin square")),
  prob("4.29", 4, "Problem 4.9 with random days: block variance component",
       c("Consider the randomized complete block design in Problem 4.9. Assume that the days are random. Estimate the block variance component.", ctx("4.9", "p49")),
       character(0), g49(), roles = R_RCBD("Solution", "Day", "Bacteria"), labels = LAB49, design = "rcbd", random_blocks = TRUE,
       note = "The block variance component is estimated in step 5, under the ANOVA table.", tags = c("RCBD", "random blocks")),
  prob("4.31", 4, "Problem 4.14 with random trucks: block variance component",
       c("Consider the randomized complete block design in Problem 4.14. Assume that the trucks are random. Estimate the block variance component.", ctx("4.14", "p414")),
       character(0), g414(), roles = R_RCBD("Oil", "Truck", "BSFC"), labels = LAB414, design = "rcbd", random_blocks = TRUE,
       note = "The block variance component is estimated in step 5, under the ANOVA table.", tags = c("RCBD", "random blocks")),
  prob("4.34", 4, "Problem 4.27 with one missing value (Latin square)",
       c("Suppose that in Problem 4.27 the observation from batch 3 on day 4 is missing. Estimate the missing value and perform the analysis using the value.", ctx("4.27", "p427")),
       character(0), s427(c(3, 4)), roles = list(response = "Time", treatment = "Ingredient", row = "Batch", column = "Day"), labels = LAB427,
       note = "The missing value is estimated in step 5.", tags = c("Latin square", "missing value")),
  prob("4.40", 4, "Chemical yield: Graeco-Latin square",
       "The yield of a chemical process was measured using five batches of raw material, five acid concentrations, five standing times (A, B, C, D, E), and five catalyst concentrations (α, β, γ, δ, ε). The Graeco-Latin square that follows was used. Analyze the data from this experiment (use α = 0.05) and draw conclusions.",
       character(0),
       ps_square("Batch", "Acid concentration", "Standing time", "Yield", as.character(1:5), as.character(1:5),
                 c("A", "B", "C", "D", "E",  "B", "C", "D", "E", "A",  "C", "D", "E", "A", "B",  "D", "E", "A", "B", "C",  "E", "A", "B", "C", "D"),
                 c(26, 16, 19, 16, 13,  18, 21, 18, 11, 21,  20, 12, 16, 25, 13,  15, 15, 22, 14, 17,  10, 24, 17, 17, 14),
                 c("α", "β", "γ", "δ", "ε",  "γ", "δ", "ε", "α", "β",  "ε", "α", "β", "γ", "δ",  "β", "γ", "δ", "ε", "α",  "δ", "ε", "α", "β", "γ"), "Catalyst"),
       roles = list(response = "Yield", treatment = "Standing time", row = "Batch", column = "Acid concentration", greek = "Catalyst"),
       labels = list(response = "yield", treatment = "standing time", row = "batch", column = "acid concentration", greek = "catalyst concentration"),
       note = "Latin letters are the standing times and Greek letters the catalyst concentrations; the ANOVA table tests both.", tags = c("Graeco-Latin square")),
  prob("4.41", 4, "Assembly methods with workplaces: Graeco-Latin square",
       c("Suppose that in Problem 4.28 the engineer suspects that the workplaces used by the four operators may represent an additional source of variation. A fourth factor, workplace (α, β, γ, δ) may be introduced and another experiment conducted, yielding the Graeco-Latin square that follows. Analyze the data from this experiment (use α = 0.05) and draw conclusions.", ctx("4.28", "p428")),
       character(0),
       ps_square("Order", "Operator", "Method", "Time", as.character(1:4), as.character(1:4),
                 c("C", "B", "D", "A",  "B", "C", "A", "D",  "A", "D", "B", "C",  "D", "A", "C", "B"),
                 c(11, 10, 14, 8,  8, 12, 10, 12,  9, 11, 7, 15,  9, 8, 18, 6),
                 c("β", "γ", "δ", "α",  "α", "δ", "γ", "β",  "δ", "α", "β", "γ",  "γ", "β", "α", "δ"), "Workplace"),
       roles = list(response = "Time", treatment = "Method", row = "Order", column = "Operator", greek = "Workplace"),
       labels = list(response = "assembly time", treatment = "assembly method", row = "order of assembly", column = "operator", greek = "workplace"),
       tags = c("Graeco-Latin square")),
  prob("4.45", 4, "Gasoline additives and mileage (balanced incomplete blocks)",
       "An engineer is studying the mileage performance characteristics of five types of gasoline additives. In the road test he wishes to use cars as blocks; however, because of a time constraint, he must use an incomplete block design. He runs the balanced design with the five blocks that follow. Analyze the data from this experiment (use α = 0.05) and draw conclusions.",
       character(0),
       ps_grid("Additive", "Car", "Mileage", as.character(1:5), as.character(1:5),
               c(NA, 17, 14, 13, 12,  14, 14, NA, 13, 10,  12, NA, 13, 12, 9,  13, 11, 11, 12, NA,  11, 12, 10, NA, 8)),
       roles = R_RCBD("Additive", "Car", "Mileage"), labels = list(response = "mileage", treatment = "additive", block = "car"), design = "bibd", tags = c("BIBD")),
  prob("4.47", 4, "Hardwood concentration and paper strength (balanced incomplete blocks)",
       "Seven different hardwood concentrations are being studied to determine their effect on the strength of the paper produced. However, the pilot plant can only produce three runs each day. As days may differ, the analyst uses the BIBD that follows. Analyze the data from this experiment (use α = 0.05) and draw conclusions.",
       character(0),
       ps_grid("Hardwood (%)", "Day", "Strength", c("2", "4", "6", "8", "10", "12", "14"), as.character(1:7),
               c(114, NA, NA, NA, 120, NA, 117,  126, 120, NA, NA, NA, 119, NA,  NA, 137, 117, NA, NA, NA, 134,  141, NA, 129, 149, NA, NA, NA,
                 NA, 145, NA, 150, 143, NA, NA,  NA, NA, 120, NA, 118, 123, NA,  NA, NA, NA, 136, NA, 130, 127)),
       roles = R_RCBD("Hardwood (%)", "Day", "Strength"), labels = list(response = "paper strength", treatment = "hardwood concentration", block = "day"),
       design = "bibd", tags = c("BIBD")),
  prob("4.59", 4, "Problem 4.13 analysed as a completely randomized design",
       c("Consider the direct mail marketing experiment in Problem 4.13. Suppose that this experiment had been run as a completely randomized design, ignoring potential regional differences, but that exactly the same data was obtained. Reanalyze the experiment under this new assumption. What difference would ignoring blocking have on the results and conclusions?", ctx("4.13", "p413")),
       character(0), g413(), roles = list(response = "Responses", treatment = "Design"), labels = LAB413,
       note = "The regions are ignored here, so the problem follows the one-way (Chapter 3) steps. Compare the result with Problem 4.13.", tags = c("one-way ANOVA", "CRD", "blocking"))
)

PROBLEMS <- c(PROBLEMS_CH2, PROBLEMS_CH3, PROBLEMS_CH4)
names(PROBLEMS) <- vapply(PROBLEMS, `[[`, "", "id")

ps_by_chapter <- function(ch) Filter(function(p) p$chapter == PS_CHAPTERS[[ch]]$num, PROBLEMS)
