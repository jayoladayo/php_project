# One, two and k sample tests, following the class script "One, Two & K Samples Analysis".
# Every test returns the same shape so the results page can show them side by side.

ALT_NAMES <- c(two.sided = "two-sided", less = "less than", greater = "greater than")
alt_sym <- function(alt) switch(alt, two.sided = "≠", less = "<", greater = ">")

verdict_p <- function(p, alpha) if (is.na(p)) "na" else if (p < alpha) "fail" else if (alpha < 0.10 && p < 0.10) "borderline" else "pass"

sw_test <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || length(x) > 5000 || stats::var(x) == 0) return(list(W = NA_real_, p = NA_real_))
  s <- stats::shapiro.test(x); list(W = unname(s$statistic), p = s$p.value)
}

p_from <- function(stat, alt, cdf) switch(alt, two.sided = 2 * min(cdf(stat), 1 - cdf(stat)), less = cdf(stat), greater = 1 - cdf(stat))

# Confidence bounds the way t.test reports them: two-sided, or one-sided for "less"/"greater".
ci_from <- function(est, se, conf, alt, q) {
  switch(alt,
         two.sided = { h <- q(1 - (1 - conf) / 2) * se; c(est - h, est + h) },
         less = c(-Inf, est + q(conf) * se),
         greater = c(est - q(conf) * se, Inf))
}

mk_test <- function(key, name, family, stat_name, stat, p, est_name, est, lwr = NA, upr = NA,
                    df = NA, dist = "none", about = "", note = NULL, effect = NULL) {
  list(key = key, name = name, family = family, stat_name = stat_name, stat = unname(stat), p = unname(p),
       est_name = est_name, est = unname(est), lwr = unname(lwr), upr = unname(upr), df = unname(df), dist = dist,
       about = about, note = note, effect = effect)
}

from_htest <- function(h, key, name, family, stat_name, est_name, est = NULL, dist = "none", about = "", note = NULL) {
  ci <- h$conf.int %||% c(NA, NA)
  e <- est %||% (if (length(h$estimate) == 2) unname(h$estimate[1] - h$estimate[2]) else unname(h$estimate[1] %||% NA))
  mk_test(key, name, family, stat_name, h$statistic, h$p.value, est_name, e, ci[1], ci[2],
          df = if (!is.null(h$parameter)) unname(h$parameter[1]) else NA, dist = dist, about = about, note = note)
}

quiet <- function(expr) tryCatch(suppressWarnings(expr), error = function(e) NULL)

sign_test <- function(x, mu, alt, conf) {
  d <- x - mu; d <- d[d != 0]
  if (!length(d)) return(NULL)
  b <- stats::binom.test(sum(d > 0), length(d), 0.5, alternative = alt, conf.level = conf)
  mk_test("sign", "Sign test", "nonpar", "Positive signs", b$statistic, b$p.value, "Median", stats::median(x),
          about = sprintf("Counts how many values are above %s and compares with a fair coin. Needs no assumption about the shape of the data.", format(mu)),
          note = sprintf("%d of %d non-tied values are above %s.", sum(d > 0), length(d), format(mu)))
}

describe_vec <- function(x, conf = 0.95) {
  x <- x[is.finite(x)]; n <- length(x)
  se <- if (n > 1) stats::sd(x) / sqrt(n) else NA
  tc <- if (n > 1) stats::qt(1 - (1 - conf) / 2, n - 1) else NA
  data.frame(n = n, mean = mean(x), sd = if (n > 1) stats::sd(x) else NA, se = se, median = stats::median(x),
             min = min(x), max = max(x), lwr = mean(x) - tc * se, upr = mean(x) + tc * se)
}

# Recommended test: parametric when the data look normal (or every sample has n >= 30), else rank-based.
normal_ok <- function(status, n) all(status %in% c("pass", "borderline") | n >= 30)

pick <- function(method, param, nonpar, normal) {
  if (method == "param") param else if (method == "nonpar") nonpar else if (normal) param else nonpar
}

# ---------- One sample ----------
run_one <- function(x, s) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 2) stop("At least 2 values are needed.")
  mu <- s$mu %||% 0; alt <- s$alt %||% "two.sided"; conf <- s$conf %||% 0.95; alpha <- s$alpha %||% 0.05
  sigma <- s$sigma; has_sigma <- !is.null(sigma) && is.finite(sigma) && sigma > 0
  sw <- sw_test(x); st <- verdict_p(sw$p, alpha)
  tests <- list()
  if (has_sigma) {
    se <- sigma / sqrt(n); z <- (mean(x) - mu) / se; ci <- ci_from(mean(x), se, conf, alt, stats::qnorm)
    tests$z <- mk_test("z", "One-sample z-test", "param", "z", z, p_from(z, alt, stats::pnorm), "Mean", mean(x), ci[1], ci[2],
                       dist = "z", about = sprintf("Uses the known population standard deviation σ = %s.", format(sigma)))
  }
  tt <- stats::t.test(x, mu = mu, alternative = alt, conf.level = conf)
  tests$t <- from_htest(tt, "t", "One-sample t-test", "param", "t", "Mean", dist = "t",
                        about = "Compares the sample mean with the target using the sample standard deviation.")
  tests$t$effect <- list(name = "Cohen's d", value = (mean(x) - mu) / stats::sd(x))
  w <- quiet(stats::wilcox.test(x, mu = mu, alternative = alt, conf.level = conf, conf.int = TRUE, exact = FALSE))
  if (!is.null(w)) tests$wilcox <- from_htest(w, "wilcox", "Wilcoxon signed-rank test", "nonpar", "V", "Pseudo-median",
                                              about = "Ranks the distances from the target. Assumes a symmetric distribution, not a normal one.")
  sg <- sign_test(x, mu, alt, conf); if (!is.null(sg)) tests$sign <- sg
  normal <- normal_ok(st, n)
  rec <- pick(s$method %||% "auto", if (has_sigma) "z" else "t", "wilcox", normal)
  list(kind = "one", tests = tests, recommend = rec, normal = normal,
       normality = data.frame(sample = s$names[1] %||% "Sample", n = n, W = sw$W, p = sw$p, status = st, stringsAsFactors = FALSE),
       desc = cbind(sample = s$names[1] %||% "Sample", describe_vec(x, conf)), data = list(x), settings = s)
}

# ---------- Two independent samples ----------
run_two <- function(x1, x2, s) {
  x1 <- x1[is.finite(x1)]; x2 <- x2[is.finite(x2)]
  if (length(x1) < 2 || length(x2) < 2) stop("Each sample needs at least 2 values.")
  d0 <- s$mu %||% 0; alt <- s$alt %||% "two.sided"; conf <- s$conf %||% 0.95; alpha <- s$alpha %||% 0.05
  nm <- s$names %||% c("Sample 1", "Sample 2")
  sw1 <- sw_test(x1); sw2 <- sw_test(x2)
  st <- c(verdict_p(sw1$p, alpha), verdict_p(sw2$p, alpha)); n <- c(length(x1), length(x2))
  ft <- quiet(stats::var.test(x1, x2))
  y <- c(x1, x2); g <- factor(rep(nm, n), levels = nm)
  lev <- quiet(car::leveneTest(y ~ g, center = stats::median))
  vt <- data.frame(test = c(sprintf("F test (var %s / var %s)", nm[1], nm[2]), "Levene (median)"),
                   statistic = c("F", "W"), value = c(if (!is.null(ft)) unname(ft$statistic) else NA, if (!is.null(lev)) lev$`F value`[1] else NA),
                   df = c(if (!is.null(ft)) paste(ft$parameter, collapse = ", ") else NA, if (!is.null(lev)) paste(lev$Df, collapse = ", ") else NA),
                   p = c(if (!is.null(ft)) ft$p.value else NA, if (!is.null(lev)) lev$`Pr(>F)`[1] else NA), stringsAsFactors = FALSE)
  equal_var <- switch(s$var_mode %||% "auto", equal = TRUE, unequal = FALSE, all(is.na(vt$p) | vt$p >= alpha))
  tests <- list()
  pooled <- stats::t.test(x1, x2, mu = d0, alternative = alt, conf.level = conf, var.equal = TRUE)
  tests$pooled <- from_htest(pooled, "pooled", "Pooled two-sample t-test", "param", "t", "Mean difference", dist = "t",
                             about = "Assumes both groups have the same variance and pools them into one estimate.")
  sp <- sqrt(((n[1] - 1) * stats::var(x1) + (n[2] - 1) * stats::var(x2)) / (sum(n) - 2))
  tests$pooled$effect <- list(name = "Cohen's d", value = (mean(x1) - mean(x2)) / sp)
  welch <- stats::t.test(x1, x2, mu = d0, alternative = alt, conf.level = conf, var.equal = FALSE)
  tests$welch <- from_htest(welch, "welch", "Welch t-test", "param", "t", "Mean difference", dist = "t",
                            about = "Does not assume equal variances; the degrees of freedom are adjusted (Welch-Satterthwaite).")
  s1 <- s$sigma; s2 <- s$sigma2 %||% s$sigma
  if (!is.null(s1) && !is.null(s2) && is.finite(s1) && is.finite(s2) && s1 > 0 && s2 > 0) {
    se <- sqrt(s1^2 / n[1] + s2^2 / n[2]); est <- mean(x1) - mean(x2); z <- (est - d0) / se
    ci <- ci_from(est, se, conf, alt, stats::qnorm)
    tests$z <- mk_test("z", "Two-sample z-test", "param", "z", z, p_from(z, alt, stats::pnorm), "Mean difference", est, ci[1], ci[2], dist = "z",
                       about = sprintf("Uses known population standard deviations σ1 = %s and σ2 = %s.", format(s1), format(s2)))
  }
  w <- quiet(stats::wilcox.test(x1, x2, mu = d0, alternative = alt, conf.level = conf, conf.int = TRUE, exact = FALSE))
  if (!is.null(w)) tests$mw <- from_htest(w, "mw", "Mann-Whitney U (Wilcoxon rank-sum)", "nonpar", "W", "Location shift",
                                          about = "Compares the ranks of the two groups. Does not assume normality.")
  normal <- normal_ok(st, n)
  has_z <- !is.null(tests$z)
  param <- if (has_z) "z" else if (equal_var) "pooled" else "welch"
  rec <- pick(s$method %||% "auto", param, "mw", normal)
  list(kind = "two", tests = tests, recommend = rec, normal = normal, equal_var = equal_var, var_tests = vt,
       normality = data.frame(sample = nm, n = n, W = c(sw1$W, sw2$W), p = c(sw1$p, sw2$p), status = st, stringsAsFactors = FALSE),
       desc = cbind(sample = nm, rbind(describe_vec(x1, conf), describe_vec(x2, conf))), data = list(x1, x2), settings = s)
}

# ---------- Two paired samples ----------
run_paired <- function(x1, x2, s) {
  ok <- is.finite(x1) & is.finite(x2); x1 <- x1[ok]; x2 <- x2[ok]; n <- length(x1)
  if (n < 2) stop("At least 2 complete pairs are needed.")
  d0 <- s$mu %||% 0; alt <- s$alt %||% "two.sided"; conf <- s$conf %||% 0.95; alpha <- s$alpha %||% 0.05
  nm <- s$names %||% c("Sample 1", "Sample 2")
  dd <- x1 - x2; sw <- sw_test(dd); st <- verdict_p(sw$p, alpha)
  tests <- list()
  pt <- stats::t.test(x1, x2, mu = d0, alternative = alt, conf.level = conf, paired = TRUE)
  tests$paired <- from_htest(pt, "paired", "Paired t-test", "param", "t", "Mean difference", dist = "t",
                             about = "Works on the differences within each pair, so unit-to-unit variation cancels out.")
  tests$paired$effect <- list(name = "Cohen's d (differences)", value = (mean(dd) - d0) / stats::sd(dd))
  w <- quiet(stats::wilcox.test(x1, x2, mu = d0, alternative = alt, conf.level = conf, conf.int = TRUE, exact = FALSE, paired = TRUE))
  if (!is.null(w)) tests$wsr <- from_htest(w, "wsr", "Wilcoxon signed-rank (paired)", "nonpar", "V", "Pseudo-median difference",
                                           about = "Ranks the differences within pairs. Assumes they are symmetric, not normal.")
  sg <- sign_test(dd, d0, alt, conf)
  if (!is.null(sg)) { sg$name <- "Sign test (paired)"; sg$est_name <- "Median difference"; tests$sign <- sg }
  normal <- normal_ok(st, n)
  rec <- pick(s$method %||% "auto", "paired", "wsr", normal)
  list(kind = "paired", tests = tests, recommend = rec, normal = normal,
       normality = data.frame(sample = sprintf("Differences (%s − %s)", nm[1], nm[2]), n = n, W = sw$W, p = sw$p, status = st, stringsAsFactors = FALSE),
       desc = cbind(sample = c(nm, "Difference"), rbind(describe_vec(x1, conf), describe_vec(x2, conf), describe_vec(dd, conf))),
       data = list(x1, x2), diffs = dd, settings = s)
}

# ---------- k independent samples ----------
games_howell <- function(y, g, conf = 0.95) {
  lev <- levels(g); k <- length(lev)
  m <- tapply(y, g, mean); v <- tapply(y, g, stats::var); n <- tapply(y, g, length)
  P <- pair_index(k)
  a <- v[P$i] / n[P$i]; b <- v[P$j] / n[P$j]
  diff <- unname(m[P$j] - m[P$i]); se <- unname(sqrt(a + b))
  df <- unname((a + b)^2 / (a^2 / (n[P$i] - 1) + b^2 / (n[P$j] - 1)))
  q <- abs(diff) / se * sqrt(2)
  qc <- stats::qtukey(conf, k, df) / sqrt(2)
  data.frame(pair = paste(lev[P$j], "−", lev[P$i]), i = lev[P$i], j = lev[P$j], diff = diff, se = se, df = df,
             stat = q, p = stats::ptukey(q, k, df, lower.tail = FALSE), lwr = diff - qc * se, upr = diff + qc * se, stringsAsFactors = FALSE)
}

run_k <- function(y, g, s) {
  ok <- is.finite(y) & !is.na(g); y <- y[ok]; g <- droplevels(g[ok])
  if (nlevels(g) < 2) stop("At least 2 groups are needed.")
  if (any(table(g) < 2)) stop("Each group needs at least 2 values.")
  conf <- s$conf %||% 0.95; alpha <- s$alpha %||% 0.05
  d <- data.frame(y = y, treatment = g, .order = seq_along(y))
  fit <- stats::aov(y ~ treatment, data = d)
  an <- anova_table(fit, d, "treatment", list(treatment = s$names[1] %||% "Group"), "I", alpha)
  rs <- stats::residuals(fit); sw <- sw_test(rs)
  per <- lapply(levels(g), function(l) sw_test(y[g == l]))
  lev <- quiet(car::leveneTest(y ~ treatment, data = d))
  bart <- quiet(stats::bartlett.test(y ~ treatment, data = d))
  vt <- data.frame(test = c("Levene (median)", "Bartlett"), statistic = c("F", "K²"),
                   value = c(if (!is.null(lev)) lev$`F value`[1] else NA, if (!is.null(bart)) unname(bart$statistic) else NA),
                   df = c(if (!is.null(lev)) paste(lev$Df, collapse = ", ") else NA, if (!is.null(bart)) unname(bart$parameter) else NA),
                   p = c(if (!is.null(lev)) lev$`Pr(>F)`[1] else NA, if (!is.null(bart)) bart$p.value else NA), stringsAsFactors = FALSE)
  status <- verdict_p(sw$p, alpha)
  equal_var <- switch(s$var_mode %||% "auto", equal = TRUE, unequal = FALSE, is.na(vt$p[1]) || vt$p[1] >= alpha)
  tr <- an$table[1, ]
  tests <- list()
  tests$anova <- mk_test("anova", "One-way ANOVA (F test)", "param", "F", tr$F, tr$p, "η²", tr$ss / an$sst,
                         df = sprintf("%d, %d", tr$df, an$dfe), dist = "F",
                         about = "Assumes normal errors with equal variance in every group.")
  tests$anova$df1 <- tr$df; tests$anova$df2 <- an$dfe
  wt <- stats::oneway.test(y ~ treatment, data = d, var.equal = FALSE)
  tests$welch <- mk_test("welch", "Welch ANOVA", "param", "F", wt$statistic, wt$p.value, NULL, NA,
                         df = sprintf("%d, %.2f", round(wt$parameter[1]), wt$parameter[2]), dist = "F",
                         about = "Does not assume equal variances (oneway.test with var.equal = FALSE).")
  tests$welch$df1 <- unname(wt$parameter[1]); tests$welch$df2 <- unname(wt$parameter[2])
  kw <- stats::kruskal.test(y ~ treatment, data = d); N <- length(y)
  tests$kw <- mk_test("kw", "Kruskal-Wallis test", "nonpar", "χ²", kw$statistic, kw$p.value, "ε²",
                      unname(kw$statistic) / ((N^2 - 1) / (N + 1)), df = unname(kw$parameter), dist = "chisq",
                      about = "Compares the ranks of all groups. Does not assume normality.")
  tests$kw$df1 <- unname(kw$parameter)
  ph <- posthoc_all(fit, d, "treatment", an$mse, an$dfe, alpha, conf)
  gh <- games_howell(y, g, conf); gh$signif <- gh$p < alpha
  dn <- dunn_test(y, g, "holm")
  posthoc <- list(tukey = ph$methods$tukey, gh = gh, dunn = dn)
  normal <- normal_ok(status, min(table(g)))
  param <- if (equal_var) "anova" else "welch"
  rec <- pick(s$method %||% "auto", param, "kw", normal)
  letters <- list(tukey = ph$letters$tukey,
                  gh = cld_letters(levels(g), as.numeric(tapply(y, g, mean)), gh$i[gh$signif], gh$j[gh$signif]),
                  dunn = cld_letters(levels(g), as.numeric(tapply(rank(y), g, mean)), dn$i[dn$p < alpha], dn$j[dn$p < alpha]))
  desc <- do.call(rbind, lapply(levels(g), function(l) cbind(sample = l, describe_vec(y[g == l], conf))))
  list(kind = "k", tests = tests, recommend = rec, normal = normal, equal_var = equal_var, var_tests = vt,
       anova = an, fit = fit, posthoc = posthoc, letters = letters, means = ph$means,
       normality = rbind(data.frame(sample = "Residuals (all groups)", n = length(rs), W = sw$W, p = sw$p, status = status, stringsAsFactors = FALSE),
                         data.frame(sample = levels(g), n = as.integer(table(g)), W = vapply(per, `[[`, 0, "W"), p = vapply(per, `[[`, 0, "p"),
                                    status = vapply(per, function(z) verdict_p(z$p, alpha), ""), stringsAsFactors = FALSE)),
       desc = desc, data = list(y = y, g = g), settings = s)
}

# Which posthoc goes with which main test.
K_POSTHOC <- c(anova = "tukey", welch = "gh", kw = "dunn")
K_POSTHOC_NAMES <- c(tukey = "Tukey HSD", gh = "Games-Howell", dunn = "Dunn (Holm)")
