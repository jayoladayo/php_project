# Charts for one, two and k sample tests and for Descriptive statistics.

sample_cols <- function(n) LEVEL_COLOURS[(seq_len(n) - 1) %% length(LEVEL_COLOURS) + 1]

ylab_s <- function(s) paste0(s$response_name %||% "Value", if (nzchar(s$unit %||% "")) paste0(" (", s$unit, ")") else "")

samples_long <- function(res) {
  if (res$kind == "k") return(data.frame(y = res$data$y, g = res$data$g))
  nm <- res$settings$names %||% paste("Sample", seq_along(res$data))
  data.frame(y = unlist(res$data), g = factor(rep(nm[seq_along(res$data)], lengths(res$data)), levels = nm[seq_along(res$data)]))
}

plot_samples_box <- function(res) {
  d <- samples_long(res); cols <- setNames(sample_cols(nlevels(d$g)), levels(d$g))
  ggplot(d, aes(g, y, fill = g, colour = g)) +
    geom_boxplot(alpha = .18, width = .5, outlier.shape = NA, linewidth = .7) +
    geom_jitter(width = .07, height = 0, size = 2.3, alpha = .85) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    { if (res$kind == "one") geom_hline(yintercept = res$settings$mu %||% 0, colour = RED, linetype = "dashed") } +
    scale_fill_manual(values = cols) + scale_colour_manual(values = cols) +
    labs(x = NULL, y = ylab_s(res$settings)) + theme_sds()
}

plot_paired <- function(res) {
  nm <- res$settings$names; x1 <- res$data[[1]]; x2 <- res$data[[2]]; ok <- is.finite(x1) & is.finite(x2)
  d <- data.frame(pair = rep(seq_len(sum(ok)), 2), g = factor(rep(nm, each = sum(ok)), levels = nm), y = c(x1[ok], x2[ok]))
  d$up <- rep(x1[ok] > x2[ok], 2)
  ggplot(d, aes(g, y, group = pair)) + geom_line(aes(colour = up), linewidth = .8, alpha = .8) +
    geom_point(aes(fill = g), shape = 21, colour = "white", size = 3.2) +
    scale_colour_manual(values = c(`TRUE` = GREEN, `FALSE` = "#8A8F9C")) +
    scale_fill_manual(values = setNames(sample_cols(2), nm)) +
    labs(x = NULL, y = ylab_s(res$settings), subtitle = sprintf("One line per pair; green where %s is higher", nm[1])) +
    theme_sds() + theme(plot.subtitle = element_text(colour = INK, size = 10.5))
}

# Rejection region for t, z, F or chi-squared, one- or two-sided.
plot_dist_alt <- function(type, stat, df1 = NA, df2 = NA, alpha = 0.05, alt = "two.sided") {
  if (type %in% c("F", "chisq")) return(plot_dist(type, stat, df1, df2, alpha))
  q <- if (type == "z") stats::qnorm else function(p) stats::qt(p, df1)
  dn <- if (type == "z") stats::dnorm else function(x) stats::dt(x, df1)
  crit <- switch(alt, two.sided = c(q(alpha / 2), q(1 - alpha / 2)), less = q(alpha), greater = q(1 - alpha))
  lim <- max(4, abs(stat) * 1.15, abs(crit) * 1.4); lim <- min(lim, 12)
  x <- seq(-lim, lim, length.out = 500); d <- data.frame(x = x, y = dn(x))
  rej <- switch(alt, two.sided = d$x <= crit[1] | d$x >= crit[2], less = d$x <= crit, greater = d$x >= crit)
  shown <- max(min(stat, lim), -lim); top <- max(d$y)
  lab <- if (type == "z") "z" else "t"
  ggplot(d, aes(x, y)) +
    geom_area(data = transform(d, y = ifelse(rej, y, NA)), fill = RED, alpha = .25, na.rm = TRUE) +
    geom_line(colour = INK, linewidth = .8) +
    geom_vline(xintercept = crit, colour = RED, linetype = "dashed") +
    geom_vline(xintercept = shown, colour = GREEN, linewidth = 1.1) +
    annotate("text", x = crit, y = top * .95, label = sprintf("%.2f", crit), hjust = ifelse(crit < 0, 1.1, -0.1), colour = RED, size = 3.6) +
    annotate("text", x = shown, y = top * .6, label = sprintf("%s = %.2f%s", lab, stat, if (abs(stat) > lim) " →" else ""),
             hjust = if (shown > 0) 1.08 else -0.08, colour = GREEN, size = 3.8, fontface = "bold") +
    labs(x = if (type == "z") "Standard normal" else sprintf("t(%s)", format(round(df1, 2))), y = NULL) +
    theme_sds() + theme(axis.text.y = element_blank())
}

# One confidence interval against the hypothesised value.
plot_ci_one <- function(t, ref = 0, conf = 0.95, unit = "") {
  lo <- t$lwr; hi <- t$upr; est <- t$est
  span <- max(abs(c(est, ref, lo[is.finite(lo)], hi[is.finite(hi)]) - est), 1e-9)
  lo2 <- if (is.finite(lo)) lo else est - span * 1.6; hi2 <- if (is.finite(hi)) hi else est + span * 1.6
  d <- data.frame(y = t$est_name %||% "Estimate", est = est, lo = lo2, hi = hi2)
  excl <- (is.finite(lo) && lo > ref) || (is.finite(hi) && hi < ref)
  col <- if (excl) GREEN else "#8A8F9C"
  ggplot(d, aes(est, y)) + geom_vline(xintercept = ref, colour = RED, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = .15, linewidth = 1.1, colour = col) +
    geom_point(size = 3.4, colour = col) +
    { if (!is.finite(lo)) annotate("text", x = lo2, y = 1.25, label = "← −∞", colour = INK, size = 3.4, hjust = 0) } +
    { if (!is.finite(hi)) annotate("text", x = hi2, y = 1.25, label = "+∞ →", colour = INK, size = 3.4, hjust = 1) } +
    labs(x = sprintf("%s%s, %g%% interval", t$est_name %||% "Estimate", if (nzchar(unit)) paste0(" (", unit, ")") else "", 100 * conf), y = NULL) +
    theme_sds() + theme(axis.text.y = element_blank())
}

plot_hist <- function(x, xlab = "Value", bins = NULL) {
  x <- x[is.finite(x)]; n <- length(x)
  bins <- bins %||% max(5, min(20, ceiling(log2(n) + 1)))
  d <- data.frame(x = x)
  p <- ggplot(d, aes(x)) + geom_histogram(bins = bins, fill = GREEN, colour = "white", alpha = .85)
  if (n > 2 && stats::sd(x) > 0) {
    bw <- diff(range(x)) / bins; if (bw == 0) bw <- 1
    xs <- seq(min(x) - stats::sd(x), max(x) + stats::sd(x), length.out = 200)
    p <- p + geom_line(data = data.frame(x = xs, y = stats::dnorm(xs, mean(x), stats::sd(x)) * n * bw), aes(x, y), colour = GOLD, linewidth = 1)
  }
  p + labs(x = xlab, y = "Count") + theme_sds()
}

plot_qq_vec <- function(x, ylab = "Sample quantiles") {
  x <- sort(x[is.finite(x)]); n <- length(x); q <- stats::qnorm(stats::ppoints(n))
  ggplot(data.frame(q = q, x = x), aes(q, x)) +
    geom_abline(slope = stats::sd(x), intercept = mean(x), colour = GREEN, linewidth = .9) +
    geom_point(colour = PETROL, size = 2.6) + labs(x = "Theoretical quantiles", y = ylab) + theme_sds()
}

plot_box_by <- function(y, g, ylab) {
  d <- data.frame(y = y, g = g); d <- d[is.finite(d$y) & !is.na(d$g), ]
  cols <- setNames(sample_cols(nlevels(d$g)), levels(d$g))
  ggplot(d, aes(g, y, fill = g, colour = g)) + geom_boxplot(alpha = .18, width = .55, outlier.shape = 21, linewidth = .7) +
    geom_jitter(width = .07, height = 0, size = 2, alpha = .7) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    scale_fill_manual(values = cols) + scale_colour_manual(values = cols) + labs(x = NULL, y = ylab) + theme_sds()
}

plot_scatter <- function(x, y, xlab, ylab) {
  d <- data.frame(x = x, y = y); d <- d[is.finite(d$x) & is.finite(d$y), ]
  ggplot(d, aes(x, y)) + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = GREEN, fill = GREEN, alpha = .12) +
    geom_point(colour = PETROL, size = 2.6) + labs(x = xlab, y = ylab) + theme_sds()
}

plot_corr_heat <- function(m) {
  d <- expand.grid(a = rownames(m), b = colnames(m), stringsAsFactors = FALSE); d$r <- as.vector(m)
  d$a <- factor(d$a, levels = rownames(m)); d$b <- factor(d$b, levels = rev(colnames(m)))
  ggplot(d, aes(a, b, fill = r)) + geom_tile(colour = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.2f", r)), colour = "#1F2321", size = 3.8) +
    scale_fill_gradient2(low = "#A42A1F", mid = "#F4F4F0", high = GREEN, limits = c(-1, 1), name = "r") +
    labs(x = NULL, y = NULL) + theme_sds() + theme(panel.grid = element_blank(), legend.position = "right")
}

plot_qq_groups <- function(xs, nm) {
  d <- do.call(rbind, lapply(seq_along(xs), function(i) { x <- sort(xs[[i]][is.finite(xs[[i]])])
    data.frame(g = nm[i], q = stats::qnorm(stats::ppoints(length(x))), x = x, m = mean(x), s = stats::sd(x)) }))
  d$g <- factor(d$g, levels = nm)
  ggplot(d, aes(q, x, colour = g)) + geom_abline(aes(slope = s, intercept = m, colour = g), linewidth = .8) +
    geom_point(size = 2.6) + facet_wrap(~g, scales = "free_y") +
    scale_colour_manual(values = setNames(sample_cols(length(nm)), nm)) +
    labs(x = "Theoretical quantiles", y = "Sample quantiles") + theme_sds()
}

plot_hist_groups <- function(xs, nm, xlab) {
  d <- data.frame(g = factor(rep(nm, lengths(xs)), levels = nm), x = unlist(xs)); d <- d[is.finite(d$x), ]
  bins <- max(5, min(15, ceiling(log2(nrow(d)) + 1)))
  ggplot(d, aes(x, fill = g)) + geom_histogram(bins = bins, colour = "white", alpha = .8, position = "identity") +
    facet_wrap(~g, ncol = 1) + scale_fill_manual(values = setNames(sample_cols(length(nm)), nm)) +
    labs(x = xlab, y = "Count") + theme_sds()
}
