# Charts for the Results step (ggplot2), styled to sit on light and dark cards.

library(ggplot2)

INK <- "#5B606B"; GRID <- "#8A8F9C33"; GREEN <- "#008540"; PETROL <- "#003E51"; RED <- "#C0392B"; GOLD <- "#AA8A00"

theme_sds <- function(base = 12.5) {
  theme_minimal(base_size = base, base_family = "") +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_line(colour = GRID, linewidth = .4),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = INK), axis.title = element_text(colour = INK, size = base * .9),
          legend.position = "none", plot.margin = margin(8, 12, 8, 8),
          strip.text = element_text(colour = INK, face = "bold"))
}

level_cols <- function(L, levels) {
  if (is.null(L)) return(setNames(rep(GREEN, length(levels)), levels))
  setNames(L$colour[seq_along(levels)], make.unique(L$label))[levels]
}

ylab_of <- function(res) {
  s <- res$settings
  paste0(s$response_name %||% res$map$response, if (nzchar(s$unit %||% "")) paste0(" (", s$unit, ")") else "")
}

plot_means <- function(res, term = "treatment", method = "tukey") {
  ph <- res$posthoc[[term]]; m <- ph$means
  m$level <- factor(m$level, levels = m$level)
  m$letter <- ph$letters[[method]][as.character(m$level)] %||% ""
  d <- res$data; d$level <- d[[term]]
  cols <- level_cols(res$settings$labels[[term]], levels(m$level))
  top <- max(c(m$upr, d$y), na.rm = TRUE); rng <- diff(range(c(m$lwr, d$y), na.rm = TRUE))
  ggplot(m, aes(level, mean, colour = level)) +
    geom_jitter(data = d, aes(level, y), width = .08, height = 0, alpha = .45, size = 2.2) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = .12, linewidth = .9) +
    geom_point(shape = 15, size = 4) +
    geom_text(aes(y = top + rng * .08, label = letter), colour = INK, fontface = "bold", size = 4.6) +
    scale_colour_manual(values = cols) +
    labs(x = res$names[[term]], y = ylab_of(res)) + theme_sds()
}

plot_box <- function(res, term = "treatment") {
  d <- res$data; d$level <- d[[term]]
  cols <- level_cols(res$settings$labels[[term]], levels(d$level))
  ggplot(d, aes(level, y, fill = level, colour = level)) +
    geom_boxplot(alpha = .18, width = .55, outlier.shape = NA, linewidth = .7) +
    geom_jitter(width = .08, height = 0, size = 2.3, alpha = .8) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    scale_fill_manual(values = cols) + scale_colour_manual(values = cols) +
    labs(x = res$names[[term]], y = ylab_of(res)) + theme_sds()
}

plot_profile <- function(res) {
  d <- res$data
  cols <- level_cols(res$settings$labels$treatment, levels(d$treatment))
  m <- stats::aggregate(y ~ treatment + block, data = d, FUN = mean)
  ggplot(m, aes(treatment, y, group = block)) +
    geom_line(colour = "#8A8F9C", linewidth = .7) +
    geom_point(aes(colour = treatment), size = 3) +
    geom_text(data = m[m$treatment == levels(m$treatment)[nlevels(m$treatment)], ],
              aes(label = block), hjust = -.6, colour = INK, size = 3.6) +
    scale_colour_manual(values = cols) +
    labs(x = res$names$treatment, y = ylab_of(res), subtitle = sprintf("One line per %s", res$names$block)) + theme_sds() +
    theme(plot.subtitle = element_text(colour = INK, size = 10.5))
}

plot_square <- function(res) {
  d <- res$data
  lab <- if (res$design$key == "GLSD") paste0(d$treatment, "\n", d$greek, "\n", format(d$y)) else paste0(d$treatment, "\n", format(d$y))
  ggplot(d, aes(column, row, fill = y)) + geom_tile(colour = "white", linewidth = 1.2) +
    geom_text(aes(label = lab), colour = "#1F2321", size = 3.6, lineheight = .9) +
    scale_fill_gradient(low = "#E8F4EE", high = "#5FB98A") +
    scale_y_discrete(limits = rev) +
    labs(x = res$names$column, y = res$names$row) + theme_sds() + theme(panel.grid = element_blank())
}

plot_qq <- function(res) {
  r <- res$assumptions$resid; r <- r[is.finite(r$std), ]
  n <- nrow(r); q <- stats::qnorm(stats::ppoints(n)); s <- sort(r$std)
  # pointwise 95% band for normal order statistics
  p <- stats::ppoints(n); se <- sqrt(p * (1 - p) / n) / stats::dnorm(q)
  band <- data.frame(q = q, lo = q - 1.96 * se, hi = q + 1.96 * se)
  ggplot() + geom_ribbon(data = band, aes(q, ymin = lo, ymax = hi), fill = GREEN, alpha = .10) +
    geom_abline(slope = 1, intercept = 0, colour = GREEN, linewidth = .9) +
    geom_point(aes(q, s), colour = PETROL, size = 2.6) +
    labs(x = "Theoretical quantiles", y = "Standardised residuals") + theme_sds()
}

plot_rvf <- function(res) {
  r <- res$assumptions$resid
  ggplot(r, aes(fitted, resid)) + geom_hline(yintercept = 0, colour = RED, linetype = "dashed") +
    geom_point(colour = PETROL, size = 2.6) +
    labs(x = "Fitted values", y = "Residuals") + theme_sds()
}

plot_order <- function(res) {
  r <- res$assumptions$resid
  ggplot(r, aes(order, resid)) + geom_hline(yintercept = 0, colour = RED, linetype = "dashed") +
    geom_line(colour = GREEN, linewidth = .7) + geom_point(colour = GREEN, size = 2.4) +
    scale_x_continuous(breaks = function(l) unique(round(pretty(l)))) +
    labs(x = "Run order", y = "Residuals") + theme_sds()
}

plot_ci <- function(tab, conf = 0.95, unit = "") {
  tab$pair <- factor(tab$pair, levels = rev(tab$pair))
  tab$sig <- ifelse(tab$signif, "Different", "Not different")
  ggplot(tab, aes(diff, pair, colour = sig)) +
    geom_vline(xintercept = 0, colour = RED, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lwr, xmax = upr), height = .18, linewidth = 1) +
    geom_point(size = 3) +
    scale_colour_manual(values = c("Different" = GREEN, "Not different" = "#8A8F9C")) +
    labs(x = sprintf("Difference in means%s, %g%% interval", if (nzchar(unit)) paste0(" (", unit, ")") else "", 100 * conf), y = NULL) +
    theme_sds()
}

plot_dist <- function(type = c("F", "chisq"), stat, df1, df2 = NULL, alpha = 0.05) {
  type <- match.arg(type)
  crit <- if (type == "F") stats::qf(1 - alpha, df1, df2) else stats::qchisq(1 - alpha, df1)
  xmax <- max(crit * 1.8, min(stat * 1.1, crit * 4), if (type == "F") 6 else df1 + 4 * sqrt(2 * df1))
  x <- seq(0.001, xmax, length.out = 400)
  dens <- if (type == "F") stats::df(x, df1, df2) else stats::dchisq(x, df1)
  dens[!is.finite(dens)] <- NA
  cap <- stats::quantile(dens, .98, na.rm = TRUE) * 1.2; dens <- pmin(dens, cap)
  d <- data.frame(x = x, y = dens)
  shown <- min(stat, xmax)
  ggplot(d, aes(x, y)) + geom_area(data = d[d$x >= crit, ], fill = RED, alpha = .25) +
    geom_line(colour = INK, linewidth = .8) +
    geom_vline(xintercept = crit, colour = RED, linetype = "dashed") +
    geom_vline(xintercept = shown, colour = GREEN, linewidth = 1.1) +
    annotate("text", x = crit, y = max(dens, na.rm = TRUE) * .95, label = sprintf("critical %.2f", crit), hjust = -.08, colour = RED, size = 3.6) +
    annotate("text", x = shown, y = max(dens, na.rm = TRUE) * .75,
             label = sprintf("%s %.2f%s", if (type == "F") "F =" else "χ² =", stat, if (stat > xmax) " →" else ""),
             hjust = if (shown > xmax * .7) 1.08 else -.08, colour = GREEN, size = 3.8, fontface = "bold") +
    labs(x = if (type == "F") sprintf("F(%g, %g)", df1, df2) else sprintf("χ²(%g)", df1), y = NULL) +
    theme_sds() + theme(axis.text.y = element_blank())
}
