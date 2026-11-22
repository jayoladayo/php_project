# Independent check of paper 12 (Chacrabati et al. 2013): one-way ANOVA of the rebuilt live weights, and the same F from the published means and SEs. Base R only.
s <- read.csv('../data/source_supplement/Chacrabati2013_published_means_SE.csv', check.names = FALSE)
n <- s$n; m <- s[['Mean live weight (g)']]; sd <- s$SE * sqrt(n); k <- length(n); N <- sum(n); gm <- sum(n * m) / N
ssb <- sum(n * (m - gm)^2); sse <- sum((n - 1) * sd^2); F <- (ssb / (k - 1)) / (sse / (N - k))
cat(sprintf('From the published means and SE: F(%d, %d) = %.4f, p = %.3g\n\n', k - 1, N - k, F, pf(F, k - 1, N - k, lower.tail = FALSE)))
d <- read.csv('../data/Chacrabati2013_broiler_live_weight_4_treatments_reconstructed.csv', check.names = FALSE)
d$Treatment <- factor(d$Treatment, levels = unique(d$Treatment))
print(aggregate(`Live weight (g)` ~ Treatment, d, mean)); print(summary(aov(`Live weight (g)` ~ Treatment, d)))
