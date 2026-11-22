# Independent check of paper 11 (Adawi et al. 2024): one-way ANOVA from the published Table 2 (means, SDs, n = 16), and from the rebuilt data. Base R only.
s <- read.csv('../data/source_supplement/Adawi2024_Table2_published_means_SD.csv', check.names = FALSE)
n <- s$n; m <- s[['Mean (MPa)']]; sd <- s[['SD (MPa)']]; k <- length(n); N <- sum(n); gm <- sum(n * m) / N
ssb <- sum(n * (m - gm)^2); sse <- sum((n - 1) * sd^2); F <- (ssb / (k - 1)) / (sse / (N - k))
cat(sprintf('From Table 2: SS between = %.4f, SS error = %.4f, F(%d, %d) = %.4f, p = %.3g, eta^2 = %.4f\n', ssb, sse, k - 1, N - k, F, pf(F, k - 1, N - k, lower.tail = FALSE), ssb / (ssb + sse)))
cat('The paper prints F = 13.55 (df 4), p = 0.00001 in its text; it does not follow from Table 2.\n\n')
d <- read.csv('../data/Adawi2024_bond_strength_5_aging_groups_reconstructed.csv', check.names = FALSE)
d[['Aging group']] <- factor(d[['Aging group']], levels = unique(d[['Aging group']]))
print(aggregate(`Shear bond strength (MPa)` ~ `Aging group`, d, function(v) c(n = length(v), mean = mean(v), sd = sd(v))))
print(summary(aov(`Shear bond strength (MPa)` ~ `Aging group`, d)))
