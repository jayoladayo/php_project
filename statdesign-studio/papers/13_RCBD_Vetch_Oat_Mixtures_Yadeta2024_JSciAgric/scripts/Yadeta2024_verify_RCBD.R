# Independent check of paper 13 (Yadeta 2024): RCBD ANOVA of the rebuilt seed yields, and the CV against the printed 18.68 %. Base R only.
d <- read.csv('../data/Yadeta2024_seed_yield_5_treatments_5_blocks_reconstructed.csv', check.names = FALSE)
d$Treatment <- factor(d$Treatment, levels = unique(d$Treatment)); d$Block <- factor(d$Block)
y <- d[['Seed yield (t/ha)']]; print(aggregate(y ~ Treatment, d, mean)); cat('grand mean =', round(mean(y), 4), '\n')
fit <- aov(y ~ Treatment + Block, d); print(summary(fit)); a <- anova(fit)
cat(sprintf('CV = %.4f %% (paper: 18.68 %%)\n', 100 * sqrt(a['Residuals', 'Mean Sq']) / mean(y)))
