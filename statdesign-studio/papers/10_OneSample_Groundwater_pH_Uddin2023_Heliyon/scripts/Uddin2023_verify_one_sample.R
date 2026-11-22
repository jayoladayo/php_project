# Independent check of paper 10 (Uddin et al. 2023): one-sample t-test of the 18 groundwater pH values against 8.5. Base R only.
d <- read.csv('../data/Uddin2023_groundwater_pH_9_sites_2_seasons.csv', check.names = FALSE)
x <- d[['Groundwater pH']]; cat('n =', length(x), ' mean =', round(mean(x), 4), ' sd =', round(sd(x), 4), '\n')
tt <- t.test(x, mu = 8.5); print(tt)
cat(sprintf('t = %.4f, df = %d, p = %.3g (paper: t = -9.52, df = 17, p < 0.05)\n', tt$statistic, tt$parameter, tt$p.value))
