# Independent check of paper 15 (Chan & Asrah 2026): 3 x 3 x 3 factorial ANOVA. Base R only.
d <- read.csv('../data/ChanAsrah2026_shelf_life_3x3x3_factorial.csv', check.names = FALSE, na.strings = character(0))   # na.strings: a level called None must not become NA
for (v in c('Preservative', 'Moisture control', 'Storage condition')) d[[v]] <- factor(d[[v]])
print(table(d$Preservative)); fit <- aov(`Shelf life (days)` ~ Preservative * `Moisture control` * `Storage condition`, d)
a <- anova(fit); print(a); cat('Total SS =', round(sum(a[['Sum Sq']]), 3), '(paper: 2522.10)\n')
