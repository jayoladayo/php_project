# Independent check of paper 14 (Bacharach, Chance & Middleton 1940): the 6 x 6 Latin square ANOVA. Base R only.
d <- read.csv('../data/Bacharach1940_blister_area_6x6_latin_square.csv', check.names = FALSE)
for (v in c('Position', 'Rabbit', 'Order of injection')) d[[v]] <- factor(d[[v]])
print(table(d[['Order of injection']], d$Position))   # every order once in every position
print(summary(aov(`Blister area (cm2)` ~ `Order of injection` + Position + Rabbit, d)))
cat('Paper: F(order) = 0.172, F(positions) = 1.168, F(rabbits) = 3.910\n')
