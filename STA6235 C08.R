library(tidyverse)

#### EXAMPLE 1 ####

y <- c(17, 26, 21, 30, 22,
        0, 12, 19,  4, 16,
       28, 15, 11, 38, 31,
       21, 20, 13, 30, 14)

x1 <- c(151,  92, 175,  31, 104,
        277, 210, 120, 290, 238,
        164, 272, 295,  68,  85,
        224, 166, 305, 124, 246)

x2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

one <- tibble(y, x1, x2)

m1 <- lm(y ~ x1 + x2, data = one)
summary(m1)

# create interaction term in dataset
one$x1x2 <- one$x1 * one$x2

m2 <- lm(y ~ x1 + x2 + x1x2, data = one)
summary(m2)

# other ways to specify interaction term -- : 
m3 <- lm(y ~ x1 + x2 + x1:x2, data = one)
summary(m3)

# can use * to enumerate all possible combinations
# fine for when we have two variables + one interaction
# gets hairy when we have more than two variables
# e.g., see below (for enumeration)
m4 <- lm(y ~ x1*x2*x1x2, data = one)
summary(m4)


#### EXAMPLE 2 ####

y <- c(218, 248, 360, 351, 470, 394, 332, 321, 410, 260, 241, 331,
       275, 425, 367, 140, 277, 384, 341, 215, 180, 260, 361, 252,
       422, 273, 410)

x1 <- c(100, 125, 220, 205, 300, 255, 225, 175, 270, 170, 155, 190,
        140, 290, 265, 105, 215, 270, 255, 175, 135, 200, 275, 155,
        320, 190, 295)

x2 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

two <- tibble(y, x1, as.factor(x2))

# No differentiation between lines (X2)
p <- ggplot(two, aes(x = x1, y = y)) +
        geom_point() + 
        xlab("Line Speed") +
        ylab("Scrap") +
        theme_minimal()
p

# Differentiate between lines (X2)
p <- ggplot(two, aes(x = x1, y = y, color = as.factor(x2))) +
        geom_point() + 
        scale_color_discrete(name="Line", breaks=c(1,0), labels=c("Line 1","Line 2")) +
        xlab("Line Speed") +
        ylab("Scrap") +
        theme_minimal()
p

m1 <- lm(y ~ x1 + x2 + x1:x2, data=two)
summary(m1)

anova(m1)
