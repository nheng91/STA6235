library(tidyverse)
library(MASS)
library(olsrr)
library(lindia)
library(car)

x1 <- c(19.5, 24.7, 30.7, 29.8, 19.1, 25.6, 31.4, 27.9,
        22.1, 25.5, 31.1, 30.4, 18.7, 19.7, 14.6, 29.5,
        27.7, 30.2, 22.7, 25.2)
    
x2 <- c(43.1, 49.8, 51.9, 54.3, 42.2, 53.9, 58.5, 52.1,
        49.9, 53.5, 56.6, 56.7, 46.5, 44.2, 42.7, 54.4,
        55.3, 58.6, 48.2, 51.0)

x3 <- c(29.1, 28.2, 37.0, 31.1, 30.9, 23.7, 27.6, 30.6,
        23.2, 24.8, 30.0, 28.3, 23.0, 28.6, 21.3, 30.1,
        25.7, 24.6, 27.1, 27.5)

y <- c(11.9, 22.8, 18.7, 20.1, 12.9, 21.7, 27.1, 25.4,
       21.3, 19.3, 25.4, 27.2, 11.7, 17.8, 12.8, 23.9,
       22.6, 25.4, 14.8, 21.1)

one <- tibble(y, x1, x2, x3)

m1 <- lm(y ~ x1 + x2 + x3, data=one)

# studentized deleted residuals
one$studres <- studres(m1)

# create flag variable
one$studres_flag <- if_else(abs(one$studres)>=3, 1, 0)

# count the number with an issue
table(one$studres_flag)

# filter to the observations we're concerned about
two <- filter(one, studres_flag==1)

# visualize the residuals
ols_plot_resid_stud(m1)

# hat diagonal
one$hat <- hatvalues(m1)

# create flag variable
# compare to 2p/n; 2(4)/20=8/20=0.4
one$hat_flag <- if_else(abs(one$hat)>=0.4, 1, 0)

# count the number with an issue
table(one$hat_flag)

# filter to the observations we're concerned about
two <- filter(one, hat_flag==1)

# Cook's distance
ols_plot_cooksd_chart(m1) + theme_minimal()
gg_cooksd(m1) + theme_bw()

# DFBETAS and DFFITS
# (and Cook's distance and hat diagonal)
m.inf <- as_tibble(influence.measures(m1)[1])

#VIF
vif(m1)

m2 <- lm(y ~ x2 + x3, data = one)
vif(m2)

m3 <- lm(y ~ x1 + x3, data = one)
vif(m3)

m4 <- lm(y ~ x2 + x1, data = one)
vif(m4)

# ultimately, x1 and x2 cannot be in the same model together
# should discuss this with collaborator to see if they want to
# (1) drop one of the predictors all together
# (2) report one model with x1 and, separately, one model with x2

# what happens if we include an interaction?
m5 <- lm(y ~ x2 + x3 + x2:x3, data = one)
vif(m5)

# drop points from dataset for sensitivity analysis
one$i <- as.numeric(rownames(one))
two <- filter(one, i != 1 & i !=3)

# then recreate models using the new dataset
m1 <- lm(y ~ x2 + x3, data=one)
m2 <- lm(y ~ x2 + x3, data=two)
summary(m1)
summary(m2)





