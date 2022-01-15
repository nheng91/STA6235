library(tidyverse)
library(googlesheets4)
library(corrplot)
library(olsrr)

# library(readxl)

almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

data <- read_sheet("https://docs.google.com/spreadsheets/d/1S7YbhQG0_n_8L1qyY2di4ASfjjQNKk1hXG1lE-kNoek/edit#gid=2000654661")

data$i <- as.numeric(rownames(data))
# want first 54; we will use second 54 later for model validation
first <- filter(data, i<55)

################### explore which outcome to use #####################

# model y as a function of x1-x4
m1 <- lm(y ~ x1 + x2 + x3 + x4, data = first)
almost_sas(m1)

# model ln(y) as a function of x1-x4
# looking at ln(y) because y ~ x1 + x2 + x3 + x4 is skewed
m2 <- lm(ln_y ~ x1 + x2 + x3 + x4, data = first)
almost_sas(m2)

# matrix of scatterplots
data1 <- first[, c(1:4, 10)]
plot(data1, pch = 20, cex = 1.5, col = "#69b3a2")

# correlation matrix
M <- cor(data1)
cor(data1)

# visualize the correlation matrix
corrplot(M, method = "number")

m3 <- lm(ln_y ~ x1 + x2, data = first)

# R^2 and R^2 adj
s1 <- summary(m1)
s1$r.squared
s1$adj.r.squared

############### looking at model fit #################

m1 <- lm(ln_y ~ x1+x2+x3+x4, data = first)
m2 <- lm(ln_y ~ x4, data = first)

# Mallow's C 
ols_mallows_cp(m2, m1)

# AIC and BIC
ols_aic(m2, method = "SAS") # uses the formula shown in class and uses SSE
AIC(m2) # this uses the likelihood function
ols_sbic(m2, m1)
BIC(m2)
BIC(m1)

# PRESS
ols_press(m2)
