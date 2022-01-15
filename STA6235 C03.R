library(tidyverse)
library(olsrr)
library(lmtest)

# function to create assumption assessment plots
# written by Reid Ginoza Fall 2019
# this pulls together the ANOVA assessment plots into a single image
almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}
# can someone edit this function to change the titles on the graphs?

# enter data
x <- c( 80,  30,  50,  90,  70,  60, 120,  80, 100,
        50,  40,  70,  90,  20, 110, 100,  30,  50,
        90, 110,  30,  90,  40,  80,  70)

y <- c(399, 121, 221, 376, 361, 224, 546, 352, 353,
       157, 160, 252, 389, 113, 435, 420, 212, 268,
       377, 421, 273, 468, 244, 342, 323)

# put together as tibble
one <- tibble(x, y)

# model
one_model <- lm(y ~ x, data = one)

# find predicted values
one$pred  <- predict(one_model)  

# find residual values
one$resid <- residuals(one_model)

# Residual against predictor
p <- ggplot(one, aes(x=x, y=resid)) + 
      geom_point() +
      xlab("Number of Bids") + 
      ylab("Residual") +
      theme_minimal()

p

# abs(residual) against predictor
p <- ggplot(one, aes(x=x, y=abs(resid))) + 
  geom_point() +
  xlab("Number of Bids") + 
  ylab("Absolute Value of Residual") +
  theme_minimal()

p

# Residual against predicted
p <- ggplot(one, aes(x=pred, y=resid)) + 
  geom_point() +
  xlab("Predicted Value") + 
  ylab("Residual") +
  theme_minimal()

p

# get MSE
one_anova <- anova(one_model)
MSE <- one_anova$`Mean Sq`[2]

# standardized residual
one$resid_s <- one$resid/sqrt(MSE)

# Standardized residual against predicted
p <- ggplot(one, aes(x=pred, y=resid_s)) + 
  geom_point() +
  xlab("Predicted Value") + 
  ylab("Standardized Residual") +
  theme_minimal()

p

# QQ plot
p <- ggplot(one, aes(sample = y)) +
      stat_qq() + 
      theme_minimal()

p

# Breusch-Pagan test for homogeneity of variance
one_bp <- bptest(one_model)
# why is the test statistic different? (how is it being calculated?)
# p-value lines up with what we found in class

# lack of fit
one_lack <- ols_pure_error_anova(one_model)
# this is different than in class - in class we used different data

# loess curve

p <- ggplot(one, aes(x=x, y=y)) + 
  geom_smooth() +
  theme_minimal()

p

plot(one$x, one$y, main = "lowess(one)")

p <- ggplot(one, aes(x=x, y=y)) +
  geom_point() +
  stat_smooth(method = lm,color="black") +
  geom_smooth(se = FALSE) + 
  theme_bw()

p

# the graphs I use for model assessment
almost_sas(one_model)
