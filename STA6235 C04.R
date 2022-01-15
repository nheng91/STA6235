library(tidyverse)

# NOTE: code from this chapter found here (by Aaron Schlegel):
# http://rstudio-pubs-static.s3.amazonaws.com/5218_61195adcdb7441f7b08af3dba795354f.html
#
# enter data
x <- c( 80,  30,  50,  90,  70,  60, 120,  80, 100,
        50,  40,  70,  90,  20, 110, 100,  30,  50,
        90, 110,  30,  90,  40,  80,  70)

y <- c(399, 121, 221, 376, 361, 224, 546, 352, 353,
       157, 160, 252, 389, 113, 435, 420, 212, 268,
       377, 421, 273, 468, 244, 342, 323)

# put together as tibble
one <- tibble(x, y)
# clean up the x and y vectors: rm(x, y)

# construct model
one_model <- lm(y ~ x, data = one)

# Bonferroni-adjusted CI for B_0 and B_1
# we use the usual confint() function, but adjust in the LEVEL 
one_ci_bonf <- confint(one_model, level = (1 - 0.1/2))

# a user-built function for the Working-Hotelling CI
# for several mean responses
ci.wh <- function(model, newdata, alpha = 0.1)
{
  df    <- nrow(model.frame(model)) - length(coef(model))  # 23
  W     <- sqrt( 2 * qf(1 - alpha, 2, df) )                # 2.2580
  ci    <- predict(model, newdata, se.fit = TRUE)   
  x <- cbind(
    'x'   = newdata,
    's'   = ci$se.fit,
    'fit' = ci$fit,
    'lwr' = ci$fit - W * ci$se.fit,
    'upr' = ci$fit + W * ci$se.fit)
  
  return(x)
}

# we wanted the CI for X_h=30, X_h=65, and X_h=100
new <- data.frame(x = c(30, 65, 100))

# call the function to construct the W-H CI:
one_ci_wh <- ci.wh(one_model, new)
# NOTE: these are slightly different from what we found
# in class -- remember that we had to approximate our F
# this function finds the true F critical value

# Bonferroni for several mean responses
predict(one_model, new, int = "c", level = (1 - 0.1/nrow(new)), se.fit = TRUE)


# user-defined function for simultaneous prediction intervals
# specify "S" for Scheffe, "B" for Bonferroni
ci.sim <- function(model, newdata, type = c("B", "S"), alpha = 0.05)
{
  g  <- nrow(newdata)
  CI <- predict(model, newdata, se.fit = TRUE)
  M  <- ifelse(match.arg(type) == "B",
               qt(1 - alpha / (2*g), model$df),              # B = (4.9a)
               sqrt( g * qf( 1 - alpha, g, model$df)))       # S = (4.8a)
  
  spred <- sqrt( CI$residual.scale^2 + (CI$se.fit)^2 )  # (2.38) 
  x <- data.frame(
    "x"     = newdata,
    "spred" = spred,
    "fit"   = CI$fit,
    "lower" = CI$fit - M * spred,
    "upper" = CI$fit + M * spred)
  
  return(x)
}

new <- data.frame(x = c(80, 100))

ci.sim(one_model, new, type = "S")
# NOTE: these are slightly different from what we found
# in class -- remember that we had to approximate our F
# this function finds the true F critical value

ci.sim(one_model, new, type = "B")
# NOTE: these are slightly different from what we found
# in class -- remember that we had to approximate our t
# this function finds the true t critical value






x <- c(20, 196, 115, 50, 122, 100, 33, 154, 80, 147, 182, 160)
y <- c(114, 921, 560, 245, 575, 475, 138, 727, 375, 670, 828, 762)

two <- tibble(x, y)

p <- ggplot(data=one, aes(x=x, y=y)) + 
     geom_point() +
     xlim(0, 200) + 
     ylim(0, 1000) +
     theme_minimal() 
p

# model with intercept
summary(lm(y ~ x, data=one))

# model without intercept
summary(lm(y ~ x - 1, data=one))
