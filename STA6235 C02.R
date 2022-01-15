library(tidyverse)

# enter data
x <- c( 80,  30,  50,  90,  70,  60, 120,  80, 100,
        50,  40,  70,  90,  20, 110, 100,  30,  50,
        90, 110,  30,  90,  40,  80,  70)

y <- c(399, 121, 221, 376, 361, 224, 546, 352, 353,
       157, 160, 252, 389, 113, 435, 420, 212, 268,
       377, 421, 273, 468, 244, 342, 323)

# put together as tibble
one <- tibble(x, y)

# find summary stats
means <- summarize(one, mean_x = mean(x, na.rm=TRUE), sd_x = sd(x, na.rm=TRUE), 
                   mean_y = mean(y, na.rm=TRUE), sd_y = sd(y, na.rm=TRUE))

# construct model
one_model <- lm(y ~ x, data = one)
one_summary <- summary(one_model)
#one_summary

# construct 95% CI for betas
one_ci <- as_tibble(confint(one_model, level=0.95))

# construct 90% CI for betas
one_ci <- as_tibble(confint(one_model, level=0.90))

# want 90% CI for mu when X_h=65, but this doesn't exist in dataset
x <- 65
y <- NA

new <- tibble(x, y)

# combine old and new data
one_cb <- rbind(one, new)

# find predictions and 90% CI
# just for X_h=65
new$Prediction <- predict(one_model, newdata = new)

new$LCL_CI <- predict(one_model, newdata = new, 
                      interval = "confidence", 
                      level = 0.90)[, 2]
new$UCL_CI <- predict(one_model, newdata = new, 
                      interval = "confidence", 
                      level = 0.90)[, 3]

# for all values observed
one_cb$Prediction <- predict(one_model, newdata = one_cb)

one_cb$LCL_CI <- predict(one_model, newdata = one_cb, 
                      interval = "confidence", 
                      level = 0.90)[, 2]
one_cb$UCL_CI <- predict(one_model, newdata = one_cb, 
                      interval = "confidence", 
                      level = 0.90)[, 3]

# want 90% CI for mu when X_h=100
# filter down to just X_h=100
one_ci_x100 <- filter(one_cb, x==100)

# want 90% PI for Y when X_h=100
one_cb$LCL_PI <- predict(one_model, newdata = one_cb, 
                      interval = "predict", 
                      level = 0.90)[, 2]
one_cb$UCL_PI <- predict(one_model, newdata = one_cb, 
                      interval = "predict", 
                      level = 0.90)[, 3]

# filter down to just X_h=100
one_pi_x100 <- filter(one_cb, x==100)

# correlation - Pearson
one_corr <- cor(one$x, one$y, method="pearson")

# is the correlation non-zero?
one_corr_test0 <- cor.test(one$x, one$y, method="pearson")

# correlation - Spearman
one_corr <- cor(one$x, one$y, method="spearman")
