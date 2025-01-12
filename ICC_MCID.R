# ICC and Test-Retest-Reliability

library(pacman)
p_load(tidyverse, lme4, conflicted, psych)

# MCID Minimal Clinically Important Difference---------

# HADS score---------
# The Hospital Anxiety and Depression Scale 

# Test-Retest-Reliability:
# https://doi.org/10.1016/S1361-9004(02)00029-8

# Let's use the numbers from here (Table 1):
# https://www.sciencedirect.com/science/article/abs/pii/S1361900402000298
# just for demonstration purposes.

# Minimal Clinically Important Difference (MCID) for HADS-A:
# https://pmc.ncbi.nlm.nih.gov/articles/PMC2459149/
# Not exactly the same population as shift workers, but suffices for demonstration purposes.
# MCID HADS anxiety score and 1.68 (1.48–1.87)

# Create 2 correlated measurements:
# (This could probably be improved by drawing from a bivariate normal distribution)
# HADS-A Anxiety subscale
# Use n=100, instead of n=24 for more stability
# Load necessary package
library(MASS)

# Parameters
sigma1 <- 3.93  # Standard deviation of variable 1
sigma2 <- 3.52  # Standard deviation of variable 2
rho <- 0.82    # Correlation

# Covariance matrix
cov_matrix <- matrix(c(sigma1^2, rho * sigma1 * sigma2,
                       rho * sigma1 * sigma2, sigma2^2), nrow = 2)

# Number of samples
n <- 100

# Generate random samples
set.seed(188)  # For reproducibility
samples <- mvrnorm(n = n, mu = c(7.92, 7.83), Sigma = cov_matrix)

# Convert to data frame for visualization
df <- as.data.frame(samples)
colnames(df) <- c("TP1", "TP2")

# Plot the samples
plot(df, main = "Scatterplot of Multivariate Normal Samples",
     xlab = "TP1", ylab = "TP2", pch = 19, col = rgb(0, 0, 1, alpha = 0.5))

# https://en.wikipedia.org/wiki/Intraclass_correlation#Modern_ICC_definitions:_simpler_formula_but_positive_bias
# ICC should be the correlation within the group (i.e. patient)

cor(df$TP1, df$TP2, method = "pearson") # ~0.8-0.9
ICC(df) # 0.82

# manually
df_mod <- data.frame(id = 1:n, df$TP1, df$TP2)
names(df_mod) <- c("id", "TP1", "TP2")
df_mod_long <- df_mod %>% pivot_longer(cols = c(TP1, TP2), names_to = "time_point", values_to = "score")
mod <- lme4::lmer(score ~ time_point + (1|id), data = df_mod_long)
variance_df <- as.data.frame(summary(mod)$varcor)
# ICC=
variance_df$sdcor[1]^2 / (variance_df$sdcor[1]^2 + variance_df$sdcor[2]^2) # ~0.9
# cor and ICC are very very similar, should actually be identical.

# check
mean(df$TP1)
mean(df$TP2) 

data.frame(TP1 = df$TP1, TP2 = df$TP2) %>% 
  ggplot(aes(x = TP1, y = TP2)) +
  geom_point() +
  xlab("Measurement of patients at time point 1") +
  ylab("Measurement of the same patients at time point 2")

# Range for HADS-A should be 0-21 
# (according to "The Hospital Anxiety and Depression Scale, Zigmond & Snaith, 1983")

df <- data.frame(TP1 = df$TP1, TP2 = df$TP2) %>% 
  dplyr::filter(TP1 >= 0) %>%
  dplyr::filter(TP2 >= 0) %>% # negative not possible
  dplyr::filter(TP1 <= 21) %>%
  dplyr::filter(TP2 <= 21) # max. score is 21
df

mod <- lm(TP2 ~ TP1, data = df)
pred <- predict(mod, df, interval = "prediction")

# How wide are the prediction intervals for a patient?
# This answers the question: Where can I expect the second measurement 
# of the SAME patient to be in x% of cases?
as.data.frame(pred) %>% mutate(width_prediction_interval = upr - lwr) # width of the prediction interval 8-10 points!

# example:
predict(mod, newdata = data.frame(TP1 = 10), interval = "prediction") # 95% prediction interval for a patient with a score of 10 at time point 1.
(13.56369 - 5.226282)/1.68

# Prediction interval width is ~5 times our minimally clinically important 
# change of 1.68 for HADS-A.

df %>% 
  ggplot(aes(x = TP1, y = TP2)) +
  # Color points conditionally
  geom_point(aes(color = ifelse(TP2 > TP1 + 1.68 | TP2 < TP1 - 1.68, "red", "black"))) +
  scale_color_manual(values = c("red" = "red", "black" = "black"), guide = "none") +
  geom_abline(intercept = mod$coefficients[1], slope = mod$coefficients[2]) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_ribbon(aes(ymin = pred[,2], ymax = pred[,3]), alpha = 0.2) + 
  ggtitle("HADS-A and 95% Prediction Interval for TP2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(intercept = 0, slope = 1, color = "green", linetype = "dashed") +
  geom_abline(intercept = 1.68, slope = 1, color = "red", linetype = "dashed") +
  geom_abline(intercept = -1.68, slope = 1, color = "red", linetype = "dashed")

# How often is TP2 within the MCIC of 1.68 points?---------
df$abs_diff <- abs(df$TP1 - df$TP2)
hist(df$abs_diff)
abline(v = 1.68, col = "red")

table(df$abs_diff > 1.68)/sum(table(df$abs_diff > 1.68)) # 
# -> in ~46% of cases we detect a minimally clinically important change of 1.68 points.
# even though the underlying truth did not change.

# This result is near random guessing




