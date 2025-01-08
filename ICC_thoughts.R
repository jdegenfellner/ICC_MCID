# ICC and Test-Retest-Reliability

library(pacman)
p_load(tidyverse, lme4, conflicted)
conflicts_prefer(stats::filter)

# RMDQ: The score ranges from 0 (no disability) to 24 (max. disability) 

#set.seed(123443)

# Create 2 correlated measurements:
x_TP1 <- rnorm(84, mean = 10.21, sd = 4.44) # 84 Measurements of Patients at time point 1
x_TP2 <- x_TP1 + rnorm(84, mean = 0, sd = 2.5) # 84 Measurements of the same Patients at time point 2
# sd was chosen to produce high correlation.

# https://en.wikipedia.org/wiki/Intraclass_correlation#Modern_ICC_definitions:_simpler_formula_but_positive_bias
# ICC should be the correlation within the group (i.e. patient)
cor(x_TP1, x_TP2, method = "pearson") # ~0.8-0.9

df <- data.frame(x_TP1, x_TP2)
ICC(df)

# manually
df_mod <- data.frame(id = 1:84, x_TP1, x_TP2)
df_mod_long <- df_mod %>% pivot_longer(cols = c(x_TP1, x_TP2), names_to = "time_point", values_to = "score")
mod <- lme4::lmer(score ~ time_point + (1|id), data = df_mod_long)
variance_df <- as.data.frame(summary(mod)$varcor)
# ICC=
variance_df$sdcor[1]^2 / (variance_df$sdcor[1]^2 + variance_df$sdcor[2]^2)

# cor and ICC are very very similar, should actually be identical.

mean(x_TP1)
mean(x_TP2) # should be equal, since errors with mean=0 were added.

data.frame(x_TP1, x_TP2) %>% 
  ggplot(aes(x = x_TP1, y = x_TP2)) +
  geom_point() +
  xlab("Measurement of patients at time point 1") +
  ylab("Measurement of the same patients at time point 2")

df <- data.frame(x_TP1, x_TP2) %>%
  dplyr::filter(x_TP1 >= 0) %>%
  dplyr::filter(x_TP2 >= 0) %>% # negative not possible
  dplyr::filter(x_TP1 <= 24) %>%
  dplyr::filter(x_TP2 <= 24) # max. score is 24
df

mod <- lm(x_TP2 ~ x_TP1, data = df)
pred <- predict(mod, df, interval = "prediction")

# How wide are the prediction intervals for a patient?
# This answers the question: Where can I expect the second measurement 
# of the SAME patient to be?
as.data.frame(pred) %>% mutate(width_prediction_interval = upr - lwr) # width of the prediction interval 8-10 points!

# example:
predict(mod, newdata = data.frame(x_TP1 = 10), interval = "prediction") # 95% prediction interval for a patient with a score of 10 at time point 1.
predict(mod, newdata = data.frame(x_TP1 = 10), interval = "prediction", level = 0.60) # 99% prediction interval for a patient with a score of 10 at time point 1.

# Prediction interval width is ~5 times our minimally clinically important change of 2 for RMDQ.

# Is that a problem?

# Draw the regression line with the prediction interval
df %>% ggplot(aes(x = x_TP1, y = x_TP2)) +
  geom_point() +
  geom_abline(intercept = mod$coefficients[1], slope = mod$coefficients[2]) +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_ribbon(aes(ymin = pred[,2], ymax = pred[,3]), alpha = 0.2)

