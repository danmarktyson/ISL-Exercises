library(tidyverse)
library(plotly)
library(boot)
library(ISLR2)
attach(Wage)

# Exercise 6 ====
# In this exercise, you will further analyze the Wage data set considered
# throughout this chapter.

## 6a ====
# (a) Perform polynomial regression to predict wage using age. Use
# cross-validation to select the optimal degree d for the polynomial. What
# degree was chosen, and how does this compare to the results of hypothesis
# testing using ANOVA? Make a plot of the resulting polynomial fit to the data.

d <- 12
k <- 10
n <- nrow(Wage)
folds <- sample(rep(1:k, length = n))
cv_errors <- matrix(NA, k, d, dimnames = list(NULL, paste(1:d)))

### Cross Validation ====
# The following code not only finds the mean MSE at each degree d, but it also finds
# the standard error of the mean. The intent is to use the principle of parsimony,
# in the form of the one standard error rule to choose the simplest model where
# competing and more complex models stand as contenders.

for (j in 1:d) {
  for(i in 1:k) {
    fit_glm <- glm(wage ~ poly(age, j), data = Wage[folds != i, ])
    pred <- predict(fit_glm, Wage[folds == i, ])
    cv_errors[i, j] <- mean((Wage$wage[folds == i] - pred)^2)
  }
}

df_cv <- as_tibble(cv_errors) %>%
  pivot_longer(cols = everything(),
               names_to = "d",
               values_to = "mse") %>%
  mutate(d = as.numeric(d)) %>%
  group_by(d) %>%
  summarise(
    mean   = mean(mse),
    se_mse = sd(mse),
    LSE    = mean - se_mse,
    USE    = mean + se_mse
  ) %>%
  arrange(d) %>%
  ungroup()

# The degree giving the lowest CV:
d_min <- which.min(df_cv$mean)
d_min

### Plotting ====
p <- ggplot(df_cv, aes(d, mean)) +
  geom_point() +
  geom_line() +
  geom_line(aes(y = LSE), linetype = "dashed") +
  geom_line(aes(y = USE), linetype = "dashed") +
  geom_point(data = df_cv[which.min(df_cv$mean), ], color = "red", shape = 4, size = 6) + 
  labs(title = "Cross Validation",
       x = "Degree",
       y = "MSE")

ggplotly(p)

# A 9th degree polynomial function seems an extreme polynomial to fit. Plotting
# shows that the 9th degree model does not produce a wildly better model than 
# a 3rd degree model. Below compares the models using anova.

# ANOVA
fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)
fit.6 <- lm(wage ~ poly(age, 6), data = Wage)
fit.7 <- lm(wage ~ poly(age, 7), data = Wage)
fit.8 <- lm(wage ~ poly(age, 8), data = Wage)
fit.9 <- lm(wage ~ poly(age, 9), data = Wage)
fit.10 <- lm(wage ~ poly(age, 10), data = Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)

# Equivalently we could fit orthogonal polynomials in the lm function and produce
# the same results of significance

fit <- lm(wage ~ poly(age, 10), data = Wage)
summary(fit)

# Both variations of the model show that the 3rd degree polynomial produces
# significantly better results than the lower level models. This is also true
# of the 9th degree polynomial, but as the 3rd degree model produces approximately
# the same amount of error and is simpler, this model should be chosen.

## 6b ====
# (b) Fit a step function to predict wage using age, and perform cross validation
# to choose the optimal number of cuts. Make a plot of the fit obtained.

# Need to do stratified random sampling in the k-folds for cross validation

k <- 10
cuts <- 10
cv_errors_step <- rep(0, cuts)

cuts <- cut(age, 4)

for(i in 2:cuts) {
  fit.step <- glm(wage ~ cut(age, 4))
  cv_errors_step[i] <- cv.glm(Wage, fit.step, K = k)$delta[1]
}



age_grid <- range(Wage$age)
age_seq <- seq(age_grid[1], age_grid[2])

pred_step <- predict(fit.step, newdata = list(age = age_seq))
plot(age, wage)
lines(age_seq, pred_step, lwd = 2, col = "red")
