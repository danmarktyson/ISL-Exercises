library(tidyverse)
library(boot)
library(broom)
library(ISLR2)
attach(Wage)

exponent <- 20
k <- 30
n <- nrow(Wage)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, exponent, dimnames = list(NULL, paste(1:exponent)))

for(i in 1:k) {
  for (j in 1:exponent) {
    fit.glm <- glm(wage ~ poly(age, j), data = Wage[folds != i,])
    pred <- predict(fit.glm, Wage[folds == i,])
    cv.errors[i, j] <- mean((Wage$wage[folds == i] - pred)^2)
  }
}

df_cv <- as_tibble(cv.errors) %>%
  pivot_longer(cols = everything(),
               names_to = "exponent",
               values_to = "mse") %>%
  mutate(exponent = as.numeric(exponent)) %>%
  group_by(exponent) %>%
  summarise(mean   = mean(mse),
            sd_mse = sd(mse),
            LSE    = mean - sd_mse,
            USE    = mean + sd_mse) %>%
  arrange(exponent) %>%
  ungroup()

which.min(df_cv$mean)

ggplot(df_cv, aes(exponent, mean)) +
  geom_point() +
  geom_line() +
  geom_line(aes(y = LSE), linetype = "dashed") +
  geom_line(aes(y = USE), linetype = "dashed")
  
plot(result, type = "b")

##

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

fit <- lm(wage ~ poly(age, 20), data = Wage)
summary(fit)
