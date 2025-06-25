## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(BCD)

## -----------------------------------------------------------------------------
dpoisBCD(x = 1, y = 2, lambda1 = 0.5, lambda2 =  0.5, lambda3 =  0.5)
dpoisBCD(x = 0, y = 1, lambda1 = 0.5, lambda2 =  0.5, lambda3 =  1)

## -----------------------------------------------------------------------------
ppoisBCD(x = 1, y = 1, lambda1 = 0.5, lambda2 = 0.5, lambda3 = 0.5)
ppoisBCD(x = 2, y = 2, lambda1 = 1.0, lambda2 = 1.0, lambda3 = 0.8)

## -----------------------------------------------------------------------------
set.seed(123)
samples <- rpoisBCD(n = 100, lambda1 = 0.5, lambda2 = 0.5, lambda3 = 0.5)
head(samples)
cor(samples$X, samples$Y) # Should be negative

## -----------------------------------------------------------------------------
data <- rpoisBCD(n = 100, lambda1 = 0.5, lambda2 = 0.5, lambda3 = 0.5)
fit <- MLEpoisBCD(data)
fit

## -----------------------------------------------------------------------------
data(lensfaults)
head(lensfaults)
plot(lensfaults$X, lensfaults$Y, xlab = "X", ylab = "Y")

## -----------------------------------------------------------------------------
fit <- MLEpoisBCD(lensfaults)
FTtest(lensfaults, "BPCD", params = fit, num_params = 3)

## -----------------------------------------------------------------------------
data(eplSeasonGoals)
plot(eplSeasonGoals[["1415"]]$X, eplSeasonGoals[["1415"]]$Y, xlab = "X", ylab = "Y")
plot(eplSeasonGoals[["2425"]]$X, eplSeasonGoals[["2425"]]$Y, xlab = "X", ylab = "Y")

## -----------------------------------------------------------------------------
fit <- MLEpoisBCD(eplSeasonGoals[["1415"]])
FTtest(eplSeasonGoals[["1415"]], "BPCD", params = fit, num_params = 3)

## -----------------------------------------------------------------------------
fit <- MLEpoisBCD(eplSeasonGoals[["2425"]])
FTtest(eplSeasonGoals[["2425"]], "BPCD", params = fit, num_params = 3)

