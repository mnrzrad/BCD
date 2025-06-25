## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(BCD)

## -----------------------------------------------------------------------------
dgeomBCD(x = 1, y = 2, q1 = 0.5, q2 = 0.6, q3 = 0.8)
dgeomBCD(x = 0, y = 4, q1 = 0.5, q2 = 0.6, q3 = 0.8)

## -----------------------------------------------------------------------------
pgeomBCD(x = 1, y = 2, q1 = 0.5, q2 = 0.6, q3 = 0.8)
pgeomBCD(x = 0, y = 0, q1 = 0.4, q2 = 0.3, q3 = 0.9)

## -----------------------------------------------------------------------------
set.seed(123)
samples <- rgeomBCD(n = 100, q1 = 0.5, q2 = 0.5, q3 = 0.1)
head(samples)
cor(samples$X, samples$Y)  # Should be negative

## -----------------------------------------------------------------------------
samples <- rgeomBCD(n = 50, q1 = 0.2, q2 = 0.2, q3 = 0.5)
result <-MLEgeomBCD(samples)
print(result)

## -----------------------------------------------------------------------------
samples <- rgeomBCD(n = 1000, q1 = 0.2, q2 = 0.2, q3 = 0.5)
result <-MLEgeomBCD(samples)
print(result)

## -----------------------------------------------------------------------------
data(abortflights)
head(abortflights)
table(abortflights$X, abortflights$Y)

## -----------------------------------------------------------------------------
fit <- MLEgeomBCD(abortflights)
FTtest(abortflights, "BGCD", params = fit, num_params = 3)

