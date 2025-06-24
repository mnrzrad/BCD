## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(BCD)

## -----------------------------------------------------------------------------
dbinomBCD(x = 2, y = 1, n1 = 5, n2 = 5, p1 = 0.5, p2 = 0.4, lambda = 0.5)
# independence case
dbinomBCD(x = 2, y = 1, n1 = 5, n2 = 5, p1 = 0.5, p2 = 0.4, lambda = 1.0) 

## -----------------------------------------------------------------------------
pbinomBCD(x = 2, y = 5, n1 = 5, n2 = 5, p1 = 0.5, p2 = 0.4, lambda = 0.5)
pbinomBCD(x = 1, y = 1, n1 = 10, n2 = 10, p1 = 0.3, p2 = 0.6, lambda = 1)

## -----------------------------------------------------------------------------
set.seed(123)
samples <- rbinomBCD(n = 100, n1 = 10, n2 = 10, p1 = 0.5, p2 = 0.4, lambda = 1.2)
head(samples)

## -----------------------------------------------------------------------------
data <- rbinomBCD(n = 100, n1 = 6, n2 = 4, p1 = 0.6, p2 = 0.3, lambda = 1.5)
fit <- MLEbinomBCD(data)
fit

## -----------------------------------------------------------------------------
MLEbinomBCD(data, fixed_n1 = 6, fixed_n2 = 4)

## -----------------------------------------------------------------------------
data(shacc)
head(shacc)
plot(shacc$X, shacc$Y, xlab = "Accidents 1937–42", ylab = "Accidents 1943–47")

## -----------------------------------------------------------------------------
fit <- MLEbinomBCD(shacc, fixed_n1 = 33, fixed_n2 = 27)
FTtest(shacc, "BBCD", params = fit, num_params = 3)

## -----------------------------------------------------------------------------
data(seedplant)
head(seedplant)
plot(seedplant$X, seedplant$Y, xlab = "Seeds Sown", ylab = "Plants Grown")

## -----------------------------------------------------------------------------
EstParams <- MLEbinomBCD(shacc, fixed_n1 = 13, fixed_n2 = 11)
FTtest(shacc, "BBCD", params = EstParams, num_params = 3)

