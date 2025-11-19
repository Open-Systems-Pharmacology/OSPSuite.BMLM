# Test cases for calculateLogLikelihood function
test_that("calculateLogLikelihood works for uncensored absolute model", {
  yValue <- 1.0
  predicted <- 1.1
  sigma <- 0.5
  result <- calculateLogLikelihood(yValue, predicted, model = "absolute", sigma = sigma, isCensored = FALSE, lloq = 0)
  expect_type(result, "double")
  expect_true(result < 0) # Expect a negative log likelihood
})

test_that("calculateLogLikelihood works for uncensored proportional model", {
  yValue <- 1.0
  predicted <- 1.1
  sigma <- 0.5
  result <- calculateLogLikelihood(yValue, predicted, model = "proportional", sigma = sigma, isCensored = FALSE, lloq = 0)
  expect_type(result, "double")
  expect_true(result < 0) # Expect a negative log likelihood
})

test_that("calculateLogLikelihood works for uncensored log_absolute model", {
  yValue <- 1.0
  predicted <- 1.1
  sigma <- 0.5
  result <- calculateLogLikelihood(yValue, predicted, model = "log_absolute", sigma = sigma, isCensored = FALSE, lloq = 0)
  expect_type(result, "double")
  expect_true(result < 0) # Expect a negative log likelihood
})

test_that("calculateLogLikelihood works for censored absolute model", {
  yValue <- 1.0
  predicted <- 1.1
  sigma <- 0.5
  lloq <- 1.0
  result <- calculateLogLikelihood(yValue, predicted, model = "absolute", sigma = sigma, isCensored = TRUE, lloq = lloq)
  expect_type(result, "double")
  expect_true(result < 0) # Expect a negative log likelihood
})

test_that("calculateLogLikelihood works for censored proportional model", {
  yValue <- 1.0
  predicted <- 1.1
  sigma <- 0.5
  lloq <- 1.0
  result <- calculateLogLikelihood(yValue, predicted, model = "proportional", sigma = sigma, isCensored = TRUE, lloq = lloq)
  expect_type(result, "double")
  expect_true(result < 0) # Expect a negative log likelihood
})

test_that("calculateLogLikelihood works for censored log_absolute model", {
  yValue <- 1.0
  predicted <- 1.1
  sigma <- 0.5
  lloq <- 1.0
  result <- calculateLogLikelihood(yValue, predicted, model = "log_absolute", sigma = sigma, isCensored = TRUE, lloq = lloq)
  expect_type(result, "double")
  expect_true(result < 0) # Expect a negative log likelihood
})

test_that("calculateLogLikelihood returns negative infinity for predictions below lowerBound", {
  yValue <- 1.0
  predicted <- -1.0 # Invalid prediction
  sigma <- 0.5
  result <- calculateLogLikelihood(yValue, predicted, model = "absolute", sigma = sigma, isCensored = FALSE, lloq = 0, lowerBound = 0)
  expect_equal(result, log(0)) # Expect log(0) which is -Inf
})

test_that("calculateLogLikelihood throws an error for invalid model", {
  yValue <- 1.0
  predicted <- 1.1
  sigma <- 0.5
  expect_error(calculateLogLikelihood(yValue, predicted, model = "invalid_model", sigma = sigma, isCensored = FALSE, lloq = 0))
})

test_that("calculateLogLikelihood throws an error for non-numeric inputs", {
  expect_error(calculateLogLikelihood("a", 1.1, model = "absolute", sigma = 0.5, isCensored = FALSE, lloq = 0))
  expect_error(calculateLogLikelihood(1.0, "b", model = "absolute", sigma = 0.5, isCensored = FALSE, lloq = 0))
  expect_error(calculateLogLikelihood(1.0, 1.1, model = "absolute", sigma = "c", isCensored = FALSE, lloq = 0))
  expect_error(calculateLogLikelihood(1.0, 1.1, model = "absolute", sigma = 0.5, isCensored = FALSE, lloq = "d"))
})

test_that("calculateLogLikelihood throws an error for incorrect lengths", {
  expect_error(calculateLogLikelihood(c(1.0, 2.0), 1.1, model = "absolute", sigma = 0.5, isCensored = FALSE, lloq = 0))
  expect_error(calculateLogLikelihood(1.0, c(1.1, 2.1), model = "absolute", sigma = 0.5, isCensored = FALSE, lloq = 0))
  expect_error(calculateLogLikelihood(1.0, 1.1, model = "absolute", sigma = c(0.5, 1.0), isCensored = FALSE, lloq = 0))
  expect_error(calculateLogLikelihood(1.0, 1.1, model = "absolute", sigma = 0.5, isCensored = FALSE, lloq = c(0, 1)))
})

test_that("calculateLogLikelihood works for yValue equal to lloq in censored model", {
  yValue <- 1.0
  predicted <- 1.0
  sigma <- 0.5
  lloq <- 1.0
  result <- calculateLogLikelihood(yValue, predicted, model = "absolute", sigma = sigma, isCensored = TRUE, lloq = lloq)
  expect_type(result, "double")
  expect_true(result < 0) # Expect a negative log likelihood
})

test_that("calculateLogLikelihood works for predicted equal to lowerBound", {
  yValue <- 1.0
  predicted <- 0.0 # Equal to lowerBound
  sigma <- 0.5
  result <- calculateLogLikelihood(yValue, predicted, model = "absolute", sigma = sigma, isCensored = FALSE, lloq = 0, lowerBound = 0)
  expect_equal(result, -Inf) # Expect negative infinity
})

test_that("calculateLogLikelihood handles sigma equal to zero", {
  yValue <- 1.0
  predicted <- 1.1
  sigma <- 0.0
  expect_true(calculateLogLikelihood(yValue, predicted, model = "absolute", sigma = sigma, isCensored = FALSE, lloq = 0) == -Inf)
})

test_that("calculateLogLikelihood handles very large sigma", {
  yValue <- 1.0
  predicted <- 1.1
  sigma <- 1e6
  result <- calculateLogLikelihood(yValue, predicted, model = "absolute", sigma = sigma, isCensored = FALSE, lloq = 0)
  expect_type(result, "double")
  expect_true(result < 0) # Expect a negative log likelihood
})

test_that("calculateLogLikelihood identifies uncensored when lloq <= yValue", {
  yValue <- 1.0
  predicted <- 1.1
  sigma <- 0.5
  lloq <- 0.5
  result <- calculateLogLikelihood(yValue, predicted, model = "absolute", sigma = sigma, isCensored = TRUE, lloq = lloq)
  expect_type(result, "double")
  expect_true(result < 0) # Expect a negative log likelihood
})

## TODO -----------

dtPrior <- fread(file.path(myTestRun$outputDir, "prior.csv"))
dtStartValues <- fread(file.path(myTestRun$outputDir, "startValues.csv"))
dtRes <- readRDS(file.path(myTestRun$outputDir, "bestPrediction.RDS"))[[1]]

# Test cases for getLogLikelihood function
test_that("getLogLikelihood calculates total log likelihood correctly", {
  result <- getLogLikelihood(dtPrior, dtStartValues, dtRes)
  expect_type(result, "double")
  expect_true(all(is.finite(result))) # Expect a finite log likelihood value
})

# Test cases for getLikelihoodTimeProfiles function
test_that("getLikelihoodTimeProfiles calculates likelihood for time profiles", {
  result <- getLikelihoodTimeProfiles(dtPrior, dtRes)
  expect_type(result, "double")
  expect_true(is.finite(result)) # Expect a finite log likelihood value
})

# Test cases for getLikelihoodPriors function
test_that("getLikelihoodPriors calculates likelihood priors correctly", {
  result <- getLikelihoodPriors(dtPrior)
  expect_type(result, "double")
  expect_true(is.finite(result)) # Expect a finite log likelihood value
})

# Test cases for getLikelihoodHyperParameter function
test_that("getLikelihoodHyperParameter calculates likelihood for hyperparameters", {
  result <- getLikelihoodHyperParameter(dtStartValues, dtPrior)
  expect_type(result, "double")
  expect_true(is.finite(result)) # Expect a finite log likelihood value
})

# Test cases for setlogTruncationOffset function
test_that("setlogTruncationOffset calculates log truncation offsets", {
  result <- setlogTruncationOffset(dtPrior, dtStartValues)
  expect_s3_class(result, "data.table")
  expect_true("logTruncationOffset" %in% colnames(result)) # Expect logTruncationOffset column in result
})
