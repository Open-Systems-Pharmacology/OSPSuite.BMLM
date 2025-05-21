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
