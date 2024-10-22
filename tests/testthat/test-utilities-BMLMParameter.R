# Unit tests
test_that("calculateLogLikelihood works correctly", {
  # Example data
  yValue <- 0.5
  predicted <- 1
  sigma <- 0.1
  lloq <- 0.2
  lowerBound <- 0.01

  # Test for absolute model without censoring
  resultAbsolute <- calculateLogLikelihood(yValue, predicted, "absolute", sigma, FALSE, lloq, lowerBound)
  expectedAbsolute <- dnorm(x = yValue, mean = predicted, sd = sigma, log = TRUE)
  expect_equal(resultAbsolute, expectedAbsolute)

  # Test for relative model without censoring
  resultRelative <- calculateLogLikelihood(yValue, predicted, "relative", sigma, FALSE, lloq, lowerBound)
  expectedRelative <- dnorm(x = yValue / predicted, mean = 1, sd = sigma, log = TRUE)
  expect_equal(resultRelative, expectedRelative)

  # Test for log_absolute model without censoring
  resultLogAbsolute <- calculateLogLikelihood(yValue, predicted, "log_absolute", sigma, FALSE, lloq, lowerBound)
  expectedLogAbsolute <- dlnorm(x = yValue, meanlog = predicted, sdlog = sigma, log = TRUE)
  expect_equal(resultLogAbsolute, expectedLogAbsolute)

  # Test for absolute model with censoring
  resultCensoredAbsolute <- calculateLogLikelihood(yValue, predicted, "absolute", sigma, TRUE, lloq, lowerBound)
  p1 <- pnorm(q = lloq, mean = predicted, sd = sigma)
  p2 <- pnorm(q = lowerBound, mean = predicted, sd = sigma)
  expectedCensoredAbsolute <- log(p1 - p2) - log(1 - p2)
  expect_equal(resultCensoredAbsolute, expectedCensoredAbsolute)

  # Test for relative model with censoring
  resultCensoredRelative <- calculateLogLikelihood(yValue, predicted, "relative", sigma, TRUE, lloq, lowerBound)
  p1 <- pnorm(q = lloq / predicted, mean = 1, sd = sigma)
  p2 <- pnorm(q = lowerBound / predicted, mean = 1, sd = sigma)
  expectedCensoredRelative <- log(p1 - p2) - log(1 - p2)
  expect_equal(resultCensoredRelative, expectedCensoredRelative)

  # Test for log_absolute model with censoring
  resultCensoredLogAbsolute <- calculateLogLikelihood(yValue, predicted, "log_absolute", sigma, TRUE, lloq, lowerBound)
  p1 <- plnorm(q = lloq, meanlog = predicted, sdlog = sigma)
  p2 <- plnorm(q = lowerBound, meanlog = predicted, sdlog = sigma)
  expectedCensoredLogAbsolute <- log(p1 - p2) - log(1 - p2)
  expect_equal(resultCensoredLogAbsolute, expectedCensoredLogAbsolute)

  # Test for unexpected model
  expect_error(
    calculateLogLikelihood(yValue, predicted, "unknown_model", sigma, FALSE, lloq, lowerBound),
    "Unexpected error model: unknown_model"
  )
})
