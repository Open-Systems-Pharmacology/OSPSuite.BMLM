geomean <- 1
geosd <- 1.4

# Test for dlnorm_geomean
test_that("dlnorm_geomean matches dlnorm", {
  x <- 0.5

  density_new <- dlnorm_geomean(geomean, geosd, x = x)
  density_old <- dlnorm(x, meanlog = log(geomean), sdlog = log(geosd))

  expect_equal(density_new, density_old, tolerance = 1e-8)
})

# Test for plnorm_geomean
test_that("plnorm_geomean matches plnorm", {
  q <- 0.5

  cumulative_new <- plnorm_geomean(q, geomean = geomean, geosd = geosd)
  cumulative_old <- plnorm(q, meanlog = log(geomean), sdlog = log(geosd))

  expect_equal(cumulative_new, cumulative_old, tolerance = 1e-8)
})

# Test for qlnorm_geomean
test_that("qlnorm_geomean matches qlnorm", {
  p <- 0.5

  quantile_new <- qlnorm_geomean(p, geomean, geosd)
  quantile_old <- qlnorm(p, meanlog = log(geomean), sdlog = log(geosd))

  expect_equal(quantile_new, quantile_old, tolerance = 1e-8)
})

# Test for rlnorm_geomean
test_that("rlnorm_geomean matches rlnorm", {
  n <- 10
  set.seed(1234)
  random_new <- rlnorm_geomean(n, geomean, geosd)
  set.seed(1234)
  random_old <- rlnorm(n, meanlog = log(geomean), sdlog = log(geosd))

  expect_equal(sort(random_new), sort(random_old), tolerance = 1e-8)
})


test_that("computeStatFunction works correctly", {

  # Test for the normal distribution (mean = 0, sd = 1)
  values <- c(0, 1)
  parameters <- c("mean", "sd")

  # Test Probability (P)
  expect_equal(computeStatFunction(values, parameters, "norm", 0, type = "P"),
               pnorm(0, mean = 0, sd = 1))

  # Test Density (D)
  expect_equal(computeStatFunction(values, parameters, "norm", 0, type = "D"),
               dnorm(0, mean = 0, sd = 1))

  # Test Quantile (Q)
  expect_equal(computeStatFunction(values, parameters, "norm", 0.5, type = "Q"),
               qnorm(0.5, mean = 0, sd = 1))

  # Test Random Generation (R)
  set.seed(123)  # For reproducibility
  r <- rnorm(5, mean = 0, sd = 1)
  set.seed(123)  # For reproducibility
  expect_equal(computeStatFunction(values, parameters, "norm", 5, type = "R"),
               r, tolerance = 1e-5)  # Allow small tolerance for randomness

  # Test Log Probability
  expect_equal(computeStatFunction(values, parameters, "norm", 0, type = "P", log = TRUE),
               log(pnorm(0, mean = 0, sd = 1)))

  # Test Log Density
  expect_equal(computeStatFunction(values, parameters, "norm", 0, type = "D", log = TRUE),
               log(dnorm(0, mean = 0, sd = 1)))

})

test_that("calculateProbability works correctly", {

  # Test for normal distribution (mean = 0, sd = 1)
  rowNorm <- c(distribution = "norm", value = 0, mean_type = "mean", mean_value = 0, sd_type = "sd", sd_value = 1)

  # Test Density for normal distribution
  expect_equal(calculateProbability(rowNorm), dnorm(0, mean = 0, sd = 1))

  # Test Log Density for normal distribution
  expect_equal(calculateProbability(rowNorm, log = TRUE), log(dnorm(0, mean = 0, sd = 1)))

  # Test for uniform distribution (min = 0, max = 1)
  rowUnif <- c(distribution = "unif", value = 0.5, min_type = "min", min_value = 0, max_type = "max", max_value = 1)

  # Test Density for uniform distribution
  expect_equal(calculateProbability(rowUnif), dunif(0.5, min = 0, max = 1))

  # Test Log Density for uniform distribution
  expect_equal(calculateProbability(rowUnif, log = TRUE), log(dunif(0.5, min = 0, max = 1)))

  # Test for flat distribution
  rowFlat <- c(distribution = "flat", value = 0)  # Flat distribution should return 1 or 0 based on log

  # Test Flat Density (should return 1)
  expect_equal(calculateProbability(rowFlat), 1)

  # Test Log Flat Density (should return 0)
  expect_equal(calculateProbability(rowFlat, log = TRUE), 0)

  # Test for invalid distribution
  rowInvalid <- c(distribution = "invalid", value = 0)

  # Test that an invalid distribution returns NA
  expect_equal(calculateProbability(rowInvalid), NA)

  # Test for missing value in parameters
  rowMissingParam <- c(distribution = "norm", value = 0, mean_type = "mean", mean_value = NA, sd_type = "sd", sd_value = 1)

  # Test that missing parameters return NA
  expect_equal(calculateProbability(rowMissingParam), dnorm(x = 0,sd = 1))
})

