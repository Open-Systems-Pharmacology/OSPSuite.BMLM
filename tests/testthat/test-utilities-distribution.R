
# Test for dlnorm_geomean
test_that("dlnorm_geomean matches dlnorm", {
  geomean <- 1
  sdlog <- 1
  x <- 0.5

  density_new <- dlnorm_geomean(geomean, sdlog, x = x)
  density_old <- dlnorm(x, meanlog = log(geomean), sdlog = sdlog)

  expect_equal(density_new, density_old, tolerance = 1e-8)
})

# Test for plnorm_geomean
test_that("plnorm_geomean matches plnorm", {
  geomean <- 1
  sdlog <- 1
  q <- 0.5

  cumulative_new <- plnorm_geomean(q, geomean, sdlog)
  cumulative_old <- plnorm(q, meanlog = log(geomean), sdlog = sdlog)

  expect_equal(cumulative_new, cumulative_old, tolerance = 1e-8)
})

# Test for qlnorm_geomean
test_that("qlnorm_geomean matches qlnorm", {
  geomean <- 1
  sdlog <- 1
  p <- 0.5

  quantile_new <- qlnorm_geomean(p, geomean, sdlog)
  quantile_old <- qlnorm(p, meanlog = log(geomean), sdlog = sdlog)

  expect_equal(quantile_new, quantile_old, tolerance = 1e-8)
})

# Test for rlnorm_geomean
test_that("rlnorm_geomean matches rlnorm", {
  n <- 10
  geomean <- 1
  sdlog <- 1

  set.seed(1234)
  random_new <- rlnorm_geomean(n, geomean, sdlog)
  set.seed(1234)
  random_old <- rlnorm(n, meanlog = log(geomean), sdlog = sdlog)

  expect_equal(sort(random_new), sort(random_old), tolerance = 1e-8)
})
