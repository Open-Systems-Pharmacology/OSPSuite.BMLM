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
