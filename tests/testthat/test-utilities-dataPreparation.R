# Unit tests for transformToUnbounded boundaryies
test_that("transformToUnbounded works correctly for log scaling", {
  result <- c(
    transformToUnbounded(value = c(1, 3), minValue = c(1, 1), maxValue = c(3, 3), scaling = SCALING$log),
    transformToUnbounded(value = c(1, 3), minValue = c(1, 1), maxValue = c(3, 3), scaling = SCALING$linear)
  )
  expect_equal(result,rep(c(-20,20),2))
})


test_that("transformToUnbounded works correctly for log scaling", {
  result <- c(
    inverseTransformParams(c(-Inf, Inf), c(1, 1), c(3, 3), SCALING$log),
    inverseTransformParams(c(-Inf, Inf), c(1, 1), c(3, 3), SCALING$linear)
  )
  expect_equal(result, expected = c(1, 3, 1, 3))
})


# Unit tests for inverseTransformParams
test_that("inverseTransformParams works correctly for log scaling", {
  testValues <- c(0.4, 5, 80)
  transformedValues <- transformToUnbounded(value = testValues, minValue = c(0.1, 0.1, 0.1), maxValue = c(100, 100, 100), scaling = SCALING$linear)
  result <- inverseTransformParams(transformedValues, minValue = c(0.1, 0.1, 0.1), maxValue = c(100, 100, 100), scaling = SCALING$linear)
  expect_equal(result, testValues)
})

test_that("inverseTransformParams works correctly for linear scaling", {
  testValues <- c(-1, 0, 5)
  transformedValues <- transformToUnbounded(value = testValues, minValue = c(-2, -2, -2), maxValue = c(10, 10, 10), scaling = SCALING$linear)
  result <- inverseTransformParams(transformedValues, minValue = c(-2, -2, -2), maxValue = c(10, 10, 10), scaling = SCALING$linear)
  expect_equal(result, testValues)
})
