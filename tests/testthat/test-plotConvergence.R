# Sample data for testing
sampleData <- data.table(
  iteration = rev(1:10),
  logTimeProfile = runif(10, 0, 1),
  logHyperParameter = runif(10, 0, 1),
  logPrior = runif(10, 0, 1),
  NAcounter = sample(0:10, 10, replace = TRUE),
  outsideRangeCounter = sample(0:10, 10, replace = TRUE),
  event = c(rep("best", 5), rep("restart", 3), rep("best", 2))
)

# Unit tests for plotConvergence
test_that("plotConvergence produces a ggplot object", {
  plotObj <- plotConvergence(sampleData, displayVariablesIndx = c(1, 2, 3))
  vdiffr::expect_doppelganger("plot_convergence", plotObj)
  expect_s3_class(plotObj$convergence, "ggplot")
})

test_that("plotConvergence handles insufficient points", {
  expect_message(plotConvergence(sampleData[1], displayVariablesIndx = c(1, 2, 3)),
                 "only one point available, please wait for plots")
})

test_that("plotConvergence throws an error for missing data.table", {
  expect_error(plotConvergence(NULL), "Assertion on 'convergence table' failed: Must be a data.table, not 'NULL'.")
})

# Unit tests for calculateConvergenceMetrics
test_that("calculateConvergenceMetrics adds required columns", {
  result <- calculateConvergenceMetrics(sampleData)
  expect_true("objectiveValue" %in% names(result))
  expect_true("percentageOfFailure" %in% names(result))
  expect_true("percentageOutsideRange" %in% names(result))
})

test_that("calculateConvergenceMetrics throws an error for missing columns", {
  incompleteData <- data.table(iteration = 1:10)
  expect_error(calculateConvergenceMetrics(incompleteData),
               "Convergence table must contain the following columns:")
})

# Unit tests for getConvergenceColumnHeaders
test_that("getConvergenceColumnHeaders returns correct headers", {
  result <- getConvergenceColumnHeaders(sampleData, displayVariablesIndx = c(1, 2))
  expect_equal(names(result), c("objectiveValue", "logTimeProfile"))
})

test_that("getConvergenceColumnHeaders handles NULL displayVariablesIndx", {
  result <- getConvergenceColumnHeaders(sampleData, NULL)
  expect_true(length(result) > 0)
})

# Unit tests for selectIterations
test_that("selectIterations selects correct number of points", {
  selectedData <- selectIterations(sampleData, nPointsAvailable = 10, nPoints = 5, selectionMode = "first")
  expect_equal(nrow(selectedData), 5)
  expect_equal(selectedData$iteration, 1:5)  # First 5 iterations when nPoints is 5
})

test_that("selectIterations throws an error for unknown selection mode", {
  expect_error(selectIterations(sampleData, nPointsAvailable = 10, nPoints = 5, selectionMode = "unknown"),
               "unknown sectionMode")
})

test_that("selectIterations selects correct points using 'last' mode", {
  selectedData <- selectIterations(sampleData, nPointsAvailable = 10, nPoints = 5, selectionMode = "last")
  expect_equal(nrow(selectedData), 5)
  expect_equal(selectedData$iteration, 6:10)
})

test_that("selectIterations selects correct points using 'random' mode", {
  set.seed(123)  # Set seed for reproducibility
  selectedData <- selectIterations(sampleData, nPointsAvailable = 10, nPoints = 5, selectionMode = "random")
  expect_equal(nrow(selectedData), 5)
  expect_contains(selectedData$iteration ,expected =  c(1,10))  # Ensure start and end are included
  expect_true(dplyr::n_distinct(selectedData$iteration) ==  5)  # Ensure points ar unique
})

# Test for edge cases
test_that("selectIterations returns all nPoints exceeds nPointsAvailable", {
  selectedData <- selectIterations(sampleData, nPointsAvailable = 10, nPoints = 20, selectionMode = "first")
  expect_equal(nrow(selectedData), 10)

})

# Unit tests for edge cases
test_that("plotConvergence handles edge cases", {
  expect_error(plotConvergence(sampleData[0]), "Assertion on 'convergence table' failed: Must have at least 1 rows, but has 0 rows.")
})

