# Example test data
set.seed(123)
testData <- data.table(
  outputPathId = rep("path1", 16),
  scenarioName = rep(c("scenario1"), each = 16),
  scenario = rep(c("scenario1"), each = 16),
  group = rep(c("group1"), each = 16),
  individualId = c(rep(c("Ind1", "ind2", "ind3"), each = 5), "Ind4"),
  xValues = c(rep(seq(1, 5), times = 3), 5),
  xUnit = "h",
  yUnit = "ng/mL",
  dataType = "observed",
  errorModel = "absolute",
  lloq = 0.03,
  predicted = c(rep(exp(-seq(1, 5)), times = 3), exp(-5)),
  yValues = c(rep(exp(-seq(1, 5)) * 0.4, times = 3), exp(-5)),
  resNorm = rnorm(16)
)
testData[, yValues := exp(-xValues) * (0.5 + seq(0, 1, length.out = 15))]
testData[, isCensored := yValues < lloq]

# Test for plotPredictedVsTime
test_that("plotPredictedVsTime generates a plot", {
  plotList <- plotPredictedVsTime(testData, yScale = "log", titeltxt = "Predicted vs Time")
  vdiffr::expect_doppelganger("plotPredictedVsTime_log", plotList$path1_scenario1)
})

# Test for plotPredictedVsObserved
test_that("plotPredictedVsObserved generates a plot", {
  plotList <- plotResidualLoop(dtRes = testData, plotFunction = plotPredictedVsObserved)
  vdiffr::expect_doppelganger("plotPredictedVsObserved", plotList$path1)
})

# Test for plotResidualsVsTime
test_that("plotResidualsVsTime generates a plot", {
  plotList <- plotResidualLoop(dtRes = testData, plotFunction = plotResidualsVsTime, excludeCensored = TRUE)
  vdiffr::expect_doppelganger("plotResidualsVsTime", plotList$path1)
})

# Test for plotResidualsDistribution
test_that("plotResidualsDistribution generates a plot", {
  plotList <- plotResidualLoop(dtRes = testData, plotFunction = plotResidualsDistribution)
  vdiffr::expect_doppelganger("plotResidualsDistribution", plotList$path1)
})

# Test for plotResidualsAsHistogram
test_that("plotResidualsAsHistogram generates a plot", {
  plotList <- plotResidualLoop(dtRes = testData, plotFunction = plotResidualsAsHistogram)
  vdiffr::expect_doppelganger("plotResidualsAsHistogram", plotList$path1)
})

# Test for plotResidualsAsQQ
test_that("plotResidualsAsQQ generates a plot", {
  plotList <- plotResidualLoop(dtRes = testData, plotFunction = plotResidualsAsQQ, excludeCensored = TRUE)
  vdiffr::expect_doppelganger("plotResidualsAsQQ", plotList$path1)
})
