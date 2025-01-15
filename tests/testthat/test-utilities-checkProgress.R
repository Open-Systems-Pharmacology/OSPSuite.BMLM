
# Sample data for testing
set.seed(123)
dtConvergence <- data.table(
  iteration = 1:300,
  logTimeProfile = rnorm(300, mean = -10, sd = 1),
  logHyperParameter = rnorm(300, mean = -10, sd = 1),
  logPrior = rnorm(300, mean = -10, sd = 1),
  NAcounter = sample(0:10, 300, replace = TRUE),
  event = sample(c('best', 'restart'), size = 300, prob = c(0.95,0.05),replace = TRUE)
)

# Visual test for plotConvergence
test_that("plotConvergence produces expected plot", {
  vdiffr::expect_doppelganger("plot_convergence", plotConvergence(dtConvergence, displayVariablesIndx = c(1, 2, 3), titletxt = "Convergence Plot"))
})

# # Sample data for plotParameterLimits
# dtList <- list(
#   prior = data.table(id = 1:5, name = paste0("Param", 1:5), startValue = runif(5), minValue = 0, maxValue = 1, scaling = "linear"),
#   startValues = data.table(id = 1:5, startValue = runif(5), name = paste0("Param", 1:5), individualId = 1:5)
# )
#
# statusList <- list(
#   current = list(params = runif(5)),
#   best = list(params = runif(5))
# )
#
# # # Visual test for plotParameterLimits
# # test_that("plotParameterLimits produces expected plots", {
# #   vdiffr::expect_doppelganger("plot_parameter_limits", plotParameterLimits(dtList, statusList, titeltxt = "Parameter Limits", nCols = 2, nRows = 2))
# # })
#
# # Sample data for plotDistributions
# dtListDist <- list(
#   prior = data.table(name = paste0("Param", 1:5), categoricCovariate = NA, unit = "unit"),
#   current = data.table(name = paste0("Param", 1:5), statusValue = runif(5), status = "current"),
#   best = data.table(name = paste0("Param", 1:5), statusValue = runif(5), status = "best")
# )
#
# # Visual test for plotDistributions
# test_that("plotDistributions produces expected plots", {
#   vdiffr::expect_doppelganger("plot_distributions", plotDistributions(dtListDist, currentStatus = dtListDist$current, bestStatus = dtListDist$best, titeltxt = "Distributions"))
# })
#
# # Sample data for plotPredictedVsObserved
# dtRes <- data.table(
#   scenario = rep(c("Scenario1", "Scenario2"), each = 50),
#   outputPathId = rep(1:2, each = 50),
#   group = rep(c("Group1", "Group2"), times = 50),
#   observed = rnorm(100),
#   predicted = rnorm(100)
# )
#
# # Visual test for plotPredictedVsObserved
# test_that("plotPredictedVsObserved produces expected plots", {
#   vdiffr::expect_doppelganger("plot_predicted_vs_observed", plotPredictedVsObserved(dtRes, titletxt = "Predicted vs Observed"))
# })
#
# # Sample data for plotPredictedVsTime
# dtResTime <- data.table(
#   outputPathId = rep(1:2, each = 50),
#   scenarioName = rep(c("Scenario1", "Scenario2"), each = 50),
#   individualId = rep(1:10, times = 10),
#   predicted = rnorm(100),
#   time = rep(1:10, times = 10)
# )
#
# # Visual test for plotPredictedVsTime
# test_that("plotPredictedVsTime produces expected plots", {
#   vdiffr::expect_doppelganger("plot_predicted_vs_time", plotPredictedVsTime(dtResTime, titletxt = "Predicted vs Time"))
# })
#
# # Sample data for plotResidualsVsTime
# dtResResiduals <- data.table(
#   outputPathId = rep(1:2, each = 50),
#   scenario = rep(c("Scenario1", "Scenario2"), each = 50),
#   group = rep(c("Group1", "Group2"), times = 50),
#   resNorm = rnorm(100)
# )
#
# # Visual test for plotResidualsVsTime
# test_that("plotResidualsVsTime produces expected plots", {
#   vdiffr::expect_doppelganger("plot_residuals_vs_time", plotResidualsVsTime(dtResResiduals, titletxt = "Residuals vs Time"))
# })
#
# # Sample data for plotResidualsAsHistogram
# dtResHist <- data.table(
#   outputPathId = rep(1:2, each = 50),
#   scenario = rep(c("Scenario1", "Scenario2"), each = 50),
#   group = rep(c("Group1", "Group2"), times = 50),
#   resNorm = rnorm(100)
# )
#
# # Visual test for plotResidualsAsHistogram
# test_that("plotResidualsAsHistogram produces expected plots", {
#   vdiffr::expect_doppelganger("plot_residuals_histogram", plotResidualsAsHistogram(dtResHist, titletxt = "Residuals Histogram"))
# })
#
# # Sample data for plotResidualsAsQQ
# dtResQQ <- data.table(
#   outputPathId = rep(1:2, each = 50),
#   scenario = rep(c("Scenario1", "Scenario2"), each = 50),
#   group = rep(c("Group1", "Group2"), times = 50),
#   resNorm = rnorm(100)
# )
#
# # Visual test for plotResidualsAsQQ
# test_that("plotResidualsAsQQ produces expected plots", {
#   vdiffr::expect_doppelganger("plot_residuals_qq", plotResidualsAsQQ(dtResQQ, titletxt = "Residuals QQ Plot"))
# })
