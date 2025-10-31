
# Sample data for testing
dtList <- list(
  prior = data.table(name = c("param1", "param2",'param3'),
                     hyperParameter = c("geomean", "sd",''),
                     hyperDistribution = c("lnorm_geomean", "norm",''),
                     id = c('P1','P2','P3'),
                     categoricCovariate = c('','',''),
                     startValue = c(1,2,3),
                     minValue = c(0,1,2),
                     maxValue = c(1,5,4),
                     scaling = c('linear','log','linear'),
                     unit = 'mg',
                     valueMode = c(PARAMETERTYPE$hyperParameter,PARAMETERTYPE$hyperParameter,PARAMETERTYPE$global),
                     distribution = c('flat','flat','unif'),
                     p1_type = c('','','min'),
                     p1_value = c(NA,NA,2),
                     p2_type = c('','','max'),
                     p2_value = c(NA,NA,4),
                     p3_type = c('','',''),
                     p3_value = c(NA,NA,NA)
  ),
  startValues = data.table(name = c(rep("param1",3),rep("param2",4)),
                           minValue = c(rep(0,3),rep(1,4)),
                           maxValue = c(rep(1,3),rep(5,4)),
                           id = paste0('S',seq(1,7)),
                           categoricCovariate = '',
                           startValue = c(0.2,0.3,0.4,1.2,1.3,1.4,1.5),
                           scaling = c(rep('linear',3),rep('log',4)),
                           individualId = seq(1,7))
)

statusList <- list(
  current = list(params = c(P1 = 0.1,P2 = 0.2,P3 = 0.6,
                            S1 = 0.4, S2 = 0.5, S3 = 0.1, S4 = 0.4, S5 = 0.3, S6 = 0.2, S7 = 0.2),
                 scalingMethod = SCALINGMETHOD$hardBounds),
  best = list(params = c(P1 = 0.3,P2 = 0.4,P3 = 0.1,
                            S1 = 0.43, S2 = 0.53, S3 = 0.13, S4 = 0.2, S5 = 0.2, S6 = 0.2, S7 = 0.4),
                 scalingMethod = SCALINGMETHOD$hardBounds)
)

# Unit tests
test_that("plotParameterLimits works correctly", {
  plotList <- plotParameterLimits(dtList, statusList, "Test Plot", 2, 2)
  expect_length(plotList,n = 2)
  vdiffr::expect_doppelganger("plotParameterLimits_global", plotList$global)
  vdiffr::expect_doppelganger("plotParameterLimits_individual", plotList$individual_1)
})

test_that("plotDistributions works correctly", {
  expect_warning(plotList <- plotDistributions(dtList, statusList = statusList['best']))
  expect_length(plotList,n = 2)

  plotList <- plotDistributions(dtList, statusList = statusList,xScale = 'linear')
  expect_length(plotList,n = 3)
  vdiffr::expect_doppelganger("plotParameterLimits_global", plotList$distributions_1)

})

test_that("plotParameterValuesVsPrior works correctly", {
  plotObject <- plotParameterValuesVsPrior(dtList, statusList = statusList)
  vdiffr::expect_doppelganger("plotParameterValuesVsPrior", plotObject)
})

test_that("plotBestVsStartParameter works correctly", {
  plotObject <- plotBestVsStartParameter(dtList, statusList = statusList)
  vdiffr::expect_doppelganger("plotBestVsStartParameter", plotObject)
})

test_that("prepareDataForParameterLimits works correctly", {
  result <- prepareDataForParameterLimits(dtList, statusList)
  expect_true("individuals" %in% names(result))
  expect_true("globals" %in% names(result))
})

test_that("prepareDataForDistributionPlot works correctly", {
  result <- prepareDataForDistributionPlot(dtList = dtList, statusList = statusList,zoomOnData =  FALSE,parameterFilter = NULL,xScale = 'linear')
  expect_true(all(c("name","status","statusParam", "statusValue", "rangeMin", "rangeMax") %in% names(result)))
})


test_that("defaultColorsParameterPlots returns correct default colors", {

  newColors =  c(current = "blue", start = "black", best = "magenta")
  options(ospsuite.BMLM.defaultColors = newColors)
  expect_equal(newColors, defaultColorsParameterPlots())
  options(ospsuite.BMLM.defaultColors = NULL)

  colors <- defaultColorsParameterPlots()
  expect_equal(colors, c(current = "darkgreen", start = "lightblue", best = "orange"))

})

test_that("getFacetToPlotList works correctly", {
  labelVector <- c("A", "B", "C", "D", "E")
  facets <- getFacetToPlotList(labelVector, 2, 2)
  expect_equal(facets[[1]], expected = c("A", "B", "C", "D"))
  expect_equal(facets[[2]], expected = c("E"))
})

# Add more tests as needed for other functions
