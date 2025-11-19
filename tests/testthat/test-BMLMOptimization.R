test_that("BMLM inititalisation works", {
  myRunNew <- suppressMessages(BMLMOptimization$new(
    projectConfiguration = projectConfiguration,
    runName = "myRunNew",
    scenarioList = scenarioList,
    dataObserved = dataObserved
  ))


  expect_true(dir.exists(myRunNew$outputDir))

  unlink(dir.exists(myRunNew$outputDir), recursive = TRUE)
})


test_that("BMLM inititalisation works", {
  myTestRun <- BMLMOptimization$new(
    projectConfiguration = projectConfiguration,
    runName = "myTestRun",
    scenarioList = scenarioList,
    dataObserved = dataObserved
  )

  expect_contains(list.files(myTestRun$outputDir, ".RDS"), c("bestOptimStatus.RDS", "bestPrediction.RDS", "optimStatus.RDS", "status.RDS"))
})

test_that("BMLM optimization starts", {
  myRun <- BMLMOptimization$new(
    projectConfiguration = projectConfiguration,
    runName = "myRun",
    scenarioList = scenarioList,
    dataObserved = dataObserved
  )

  myRun$cleanUpStatus()

  # starts with scalingMethod hardBounds and without internal optimization
  myRun$startOptimization(projectConfiguration,
    method = "SANN",
    list(maxit = 2),
    scalingMethod = SCALINGMETHOD$hardBounds,
    hessian = FALSE,
    startInBackground = FALSE
  )

  expect_contains(list.files(myRun$outputDir, ".RDS"), c("bestOptimStatus.RDS", "bestPrediction.RDS", "status.RDS"))

  myRun$cleanUpStatus()

  # starts with scalingMethod logsig and with internal optimization
  expect_no_error(myRun$startOptimization(projectConfiguration,
    method = "SANN",
    list(maxit = 2),
    scalingMethod = SCALINGMETHOD$logsig,
    withInternalOptimization = TRUE,
    startInBackground = FALSE
  ))
})


test_that("check functions produces gg plots", {
  myRun <- suppressMessages(BMLMOptimization$new(
    projectConfiguration = projectConfiguration,
    runName = "myRun",
    scenarioList = scenarioList,
    dataObserved = dataObserved
  ))

  # todo copy results
  invisible(lapply(
    list.files(system.file("extdata", "BMLMTestResult", package = "ospsuite.bmlm")),
    function(f) {
      file.copy(
        from = file.path(system.file("extdata", "BMLMTestResult", package = "ospsuite.bmlm"), f),
        to = file.path(myRun$outputDir, f),
        overwrite = TRUE
      )
    }
  ))

  p <- myRun$checkConvergence()
  expect_s3_class(p$convergence, "ggplot")

  p <- myRun$checkCorrelations()
  expect_s3_class(p$correlation_fitparameter, "gg")

  p <- myRun$checkDistributions()
  expect_s3_class(p$distributions_1, "gg")

  p <- myRun$checkParameterLimits()
  expect_s3_class(p$global, "gg")

  p <- myRun$checkPredictedVsObserved()
  expect_s3_class(p$Plasma, "gg")

  p <- myRun$checkPredictedVsTime()
  expect_s3_class(p$Plasma, "gg")

  p <- myRun$checkResidualsAsHistogram()
  expect_s3_class(p$Plasma, "gg")

  p <- myRun$checkResidualsAsQQ()
  expect_s3_class(p$Plasma, "gg")

  p <- myRun$checkResidualsVsTime()
  expect_s3_class(p$Plasma, "gg")

  p <- myRun$getCurrentConfigTable(projectConfiguration)
  expect_s3_class(p, "data.table")
})


test_that("export functions creates output", {
  myRun <- suppressMessages(BMLMOptimization$new(
    projectConfiguration = projectConfiguration,
    runName = "myRun",
    scenarioList = scenarioList,
    dataObserved = dataObserved
  ))

  # copy results
  invisible(lapply(
    list.files(system.file("extdata", "BMLMTestResult", package = "ospsuite.bmlm")),
    function(f) {
      file.copy(
        from = file.path(system.file("extdata", "BMLMTestResult", package = "ospsuite.bmlm"), f),
        to = file.path(myRun$outputDir, f),
        overwrite = TRUE
      )
    }
  ))

  tmp <- capture.output(myRun$exportFinalValuesToBMLConfigTable(projectConfiguration = projectConfiguration))

  wb <- openxlsx::loadWorkbook(projectConfiguration$addOns$bMLMConfigurationFile)
  dt <- xlsxReadData(wb = wb, sheetName = "Prior", skipDescriptionRow = FALSE)
  expect_contains(names(dt), "finalValue")

  expect_warning(
    capture.output(
      suppressMessages(
        myRun$exportModelParametersToConfigTables(projectConfiguration,overwrite = TRUE)
      )
    )
  )

  wb <- openxlsx::loadWorkbook(projectConfiguration$modelParamsFile)
  expect_contains(wb$sheet_names,"myRun_global")
  dt <- xlsxReadData(wb = wb, sheetName = "myRun_global", skipDescriptionRow = FALSE)
  expect_equal(nrow(dt),expected = 2)

  tmp <- capture.output(myRun$exportIndividualResultsToPkml(
    projectConfiguration = projectConfiguration,
    individualId = dataObserved$individualId[1]
  ))
  expect_length(list.files(myRun$outputDir, pattern = dataObserved$individualId[1]), n = 1)

  tmp <- capture.output(suppressMessages(myRun$exportIndividualValuesToConfigTable(projectConfiguration)))
  wb <- openxlsx::loadWorkbook(projectConfiguration$individualsFile)
  dt <- xlsxReadData(wb = wb, sheetName = dataObserved$individualId[1], skipDescriptionRow = FALSE)
  expect_equal(nrow(dt), expected = 3)

  tmp <- capture.output(suppressMessages(myRun$exportResultAsPopulation(projectConfiguration)))
  expect_true(file.exists(file.path(projectConfiguration$populationsFolder, "1234_adults_iv_myRun.csv")))
})
