test_that("BMLM inititalisation works", {
  myRunNew <- expect_silent(BMLMOptimization$new(
    projectConfiguration = projectConfiguration,
    runName = "myRunNew",
    scenarioList = scenarioList,
    dataObserved = dataObserved
  ))


  expect_true(dir.exists(myRunNew$outputDir))

  unlink(dir.exists(myRunNew$outputDir), recursive = TRUE)
})


test_that("BMLM inititalisation works", {
  myRun$evaluateInitialValues()

  expect(list.files(myRun$outputDir, ".RDS"), c("bestOptimStatus.RDS", "bestPrediction.RDS", "optimStatus.RDS", "status.RDS"))
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

  expect(list.files(myRun$outputDir, ".RDS"), c("bestOptimStatus.RDS", "bestPrediction.RDS", "optimStatus.RDS", "status.RDS"))


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


test_that("checkConvergence works", {
  myRun <- suppressMessages(BMLMOptimization$new(
    projectConfiguration = projectConfiguration,
    runName = "myRun",
    scenarioList = scenarioList,
    dataObserved = dataObserved
  ))

  # todo copy results

  myRun$checkConvergence()
})
