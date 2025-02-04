checkInitialValues(projectConfiguration,
                   myRun){


  dtList <- loadListsForRun(myRun$outputDir,myRun$runName)

  optimEnv <- initializeOptimEnv(dtList, failValue = 1e+10)

  initialValues <- getParams(
    dtPrior = dtList$prior,
    dtStartValues = dtList$startValues,
    optimizationGroup = 'both'
  )

  dtList <- setParameterToTables(dtList = dtList, params = initialValues)

  evaluateTimeprofiles(optimEnv = optimEnv,
                       scenarioList = scenarioList,
                       dtPrior = dtList$prior,
                       dtStartValues = dtList$startValues,
                       dtMappedPaths = dtList$mappedPaths,
                       simulationRunOptions = SimulationRunOptions$new(showProgress = TRUE),
                       dataObservedForMatch = dtList$data,
                       withProtocol = TRUE)

  dtRes_NA <- optimEnv$dtRes[is.na(predicted),c("scenario","individualId")] %>%
    unique()

  loglikelihoods <- getLogLikelihood(
    dtPrior = dtList$prior,
    dtStartValues = dtList$startValues,
    dtRes = optimEnv$dtRes
  )

  dtResUpdated <- updateModelError(dtPrior =  dtList$prior,dtRes = optimEnv$dtRes)
  dtResUpdated[, isCensored := !is.na(lloq) & lloq > yValues]

  # Apply the function to calculate likelihood
  dtResUpdated[, logLikelihood := mapply(calculateLogLikelihood, yValues, predicted, errorModel, sigma, isCensored, lloq, lowerBound)]



}
