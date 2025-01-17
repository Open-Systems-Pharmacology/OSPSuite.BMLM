
#' Optimize Parameters
#'
#' Performs the optimization of parameters using the specified method and control settings.
#'
#' @param dtList A list containing data tables for optimization.
#' @param scenarioList A list of scenarios to be optimized.
#' @param simulationRunOptions An obkect of class SimulationRunOptions.
#' @param method A character string specifying the optimization method.
#' @param control A list of control parameters for the optimization function.
#' @param outputDir A character string representing the path to the output directory.
#' @param lastStatusSavingIntervalInSecs An integer specifying the interval for saving the last status in seconds.
#' @param startTime A POSIXct object representing the start time of the optimization process.
#' @param failValue A numeric value to set if evaluation of the objective function fails.
#' @param withInternalOptimization A logical indicating whether to perform internal optimization.
#' @param ... Additional arguments passed to the optimization function.
#'
#' @return A list containing the result of the optimization process.
#' @export
optimizeParameters <-
  function(dtList,
           scenarioList,
           simulationRunOptions,
           method,
           control,
           outputDir,
           lastStatusSavingIntervalInSecs,
           withInternalOptimization,
           startTime,
           failValue,
           ...) {

    # Create the optim environment
    optimEnv <- initializeOptimEnv(dtList, failValue)

    initialValues <- getParams(
      dtPrior = dtList$prior,
      dtStartValues = dtList$startValues,
      optimizationGroup = ifelse (withInternalOptimization, 'external', 'both')
    )

    # Run optimization
    result <- optim(
      par = initialValues,
      fn = createObjectiveFunction(
        optimEnv = optimEnv,
        dtList = dtList,
        scenarioList = scenarioList,
        simulationRunOptions = simulationRunOptions,
        outputDir = outputDir,
        lastStatusSavingIntervalInSecs = lastStatusSavingIntervalInSecs,
        withInternalOptimization = withInternalOptimization
      ),
      method = method,
      control = control,
      ...
    )

    # Save the result
    saveRDS(result, file = file.path(outputDir, "result.RDS"))


    return(result)  # Return result instead of invisible
  }

#' Initialize Optimization Environment
#'
#' Initializes the optimization environment by extracting relevant values from
#' Initialize Optimization Environment
#'
#' Initializes the optimization environment by extracting relevant values from
#' the provided data table list.
#'
#' @param dtList A list containing data tables for optimization.
#' @param failValue A numeric value to set if evaluation of the objective function fails.
#'
#' @return A new environment containing initialized optimization variables.
#' @keywords internal
initializeOptimEnv <- function(dtList, failValue) {
  optimEnv <- new.env()

  optimEnv$iteration <- dtList$iteration
  optimEnv$bestValue <- dtList$bestValue
  optimEnv$lastSaveTime <- Sys.time()
  optimEnv$failValue <- failValue
  optimEnv$NAcounter <- dtList$NAcounter
  optimEnv$dtRes <- data.table()
  optimEnv$scenarioResults <- list()

  return(optimEnv)
}

#' Create Objective Function for Optimization
#'
#' Generates an objective function to be used in the optimization process.
#'
#' This function returns a closure that encapsulates the optimization logic,
#' allowing the `optim` function to evaluate the objective based on the given
#' parameters and the current optimization environment.
#'
#' @param optimEnv An environment containing the current state variables for optimization.
#' @param dtList A list containing data tables relevant for the optimization process.
#' @param scenarioList A list of scenarios to be optimized.
#' @param simulationRunOptions An obkect of class SimulationRunOptions.
#' @param outputDir A character string representing the path to the output directory.
#' @param lastStatusSavingIntervalInSecs An integer specifying the interval for saving the last status in seconds.
#'
#' @return A function that takes a vector of parameters and returns the current evaluation of the objective function.
#'
#' @details The objective function updates the optimization environment, evaluates time profiles,
#' performs internal optimization, and updates the optimization status. It also handles errors
#' during the optimization process.
#'
#' @keywords internal
createObjectiveFunction <-
  function(optimEnv,
           dtList,
           scenarioList,
           simulationRunOptions,
           outputDir,
           lastStatusSavingIntervalInSecs,
           withInternalOptimization) {
    return(function(params) {
    optimEnv$iteration <- optimEnv$iteration + 1
    tryCatch({
      dtList <- setParameterToTables(dtList = dtList, params = params)
      optimStatus <-
        updateOptimStatus(
          dtPrior = dtList$prior,
          dtStartValues = dtList$startValues,
          optimEnv = optimEnv
        )

      # Evaluate time profiles
      evaluateTimeprofiles(
        optimEnv = optimEnv,
        scenarioList = scenarioList,
        dtPrior = dtList$prior,
        dtStartValues = dtList$startValues,
        dtMappedPaths = dtList$mappedPaths,
        simulationRunOptions = simulationRunOptions,
        dataObservedForMatch = dtList$data
      )

      # Internal optimization
      if (withInternalOptimization){
        resultInternal <- runInternalOptimization(dtList, optimEnv)

        #Update status and log likelihoods
        dtList <- setParameterToTables(
          dtList = dtList,
          params = resultInternal$par
        )
        optimStatus <- updateOptimStatus(dtPrior = dtList$prior,
                                         dtStartValues = dtList$startValues,
                                         optimEnv)

      }

      loglikelihoods <- getLogLikelihood(
        dtPrior = dtList$prior,
        dtStartValues = dtList$startValues,
        dtRes = optimEnv$dtRes
      )
      optimStatus[["loglikelihoods"]] <- loglikelihoods

      currentValue <- evaluateLogLikelihood(loglikelihoods, optimEnv)

      # Save status if enough time has passed
      saveOptimStatusIfNeeded(optimStatus, optimEnv, outputDir, lastStatusSavingIntervalInSecs)

      # Check if objective function value has improved
      updateBestValueIfImproved(currentValue, optimEnv, optimStatus, outputDir, loglikelihoods)

    }, error = function(err) {
      saveRDS(optimStatus, file = file.path(outputDir, "failedOptimStatus.RDS"))
      stop(conditionMessage(err))
    })

    return(currentValue)
  })
}

#' Run Internal Optimization
#'
#' Executes the internal optimization process using the specified parameters.
#'
#' This function performs optimization on internal parameters based on the provided
#' data tables and the current optimization environment. It utilizes the `optim`
#' function to find the best internal parameters that maximize the log likelihood.
#'
#' @param dtList A list containing data tables relevant for the optimization process.
#' @param optimEnv An environment containing the current state variables for optimization.
#'
#' @return A list containing the results of the internal optimization, including the optimized parameters
#'         and the value of the objective function.
#'
#' @details The internal optimization is performed using the BFGS method, which is suitable for
#'          smooth optimization problems. The function retrieves initial parameter values and
#'          evaluates the log likelihood to determine the best fit.
#'
#' @keywords internal
runInternalOptimization <- function(dtList, optimEnv) {
  initialValuesInternal <-
    getParams(
      dtPrior = dtList$prior,
      dtStartValues = dtList$startValues,
      optimizationGroup = c('internal')
    )

  resultInternal <- optim(
    par = initialValuesInternal,
    fn = function(params) {
      dtList <- setParameterToTables(dtList = dtList, params = params)

      loglikelihoods <- getLogLikelihood(dtPrior = dtList$prior, dtStartValues = dtList$startValues, dtRes = optimEnv$dtRes)
      return(evaluateLogLikelihood(loglikelihoods, optimEnv))
    },
    method = "Nelder-Mead"
  )
  return(resultInternal)
}

#' Update Optimization Status
#'
#' Updates the optimization status with the current parameters and iteration.
#'
#' @param dtPrior A data.table containing prior values.
#' @param dtStartValues A data.table containing start values.
#' @param optimEnv The optimization environment containing current state variables.
#'
#' @return A list containing the current optimization status.
#' @keywords internal
updateOptimStatus <- function(dtPrior, dtStartValues, optimEnv) {
  return(list(
    iteration = optimEnv$iteration,
    params = getParams(dtPrior = dtPrior,
                       dtStartValues = dtStartValues,
                       optimizationGroup = c('both')),
    NAcounter = optimEnv$NAcounter
  ))
}

#' Evaluate Log Likelihood
#'
#' Evaluates the log likelihoods and returns the current value based on the
#' provided log likelihoods.
#'
#' @param loglikelihoods A vector of log likelihood values.
#' @param optimEnv The optimization environment containing current state variables.
#'
#' @return A numeric value representing the current evaluation of the objective function.
#' @keywords internal
evaluateLogLikelihood <- function(loglikelihoods, optimEnv) {
  if (is.null(loglikelihoods) || length(loglikelihoods) < 3) {
    stop('strange loglikelihood')
  }

  if (any(is.na(loglikelihoods))) {
    if (optimEnv$iteration == 1) stop('First likelihood evaluation must not fail')
    optimEnv$NAcounter <- optimEnv$NAcounter + 1
    return(optimEnv$failValue)
  } else {
    return(-1 * sum(loglikelihoods))
  }
}

#' Save Optimization Status If Needed
#'
#' Saves the current optimization status to a file if the specified time interval
#' has passed since the last save.
#'
#' @param optimStatus A list containing the current optimization status.
#' @param optimEnv The optimization environment containing current state variables.
#' @param outputDir A character string representing the path to the output directory.
#' @param interval An integer specifying the interval for saving the status in seconds.
#' @keywords internal
saveOptimStatusIfNeeded <- function(optimStatus, optimEnv, outputDir, interval) {
  if (difftime(Sys.time(), optimEnv$lastSaveTime, units = "secs") >= interval) {
    saveRDS(optimStatus, file = file.path(outputDir, "optimStatus.RDS"))
    optimEnv$lastSaveTime <- Sys.time()
  }
}

#' Update Best Value If Improved
#'
#' Checks if the current value of the optimization is better than the best value
#' recorded and updates the best value if it is.
#'
#' @param currentValue A numeric value representing the current evaluation of the objective function.
#' @param optimEnv The optimization environment containing current state variables.
#' @param optimStatus A list containing the current optimization status.
#' @param outputDir A character string representing the path to the output directory.
#' @param loglikelihoods A vector of log likelihood values.
#' @keywords internal
updateBestValueIfImproved <- function(currentValue, optimEnv, optimStatus, outputDir, loglikelihoods) {
  if (currentValue < optimEnv$bestValue) {
    optimEnv$bestValue <- currentValue
    fwrite(
      as.data.table(as.list(
        c(
          iteration = optimEnv$iteration,
          loglikelihoods,
          NAcounter = optimEnv$NAcounter,
          event = 'best'
        )
      )),
      file = file.path(outputDir, "convergence.csv"),
      append = TRUE
    )
    saveRDS(optimStatus, file = file.path(outputDir, "bestOptimStatus.RDS"))
    saveRDS(optimEnv$dtRes, file = file.path(outputDir, "bestPrediction.RDS"))
  }
}

#' Evaluate Time Profiles
#'
#' Updates parameter values and runs results for each scenario.
#'
#' @param optimEnv The optimization environment containing current state variables.
#' @param scenarioList A list of scenarios to be evaluated.
#' @param dtPrior A data.table containing prior values.
#' @param dtStartValues A data.table containing start values.
#' @param dtMappedPaths A data.table containing mapped paths for parameters.
#' @param simulationRunOptions An obkect of class SimulationRunOptions.
#' @param dataObservedForMatch A data.table containing observed data for matching.
#'
#' @return NULL (invisible).
#' @keywords internal
evaluateTimeprofiles <-  function(optimEnv,
                                  scenarioList,
                                  dtPrior,
                                  dtStartValues,
                                  dtMappedPaths,
                                  simulationRunOptions,
                                  dataObservedForMatch) {

  # update parameter values and run result
  invisible(lapply(names(scenarioList), function(scenarioName) {
    updateParameterValues(
      scenarioName = scenarioName,
      scenario = scenarioList[[scenarioName]],
      dtPrior = dtPrior,
      dtStartValues = dtStartValues,
      dtMappedPaths = dtMappedPaths
    )
  }))
  optimEnv$scenarioResults <-
    esqlabsR::runScenarios(scenarios = scenarioList, simulationRunOptions = simulationRunOptions)


  optimEnv$dtRes <- getPredictionsForScenarios(optimEnv$scenarioResults,
                                               dataObservedForMatch)

  return(invisible())

}

#' Get Predictions for Scenarios
#'
#' This function generates predictions for a set of scenarios based on observed data.
#'
#' @param scenarioResults A list of scenario results, each containing population and covariate information.
#' @param dataObservedForMatch A data.table containing observed data, which must include the required columns:
#'        'scenario', 'outputPathId', 'yValues', and 'yUnit'.
#' @param aggregationFun A function for aggregation (optional). If provided, it will be used to aggregate the simulated results.
#' @param identifier A character vector of identifiers for matching, defaults to c("outputPath", "individualId").
#'
#' @return A data.table containing the predicted values for each scenario, along with the scenario name and other relevant identifiers.
#'
#' @details The function validates the input names, loops through each scenario to generate predictions, and combines all results into a single data.table.
#' It also handles individual matching if 'ObservedIndividualId' is present in the scenario results.
#'
#' @export
getPredictionsForScenarios <- function(scenarioResults,
                                       dataObservedForMatch,
                                       aggregationFun = NULL,
                                       identifier = c("outputPath", "individualId")) {

  # Validate input names
  checkmate::assertNames(names(dataObservedForMatch), must.include = c('scenario', 'unitFactorX','unitFactorY',identifier))
  # Initialize a list to store results for each scenario
  resultsList <- list()

  # Loop through each scenario
  for (scenarioName in names(scenarioResults)) {
    scenarioResult <- scenarioResults[[scenarioName]]

    individualMatch <- NULL
    if ("ObservedIndividualId" %in% scenarioResult$population$allCovariateNames) {
      individualMatch <- data.table(
        individualId = scenarioResult$population$allIndividualIds,
        observedIndividualId = scenarioResult$population$getCovariateValues('ObservedIndividualId')
      )
    }

    # Get simulated time profile
    dtSimulated <- getSimulatedTimeprofile(
      simulatedResult = scenarioResult,
      outputPaths = unique(dataObservedForMatch[scenario == scenarioName,]$outputPath),
      aggregationFun = aggregationFun,
      individualMatch = individualMatch
    ) %>%
      dplyr::mutate(scenario = scenarioName) %>%
      data.table::setnames('paths', 'outputPath') %>%
      merge( dataObservedForMatch[scenario == scenarioName,] %>%
               dplyr::select(all_of(c(identifier,'unitFactorX','unitFactorY'))) %>%
               unique(),
             by = identifier) %>%
      .[,xValues := xValues*unitFactorX] %>%
      .[,yValues := yValues*unitFactorY]

    # Add predicted values and store in the results list
    resultsList[[scenarioName]] <- addPredictedValues(
      dtObserved = dataObservedForMatch[scenario == scenarioName],
      dtSimulated = dtSimulated,
      identifier = identifier
    ) %>%
      dplyr::mutate(scenarioName = scenarioName)
  }

  # Combine all results into a single data.table
  dtResult <- rbindlist(resultsList, use.names = TRUE, fill = TRUE)

  return(dtResult)
}

#' Update Model Error
#'
#' Updates the model error values in the results based on prior definitions.
#'
#' @param dtPrior A data.table containing prior values.
#' @param dtRes A data.table containing results to update.
#'
#' @return A data.table containing updated results.
#' @keywords internal
updateModelError <- function(dtPrior,dtRes){
dtRes <- dtRes %>%
  dplyr::select(!any_of('sigma'))  %>%
  merge(
    dtPrior[valueMode == PARAMETERTYPE$outputError, c("name", "value")] %>%
      data.table::setnames("value", "sigma"),
    by.x = "modelErrorId",
    by.y = "name"
  )

return(dtRes)
}


#' Update Parameter Values
#'
#' This function updates parameter values in the scenario based on mapped paths, start values, and prior definitions.
#'
#' @param scenarioName A string representing the name of the scenario.
#' @param scenario An object representing the scenario.
#' @param dtPrior A data.table containing prior values.
#' @param dtStartValues A data.table containing start values.
#' @param dtMappedPaths A data.table containing mapped paths for parameters.
#'
#' @return NULL (invisible).
#' @keywords internal
updateParameterValues <- function(scenarioName, scenario, dtPrior, dtStartValues, dtMappedPaths) {
  # initialize variables to avoid linter messages
  currentValue <- value <- newValue <- individualId <- valueMode <- NULL

  scenarioName <- names(scenarioList)[1]
  scenario <- scenarioList[[scenarioName]]

  dtMappedPathsForScenarios <-
    dtMappedPaths[!is.na(get(scenarioName))] %>%
    dplyr::select(dplyr::all_of(c(
      "name", "linkedParameters", "useAsFactor", scenarioName
    ))) %>%
    data.table::setnames(scenarioName, "factor")

  # global parameter
  dtCustomParams <- dtPrior[valueMode == PARAMETERTYPE$global] %>%
    dplyr::select(c("name", "value")) %>%
    merge(dtMappedPathsForScenarios, by = "name")

  if (nrow(dtCustomParams) > 0) {
    for (dp in split(dtCustomParams, by = "linkedParameters")) {
      scenario$population$setParameterValues(
        parameterOrPath = dp$linkedParameters,
        values = rep(
          dp$value * dp$factor,
          scenario$population$count
        )
      )
    }
  }

  individualIds <-
    scenario$population$getCovariateValues("ObservedIndividualId")

  dtCustomParams <- dtStartValues[individualId %in% individualIds] %>%
    dplyr::select(c("name", "value", "individualId")) %>%
    merge(dtMappedPathsForScenarios, by = "name", allow.cartesian = TRUE)

  if (nrow(dtCustomParams) > 0) {
    for (dp in split(dtCustomParams, by = "linkedParameters")) {
      pt <- dp$linkedParameters[1]
      # make sure to use default value for individuals where no fit parameter exist
      tmp <-
        data.table(
          individualId = scenario$population$getCovariateValues("ObservedIndividualId"),
          currentValue = scenario$population$getParameterValues(pt)
        ) %>%
        merge(dp,
              by = "individualId",
              all.x = TRUE,
              sort = FALSE
        )
      tmp[, newValue := ifelse(is.na(value), currentValue, value * factor)]

      scenario$population$setParameterValues(
        parameterOrPath = pt,
        values = tmp$newValue
      )
    }
  }

  return(invisible())
}
