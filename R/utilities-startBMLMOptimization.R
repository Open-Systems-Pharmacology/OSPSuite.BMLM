
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
    optimEnv <- initializeOptimEnv(dtList = dtList,
                                   failValue = failValue,
                                   bestValue = dtList$bestValue,
                                   NAcounter = dtList$NAcounter,
                                   iteration = dtList$iteration)

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


#' Evaluate Model at Initial Values
#'
#' This function evaluates initial values for a given project configuration by running simulations for specified scenarios.
#' It logs the progress and results to both the console and a designated log file.
#'
#' @param dtList A data table containing the necessary data for the evaluation, including prior and start values.
#' @param outputDir The directory where the log file will be saved.
#'
#' @return NULL This function does not return any value but logs information to the console and a log file.
#'
#' @export
evaluateInitialValues <- function(dtList,
                                  outputDir,
                                  scenarioList){

  logAndPrintOptimization('Start modelevaluation at initial values:',outputDir = outputDir)

  optimEnv <- initializeOptimEnv(dtList)

  initialValues <- getParams(
    dtPrior = dtList$prior,
    dtStartValues = dtList$startValues,
    optimizationGroup = 'both'
  )

  dtList <- setParameterToTables(dtList = dtList, params = initialValues)

  evaluateTimeprofiles(optimEnv = optimEnv,
                       scenarioList = scenarioList,
                       dtList = dtList,
                       simulationRunOptions = SimulationRunOptions$new(showProgress = TRUE),
                       withProtocol = TRUE,
                       outputDir = outputDir)


  loglikelihoods <- getLogLikelihood(
    dtPrior = dtList$prior,
    dtStartValues = dtList$startValues,
    dtRes = rbindlist(optimEnv$dtResList)
  )

  logAndPrintOptimization(c('\n','Initial loglikelihood:'),outputDir = outputDir,quiet = FALSE,asNew = FALSE)
  logAndPrintOptimization(paste(paste0(names(loglikelihoods),': ',signif(loglikelihoods,3)),collapse = ', '),
              outputDir = outputDir,quiet = FALSE,asNew = FALSE)

  analyseInitalSimulationFailures(dtList, optimEnv, loglikelihoods, outputDir)

  if (!file.exists(file = file.path(outputDir, "optimStatus.RDS"))){

    optimStatus <- updateOptimStatus(dtPrior = dtList$prior,
                                     dtStartValues = dtList$startValues,
                                     optimEnv)
    currentValue <- evaluateLogLikelihood(loglikelihoods, optimEnv)

    if (is.finite(currentValue)){
      optimStatus[["loglikelihoods"]] <- loglikelihoods

      saveRDS(optimStatus, file = file.path(outputDir, "optimStatus.RDS"))
      # Check if objective function value has improved
      updateBestValueIfImproved(currentValue, optimEnv, optimStatus, outputDir, loglikelihoods)
    }
  }

  return(invisible())

}
#' Analyze Initial Simulation Failures
#'
#' This function analyzes the results of initial simulations to identify any failures
#' based on the log-likelihood values. It checks for individuals with missing predicted
#' values and infinite log-likelihoods, updating the model error as necessary.
#'
#' @param dtList A list containing data tables relevant to the prior model and results.
#' @param optimEnv An environment object that contains optimization results.
#' @param loglikelihoods A named vector containing log-likelihood values
#' @param outputDir A character string specifying the directory where logs and outputs
#'   should be saved.
#'
#' @return Returns nothing (invisible NULL). The function performs logging and updates
#'   the model error in the provided data tables.
#'
#' @keywords internal
analyseInitalSimulationFailures <- function(dtList, optimEnv, loglikelihoods, outputDir) {
  if (is.na(loglikelihoods['logTimeProfile'])) {
    dtRes <- rbindlist(optimEnv$dtResList)
    dtResNA <- dtRes[is.na(predicted), c("scenario", "individualId")] %>% unique()

    if (nrow(dtResNA) == 0) {
      logAndPrintOptimization('No simulation failures.', outputDir = outputDir, quiet = FALSE, asNew = FALSE)
    } else {
      logAndPrintOptimization('Individuals with simulation failures:', outputDir = outputDir, quiet = FALSE, asNew = FALSE)
      logAndPrintOptimization(utils::capture.output(print(dtResNA)), outputDir = outputDir, quiet = FALSE, asNew = FALSE)
    }

    dtResUpdated <- updateModelError(dtPrior = dtList$prior, dtRes = dtRes)
    dtResUpdated[, isCensored := !is.na(lloq) & lloq > yValues]

    dtResUpdated[, logLikelihood := mapply(calculateLogLikelihood, yValues, predicted, errorModel, sigma, isCensored, lloq, lowerBound)]
    dtResUpdated <- dtResUpdated[!is.na(predicted) & is.infinite(logLikelihood), c("scenario", "individualId", 'outputPathId', 'errorModel', 'yValues', 'predicted', 'lloq', 'lowerBound', 'sigma', 'logLikelihood')]

    if (nrow(dtResUpdated) == 0) {
      logAndPrint('No infinite loglikelihoods.', outputDir = outputDir, quiet = FALSE, asNew = FALSE)
    } else {
      logAndPrintOptimization('Rows with infinite loglikelihood:', outputDir = outputDir, quiet = FALSE, asNew = FALSE)
      logAndPrintOptimization(utils::capture.output(print(dtResUpdated)), outputDir = outputDir, quiet = FALSE, asNew = FALSE)
    }
  }

  return(invisible())
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
initializeOptimEnv <- function(dtList,
                               failValue = Inf,
                               bestValue = Inf,
                               NAcounter = 0,
                               iteration = 0) {
  optimEnv <- new.env()

  optimEnv$iteration <- iteration
  optimEnv$bestValue <- bestValue
  optimEnv$lastSaveTime <- Sys.time()
  optimEnv$failValue <- failValue
  optimEnv$NAcounter <- NAcounter
  optimEnv$dtResList <- list()
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
        dtList = dtList,
        simulationRunOptions = simulationRunOptions
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
        dtRes = rbindlist(optimEnv$dtResList)
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

      loglikelihoods <-
        getLogLikelihood(
          dtPrior = dtList$prior,
          dtStartValues = dtList$startValues,
          dtRes = rbindlist(optimEnv$dtResList)
        )
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
    return(min(-1 * sum(loglikelihoods),optimEnv$failValue))
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
  if (currentValue < optimEnv$bestValue & is.finite(currentValue)) {
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
    saveRDS(optimEnv$dtResList, file = file.path(outputDir, "bestPrediction.RDS"))
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
                                  dtList,
                                  simulationRunOptions,
                                  withProtocol = FALSE,
                                  outputDir = NULL) {


  results <- lapply(scenarioList, function(scenario) {
    scenarioName <- scenario$scenarioConfiguration$scenarioName
    if (withProtocol){
      logAndPrintOptimization(message = paste('scenario:', scenarioName),
                              outputDir = outputDir,
                              quiet = FALSE,asNew = FALSE)
      logAndPrintOptimization(
        message = paste('Individual count:', scenarioList[[scenarioName]]$population$count),
        outputDir = outputDir,
        quiet = FALSE,asNew = FALSE
      )
      logAndPrintOptimization(message = 'Simulate:',
                              outputDir = outputDir,
                              quiet = FALSE,asNew = FALSE)
      tictoc::tic()
    }

    results <- processScenario(
      scenarioName = scenarioName,
      scenario = scenario,
      dtList = dtList,
      simulationRunOptions = simulationRunOptions
    )

    if (withProtocol){
      logAndPrintOptimization(message = utils::capture.output(tictoc::toc()),
                              outputDir = outputDir,
                              quiet = FALSE,asNew = FALSE)
    }

    return(results)
  })

  # Store results in optimEnv
  for (r in results) {
    scenarioName <- r$scenarioName
    optimEnv$scenarioResults[[scenarioName]] <- r$scenarioResult
    optimEnv$dtResList[[scenarioName]] <- r$predictions
  }

  return(invisible())

}

#' Process a Scenario
#'
#' This function updates parameter values for a given scenario, runs the scenario using the specified simulation options,
#' and retrieves predictions based on the scenario results.
#'
#' @param scenarioName A character string representing the name of the scenario to be processed.
#' @param scenario A list containing the parameters and settings specific to the scenario.
#' @param dtList A list containing data tables:
#'   \itemize{
#'     \item \code{prior}: Data table for prior parameters.
#'     \item \code{startValues}: Data table for starting values.
#'     \item \code{mappedPaths}: Data table for mapped paths.
#'     \item \code{data}: Data table used for predictions.
#'   }
#' @param simulationRunOptions A list containing options for running the simulation, which may include settings such as the number of iterations, random seed, etc.
#'
#' @return A list containing:
#'   \item{result}{A list with the results of the scenario run.}
#'   \item{predictions}{A data frame or list of predictions generated from the scenario results.}
#'
#' @export
processScenario <- function(scenarioName, scenario, dtList,  simulationRunOptions) {
  # Update parameter values and run result
  updateParameterValues(
    scenarioName = scenarioName,
    scenario = scenario,
    dtPrior = dtList$prior,
    dtStartValues = dtList$startValues,
    dtMappedPaths = dtList$mappedPaths
  )

  scenarioResult <- esqlabsR::runScenarios(scenarios = list(scenarioName = scenario),
                                            simulationRunOptions = simulationRunOptions)[[1]]

  predictions <- getPredictionsForScenario(scenarioResult, scenarioName, dtList$data)

  return(list(scenarioResult = scenarioResult, predictions = predictions,scenarioName = scenarioName))
}



#' Get Predictions for Scenarios
#'
#' This function generates predictions for a set of scenarios based on observed data.
#'
#' @param scenarioResult An ScenarioResult object  containingsimulated results.
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
getPredictionsForScenario <- function(scenarioResult,
                                      scenarioName,
                                       dataObservedForMatch,
                                       aggregationFun = NULL,
                                       identifier = c("outputPath", "individualId")) {

  # Validate input names
  checkmate::assertNames(names(dataObservedForMatch), must.include = c('scenario', 'unitFactorX','unitFactorY',identifier))
  # Initialize a list to store results for each scenario
  resultsList <- list()

  # Loop through each scenario
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
  dtResult <- addPredictedValues(
      dtObserved = dataObservedForMatch[scenario == scenarioName],
      dtSimulated = dtSimulated,
      identifier = identifier
    ) %>%
      dplyr::mutate(scenarioName = scenarioName)


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
      currentValue = scenario$population$getParameterValues(pt)
      if (is.null(currentValue))
        currentValue <- getParameter(path = pt,container = scenario$simulation)$value

      tmp <-
        data.table(
          individualId = scenario$population$getCovariateValues("ObservedIndividualId"),
          currentValue = currentValue
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


logAndPrintOptimization <- function(message,outputDir,quiet = TRUE,asNew = TRUE) {

  cat(
    ifelse(asNew,format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"     "),
    message,
    "\n",
    file = file.path(outputDir, "optimization_log.txt"),
    append = TRUE
  )

  # Print to console
  if (!quiet)  cat(message, "\n")

  return(invisible())

}

