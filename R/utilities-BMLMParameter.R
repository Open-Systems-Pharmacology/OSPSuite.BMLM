#' Start BMLM Optimization
#'
#' This function initiates the BMLM optimization process based on the provided configuration and parameters.
#'
#' @param projectConfiguration A list containing project configuration details.
#' @param runName A character string representing the name of the current run.
#' @param scenarioList A list of scenarios to be optimized.
#' @param dataObserved A data.table containing the observed data to be fitted
#' @param seed An integer for random number generation (default is 1234).
#' @param method A character string specifying the optimization method (default is "L-BFGS-B").
#' @param control A list of control parameters for the optimization function.
#' @param hessian A logical indicating whether to compute the Hessian matrix (default is FALSE).
#' @param simulationRunOptions A list of options for simulation runs.
#' @param restartAtBestValues A logical indicating whether to restart from the best values found (default is FALSE).
#' @param failValue value to set if evaluation of objective function fails.
#' @param ... Additional arguments passed to the optimization function.
#'
#' @return Returns NULL invisibly.
#' @export
startBMLMOptimization <- function(projectConfiguration,
                                  runName,
                                  scenarioList,
                                  dataObserved,
                                  seed = 1234,
                                  method = "L-BFGS-B",
                                  control = list(),
                                  hessian = FALSE,
                                  simulationRunOptions = NULL,
                                  restartAtBestValues = FALSE,
                                  failValue = -1e+10,
                                  ...) {

  # Check BMLM Configuration
  checkBMLMConfiguration(projectConfiguration)

  # Create output directory and handle user confirmation
  outputDir <- manageOutputDirectory(projectConfiguration$outputFolder, runName)

  # Create log file
  logFile <- file.path(outputDir, "optimization_log.txt")

  # Log start time
  startTime <- Sys.time()
  logMessage(logFile, "Optimization started.")

  # Save function call details

  browser()
  logFunctionCall(
    logFile = logFile,
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioList,
    seed = seed,
    dataObservedExpr = deparse(substitute(dataObserved)),
    method = method,
    controlExpr = deparse(substitute(control)),
    hessian = hessian,
    restartAtBestValues = restartAtBestValues,
    failValue = failValue,
    ...
  )


  # Initialize data list
  dtList <- createDtList(projectConfiguration, scenarioList, dataObserved, seed)

  # # Save counts per identifier
  # saveRDS(object = dtList$data[,.N,by = c('scenario','outputPathId','individualId','group')],
  #         file =

  # Initialize optimization variables
  bestValue <- initializeOptimization(restartAtBestValues, outputDir, dtList,failValue)

  # Perform optimization
  result <- optimizeParameters(dtList, scenarioList, simulationRunOptions, method, control, hessian, outputDir)

  # Save data tables as CSV
  saveDataTablesAsCSV(dtList = dtList, outputDir = outputDir)

  # Log end time
  logMessage(logFile, "Optimization finalized")
  logMessage(logFile, paste("Total duration:", Sys.time() - startTime))

  return(invisible())
}

#' Check BMLM Configuration
#'
#' Validates the presence of the BMLM configuration file in the project configuration.
#'
#' @param projectConfiguration A list containing project configuration details.
#'
#' @return NULL if the configuration is valid; stops execution if invalid.
#' @keywords internal
checkBMLMConfiguration <- function(projectConfiguration) {
  if (is.null(projectConfiguration$addOns$bMLMConfigurationFile)) {
    stop("Project configuration has no BMLM Configuration attached!")
  }
  checkmate::assertFileExists(projectConfiguration$addOns$bMLMConfigurationFile)
}

#' Manage Output Directory
#'
#' Creates the output directory for the optimization run and prompts the user for confirmation if it already exists.
#'
#' @param baseDir A character string representing the base directory for output.
#' @param runName A character string representing the name of the current run.
#'
#' @return A character string representing the path to the output directory.
#' @keywords internal
manageOutputDirectory <- function(baseDir, runName) {
  outputDir <- file.path(baseDir, "BMLM", runName)

  if (dir.exists(outputDir) & interactive()) {
    message("The output folder for ", runName, " already exists!")
    userInput <- readline(prompt = paste("Do you want to continue? (yes/no): "))
    if (tolower(userInput) != "yes") {
      cat("Operation cancelled by user.\n")
      return(NULL)  # Exit the function if the user does not confirm
    }
  } else {
    dir.create(outputDir, recursive = TRUE)
  }

  return(outputDir)
}

#' Log Message
#'
#' Logs a message to the specified log file with a timestamp.
#'
#' @param logFile A character string representing the path to the log file.
#' @param message A character string containing the message to log.
#'
#' @return NULL
#' @keywords internal
logMessage <- function(logFile, message) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message, "\n", file = logFile, append = TRUE)
}

#' Log Function Call
#'
#' Logs the details of the function call to the specified log file.
#'
#' @param logFile A character string representing the path to the log file.
#' @param projectConfiguration A list containing project configuration details.
#' @param scenarioList A list of scenarios to be optimized.
#' @param dataObservedExpr dataObserved
#' @param seed An integer for random number generation.
#' @param method A character string specifying the optimization method.
#' @param controlExpr A list of control parameters for the optimization function.
#' @param hessian A logical indicating whether to compute the Hessian matrix.
#' @param restartAtBestValues A logical indicating whether to restart from the best values found.
#' @param failValue value to set if evaluation of objective function fails
#' @param ... Additional arguments passed to the optimization function.
#'
#' @return NULL
#' @keywords internal
logFunctionCall <- function(logFile, projectConfiguration, scenarioList, dataObservedExpr,seed, method, controlExpr, hessian, restartAtBestValues,failValue, ...) {


  # Capture additional arguments
  additionalArgs <- list(...)
  additionalArgsExpr <- sapply(names(additionalArgs), function(arg) {
    if (is.null(arg)) {
      return(deparse(additionalArgs[[arg]]))
    } else {
      return(paste(arg, "=", deparse(additionalArgs[[arg]]), sep = ""))
    }
  })

  callDetails <- paste("Function call:\n",
                       "      BMLM Configuration file: ", basename(projectConfiguration$addOns$bMLMConfigurationFile), "\n",
                       "      scenarios: ", paste(names(scenarioList), collapse = ', '), "\n",
                       "      dataObserved: ",dataObservedExpr,"\n",
                       "      seed: ", seed, "\n",
                       "      method: ", method, "\n",
                       "      control: ", controlExpr, "\n",
                       "      hessian: ", hessian, "\n",
                       "      restartAtBestValues: ", restartAtBestValues, "\n",
                       "      failValue: ", getOption("OSPSuite.BMLM.failValue"), "\n",
                       "      Additional arguments: ", paste(additionalArgsExpr, collapse = ", "), "\n",
                       sep = "")

  logMessage(logFile, callDetails)
}

#' Initialize Optimization
#'
#' Initializes optimization variables and loads previous results if restarting from the best values.
#'
#' @param restartAtBestValues A logical indicating whether to restart from the best values found.
#' @param outputDir A character string representing the path to the output directory.
#' @param dtList A list containing data tables for optimization.
#'
#' @return A numeric value representing the initialized best value.
#' @keywords internal
initializeOptimization <- function(restartAtBestValues, outputDir, dtList,failValue) {
  options(OSPSuite.BMLM.failValue = failValue)
  if (restartAtBestValues) {
    load(file = file.path(outputDir, "tmpbestResult.Rdata"))
    dtList$input$param <- params
    return(-1 * sum(loglikelihoods))
  } else {
    return(Inf)  # Initialize bestValue to Inf
  }
}

#' Optimize Parameters
#'
#' Performs the optimization of parameters using the specified method and control settings.
#'
#' @param dtList A list containing data tables for optimization.
#' @param scenarioList A list of scenarios to be optimized.
#' @param simulationRunOptions A list of options for simulation runs.
#' @param method A character string specifying the optimization method.
#' @param control A list of control parameters for the optimization function.
#' @param hessian A logical indicating whether to compute the Hessian matrix.
#' @param outputDir A character string representing the path to the output directory.
#'
#' @return The result of the optimization process.
#' @keywords internal
optimizeParameters <- function(dtList, scenarioList, simulationRunOptions, method, control, hessian, outputDir) {
  iteration <- 0
  bestValue <- Inf

  result <- optim(
    par = dtList$input$param,
    fn = function(params) {
      iteration <<- iteration + 1
      loglikelihoods <- getLogLikelihood(params, scenarioList, dtList, simulationRunOptions)
      currentValue <- -1 * sum(loglikelihoods)

      if (!is.numeric(currentValue)) {
        message(paste(loglikelihoods[1], loglikelihoods[2], loglikelihoods[3], iteration, sep = ", "))
        save(params, iteration, loglikelihoods, file = file.path(outputDir, "tmpStrangeResult.Rdata"))  # Save the current parameters
        currentValue <- getOption("OSPSuite.BMLM.failValue")
      }

      if (currentValue < bestValue) {
        bestValue <<- currentValue
        message(paste(loglikelihoods[1], loglikelihoods[2], loglikelihoods[3], iteration, sep = ", "))
        save(params, iteration, loglikelihoods, file = file.path(outputDir, "tmpbestResult.Rdata"))  # Save the current parameters
      }

      return(currentValue)
    },
    method = method,
    control = control,
    hessian = hessian
  )

  return(result)
}
# initialization functions ------------------


#' Create Data List for BMLM Optimization
#'
#' This helper function creates a list of data.tables (`dtList`) required for the BMLM optimization process.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details.
#' @param scenarioList A list of scenarios to be used in the optimization.
#' @param dataObserved A data.table containing observed data.
#' @param seed A numeric seed for random number generation.
#'
#' @return A list of data.tables required for the BMLM optimization process.
#' @keywords internal
createDtList <- function(projectConfiguration, scenarioList, dataObserved, seed) {
  dtList <- list()

  dtList$definition <- validateAndLoadDefinition(projectConfiguration)

  dtList$data <- prepareDataForMatch(
    projectConfiguration = projectConfiguration,
    dataObserved = dataObserved,
    scenarioList = scenarioList
  )

  dtList$mappedPaths <- validateAndLoadMappedPaths(projectConfiguration, dtList$definition, scenarioList)

  dtList$prior <- validateAndLoadPriorDefinition(
    projectConfiguration = projectConfiguration,
    dtDefinition = dtList$definition
  )

  dtList$startValues <- validateAndLoadIndividualStartValues(
    projectConfiguration = projectConfiguration,
    dtHyperParameter = getHyperParameter(dtPrior = dtList$prior),
    dtDefinition = dtList$definition,
    seed = seed
  )

  dtList$data <- addAndValidateErrorModel(
    projectConfiguration = projectConfiguration,
    dtPrior = dtList$prior,
    dataObservedForMatch = dtList$data
  )

  dtList$input <-
    prepareInputData(
      dtPrior = dtList$prior,
      dtStartValues = dtList$startValues
    )

  return(dtList)
}

#' Validate and Load Definition
#'
#' This function validates and loads parameter definitions from the BMLM configuration file.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details.
#'
#' @return A data.table containing validated parameter definitions.
#' @keywords internal
validateAndLoadDefinition <- function(projectConfiguration) {
  dtDefinition <- xlsxReadData(
    projectConfiguration$addOns$bMLMConfigurationFile,
    sheetName = "ParameterDefinition",
    skipDescriptionRow = TRUE
  )
  checkmate::assertCharacter(dtDefinition$name, any.missing = FALSE, unique = TRUE)
  checkmate::assertNames(dtDefinition$valueMode, subset.of = c(PARAMETERTYPE$global, PARAMETERTYPE$individual))
  checkmate::assertNames(dtDefinition$distribution, subset.of = getAllDistributions())
  checkmate::assertLogical(as.logical(dtDefinition$useAsFactor), any.missing = FALSE)
  checkmate::assertLogical(as.logical(dtDefinition$hasHyperparameter), any.missing = FALSE)

  dtDefinition$scaling <- NULL

  return(dtDefinition)
}

#' Validate and Load Mapped Paths
#'
#' This function validates and loads mapped paths from the BMLM configuration file.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details.
#' @param dtDefinition A data.table containing parameter definitions.
#' @param scenarioList A list of scenarios to be used in the optimization.
#'
#' @return A data.table containing mapped paths for parameters.
#' @keywords internal
validateAndLoadMappedPaths <- # nolint cyclocomp
  function(projectConfiguration,
           dtDefinition,
           scenarioList) {
    # initialize variable to avoid linter message
    scenarioName <- linkedParameters <- NULL

    dtMappedPaths <-
      xlsxReadData(
        projectConfiguration$addOns$bMLMConfigurationFile,
        sheetName = "ParameterMappedPaths",
        skipDescriptionRow = TRUE
      )

    checkmate::assertNames(unique(dtMappedPaths$name), must.include = unique(dtDefinition$name))

    dtMappedPaths <- dtMappedPaths %>%
      merge(dtDefinition[, c("name", "unit", "useAsFactor")], by = "name", sort = FALSE)
    dtMappedPaths[is.na(unit), unit := ""]


    for (scenarioName in names(scenarioList)) {
      scenario <- scenarioList[[scenarioName]]

      sim <- scenario$simulation
      dtMappedPaths[, (scenarioName) := NA_real_]

      for (dp in split(dtMappedPaths, by = "linkedParameters")) {
        if (is.na(dp$scenarios) ||
          grepl(scenarioName, splitInputs(dp$scenarios))) { # nolint identation
          par <-
            ospsuite::getParameter(
              path = dp$linkedParameters,
              container = sim,
              stopIfNotFound = FALSE
            )
          if (is.null(par)) {
            factor <- NA
          } else if (as.logical(dp$useAsFactor)) {
            factor <- par$value
          } else {
            factor <-
              ospsuite::toBaseUnit(
                quantityOrDimension = par$dimension,
                values = 1,
                unit = dp$unit
              )
          }
          dtMappedPaths[linkedParameters == dp$linkedParameters, (scenarioName) := as.numeric(factor)]

          # set all parameters as population parameters
          if (!(dp$linkedParameters %in% scenario$population$allParameterPaths) & !is.na(factor)) {
            scenario$population$setParameterValues(
              parameterOrPath = dp$linkedParameters,
              values = rep(par$value, scenario$population$count)
            )
          }
        }
      }
    }

    return(dtMappedPaths)
  }


#' Get Hyper Parameters
#'
#' This function retrieves hyperparameters from the prior definition.
#'
#' @param dtPrior A data.table containing prior definitions.
#'
#' @return A data.table containing hyperparameters and their distributions.
#' @keywords internal
getHyperParameter <- function(dtPrior) {
  # initialize variable to avoid linter message
  valueMode <- NULL

  dtHyperParameter <-
    dtPrior[valueMode == PARAMETERTYPE$hyperParameter] %>%
    dplyr::select(c(
      "name",
      "hyperParameter",
      "categoricCovariate",
      "value",
      "hyperDistribution",
      "scaling"
    ))

  return(dtHyperParameter)
}

#' Randomize Start Values
#'
#' This function processes a single group of start values, merging with hyperparameters and generating new values.
#'
#' @param indGroup A data.table containing a subset of start values for a specific individual group.
#' @param dtHyperParameter A data.table containing hyperparameters and their distributions.
#'
#' @return A data.table with updated start values for the individual group.
#' @keywords internal
randomizeIndividualStartValues <-
  function(indGroup, dtHyperParameter) {
    # initialize variable to avoid linter message
    name <- categoricCovariate <- scaling <- value <- minValue <- maxValue <- NULL
    setDT(indGroup)
    nNew <- sum(is.na(indGroup$value))

    # Merge with hyperparameters
    tmp <-
      merge(dtHyperParameter,
        unique(indGroup[, .(name, categoricCovariate)]),
        by = c("name", "categoricCovariate")
      )

    # Apply log scaling if necessary
    tmp[scaling == SCALING$log, value := log(value)]

    # Create a list of parameters for the distribution function
    paramList <- stats::setNames(tmp$value, tmp$hyperParameter)

    # Generate new values based on the distribution
    trials <- 0
    while (nNew > 0 & trials < 10) {
      values <-
        do.call(paste0("r", tmp$hyperDistribution[1]), c(list(n = nNew), paramList))
      indGroup[is.na(indGroup$value), value := values]

      # set Values outside limits to limits
      indGroup[value <= minValue, value := NA]
      indGroup[value >= maxValue, value := NA]

      nNew <- sum(is.na(indGroup$value))
      trials <- trials + 1
    }
    if (nNew > 0) {
      stop("Not possible to generate random startValues in boundarys")
    }


    # Check if all values are within the distribution range
    probs <-
      do.call(paste0("d", tmp$hyperDistribution[1]), c(list(x = indGroup$value), paramList))

    if (any(is.na(probs)) | any(probs <= 0)) {
      writeTableToLog(indGroup[which(is.na(probs) | probs <= 0)])
      stop(paste("There are start values outside the distribution range"))
    }

    return(indGroup)
  }

#' Get Individual Start Values
#'
#' This function reads individual start values from an Excel file and adjusts them based on hyperparameters.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details.
#' @param dtHyperParameter A data.table containing hyperparameters and their distributions.
#' @param dtDefinition A data.table containing parameter definitions.
#' @param seed A numeric seed for random number generation.
#'
#' @return A data.table with updated start values.
#' @keywords internal
validateAndLoadIndividualStartValues <-
  function(projectConfiguration,
           dtHyperParameter,
           dtDefinition,
           seed) {
    # initialize variables to avoid linter messages
    value <- startValue <- NULL

    set.seed(seed)

    dtStartValues <- xlsxReadData(
      projectConfiguration$addOns$bMLMConfigurationFile,
      sheetName = "IndividualStartValues",
      skipDescriptionRow = TRUE
    ) %>%
      merge(dtDefinition[,c('name','minValue','maxValue')],
            by = "name",
            sort = FALSE,
            all.x = TRUE
      )

    dtStartValues <- checkMinMaxValues(dtStartValues)

    # Randomize startValues
    dtStartValuesNew <- data.table::rbindlist(
      lapply(
        split(dtStartValues, by = c("name", "categoricCovariate")),
        randomizeIndividualStartValues,
        dtHyperParameter = dtHyperParameter
      )
    )

    dtStartValuesNew[, startValue := value]

    return(dtStartValuesNew)
  }

#' Get Prior Definition
#'
#' This function retrieves prior definitions from the configuration file.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details.
#' @param dtDefinition A data.table containing parameter definitions.
#'
#' @return A data.table with prior definitions.
#' @keywords internal
validateAndLoadPriorDefinition <- function(projectConfiguration, dtDefinition) {
  # initialize variables to avoid linter messages
  probability <- startValue <- NULL

  dtPrior <- xlsxReadData(
    projectConfiguration$addOns$bMLMConfigurationFile,
    sheetName = "Prior",
    skipDescriptionRow = TRUE
  ) %>%
    merge(dtDefinition[, c("name",  "distribution")] %>%
            data.table::setnames("distribution", "hyperDistribution"),
          by = 'name',
          all.x = TRUE
    )

  getPriorDistributionLimit <- function(distribution,p1_type, p1_value, p2_type, p2_value, p3_type, p3_value,limitName){
    params <- stats::setNames(list(p1_value,p2_value,p3_value),c(p1_type,p2_type,p3_type))
    params <- params[!is.na(params)]
    if (distribution == 'unif'){
      return(params[[limitName]])
    } else{
      p <- switch(limitName,
                  min = 0,
                  max = 1)
      do.call(paste0('q',distribution),args = c(list(p = p),params))
    }
  }

  dtPrior[,minValue := getPriorDistributionLimit(distribution,p1_type, p1_value, p2_type, p2_value, p3_type, p3_value,'min'),by = .I]
  dtPrior[,maxValue := getPriorDistributionLimit(distribution,p1_type, p1_value, p2_type, p2_value, p3_type, p3_value,'max'),by = .I]

  dtPrior <- checkMinMaxValues(dtPrior)

  if (any(is.na(dtPrior$startValue))) {
    stop(paste(
      'Please insert startValues in sheet "Prior" for:',
      paste(unique(dtPrior[is.na(startValue)]$name), collapse = ", ")
    ))
  }

  checkmate::assertNames(dtPrior$distribution, subset.of = getAllDistributions())


  dtPrior[, probability := apply(.SD, 1, calculateProbability)]


  if (any(is.na(dtPrior$probability))) {
    stop(paste(
      "Probability of startvalue is NA, check priors ",
      paste(dtPrior[is.na(probability)]$name, collapse = ", ")
    ))
  }

  if (any(dtPrior$probability <= 0)) {
    stop(paste(
      "Start value outside distribution range, check",
      paste(dtPrior[probability <= 0]$name, collapse = ", ")
    ))
  }

  return(dtPrior)
}

#' Add and Validate Error Model
#'
#' This function adds and validates the error model based on the project configuration and observed data.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details.
#' @param dtPrior A data.table containing prior definitions.
#' @param dataObservedForMatch A data.table containing observed data for matching.
#'
#' @return The data.table `dataObservedForMatch` with merged error model information.
#' @keywords internal
addAndValidateErrorModel <- function(projectConfiguration = projectConfiguration, dtPrior, dataObservedForMatch) {
  # initialize variables to avoid linter messages
  lowerBound <- valueMode <- NULL

  dtErrorModel <- xlsxReadData(
    projectConfiguration$addOns$bMLMConfigurationFile,
    sheetName = "ModelError",
    skipDescriptionRow = TRUE
  )

  checkmate::assertNames(dtErrorModel$outputPathId, must.include = unique(dataObservedForMatch$outputPathId))
  checkmate::assertNames(dtErrorModel$modelErrorId, must.include = dtPrior[valueMode == PARAMETERTYPE$outputError, ]$name)
  checkmate::assertNames(dtErrorModel$errorModel, subset.of = unlist(ERRORMODEL))
  checkmate::assertNumeric(dtErrorModel$lowerBound)

  dtErrorModel[is.na(lowerBound), lowerBound := 0]

  dataObservedForMatch <- dataObservedForMatch %>%
    merge(dtErrorModel,
      by = "outputPathId"
    )

  if (nrow(dataObservedForMatch[yValues < lloq / 2]) > 0) {
    warning("Set Data Values below lloq to lloq/2")
    writeTableToLog(dt = dataObservedForMatch[yValues < lloq, c("outputPathId", "group", "individualId", "xValues", "yValues", "lloq")])
    dataObservedForMatch[yValues < lloq, yValues := lloq / 2]
  }

  tmp <- dataObservedForMatch[errorModel %in% c(ERRORMODEL$proportional, ERRORMODEL$log_absolute) & yValues <= 0, ]

  if (nrow(tmp) > 0) {
    writeTableToLog(dt = tmp[, c("outputPathId", "group", "individualId", "xValues", "yValues", "lloq", "errorModel")])
    stop("values <= 0 not allowed for this error model")
  }



  return(dataObservedForMatch)
}



#' Add Min and Max Values
#'
#' This function merges a data.table with min and max values from a definition data.table.
#'
#' @param dt A data.table to which minValue and maxValue will be added.
#' @param dtDefinition A data.table containing parameter definitions.
#'
#' @return A data.table with minValue and maxValue added.
#' @keywords internal
checkMinMaxValues <- function(dt) {
  # initialize variables to avoid linter messages
  minValue <- value <- maxValue <- valueMode <- scaling <- startValue <- NULL

  dt[is.na(minValue), minValue := -Inf]
  dt[is.na(maxValue), maxValue := Inf]

  dt[, value := startValue]

  tmpFailing <- dt[value > maxValue | value < minValue]
  if (nrow(tmpFailing) > 0) {
    writeTableToLog(tmpFailing)
    stop("Some values do not satisfy the condition minValue <= value <= maxValue")
  }

  if ("scaling" %in% names(dt)) {
    tmpFailing <- dt[scaling == SCALING$log &
                       (value <= 0 | minValue < 0 | maxValue < 0)]
    if ("valueMode" %in% names(dt)) tmpFailing <- tmpFailing[valueMode != PARAMETERTYPE$hyperParameter]

    if (nrow(tmpFailing) > 0) {
      writeTableToLog(tmpFailing)
      stop("Columns 'value', 'minValue', and 'maxValue' must be greater than 0, for Scaling Log")
    }
  }


  return(dt)
}

#' Prepare Input Data for L-BFGS-B Algorithm
#'
#' This function prepares the input data for the L-BFGS-B algorithm by merging and transforming
#' the provided data tables. It handles log transformations based on specified conditions.
#'
#' @param dtPrior A data.table containing prior values with columns: 'value', 'MinValue', 'MaxValue',
#' @param dtStartValues A data.table containing start values with at least a 'Name' column.
#' @param dtDefinition A data.table containing definitions with columns: 'Name', 'Scaling'.
#'
#' @return A data.table with combined and transformed input data for the L-BFGS-B algorithm.
#' @keywords internal
prepareInputData <- function(dtPrior, dtStartValues) {
  # initialize variables to avoid linter messages
  logConversion <- value <- minValue <- maxValue <- scaling <- valueMode <- NULL

  # Select relevant columns from dtPrior and dtStartValues and create logConversion column
  dtInput <- rbind(
    dtPrior[,c('value', 'minValue', 'maxValue')],
    dtStartValues[,c('value', 'minValue', 'maxValue')]
  )

  # Trasnsform params to unbounded values
  dtInput[, param := fifelse(
    !is.finite(minValue) & !is.finite(maxValue), value,
    fifelse(
      is.infinite(minValue) & is.finite(maxValue), log(maxValue - value),
      fifelse(
        is.finite(minValue) & is.infinite(maxValue), log(value - minValue),
        qlogis((value - minValue) / (maxValue - minValue))
      )
    )
  )]
  return(dtInput)
}


#' Set Parameter to Tables
#'
#' This function updates the parameter values in the provided data.tables based on the optimization results.
#'
#' @param dtList A list containing various data.tables used in the optimization process.
#' @param params A numeric vector of parameters for the likelihood calculation.
#'
#' @return A list containing the updated data.tables.
#' @keywords internal
setParameterToTables <- function(dtList, params) {
  dtList$input$param <- params

  # Transform params to unbounded values
  dtList$input[, value := fifelse(
    !is.finite(minValue) & !is.finite(maxValue), param,
    fifelse(
      is.infinite(minValue) & is.finite(maxValue), exp(param) - maxValue,
      fifelse(
        is.finite(minValue) & is.infinite(maxValue), exp(param) + minValue,
        plogis(param) * (maxValue - minValue) + minValue
      )
    )
  )]

  dtList$prior$value <- dtList$input$value[seq_len(nrow(dtList$prior))]
  dtList$startValues$value <-
    dtList$input$value[nrow(dtList$prior) + seq_len(nrow(dtList$startValues))]


  return(dtList)
}




# likelihood ---------------

#' Get Log Likelihood
#'
#' This function calculates the log likelihood based on the provided parameters and observed data.
#'
#' @param params A numeric vector of parameters for the likelihood calculation.
#' @param scenarioList A list of scenarios for the optimization.
#' @param dtList A list containing various data.tables used in the optimization process.
#' @param simulationRunOptions Optional parameters for running simulations.
#'
#' @return A numeric value representing the negative log likelihood.
#' @keywords internal
getLogLikelihood <-
  function(params,
           scenarioList,
           dtList,
           simulationRunOptions) {

    dtList <- setParameterToTables(
      dtList = dtList,
      params = params
    )
    # Likelihood observed data given simulated time profiles
    logTimeProfile <- getLikelihoodTimeProfiles(
      scenarioList = scenarioList,
      dtPrior = dtList$prior,
      dtStartValues = dtList$startValues,
      dataObservedForMatch = dtList$data,
      dtMappedPaths = dtList$mappedPaths,
      simulationRunOptions = simulationRunOptions
    )

    # Likelihood of hyper parameter
    logHyperParameter <- getLikelihoodHyperParameter(
      dtStartValues = dtList$startValues,
      dtHyperParameter = getHyperParameter(dtPrior = dtList$prior)
    )

    # Likelihood of parameter estimates given prior distribution
    logPrior <- getLikelihoodPriors(dtList$prior)

    return(c(logTimeProfile, logHyperParameter, logPrior))
  }

" Get Likelihood Time Profiles
#"
#' This function calculates the likelihood of observed data given simulated time profiles.
#'
#' @param scenarioList A list of scenarios for the optimization.
#' @param dtPrior A data.table containing prior values.
#' @param dtStartValues A data.table containing start values.
#' @param dataObservedForMatch A data.table containing observed data for matching.
#' @param dtMappedPaths A data.table containing mapped paths for parameters.
#' @param simulationRunOptions Optional parameters for running simulations.
#'
#' @return A numeric value representing the total log likelihood of the time profiles.
#' @keywords internal
getLikelihoodTimeProfiles <- function(scenarioList,
                                      dtPrior,
                                      dtStartValues,
                                      dataObservedForMatch,
                                      dtMappedPaths,
                                      simulationRunOptions) {
  # initialize variables to avoid linter messages
  yValues <- predicted <- errorModel <- sigma <- isCensored <- lloq <- lowerBound <- logLikelihood <- valueMode <- NULL

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
  scenarioResults <- esqlabsR::runScenarios(scenarios = scenarioList, simulationRunOptions = simulationRunOptions)


  dtRes <- getPredictionsForScenarios(
    scenarioResults,
    dataObservedForMatch
  ) %>%
    merge(
      dtPrior[valueMode == PARAMETERTYPE$outputError, c("name", "value")] %>%
        data.table::setnames("value", "sigma"),
      by.x = "modelErrorId",
      by.y = "name"
    )

  dtRes[, isCensored := !is.na(lloq) & lloq > yValues]

  tryCatch(
    {
      # Apply the function to calculate likelihood
      dtRes[, logLikelihood := mapply(calculateLogLikelihood, yValues, predicted, errorModel, sigma, isCensored, lloq, lowerBound)]

      # Calculate total log-likelihood
      if (any(!is.finite(dtRes$logLikelihood))) {
        return(getOption("OSPSuite.BMLM.failValue"))
      }
    },
    error = function(err) {
      save(dtPrior,
           dtStartValues,
           dtRes,
           file = file.path(
             getOption('OSPSuite.RF.logFileFolder'),
             paste0('error_', format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), '.log')
           )
      )
      return(getOption("OSPSuite.BMLM.failValue"))
    }
  )

  return(sum(dtRes$logLikelihood))
}

#' Update Parameter Values
#'
#' This function updates parameter values in the scenario based on mapped paths,startValues  and prior definitions.
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




#' Calculate Likelihood Priors
#'
#' This function calculates the likelihood priors for the given data.table.
#'
#' @param dtPrior A data.table containing prior information.
#'
#' @return The updated data.table with calculated probabilities.
#' @keywords internal
getLikelihoodPriors <- function(dtPrior) {
  # initialize variable to avoid linter message
  logLikelihood <- NULL

  dtPrior[, logLikelihood := apply(.SD, 1, calculateProbability, log = TRUE)]

  return(sum(dtPrior$logLikelihood))
}


#' Get Likelihood Hyperparameter
#'
#' This function calculates the likelihood of hyperparameters based on start values.
#'
#' @param dtStartValues A data.table containing start values.
#' @param dtHyperParameter A data.table containing hyperparameters and their distributions.
#'
#' @return A numeric value representing the total log likelihood of hyperparameters.
#' @keywords internal
getLikelihoodHyperParameter <-
  function(dtStartValues, dtHyperParameter) {
    return(sum(unlist(
      lapply(
        split(dtStartValues, by = c("name", "categoricCovariate")),
        getLikelihoodForIndividualGroup,
        dtHyperParameter = dtHyperParameter
      )
    )))
  }

#' Get Likelihood for Individual Group
#'
#' This function calculates the likelihood for a specific group of individuals based on hyperparameters.
#'
#' @param indGroup A data.table containing a subset of start values for a specific individual group.
#' @param dtHyperParameter A data.table containing hyperparameters and their distributions.
#'
#' @return A numeric value representing the total log likelihood for the individual group.
#' @keywords internal
getLikelihoodForIndividualGroup <-
  function(indGroup, dtHyperParameter) {
    # initialize variable to avoid linter message
    value <- scaling <- categoricCovariate <- name <- NULL

    setDT(indGroup)

    # Merge with hyperparameters
    tmp <-
      merge(dtHyperParameter,
        unique(indGroup[, .(name, categoricCovariate)]),
        by = c("name", "categoricCovariate")
      )

    # Apply log scaling if necessary
    tmp[scaling == SCALING$log, value := log(value)]

    # Create a list of parameters for the distribution function
    paramList <- stats::setNames(tmp$value, tmp$HyperParameter)

    # Check if all values are within the distribution range
    logLikelihood <-
      do.call(paste0("d", tmp$hyperDistribution[1]), c(list(x = indGroup$value, log = TRUE), paramList))

    if (any(is.na(logLikelihood))) return(getOption("OSPSuite.BMLM.failValue"))

    return(sum(logLikelihood))
  }

# process Result --------------
saveDataTablesAsCSV <- function( dtList, outputDir) {
  dtList <- setParameterToTables(
    dtList = dtList,
    params = result$par
  )


  for (name in names(dtList)) {
    filePath <- file.path(outputDir, paste0(name, ".csv"))
    write.csv(dtList[[name]], file = filePath, row.names = FALSE)
  }
}




# auxiliaries  -----
#' Get All Distributions
#'
#' This function retrieves all distributions available for prior definitions and hyperprameter.
#'
#' @return A vector of unique distribution names.
#' @export
getAllDistributions <- function() {
  unique(distributionTable$distribution)
}


#' Get Distribution Parameters
#'
#' This function retrieves the parameters for a specified distribution.
#'
#' @param distributionName A string representing the name of the distribution.
#'
#' @return A vector of parameters for the specified distribution.
#' @export
getDistributionParameters <- function(distributionName) {
  distributionTable[distributionTable$distribution == distributionName, ]$parameter
}


#' Get Distribution Row
#'
#' This function retrieves a row from the distribution table based on the distribution name and parameter.
#'
#' @param distributionName A string representing the name of the distribution.
#' @param distributionParameter A string representing the parameter of the distribution.
#'
#' @return A data.table containing the distribution row.
#' @keywords internal
getDistributionRow <-
  function(distributionName, distributionParameter) {
    distribution <- parameter <- NULL

    tempDistributionTable <- as.data.table(distributionTable)
    return(tempDistributionTable[distribution == distributionName &
      parameter == distributionParameter])
  }

#' Calculate Probability Based on Distribution
#'
#' This function calculates the probability for a given distribution using specified parameters.
#' It extracts the distribution type and its parameters from a data frame row, constructs the
#' appropriate function call, and returns the calculated probability. If an error occurs during
#' calculation, it returns NA and prints an error message.
#'
#' @param row A named vector representing a row from a data frame. It must contain:
#'   - `Distribution`: A string that specifies the distribution type (e.g., "norm" for normal distribution).
#'   - `StartValue`: A numeric value representing the starting point for the probability calculation.
#'   - Additional parameters with names ending in `_type` and `_value` that specify the distribution parameters.
#'
#' @return A numeric value representing the calculated probability. Returns NA in case of an error.
calculateProbability <- function(row, log = FALSE) {
  dist <- paste0("d", row["distribution"])

  value <- as.numeric(row["value"])

  # Extract parameters
  paramTypes <-
    row[grepl("_type$", names(row))] # Get all type columns
  paramValues <-
    row[grepl("_value$", names(row))] # Get all value columns

  paramList <-
    stats::setNames(as.numeric(paramValues), as.character(paramTypes))


  # set temporary upper limits for uniform for infinite limits
  if (dist == "dunif") {
    prob <- 1
    if (is.finite(paramList[["min"]])) {
      prob <- prob * as.double(paramList[["min"]] <= value)
    }

    if (is.finite(paramList[["max"]])) {
      prob <- prob * as.double(paramList[["max"]] >= value)
    }
    if (log == TRUE) {
      prob <- log(prob)
    }
  } else {
    # Remove NA values
    paramList <- paramList[!is.na(paramList)]
    paramList[["log"]] <- log

    # Calculate probability based on the distribution
    args <- c(list(value), paramList)
    prob <- tryCatch(
      {
        do.call(dist, args)
      },
      error = function(e) {
        return(NA) # Return NA or some other value in case of error
      }
    )
  }

  return(prob)
}
