#' Start BMLM Optimization
#'
#' This function initiates the BMLM optimization process based on project configuration and observed data.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details, including the path to the BMLM configuration file.
#' @param scenarioList A list of scenarios to be used in the optimization.
#' @param dataObserved A data.table containing observed data.
#' @param seed A numeric seed for random number generation (default is 1234).
#' @param method The optimization method to be used (default is "L-BFGS-B").
#' @param control A list of control parameters for the optimization.
#' @param simulationRunOptions an object of class ospsuite::SimulationRunOptions. Optional parameters for running simulations.
#'
#' @return An invisible NULL. The results of the optimization process are saved in files.
#' @export
startBMLMOptimization <- function(projectConfiguration,
                                  scenarioList,
                                  dataObserved,
                                  seed = 1234,
                                  method = "L-BFGS-B",
                                  control = list(),
                                  simulationRunOptions = NULL) {
  if (is.null(projectConfiguration$addOns$bMLMConfigurationFile)) {
    stop("Project configuration has no BMLM Configuration attached!")
  }
  checkmate::assertFileExists(projectConfiguration$addOns$bMLMConfigurationFile)

  dtList <- list()

  dtList$definition <- validateAndLoadDefinition(projectConfiguration)

  dtList$data <- prepareDataForMatch(
    projectConfiguration = projectConfiguration,
    dataObserved = dataObserved,
    scenarioList = scenarioList
  )

  dtList$mappedPaths <-
    validateAndLoadMappedPaths(projectConfiguration, dtList$definition, scenarioList)

  dtList$prior <-
    validateAndLoadPriorDefinition(
      projectConfiguration = projectConfiguration,
      dtDefinition = dtList$definition
    )

  dtList$startValues <-
    validateAndLoadIndividualStartValues(
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

  dtInput <-
    prepareInputData(
      dtPrior = dtList$prior,
      dtStartValues = dtList$startValues,
      dtDefinition = dtList$definition
    )

  # Optimization
  result <- optim(
    par = dtInput$value,
    fn = getLogLikelihood,
    method = method,
    lower = dtInput$minValue,
    upper = dtInput$maxValue,
    control = control,
    logScaleIndex = dtInput$logConversion,
    scenarioList = scenarioList,
    dtList = dtList,
    simulationRunOptions = simulationRunOptions
  )

  dtList <- setParameterToTables(
    dtList = dtList,
    params = result$par,
    logScaleIndex = dtInput$logConversion
  )

  saveFinalValuesToTables(projectConfiguration = projectConfiguration, dtList = dtList)
  exportOptimizedPopulation(projectConfiguration = projectConfiguration, dtList = dtList)

  save(result, file = "tmpResult.Rdata")
  load(file = "tmpResult.Rdata")




  return(invisible())
}

# initialization functions ------------------


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
  checkmate::assertNames(dtDefinition$scaling, subset.of = unlist(SCALING))

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
validateAndLoadMappedPaths <-  # nolint cyclocomp
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
      ) %>%
      merge(dtDefinition[, c("name", "unit", "useAsFactor")], by = "name", sort = FALSE)
    dtMappedPaths[is.na(unit), unit := ""]


    for (scenarioName in names(scenarioList)) {
      scenario <- scenarioList[[scenarioName]]

      sim <- scenario$simulation
      dtMappedPaths[, (scenarioName) := NA_real_]

      for (dp in split(dtMappedPaths, by = "linkedParameters")) {
        if (is.na(dp$scenarios) ||
          grepl(scenarioName, splitInputs(dp$scenarios))) { #nolint identation
          par <-
            ospsuite::getParameter(
              path = dp$linkedParameters,
              container = sim
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
          if (!(dp$linkedParameters %in% scenario$population$allParameterPaths)) {
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
    #initialize variable to avoid linter message
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
    values <-
      do.call(paste0("r", tmp$hyperDistribution[1]), c(list(n = nNew), paramList))
    indGroup[is.na(indGroup$value), value := values]

    # set Values outside limits to limits
    indGroup[value <= minValue, value := minValue]
    indGroup[value >= maxValue, value := maxValue]

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
    )

    dtStartValues <- addMinMaxValues(dtStartValues, dtDefinition[, c("name", "minValue", "maxValue", "scaling")])

    # Randomize startValues
    dtStartValuesNew <- rbindlist(
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
  )

  dtPrior <- addMinMaxValues(
    dtPrior,
    dtDefinition[, c("name", "minValue", "maxValue", "distribution")] %>%
      data.table::setnames("distribution", "hyperDistribution")
  )

  if (any(is.na(dtPrior$startValue))) {
    stop(paste(
      'Please insert startValues in sheet "Prior" for:',
      paste(unique(dtPrior[is.na(startValue)]$name), collapse = ", ")
    ))
  }

  checkmate::assertNames(dtPrior$distribution, subset.of = getAllDistributions())


  dtPrior[, probability := apply(.SD, 1, calculateProbability), by = "id"]


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
addMinMaxValues <- function(dt, dtDefinition) {
  # initialize variables to avoid linter messages
  minValue <- value <- maxValue <- valueMode <- scaling <- startValue <- NULL

  dt <-
    merge(dt,
      dtDefinition,
      by = "name",
      sort = FALSE,
      all.x = TRUE
    )

  dt[is.na(minValue), minValue := -Inf]
  dt[is.na(maxValue), maxValue := Inf]

  dt[, value := startValue]

  if ("valueMode" %in% names(dt)) {
    dt[valueMode == PARAMETERTYPE$outputError, minValue := 0]
    dt[valueMode == PARAMETERTYPE$hyperParameter, minValue := -Inf]
    dt[valueMode == PARAMETERTYPE$hyperParameter, maxValue := Inf]
  }

  tmpFailing <- dt[scaling == SCALING$log &
                     (value <= 0 | minValue <= 0 | maxValue <= 0)]
  if ("valueMode" %in% names(dt)) tmpFailing <- tmpFailing[valueMode != PARAMETERTYPE$hyperParameter]

  if (nrow(tmpFailing) > 0) {
    writeTableToLog(tmpFailing)
    stop("Columns 'value', 'minValue', and 'maxValue' must be greater than 0, for Scaling Log")
  }

  tmpFailing <- dt[value > maxValue | value < minValue]
  if (nrow(tmpFailing) > 0) {
    writeTableToLog(tmpFailing)
    stop("Some values do not satisfy the condition minValue <= value <= maxValue")
  }


  return(dt)
}

#' Prepare Input Data for L-BFGS-B Algorithm
#'
#' This function prepares the input data for the L-BFGS-B algorithm by merging and transforming
#' the provided data tables. It handles log transformations based on specified conditions.
#'
#' @param dtPrior A data.table containing prior values with columns: 'value', 'MinValue', 'MaxValue', 'Scaling', 'valueMode'.
#' @param dtStartValues A data.table containing start values with at least a 'Name' column.
#' @param dtDefinition A data.table containing definitions with columns: 'Name', 'Scaling'.
#'
#' @return A data.table with combined and transformed input data for the L-BFGS-B algorithm.
#' @keywords internal
prepareInputData <- function(dtPrior, dtStartValues, dtDefinition) {
  # initialize variables to avoid linter messages
  logConversion <- value <- minValue <- maxValue <- scaling <- valueMode <- NULL

  # Select relevant columns from dtPrior and dtStartValues and create logConversion column
  dtInput <- rbind(
    dtPrior %>%
      dplyr::select(value, minValue, maxValue, scaling, valueMode) %>%
      dplyr::mutate(
        logConversion = (scaling == SCALING$log)
      ) %>%
      dplyr::select(-scaling, -valueMode),
    dtStartValues %>%
      dplyr::mutate(logConversion = (scaling == SCALING$log)) %>%
      dplyr::select(value, minValue, maxValue, logConversion)
  )

  # Apply log transformation where logConversion is TRUE
  dtInput[logConversion == TRUE, value := log(value)]
  dtInput[logConversion == TRUE & is.finite(minValue), minValue := log(minValue)]
  dtInput[logConversion == TRUE & is.finite(maxValue), maxValue := log(maxValue)]

  return(dtInput)
}

# likelihood ---------------

#' Get Log Likelihood
#'
#' This function calculates the log likelihood based on the provided parameters and observed data.
#'
#' @param params A numeric vector of parameters for the likelihood calculation.
#' @param logScaleIndex A logical vector indicating which parameters are on a log scale.
#' @param scenarioList A list of scenarios for the optimization.
#' @param dtList A list containing various data.tables used in the optimization process.
#' @param simulationRunOptions Optional parameters for running simulations.
#'
#' @return A numeric value representing the negative log likelihood.
#' @keywords internal
getLogLikelihood <-
  function(params,
           logScaleIndex,
           scenarioList,
           dtList,
           simulationRunOptions) {
    dtList <- setParameterToTables(
      dtList = dtList,
      params = params,
      logScaleIndex = logScaleIndex
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

    message(paste(logTimeProfile, logHyperParameter, logPrior))

    return(-(logTimeProfile + logHyperParameter + logPrior))
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
  yValues <- predicted <- errorModel <- sigma <- isCensored <- lloq <-lowerBound <- logLikelihood <- valueMode <- NULL

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

  # Apply the function to calculate likelihood
  dtRes[, logLikelihood := mapply(calculateLogLikelihood, yValues, predicted, errorModel, sigma, isCensored, lloq, lowerBound)]

  # Calculate total log-likelihood
  if (any(is.na(dtRes$logLikelihood))) {
    return(-1e10)
  }

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
  currentValue <- value <- newValue <-  individualId <- valueMode<- NULL

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
        values = rep(dp$value * dp$factor),
        scenario$population$count
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

#' Calculate Log Likelihood
#'
#' This function calculates the log likelihood based on observed and predicted values.
#'
#' @param y A numeric vector of observed values.
#' @param pred A numeric vector of predicted values.
#' @param model A string indicating the error model type.
#' @param sigma A numeric value representing the standard deviation.
#' @param isCensored A logical indicating if the data is censored.
#' @param lloq A numeric value representing the lower limit of quantification.
#' @param lowerBound A numeric value representing the lower bound.
#'
#' @return A numeric value representing the log likelihood.
#' @keywords internal
calculateLogLikelihood <- function(y, pred, model, sigma, isCensored, lloq, lowerBound) {
  if (isCensored) {
    if (model == "absolute") {
      p1 <- stats::pnorm(q = lloq, mean = pred, sd = sigma)
      p2 <- stats::pnorm(q = lowerBound, mean = pred, sd = sigma)
      if (p1 <= p2) stop("Invalid probabilities for absolute model.")
      return(log(p1 - p2) - log(1 - p2))
    } else if (model == "relative") {
      p1 <- stats::pnorm(q = lloq / pred, mean = 1, sd = sigma)
      p2 <- stats::pnorm(q = lowerBound / pred, mean = 1, sd = sigma)
      if (p1 <= p2) stop("Invalid probabilities for relative model.")
      return(log(p1 - p2) - log(1 - p2))
    } else if (model == "log_absolute") {
      p1 <- stats::plnorm(q = lloq, meanlog = pred, sdlog = sigma)
      p2 <- stats::plnorm(q = lowerBound, meanlog = pred, sdlog = sigma)
      if (p1 <= p2) stop("Invalid probabilities for log_absolute model.")
      return(log(p1 - p2) - log(1 - p2))
    } else {
      stop("Unexpected error model: ", model)
    }
  } else {
    if (model == "absolute") {
      return(dnorm(x = y, mean = pred, sd = sigma, log = TRUE))
    } else if (model == "relative") {
      return(dnorm(x = y / pred, mean = 1, sd = sigma, log = TRUE))
    } else if (model == "log_absolute") {
      return(dlnorm(x = y, meanlog = pred, sdlog = sigma, log = TRUE))
    } else {
      stop("Unexpected error model: ", model)
    }
  }
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
  #initialize variable to avoid linter message
  logLikelihood <- NULL

  dtPrior[, logLikelihood := apply(.SD, 1, calculateProbability, log = TRUE), by = "id"]

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
    #initialize variable to avoid linter message
    value <- scaling <- categoricCovariate <- name <-  NULL

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

    return(sum(logLikelihood))
  }

# process Result --------------
saveFinalValuesToTables <- function(projectConfiguration, dtList) {
  wb <- openxlsx::loadWorkbook(file = projectConfiguration$addOns$bMLMConfigurationFile)

  addFinalValue <- function(wb, sheetName, identifier, newTable) {
    dt <- xlsxReadData(wb, sheetName = sheetName)

    headers <- names(dt)

    dt <- dt %>%
      dplyr::select(-dplyr::any_of(c("startValue", "finalValue"))) %>%
      merge(
        newTable %>%
          dplyr::select(dplyr::all_of(c(identifier, "startValue", "value"))) %>%
          data.table::setnames("value", "finalValue"),
        by = identifier,
        sort = FALSE
      ) %>%
      setcolorder(c(headers[seq(1, which(headers == "startValue"))], "finalValue"))

    xlsxWriteData(wb, sheetName = sheetName, dt = dt)

    return(wb)
  }

  wb <- addFinalValue(wb,
    sheetName = "Prior",
    identifier = c("name", "hyperParameter", "categoricCovariate"),
    newTable = dtList$prior
  )
  wb <- addFinalValue(wb,
    sheetName = "IndividualStartValues",
    identifier = c("name", "individualId", "categoricCovariate"),
    newTable = dtList$startValues
  )


  openxlsx::saveWorkbook(wb = wb, file = projectConfiguration$addOns$bMLMConfigurationFile, overwrite = TRUE)
}

exportOptimizedPopulation <- function(projectConfiguration, dtList, scenarioList) {
  #initialize variable to avoid linter message
  scenario_name <- populationId <- NULL #nolint camelCase, variable is derived by column name esqlabR

  invisible(lapply(names(scenarioList), function(scenarioName) {
    updateParameterValues(
      scenarioName = scenarioName,
      scenario = scenarioList[[scenarioName]],
      dtPrior = dtList$prior,
      dtStartValues = dtList$startValues,
      dtMappedPaths = dtList$mappedPaths
    )
  }))

  nameOfBMLM <- gsub(".xlsx", "", gsub("^BMLMConfiguration", "", basename(projectConfiguration$addOns$bMLMConfigurationFile)))

  for (scenarioName in names(scenarioList)) {
    ospsuite::exportPopulationToCSV(
      population = scenarioList[[scenarioName]]$population,
      filePath = file.path(
        projectConfiguration$populationsFolder,
        paste0(scenarioList[[scenarioName]]$scenarioConfiguration$populationId, nameOfBMLM, ".csv")
      )
    )
  }

  wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  dt <- xlsxReadData(wb, sheetName = "Scenarios")
  dtSc <- dt[scenario_name %in% names(scenarioList)]
  dtSc[, scenario_name := paste0(scenario_name, nameOfBMLM)]
  dtSc[, populationId := paste0(populationId, nameOfBMLM)]

  dt <- dt[!(scenario_name %in% dtSc$scenario_name)]

  xlsxWriteData(wb, sheetName = "Scenarios", dt = rbind(dt, dtSc))

  openxlsx::saveWorkbook(wb = wb, file = projectConfiguration$scenariosFile, overwrite = TRUE)
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

#' Set Parameter to Tables
#'
#' This function updates the parameter values in the provided data.tables based on the optimization results.
#'
#' @param dtList A list containing various data.tables used in the optimization process.
#' @param params A numeric vector of parameters for the likelihood calculation.
#' @param logScaleIndex A logical vector indicating which parameters are on a log scale.
#'
#' @return A list containing the updated data.tables.
#' @keywords internal
setParameterToTables <- function(dtList, params, logScaleIndex) {
  # set fit parameter to BMLM tables
  params[which(logScaleIndex)] <-
    exp(params[which(logScaleIndex)])

  dtList$prior$value <- params[seq_len(nrow(dtList$prior))]
  dtList$startValues$value <-
    params[nrow(dtList$prior) + seq_len(nrow(dtList$startValues))]


  return(dtList)
}
