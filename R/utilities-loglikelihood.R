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
           simulationRunOptions,
           optimEnv = NULL) {
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
      simulationRunOptions = simulationRunOptions,
      optimEnv = optimEnv
    )
    # Likelihood of hyper parameter
    logHyperParameter <- getLikelihoodHyperParameter(
      dtStartValues = dtList$startValues,
      dtHyperParameter = getHyperParameter(dtPrior = dtList$prior)
    )
    # Likelihood of parameter estimates given prior distribution
    logPrior <- getLikelihoodPriors(dtList$prior)
    return(c(logTimeProfile = logTimeProfile, logHyperParameter = logHyperParameter, logPrior = logPrior))
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
                                      simulationRunOptions,
                                      optimEnv = NULL) {
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

  # Apply the function to calculate likelihood
  dtRes[, logLikelihood := mapply(calculateLogLikelihood, yValues, predicted, errorModel, sigma, isCensored, lloq, lowerBound)]

  if (!is.null(optimEnv)){
    optimEnv$Residuals <- dtRes
  }

  # Calculate total log-likelihood
  if (any(!is.finite(dtRes$logLikelihood))) {
    return(NA)
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

#' Get Predictions for Scenarios
#'
#' This function generates predictions for a set of scenarios based on observed data.
#'
#' @param scenarioResults A list of scenario results, each containing population and covariate information.
#' @param dataObservedForMatch A data.table containing observed data, which must include the required columns:
#'        'scenario', 'outputPathId', 'yValues', and 'yUnit'.
#'        The unit factors (`unitFactorX` and `unitFactorY`) are necessary for proper scaling of the predicted values
#'        and can be generated using the `prepareDataForMatch` function prior to calling this function.
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




#' Calculate Log Likelihood
#'
#' This function calculates the log likelihood of observed values given predicted values
#' based on the specified error model. It accounts for both censored and uncensored data
#' and allows for different modeling approaches (absolute, proportional, log-transformed).
#'
#' The log likelihood is a measure of how well the predicted values explain the observed
#' data, with higher values indicating a better fit. The function can handle censored data.
#' This function is typically called within the `data.table` context (see example below)
#'
#' @param yValue A numeric vector of observed values. These are the actual measurements that
#'                will be compared against the predicted values.
#' @param predicted A numeric vector of predicted values. These values represent the expected
#'                  measurements based on the model.
#' @param model A string indicating the error model type. Options include:
#'              - "absolute": Assumes absolute errors in predictions.
#'              - "proportional": Assumes proportional errors in predictions.
#'              - "log_absolute": Assumes log-transformed absolute errors.
#' @param sigma A numeric value representing the standard deviation of the error term
#'              in the model, which quantifies the uncertainty in predictions.
#' @param isCensored A logical indicating if the data is censored. If TRUE, the function
#'                   will apply the appropriate calculations for censored data.
#' @param lloq A numeric value representing the lower limit of quantification. This is
#'              the lowest concentration of a substance that can be reliably measured.
#' @param lowerBound A numeric value representing the lower bound for the predictions.
#'                   Values below this threshold will return a log likelihood of negative
#'                   infinity. Default is 0.
#'
#' @return A numeric value representing the log likelihood of the observed data given
#'         the predicted values. If the data is censored, the log likelihood is computed
#'         based on the probabilities of falling below the lower limit of quantification.
#'         If the predictions are below the lower bound, the function returns negative
#'         infinity, indicating an invalid scenario.
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # Create a sample data.table
#' dt <- data.table(
#'   yValue = c(1.0, 2.0, 3.0),
#'   predicted = c(1.1, 2.1, 3.1),
#'   errorModel = c("absolute", "proportional", "log_absolute"),
#'   sigma = c(0.5, 0.5, 0.5),
#'   isCensored = c(FALSE, FALSE, TRUE),
#'   lloq = c(1.0, 1.0, 1.0)
#' )
#'
#' # Calculate log likelihood for each row
#' dt[, logLikelihood := mapply(calculateLogLikelihood, yValue, predicted, errorModel, sigma, isCensored, lloq)]
#'
#' # View results
#' print(dt)
#' }
#'
calculateLogLikelihood <- function(yValue, predicted, model, sigma, isCensored, lloq, lowerBound = 0) {

  # Validate that all inputs are of the correct type and length
  checkmate::assertNumeric(yValue, len = 1)  # yValue should be a single numeric value
  checkmate::assertNumeric(predicted, len = 1)  # predicted should be a single numeric value
  checkmate::assertChoice(model, c("absolute", "proportional", "log_absolute"))  # model must be one of the specified choices
  checkmate::assertNumeric(sigma, len = 1)  # sigma should be a single numeric value
  checkmate::assertLogical(isCensored, len = 1)  # isCensored should be a single logical value
  checkmate::assertNumeric(lloq, len = 1)  # lloq should be a single numeric value
  checkmate::assertNumeric(lowerBound, len = 1)  # lowerBound should be a single numeric value

  # If predicted value is NA or below the lower bound, return log(0) (indicating a very low likelihood)
  if (is.na(predicted) || predicted < lowerBound) {
    return(log(0))
  }

  # Calculate the probability of being below the lower bound based on the selected model
  pLB <- switch(model,
                absolute = pnorm(q = lowerBound, mean = predicted, sd = sigma, log = FALSE),
                proportional = pnorm(q = lowerBound, mean = predicted, sd = sigma * predicted, log = FALSE),
                log_absolute = plnorm(q = lowerBound, meanlog = log(predicted), sdlog = sigma, log = FALSE))

  # If the data is censored, calculate the probability of being above the lower limit of quantification (lloq)
  if (isCensored) {
    p <- switch(model,
                absolute = pnorm(q = clloq, mean = predicted, sd = sigma, log = FALSE),
                proportional = pnorm(q = lloq, mean = predicted, sd = sigma * predicted, log = FALSE),
                log_absolute = plnorm(q = lloq, meanlog = log(predicted), sdlog = sigma, log = FALSE))
    p <- log(p - pLB)  # Log-transform the probability adjusted for the lower bound
  } else {
    # If the data is notored, calculate the log likelihood based on the chosen model
    p <- switch(model,
                absolute = dnorm(x = yValue, mean = predicted, sd = sigma, log = TRUE),
                proportional = dnorm(x = yValue, mean = predicted, sd = sigma * predicted, log = TRUE),
                log_absolute = dlnorm(x = yValue, meanlog = log(predicted), sdlog = sigma, log = TRUE))
    return(p)  # Return the log likelihood for uncensored data
  }

  # Adjust the log likelihood by subtracting the log probability of being below the lower bound
  p <- p - log(1 - pLB)

  return(p)  # Return the final log likelihood value
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

    # Merge with hyper parameters
    tmp <-
      merge(dtHyperParameter,
        unique(indGroup[, .(name, categoricCovariate)]),
        by = c("name", "categoricCovariate")
      )

    # Get parameters for distributions
    paramList <- stats::setNames(tmp$value, tmp$hyperParameter)

    # get Loglikelihood of distribution
    logLikelihood <-
      do.call(paste0("d", tmp$hyperDistribution[1]), c(list(x = indGroup$value, log = TRUE), paramList)) -
      tmp$logTruncationOffset[1]

    return(sum(logLikelihood))
  }



