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
  function(dtPrior,
           dtStartValues,
           dtRes) {
    # Likelihood observed data given simulated time profiles
    logTimeProfile <- getLikelihoodTimeProfiles(
      dtPrior = dtPrior,
      dtRes = dtRes
    )
    # Likelihood of hyper parameter
    logHyperParameter <- getLikelihoodHyperParameter(
      dtStartValues = dtStartValues,
      dtPrior = dtPrior
    )
    # Likelihood of parameter estimates given prior distribution
    logPrior <- getLikelihoodPriors(dtPrior)
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
getLikelihoodTimeProfiles <- function(dtPrior,
                                      dtRes) {

  # initialize variables to avoid linter messages
  yValues <- predicted <- errorModel <- sigma <- isCensored <- lloq <- lowerBound <- logLikelihood <- valueMode <- NULL

  dtRes <- updateModelError(dtPrior,dtRes)

  dtRes[, isCensored := !is.na(lloq) & lloq > yValues]

  # Apply the function to calculate likelihood
  dtRes[, logLikelihood := mapply(calculateLogLikelihood, yValues, predicted, errorModel, sigma, isCensored, lloq, lowerBound)]

  # Calculate total log-likelihood
  if (any(!is.finite(dtRes$logLikelihood))) {
    return(NA)
  }

  return(sum(dtRes$logLikelihood))
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
  function(dtStartValues, dtPrior) {

    dtHyperParameter <- setlogTruncationOffset(dtPrior = dtPrior,
                                               dtStartValues = dtStartValues)

    logLikelihood <- sum(unlist(
      lapply(
        split(dtStartValues, by = c("name", "categoricCovariate")),
        getLikelihoodForIndividualGroup,
        dtHyperParameter = dtHyperParameter
      )
    ))
    return(logLikelihood)
  }

#' Calculate Log Truncation Offset for Hyperparameters
#'
#' This function computes the log truncation offset for hyperparameters based on the provided prior distribution and start values.
#' It reduces the hyperparameter data to match the available data and calculates the lower and upper probabilities for the given
#' minimum and maximum values. The log truncation offset is then derived from these probabilities.
#'
#' @param dtPrior A data.table containing prior hyperparameter distributions. It should include columns for hyperparameters,
#'                their values, and the distribution type.
#' @param dtStartValues A data.table containing the starting values for hyperparameters, including identifiers and their
#'                      respective minimum and maximum values.
#' @param identifier A character vector indicating the columns to be used as identifiers for grouping the hyperparameters.
#'                   Default is c('name', 'categoricCovariate').
#' @param colsToKeep A character vector indicating the columns to retain in the resulting data.table. Default includes
#'                   "hyperParameter", "value", "hyperDistribution", "scaling", and "logTruncationOffset".
#'
#' @return A data.table containing the hyperparameters, their values, and the computed log truncation offsets.
#'
#' @keywords internal
setlogTruncationOffset <- function(dtPrior,dtStartValues,
                                   identifier =  c('name', 'categoricCovariate'),
                                   colsToKeep = c("hyperParameter",
                                   "value",
                                   "hyperDistribution",
                                   "scaling",
                                   'logTruncationOffset')) {

  dtHyperParameter <- data.table()
  # Reduce hyperparameter to match available data
  dtPriorHyper <- dtPrior[valueMode == PARAMETERTYPE$hyperParameter] %>%
    merge(unique(dtStartValues[, .(name, categoricCovariate, minValue, maxValue)]),
          by = c('name', 'categoricCovariate'),
          suffixes = c('', '.indValues'))

  # Loop through each hyperparameter group
  for (dtGroup in split(dtPriorHyper, by = identifier)) {

    # Create a named list of parameters
    paramList <- setNames(dtGroup$value, dtGroup$hyperParameter)

    # Calculate logTruncationOffset
    dtGroup[, pLB := do.call(paste0("p", dtGroup$hyperDistribution[1]),
                             c(
                               list(q = dtGroup$minValue.indValues[1], log = FALSE), paramList
                             ))]
    dtGroup[, pUB := do.call(paste0("p", dtGroup$hyperDistribution[1]),
                             c(
                               list(q = dtGroup$maxValue.indValues[1], log = FALSE,
                                    lower.tail = FALSE), paramList
                             ))]

    dtGroup[, logTruncationOffset := log(1- (pUB + pLB))]

    # Combine results
    dtHyperParameter <- rbind(dtHyperParameter,
                        dtGroup    %>%
                          dplyr::select(any_of(c(
                            identifier,
                            colsToKeep
                          )))
    )
  }

  return(dtHyperParameter)
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

    # Merge with hyper parameters
    tmp <-
      merge(dtHyperParameter,
        unique(indGroup[, .(name, categoricCovariate)]),
        by = c("name", "categoricCovariate")
      )

    # if no Hyperparameter exist for group return probability 1
    if (nrow(tmp) == 0) return(0)

    # Get parameters for distributions
    paramList <- stats::setNames(tmp$value, tmp$hyperParameter)

    # get Loglikelihood of distribution
    logLikelihood <-
      do.call(paste0("d", tmp$hyperDistribution[1]), c(list(x = indGroup$value, log = TRUE), paramList)) -
      tmp$logTruncationOffset[1]

    return(sum(logLikelihood))
  }



