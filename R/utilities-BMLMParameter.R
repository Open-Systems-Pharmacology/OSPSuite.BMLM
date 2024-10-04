#' Start BMLM Optimization
#'
#' This function initiates the BMLM optimization process based on project configuration and observed data.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details, including the path to the BMLM configuration file.
#' @param scenarioList A list of scenarios to be used in the optimization.
#' @param dataObserved A data.table containing observed data.
#' @param seed A numeric seed for random number generation (default is 1234).
#' @param method The optimization method to be used (default is "L-BFGS-B").

#' @return A list of results from the optimization process.
startBMLMOptimization <- function(projectConfiguration,
                                  scenarioList,
                                  dataObserved,
                                  seed = 1234,
                                  method = "L-BFGS-B",
                                  control = list())
{

  scenarioNames <- names(scenarioList)

  dtDataGroups <- getDataGroups(projectConfiguration)

  if (!all(scenarioNames %in% dtDataGroups$defaultScenario)) {
    stop(
      paste(
        'There are scenarios which are not selected as "DefaultScenario" in sheet "DataGroups" "Plots.xlsx".',
        'This is mandatory to connect data and simulations:',
        paste(setdiff(scenarioNames, dtDataGroups$defaultScenario), collapse = ', ')
      )
    )
  }

  dtDataGroups <- dtDataGroups[defaultScenario %in% scenarioNames]

  dataObserved <- dataObserved[group %in% dtDataGroups$group]

  if (!all(dataObserved$dataClass == DATACLASS$tpIndividual)) {
    stop(paste('Please select only scenarios which are matched as "DefaultScenarios" in sheet "DataGroups" "Plots.xlsx" to individual data.',
               'Check dataGroup', paste(unique(dataObserved[dataClass != DATACLASS$tpIndividual]$group), collapse = ', ')))
  }

  if (is.null(projectConfiguration$BMLMConfigurationFile)) {
    stop('Project configuration has no BMLM Configuration attached!')
  }

  checkmate::assertFileExists(projectConfiguration$BMLMConfigurationFile)

  dtDefinition <- xlsxReadData(
    projectConfiguration$BMLMConfigurationFile,
    sheetName = 'ParameterDefinition',
    skipDescriptionRow = TRUE
  )

  dtMappedPaths <- getMappedPaths(projectConfiguration,dtDefinition,scenarioList)

  dtPrior <- getPriorDefinition(projectConfiguration = projectConfiguration,
                                dtDefinition = dtDefinition)

  dtHyperParameter <- getHyperParameter(dtPrior = dtPrior, dtDefinition = dtDefinition)

  dtStartValues <- getIndividualStartValues(projectConfiguration = projectConfiguration,
                                            dtHyperParameter = dtHyperParameter,
                                            dtDefinition = dtDefinition,
                                            seed = seed)

  dtInput <- prepareInputData(dtPrior = dtPrior,dtStartValues = dtStartValues,dtDefinition = dtDefinition)


  # Optimization
  result <- optim(
    par = dtInput$value,
    fn = getLogLikelihood,
    method = method,
    lower = dtInput$MinValue,
    upper = dtInput$MaxValue,
    control = control,
    logScaleIndex = dtInput$logConversion,
    scenarioList = scenarioList,
    dtPrior = dtPrior,
    dtHyperParameter = dtHyperParameter,
    dtStartValues = dtStartValues,
    dtMappedPaths = dtMappedPaths
  )

  return(list(initialParams = initialParams, lowerBounds = lowerBounds, upperBounds = upperBounds))
}

# initialisation functions ------------------


getMappedPaths <- function(projectConfiguration,dtDefinition,scenarioList){
  dtMappedPaths <- xlsxReadData(projectConfiguration$BMLMConfigurationFile,
                                sheetName = 'ParameterMappedPaths',
                                skipDescriptionRow = TRUE) %>%
    merge(dtDefinition[,c('name','unit','useAsFactor')], by = 'name', sort = FALSE)


  for (scenarioName in names(scenarioList)) {
    sim <- scenarioList[[scenarioName]]$simulation
    dtMappedPaths[is.na(scenarios) | grepl(scenarioName, scenarios),
                  (scenarioName) := sapply(1:.N, function(i) {
      pt <- linkedParameters[i]
      unit <- ifelse(is.na(unit[i]),'',unit[i])
      par <- ospsuite::getAllParametersMatching(paths = pt, container = sim)
      if (is.null(par)) {
        unitFactor <- NA
      } else {
        unitFactor <- ospsuite::toBaseUnit(quantityOrDimension = par[[1]]$dimension, values = 1, unit = unit)
      }
      return(unitFactor)
    })]
  }

  return(dtMappedPaths)
}


#' Get Hyper Parameters
#'
#' This function retrieves hyperparameters from the prior definition.
#'
#' @param dtPrior A data.table containing prior definitions.
#' @param dtDefinition A data.table containing parameter definitions.
#' @return A data.table containing hyperparameters and their distributions.
getHyperParameter <- function(dtPrior, dtDefinition) {
  dtHyperParameter <- dtPrior[valueMode == PARAMETERTYPE$hyperParameter] %>%
    dplyr::select(c(
      'name',
      'hyperParameter',
      'categoricCovariate',
      'value',
      'scaling'
    )) %>%
    merge(dtDefinition %>%
            dplyr::select(c('name', 'distribution')), by = 'name')

  return(dtHyperParameter)
}

#' Randomize Start Values
#'
#' This function processes a single group of start values, merging with hyperparameters and generating new values.
#'
#' @param indGroup A data.table containing a subset of start values for a specific individual group.
#' @param dtHyperParameter A data.table containing hyperparameters and their distributions.
#' @return A data.table with updated start values for the individual group.
randomizeIndividualStartValues <- function(indGroup, dtHyperParameter) {
  setDT(indGroup)
  nNew <- sum(is.na(indGroup$value))

  # Merge with hyperparameters
  tmp <- merge(dtHyperParameter, unique(indGroup[, .(name, categoricCovariate)]), by = c('name', 'categoricCovariate'))

  # Apply log scaling if necessary
  tmp[scaling == 'Log', value := log(value)]

  # Create a list of parameters for the distribution function
  paramList <- stats::setNames(tmp$value, tmp$HyperParameter)

  # Generate new values based on the distribution
  values <- do.call(paste0('r', tmp$Distribution[1]), c(list(n = nNew), paramList))
  indGroup[is.na(indGroup$value), value := values]

  # Check if all values are within the distribution range
  probs <- do.call(paste0('d', tmp$Distribution[1]), c(list(x = indGroup$value), paramList))

  if (any(is.na(probs)) | any(probs <= 0)) {
    stop(paste(indGroup$name, 'There are start values outside the distribution range'))
  }

  return(indGroup)
}

#' Get Individual Start Values
#'
#' This function reads individual start values from an Excel file and adjusts them based on hyperparameters.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details, including the path to the BMLM configuration file.
#' @param dtHyperParameter A data.table containing hyperparameters and their distributions.
#' @param dtDefinition A data.table containing parameter definitions.
#' @param seed A numeric seed for random number generation.
#' @return A data.table with updated start values.
getIndividualStartValues <- function(projectConfiguration, dtHyperParameter, dtDefinition, seed) {
  set.seed(seed)

  dtStartValues <- xlsxReadData(
    projectConfiguration$BMLMConfigurationFile,
    sheetName = 'IndividualStartValues',
    skipDescriptionRow = TRUE
  )

  dtStartValues <- addMinMaxValues(dtStartValues, dtDefinition)


  # Randomize startValues
  dtStartValuesNew <- rbindlist(
    lapply(split(dtStartValues, by = c('name', 'categoricCovariate')),
           randomizeIndividualStartValues,
           dtHyperParameter = dtHyperParameter)
  )

  # Add an ID column
  dtStartValuesNew[, id := paste0('s', .I)]

  return(dtStartValuesNew)
}

#' Get Prior Definition
#'
#' This function retrieves prior definitions from the configuration file.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details, including the path to the BMLM configuration file.
#' @param dtDefinition A data.table containing parameter definitions.
#' @return A data.table with prior definitions.
getPriorDefinition <- function(projectConfiguration, dtDefinition) {
  dtPrior <- xlsxReadData(
    projectConfiguration$BMLMConfigurationFile,
    sheetName = 'Prior',
    skipDescriptionRow = TRUE
  )

  dtPrior <- addMinMaxValues(dtPrior, dtDefinition)

  dtPrior[, id := paste0('p', .I)]

  if (any(is.na(dtPrior$startValue))) {
    stop(paste('Please insert startValues in sheet "Prior" for:',
               paste(unique(dtPrior[is.na(startValue)]$name), collapse = ', ')))
  }

  dtPrior[, value := startValue]

  checkmate::assertNames(dtPrior$distribution, subset.of = getAllDistributions())

  dtPrior[, probability := apply(.SD, 1, calculateProbability), by = 'id']


  if (any(is.na(dtPrior$probability))) {
    stop(paste('Probability of startvalue is NA, check priors ',
               paste(dtPrior[is.na(probability)]$name, collapse = ', ')))
  }


  if (any(dtPrior$probability <= 0)) {
    stop(paste('Start value outside distribution range, check',
               paste(dtPrior[probability <= 0]$name, collapse = ', ')))
  }

  return(dtPrior)
}


#' Add Min and Max Values
#'
#' This function merges a data.table with min and max values from a definition data.table.
#'
#' @param dt A data.table to which MinValue and MaxValue will be added.
#' @param dtDefinition A data.table containing parameter definitions.
#' @return A data.table with MinValue and MaxValue added.
addMinMaxValues <- function(dt, dtDefinition) {
  dt <-
    merge(dt,
          dtDefinition[, c('name', 'minValue', 'maxValue')],
          by = 'name',
          sort = FALSE,
          all.x = TRUE)

  dt[is.na(minValue), minValue := -Inf]
  dt[is.na(maxValue), maxValue := Inf]

  if ('valueMode' %in% names(dt)){
    dt[valueMode == PARAMETERTYPE$outputError, minValue := 0]
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
#' @param PARAMETERTYPE A list or environment containing parameter types, including 'global'.
#'
#' @return A data.table with combined and transformed input data for the L-BFGS-B algorithm.
#' @keywords internal
prepareInputData <- function(dtPrior, dtStartValues, dtDefinition) {
  # Select relevant columns from dtPrior and dtStartValues and create logConversion column
  dtInput <- rbind(dtPrior %>%
                     dplyr::select(value, MinValue, MaxValue, Scaling, valueMode) %>%
                     mutate(logConversion = (Scaling == 'Log' &
                                               valueMode == PARAMETERTYPE$global)) %>%
                     select(-Scaling, -valueMode),
                   dtStartValues %>%
                     merge(dtDefinition[, .(Name, Scaling)], by = 'Name', sort = FALSE) %>%
                     mutate(logConversion = (Scaling == 'Log')) %>%
                     select(-Scaling))

  if (any(dtInput[logConversion == TRUE, .(value, MinValue, MaxValue)] <= 0, na.rm = TRUE)) {
    stop("Columns 'value', 'MinValue', and 'MaxValue' must be greater than 0, for Scaling LOG")
  }

  # Apply log transformation where logConversion is TRUE
  dtInput[logConversion == TRUE, `:=`(
    value = log(value),
    MinValue = log(MinValue),
    MaxValue = log(MaxValue)
  )]

  return(dtInput)
}

# likelihood ---------------

getLogLikelihood <-
  function(params,
           logScaleIndex,
           scenarioList,
           dtPrior,
           dtHyperParameter,
           dtStartValues,
           dataObserved,
           dtMappedPaths) {

    # set fit parameter to BMLM tables
    params[which(logScaleIndex)] <- exp(params[which(logScaleIndex)])

    dtPrior$value <- params[seq_len(nrow(dtPrior))]
    dtStartValues$value <- params[nrow(dtPrior) + seq_len(nrow(dtStartValues))]


  # Likelihood observed data given simulated time profiles
  logTimeProfile = getLikelihoodTimeProfiles(scenarioList,dtPrior,dtStartValues,dataObserved,dtMappedPaths)

  # Likelihood of hyper parameter
  logHyperParameter = getLikelihoodHyperParameter(dtStartValues,dtHyperParameter)

  # Likelihood of parameter estimates given prior distribution
  logPrior <- getLikelihoodPriors(dtPrior)


  return(logTimeProfile + logHyperParameter + logPrior)


}

getLikelihoodTimeProfiles <- function(scenarioList,dtPrior,dtStartValues,dataObserved,dtMappedPaths){


  scenarioName <- names(scenarioList)[1]
  scenario <- scenarioList[[scenarioName]]

  IndividualId <- scenario$population$getCovariateValues('ObservedIndividualId')

  tmp <- dtPrior[valueMode == PARAMETERTYPE$global] %>%
    dplyr::select(c('Name','value')) %>%
    merge(dtMappedPaths[!is.na(get(scenarioName))] %>%
            dplyr::select(dplyr::all_of(c('Name','LinkedParameters','useAsFactor',scenarioName))) %>%
            data.table::setnames(scenarioName,'unitFactor')) %>%
    dplyr::mutate(value = unitFactor*value)


  for (dp in split(dtCustomParams, by = "paths")) {
    scenario$population$setParameterValues(
      parameterOrPath = dp$paths,
      values = rep(dp$baseValue, scenario$population$count)
    )
  }



}


#' Calculate Likelihood Priors
#'
#' This function calculates the likelihood priors for the given data.table.
#'
#' @param dtPrior A data.table containing prior information.
#' @return The updated data.table with calculated probabilities.
getLikelihoodPriors <- function(dtPrior) {
  dtPrior[, logLikelihood := apply(.SD, 1, calculateProbability, log = TRUE), by = 'id']

  return(sum(dtPrior$logLikelihood))
}



getLikelihoodHyperParameter <- function(dtStartValues,dtHyperParameter){

  return(
    sum(unlist(lapply(split(dtStartValues, by = c('Name', 'CategoricCovariate')),
                      getLikelihoodForIndividualGroup,
                      dtHyperParameter = dtHyperParameter))))

}


getLikelihoodForIndividualGroup <- function(indGroup, dtHyperParameter) {
    setDT(indGroup)

    # Merge with hyperparameters
    tmp <- merge(dtHyperParameter, unique(indGroup[, .(Name, CategoricCovariate)]), by = c('Name', 'CategoricCovariate'))

    # Apply log scaling if necessary
    tmp[Scaling == 'Log', value := log(value)]

    # Create a list of parameters for the distribution function
    paramList <- stats::setNames(tmp$value, tmp$HyperParameter)

    # Check if all values are within the distribution range
    logLikelihood <- do.call(paste0('d', tmp$Distribution[1]), c(list(x = indGroup$value,log = TRUE), paramList))

    return(sum(logLikelihood))
}

# auxiliaries  -----
#' Title
#'
#' @return
getAllDistributions <- function() {
  unique(distributionTable$distribution)
}


#' Title
#'
#' @param distributionName
#'
#' @return
#' @export
#'
#' @examples
getDistributionParameters <- function(distributionName) {
  distributionTable[distributionTable$distribution == distributionName,]$parameter
}



getDistributionRow <- function(distributionName,distributionParameter){

  tempDistributionTable <- as.data.table(distributionTable)
  distributionRow <- tempDistributionTable[distribution == distributionName
                                           & parameter == distributionParameter]


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
calculateProbability <- function(row,log = FALSE) {
  dist <- paste0('d',row["distribution"])

  value <- as.numeric(row["value"])

  # Extract parameters
  paramTypes <- row[grepl("_type$", names(row))]  # Get all type columns
  paramValues <- row[grepl("_value$", names(row))]  # Get all value columns

  paramList <- setNames(as.numeric(paramValues), as.character(paramTypes))


  # set temporary upper limits for uniform for infinite limits
  if (dist == 'dunif'){
    prob <- 1
    if (is.finite(paramList[['min']])) {
      prob <- prob * as.double(paramList[['min']] <= value)
    }

    if (is.finite(paramList[['max']])) {
      prob <- prob * as.double(paramList[['max']] >= value)
    }
    if (log == TRUE){
      prob <- log(prob)
    }
  } else{

    # Remove NA values
    paramList <- paramList[!is.na(paramList)]
    paramList[['log']] = log

    # Calculate probability based on the distribution
    args <- c(list(value), paramList)
    prob <- tryCatch({
      do.call(dist, args)
    }, error = function(e) {
      return(NA)  # Return NA or some other value in case of error
    })
  }

  return(prob)
}

