
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

  set.seed(seed)

  dtList$data <- prepareDataForMatch(
    projectConfiguration = projectConfiguration,
    dataObserved = dataObserved,
    scenarioList = scenarioList
  )

  # reduce list to match available data
  scenarioList <- scenarioList[unique(dtList$data$scenario)]

  dtList$prior <- validateAndLoadPriorDefinition(
    projectConfiguration = projectConfiguration
  )

  dtList$startValues <- validateAndLoadIndividualStartValues(
    projectConfiguration = projectConfiguration,
    dtHyperParameter = getHyperParameter(dtPrior = dtList$prior),
    dataObserved =  dtList$data
  )

  dtList$prior <- adjustHyperParameter(dtPrior = dtList$prior,
                                       dtStartValues = dtList$startValues)

  dtList$mappedPaths <-
    validateAndLoadMappedPaths(projectConfiguration = projectConfiguration,
                               dtPrior = dtList$prior,
                               dtStartValues = dtList$startValues,
                               scenarioList = scenarioList)

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


# observed data -------------

#' Prepare Data for Matching
#'
#' This function prepares observed data for matching with simulation results based on project configuration and scenario definitions.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including data groups and output paths.
#' @param dataObserved A data.table containing observed data. It must include columns for 'group', 'yValues', and 'yUnit'.
#' @param scenarioList A list of scenarios, each containing simulation details relevant to the observed data.
#'
#' @return A data.table containing the prepared observed data, with adjusted values and units for matching with simulation results.
#'
#' @details The function checks for valid scenarios, merges data tables, calculates conversion factors, and ensures that the data class matches the expected format.
#'
#' @export
prepareDataForMatch <- function(projectConfiguration, dataObserved, scenarioList) {

  # make sure not to change dataObserved outside function
  dataObservedForMatch <- data.table::copy(dataObserved)

  # Retrieve data groups and output paths
  dtDataGroups <- getDataGroups(projectConfiguration$plotsFile)
  dtOutputs <- getOutputPathIds(projectConfiguration$plotsFile)
  scenarioNames <- names(scenarioList)

  # Check if all scenario names are in the default scenarios
  if (!all(scenarioNames %in% dtDataGroups$defaultScenario)) {
    stop(
      paste(
        'There are scenarios which are not selected as "DefaultScenario" in sheet "DataGroups" "Plots.xlsx".',
        'This is mandatory to connect data and simulations:',
        paste(setdiff(scenarioNames, dtDataGroups$defaultScenario), collapse = ', ')
      )
    )
  }

  # Clean up observed data columns
  if ('scenario' %in% names(dataObservedForMatch)) dataObservedForMatch[, scenario := NULL]
  if ('outputPath' %in% names(dataObservedForMatch)) dataObservedForMatch[, outputPath := NULL]

  # Merge data tables
  dataObservedForMatch <- dataObservedForMatch %>%
    merge(unique(dtDataGroups[, c('group', 'defaultScenario')]), by = 'group') %>%
    data.table::setnames(old = 'defaultScenario', 'scenario') %>%
    merge(dtOutputs[, c('outputPathId', 'outputPath')], by = 'outputPathId')

  # reduce data for selected scenarios
  dataObservedForMatch <- dataObservedForMatch[scenario %in% names(scenarioList)]

  dataObservedForMatch <- calculateUnitFactors(dataObservedForMatch,dtOutputs,scenarioList)

  # Check data class
  if (!all(dataObservedForMatch$dataClass == DATACLASS$tpIndividual)) {
    stop(
      paste(
        'Please select only scenarios which are matched as "DefaultScenarios" in sheet "DataGroups" "Plots.xlsx" to individual data.',
        'Check dataGroup',
        paste(unique(dataObservedForMatch[dataClass != DATACLASS$tpIndividual]$group), collapse = ', ')
      )
    )
  }

  return(dataObservedForMatch)
}

#' Calculate Unit Factors and Adjust Observed Data
#'
#' This function calculates unit conversion factors for observed data and adjusts the observed data accordingly.
#'
#' @param dataObserved A data.table containing observed data. It must include columns for 'scenario', 'outputPathId', 'yUnit', and 'yValues'.
#' @param dtOutputs A data.table containing output path information, including 'outputPathId' and 'displayUnit'.
#' @param scenarioList A list of scenarios, each containing simulation details relevant to the observed data.
#'
#' @return A data.table containing the updated observed data with adjusted yValues, unit factors, and units.
#'
#' @details The function merges unit conversion factors into the observed data, adjusts the yValues based on the calculated data factors,
#' and computes the unit factor for time.
#'
#' @keywords internal
calculateUnitFactors <- function(dataObserved,dtOutputs,scenarioList){
  # Get unit conversion factors
  dtUnit <- unique(dataObserved[, c('scenario', 'outputPathId', 'yUnit')]) %>%
    merge(dtOutputs, by = 'outputPathId')

  # Calculate unit factor for x
  dtUnit[, unitFactorX := ospsuite::toUnit(quantityOrDimension = 'Time', values = 1, targetUnit = dataObserved$xUnit[1])]

  # Initialize conversion factors
  dtUnit[, dataFactor := NA_real_]
  dtUnit[, unitFactorY := NA_real_]
  dtUnit[, endTimeSimulation := NA_real_]

  # Calculate conversion factors
  for (iRow in seq_len(nrow(dtUnit))) {

    sim <- scenarioList[[dtUnit$scenario[iRow]]]$simulation

    quantity <- ospsuite::getQuantity(path = dtUnit$outputPath[iRow], container = sim)

    dtUnit$dataFactor[iRow] <- ospsuite::toUnit(
      quantityOrDimension = quantity$dimension,
      values = 1,
      targetUnit = dtUnit$displayUnit[iRow],
      sourceUnit = dtUnit$yUnit[iRow],
      molWeight = sim$molWeightFor(dtUnit$outputPath[iRow])
    )

    dtUnit$unitFactorY[iRow] <- ospsuite::toUnit(
      quantityOrDimension = quantity$dimension,
      values = 1,
      targetUnit = dtUnit$displayUnit[iRow],
      sourceUnit = quantity$unit,
      molWeight = sim$molWeightFor(dtUnit$outputPath[iRow])
    )

    dtUnit$endTimeSimulation[iRow] = sim$outputSchema$endTime * dtUnit$unitFactorX[iRow]
  }

  # Merge conversion factors back to observed data
  dataObserved <- dataObserved %>%
    merge(dtUnit[,c('scenario', 'outputPathId', 'unitFactorX', 'unitFactorY', 'dataFactor', 'displayUnit','endTimeSimulation')],
          by = c('scenario', 'outputPathId'))

  # Adjust yValues and units
  dataObserved[, yValues := yValues * dataFactor]
  dataObserved[, dataFactor := NULL]
  dataObserved[, yUnit := displayUnit]
  dataObserved[, displayUnit := NULL]

  if (any(dataObserved$xValues < 0)){
    warning('data with time < 0 is outside simulation range will be ignored')
    dataObserved <- dataObserved[xValues >= 0]
  }
  if (any(dataObserved$xValues > dataObserved$endTimeSimulation)){
    writeTableToLog(dataObserved[xValues > endTimeSimulation] %>%
                      dplyr::select(any_of(c(getColumnsForColumnType(dataObserved,'identifier'),
                                             'xValues','endTimeSimulation','timeUnit'))))
    warning('data with time outside simulation range will be ignored')
    dataObserved <- dataObserved[xValues <= endTimeSimulation]
  }
  dataObserved[,endTimeSimulation := NULL]


  return(dataObserved)
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

# Prior --------------------
#' Get Prior Definition
#'
#' This function retrieves prior definitions from the configuration file.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details.
#'
#' @return A data.table with prior definitions.
#' @keywords internal
validateAndLoadPriorDefinition <- function(projectConfiguration) {
  # initialize variables to avoid linter messages
  probability <- startValue <- NULL

  dtPrior <- xlsxReadData(
    projectConfiguration$addOns$bMLMConfigurationFile,
    sheetName = "Prior",
    skipDescriptionRow = TRUE,
    alwaysCharacter = c('P1_type','P2_type','P3_type')
  )

  checkmate::assertCharacter(dtPrior$name, any.missing = FALSE)
  checkmate::assertNames(dtPrior$valueMode, subset.of = unlist(PARAMETERTYPE))

  checkDuplicates(
    dt = dtPrior,
    identifierCols = c('name', 'categoricCovariate', 'hyperParameter'),
    sheetName = "Prior"
  )


  checkmate::assertNames(dtPrior[valueMode == PARAMETERTYPE$individual]$hyperDistribution,
                         subset.of = c(getAllDistributions()))

  validateGroupConsistency(dt = dtPrior[valueMode == PARAMETERTYPE$individual],
                           valueColumns = c('hyperDistribution'),
                           groupingColumns = c('name','categoricCovariate'))

  validateGroupConsistency(dt = dtPrior[valueMode == PARAMETERTYPE$individual],
                           valueColumns = c('unit'),
                           groupingColumns = c('name'))

  checkmate::assertNumeric(dtPrior$minValue,any.missing = FALSE)
  checkmate::assertNumeric(dtPrior$maxValue,any.missing = FALSE)

  if (any(is.na(dtPrior$startValue))){
    dtPrior[is.na(startValue),
            startValues := runif(1, min = minValue, max = maxValue),
            by = c('name','categoricCovariate','hyperParameter')]
  }

  checkmate::assertNumeric(dtPrior$startValue,any.missing = FALSE)

  dtPrior <- checkMinMaxValues(dtPrior)

  checkmate::assertNames(dtPrior[valueMode != PARAMETERTYPE$outputError]$scaling, subset.of = unlist(SCALING))
  checkmate::assertLogical(as.logical(dtPrior[valueMode != PARAMETERTYPE$outputError]$useAsFactor), any.missing = FALSE)
  checkmate::assertNames(dtPrior$distribution, subset.of = c('flat',getAllDistributions()))

  dtPrior[, probability := apply(.SD, 1, calculateProbability)]

  if (any(is.na(dtPrior$probability))) {
    stop(paste(
      "Probability of startvalue is NA, check priors ",
      paste(dtPrior[is.na(probability)]$name, collapse = ", ")
    ))
  }

  if (any(dtPrior$probability < 0 | dtPrior$probability > 1)) {
    stop(paste(
      "Start value outside distribution range, check",
      paste(dtPrior[probability < 0 | probability > 1]$name, collapse = ", ")
    ))
  }

  return(dtPrior)
}


#' Adjust Hyperparameters
#'
#' This function adjusts hyperparameters based on prior data and specified start values.
#' It filters out non-hyperparameter entries not available in startValues, and calculates
#' the log truncation offset for hyperparameters.
#'
#' @param dtPrior A data.table containing prior data, which includes hyperparameters
#'                and their associated values.
#' @param dtStartValues A data.table containing individual starting values for hyperparameters,
#'                      including names, categorical covariates, and minimum and maximum values.
#'
#' @return A data.table containing adjusted hyperparameters with calculated log truncation offsets.
#'
#' @keywords internal
adjustHyperParameter <- function(dtPrior, dtStartValues) {

  # Keep non-hyperParameter rows
  dtPriorNew <- dtPrior[valueMode != PARAMETERTYPE$hyperParameter]

  # Reduce hyperparameter to match available data
  dtHyperParameter <- dtPrior[valueMode == PARAMETERTYPE$hyperParameter] %>%
    merge(unique(dtStartValues[, .(name, categoricCovariate, minValue, maxValue)]),
          by = c('name', 'categoricCovariate'),
          suffixes = c('', '.indValues'))

  # Add truncationOffset column
  dtPriorNew[, logTruncationOffset := NA_real_]

  # Loop through each hyperparameter group
  for (dtGroup in split(dtHyperParameter, by = c('name', 'categoricCovariate'))) {
    setDT(dtGroup)

    # Create a named list of parameters
    paramList <- setNames(dtGroup$value, dtGroup$hyperParameter)

    # Calculate logTruncationOffset
    dtGroup[, logTruncationOffset :=
              log(1 -
                    do.call(paste0("p", dtGroup$hyperDistribution[1]),
                            c(
                              list(q = dtGroup$minValue.indValues[1], log = FALSE), paramList
                            )) -
                    do.call(paste0("p", dtGroup$hyperDistribution[1]),
                            c(
                              list(
                                q = dtGroup$maxValue.indValues[1],
                                log = FALSE,
                                lower.tail = FALSE
                              ),
                              paramList
                            )))]

    # Combine results
    selectedCols <- names(dtPriorNew)
    dtPriorNew <- rbind(dtPriorNew, dtGroup[, ..selectedCols])
  }

  return(dtPriorNew)
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
    dplyr::select(any_of(c(
      "name",
      "hyperParameter",
      "categoricCovariate",
      "value",
      "hyperDistribution",
      "scaling",
      'logTruncationOffset'
    )))

  return(dtHyperParameter)
}

# Individual StartValue s-----------------

#' Get Individual Start Values
#'
#' This function reads individual start values from an Excel file and adjusts them based on hyperparameters.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details.
#' @param dtHyperParameter A data.table containing hyperparameters and their distributions.
#' @param dataObserved A data.table containing observed data.
#' @param seed A numeric seed for random number generation.
#'
#' @return A data.table with updated start values.
#' @keywords internal
validateAndLoadIndividualStartValues <-
  function(projectConfiguration,
           dtHyperParameter,
           dataObserved,
           seed) {
    # initialize variables to avoid linter messages
    value <- startValue <- NULL

    dtStartValues <- xlsxReadData(
      projectConfiguration$addOns$bMLMConfigurationFile,
      sheetName = "IndividualStartValues",
      skipDescriptionRow = TRUE
    )


    # use only values avialable in oberved data
    dtStartValues <- dtStartValues[individualId %in% unique(dataObserved$individualId)]

    # validate
    checkmate::assertNames(dtStartValues$scaling, subset.of = unlist(SCALING))
    checkmate::assertLogical(as.logical(dtStartValues$useAsFactor), any.missing = FALSE)

    validateGroupConsistency(dt = dtStartValues,
                             valueColumns = c('minValue', 'maxValue','scaling','useAsFactor'),
                             groupingColumns = c('name','categoricCovariate'))

    checkDuplicates(
      dt = dtStartValues,
      identifierCols =  c('name', 'categoricCovariate', 'individualId'),
      sheetName = "StartValues"
    )

    checkmate::assertNumeric(dtStartValues$minValue,any.missing = FALSE)
    checkmate::assertNumeric(dtStartValues$maxValue,any.missing = FALSE)

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
      stop(paste("Not possible to generate random startValues in boundarys for",tmp$name[1]))
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



# Mapped Paths ---------
#' Validate and Load Mapped Paths
#'
#' This function validates and loads mapped paths from the BMLM configuration file.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details.
#' @param scenarioList A list of scenarios to be used in the optimization.
#'
#' @return A data.table containing mapped paths for parameters.
#' @keywords internal
validateAndLoadMappedPaths <- # nolint cyclocomp
  function(projectConfiguration,
           dtPrior,
           dtStartValues,
           scenarioList) {
    # initialize variable to avoid linter message
    scenarioName <- linkedParameters <- NULL

    dtDefinition <-
      rbind(dtPrior[valueMode %in% PARAMETERTYPE$global, c("name","unit", "useAsFactor")],
            dtPrior[valueMode %in% PARAMETERTYPE$hyperParameter, c("name","unit")] %>%
              unique() %>%
              merge(dtStartValues[,c("name", "useAsFactor")] %>%  unique(),
                    by = c("name")) %>%
              unique()
    )

    dtMappedPaths <-
      xlsxReadData(
        projectConfiguration$addOns$bMLMConfigurationFile,
        sheetName = "ParameterMappedPaths",
        skipDescriptionRow = TRUE
      )

    checkDuplicates(dt = dtMappedPaths, identifierCols = c('name','linkedParameters'), sheetName= 'MappedPaths')

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



# Input transformation -----------
#' Prepare Input Data
#'
#' This function prepares the input data for the L-BFGS-B algorithm by merging and transforming
#' the provided data tables. It handles log transformations based on specified conditions.
#'
#' @param dtPrior A data.table containing prior values
#' @param dtStartValues A data.table containing start values
#' @param valueColumn Name of column with value of interest either 'value' or 'startValue'.
#'
#' @return A data.table with combined and transformed input data.
#' @keywords internal
prepareInputData <- function(dtPrior, dtStartValues,valueColumn = c('value','startValue')) {
  # initialize variables to avoid linter messages
  value <- minValue <- maxValue <- scaling <- NULL

  valueColumn <- match.arg(valueColumn)

  # Select relevant columns from dtPrior and dtStartValues and create logConversion column
  dtInput <- rbind(
    dtPrior[, c(..valueColumn, 'minValue', 'maxValue', 'scaling')],
    dtStartValues[, c(..valueColumn, 'minValue', 'maxValue', 'scaling')]
  ) %>%
  setnames(old =  valueColumn,new =  "value")

  checkmate::assertNumeric(dtInput$value, any.missing = FALSE)
  checkmate::assertNumeric(dtInput$minValue, any.missing = FALSE)
  checkmate::assertNumeric(dtInput$maxValue, any.missing = FALSE)
  checkmate::assertNames(dtInput$scaling, subset.of = unlist(SCALING))

  # Transform params to unbounded values
  dtInput[, param := transformToUnbounded(value, minValue, maxValue, scaling),by =.I]

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
  dtList$input[, value := inverseTransformParams(param, maxValue, minValue, scaling),by =.I]

  dtList$prior$value <- dtList$input$value[seq_len(nrow(dtList$prior))]
  dtList$startValues$value <-
    dtList$input$value[nrow(dtList$prior) + seq_len(nrow(dtList$startValues))]

  return(dtList)
}
#' Transform to Unbounded Values
#'
#' This function transforms parameters to unbounded values based on the scaling type.
#'
#' @param value A numeric value of values to be transformed.
#' @param minValue A numeric value of minimum values.
#' @param maxValue A numeric value of maximum values.
#' @param scaling A value indicating the scaling type for value.
#'
#' @return A numeric value of transformed values.
#' @keywords internal
transformToUnbounded <- function(value, minValue, maxValue, scaling) {
  return(if(scaling == SCALING$log){
    qlogis((log(value) - log(minValue)) / (log(maxValue) - log(minValue)))
  } else {
    qlogis((value - minValue) / (maxValue - minValue))
  }
  )
}

#' Inverse Transform Parameters
#'
#' This function applies the inverse transformation to the parameters to retrieve original values.
#'
#' @param param A numeric value of parameters to be transformed back.
#' @param minValue A numeric value of minimum values.
#' @param maxValue A numeric value of maximum values.
#' @param scaling A value indicating the scaling type for each parameter.
#'
#' @return A numeric vector of original values.
#' @keywords internal
inverseTransformParams <- function(param, minValue, maxValue, scaling) {
  return(if(scaling == SCALING$log){
    exp(plogis(param) * (log(maxValue) - log(minValue)) + log(minValue))
  } else{
    plogis(param) * (maxValue - minValue) + minValue
  }
  )
}


# auxiliaries --------------

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
#' Check for Duplicates in Data Frames
#'
#' This function checks for duplicates in specified columns of a data frame
#' and throws an error if duplicates are found. It generates a descriptive
#' error message indicating which parameters are duplicated.
#'
#' @param dt A data.table to check for duplicates.
#' @param identifierCols A character vector of column names to check for uniqueness.
#' @param sheetName A character string representing the name of the sheet for error messages.
#' @return NULL. The function stops execution if duplicates are found.
#'
#' @keywords internal
checkDuplicates <- function(dt, identifierCols, sheetName) {
  if (any(duplicated(dt[, ..identifierCols]))) {
    stop(paste0(
      "Sheet ", sheetName, " must be unique in columns '",
      paste(identifierCols, collapse = "', '"), "' ",
      "\nCheck parameters with name: '",
      paste(unique(dt[duplicated(dt[, ..identifierCols]), ]$name), collapse = "', '"), "'"
    ))
  }
}
