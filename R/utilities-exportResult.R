' Save Final Values to Tables
#'
#' This function saves final values from a provided data table to specified sheets in an Excel workbook.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including paths to Excel files.
#' @param dtList A list of data.tables containing the final values to be saved.
#'
#' @return An updated workbook object.
#' @keywords internal
#' @noRd
saveFinalValuesToTables <- function(projectConfiguration, dtList) {
  wb <- openxlsx::loadWorkbook(file = projectConfiguration$addOns$bMLMConfigurationFile)

  dtNew <- addFinalValue(wb,
    sheetName = "Prior",
    identifier = c("name", "hyperParameter", "categoricCovariate"),
    newTable = dtList$prior
  )
  xlsxWriteData(wb, sheetName = "Prior", dtNew)

  dtNew <- addFinalValue(wb,
    sheetName = "IndividualStartValues",
    identifier = c("name", "individualId", "categoricCovariate"),
    newTable = dtList$startValues
  )
  xlsxWriteData(wb, sheetName = "IndividualStartValues", dtNew)


  openxlsx::saveWorkbook(wb = wb, file = projectConfiguration$addOns$bMLMConfigurationFile, overwrite = TRUE)
}
#' Export Individual Values to Configuration Table
#'
#' This function exports individual values from a given data table to a specified
#' configuration table in an Excel workbook.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including the path to the Excel file.
#' @param scenarioList A list of scenarios, each containing simulation parameters.
#' @param dtList A data.table containing BMLM configuration tables.
#'
#' @return NULL
#' @export
exportIndividualValuesToConfigTable <- function(projectConfiguration, scenarioList, dtList) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$individualsFile)
  individualIds <- unique(dtList$startValues$individualId)

  # Use lapply to process each individual
  results <- lapply(individualIds, function(sheetName) {
    message(paste("export values for", sheetName))

    dtInd <- dtList$startValues[sheetName == individualId, c("name", "categoricCovariate", "value")]
    dtAdd <- addContainerAndParameterPath(
      dtExport = dtInd,
      dtMappedPaths = dtList$mappedPaths
    ) %>%
      setnames(
        old = c("unit"),
        new = c("units")
      )

    scenarios <- unique(dtList$data[individualId == sheetName]$scenario)

    dtAdd <-
      dtAdd[, !c("scenarios"), with = FALSE] %>%
      melt(value.name = "multiplicator", variable.name = "scenario", measure.vars = scenarios)
    dtAdd <- unique(dtAdd[, !c("scenario"), with = FALSE])
    if (any(duplicated(dtAdd$linkedParameters))) {
      stop(messages$errorExportAmbiguousValues())
    }
    dtAdd[useAsFactor == 1, value := value * multiplicator]

    for (iRow in which(dtAdd$useAsFactor == 1)) {
      p <- getParameter(container = scenarioList[[scenarios[[1]]]]$simulation, path = dtAdd$linkedParameters[iRow])
      dtAdd$units[iRow] <- p$unit
    }

    if (sheetName %in% wb$sheet_names) {
      dt <- xlsxReadData(wb, sheetName = sheetName)
    } else {
      dt <- (data.table(
        "container Path" = character(),
        "parameter Name" = character(),
        value = numeric(),
        units = character()
      ))
    }

    dtAdd <- dtAdd[, names(dt), with = FALSE]
    dt <- rbind(dt, dtAdd)
    # overwrite existing parameter from dt, with the ones added by dtAdd
    dt <- dt[!duplicated(dt[, c("container Path", "parameter Name")], fromLast = TRUE)]

    xlsxAddDataUsingTemplate(
      wb = wb,
      templateSheet = "template_Ind",
      sheetName = sheetName,
      dtNewData = dt,
      templateXlsx = templateXlsx
    )
  })

  openxlsx::saveWorkbook(wb = wb, file = projectConfiguration$individualsFile, overwrite = TRUE)

  return(invisible())
}
#' Export Individual Results to PKML
#'
#' This function exports individual results to a PKML file for a specified individual ID across scenarios.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including paths for saving PKML files.
#' @param scenarioList A list of scenarios, each containing simulation parameters.
#' @param dtList A list of data.tables containing prior, start values, and mapped paths.
#' @param outputDir A string representing the directory where the PKML files will be saved.
#' @param individualId A string representing the ID of the individual whose results will be exported.
#'
#' @return NULL
#' @export
exportIndividualResultsToPkml <- function(projectConfiguration,
                                          scenarioList,
                                          dtList,
                                          outputDir,
                                          individualId) {
  invisible(lapply(names(scenarioList), function(scenarioName) {
    updateParameterValues(
      scenarioName = scenarioName,
      scenario = scenarioList[[scenarioName]],
      dtPrior = dtList$prior,
      dtStartValues = dtList$startValues,
      dtMappedPaths = dtList$mappedPaths
    )
  }))

  for (scenarioName in names(scenarioList)) {
    if (individualId %in% scenarioList[[scenarioName]]$population$getCovariateValues("ObservedIndividualId")) {
      population <-
        ospsuite::populationToDataFrame(scenarioList[[scenarioName]]$population) %>%
        setDT()
      individual <-
        population[ObservedIndividualId == individualId] %>% dplyr::select(!any_of(
          c("IndividualId", scenarioList[[scenarioName]]$population$allCovariateNames)
        ))
      sourceFile <-
        scenarioList[[scenarioName]]$simulation$sourceFile
      simNew <- ospsuite::loadSimulation(sourceFile)
      ospsuite::setParameterValuesByPath(
        parameterPaths = names(individual),
        values = unname(unlist(individual[1, ])),
        simulation = simNew,
        stopIfNotFound = FALSE
      )
      ospsuite::saveSimulation(
        simulation = simNew,
        filePath = file.path(
          outputDir,
          gsub("\\.pkml", paste0("_", individualId, ".pkml"), basename(sourceFile))
        )
      )
    }
  }
}

#' Export Optimized Population
#'
#' This function exports optimized population data to CSV files for each scenario in the scenario list.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including paths for saving population files.
#' @param dtList A list of data.tables containing the prior, start values, and mapped paths.
#' @param scenarioList A list of scenarios, each containing simulation parameters.
#' @param runName A string representing the name of the run.
#' @param addAsNewScenarios A boolean indicating whether to add new scenarios to the workbook.
#' @param overwrite A boolean indicating whether to overwrite existing files.
#'
#' @return NULL
#' @export
#' @family export
exportOptimizedPopulation <-
  function(projectConfiguration,
           dtList,
           scenarioList,
           runName,
           addAsNewScenarios = TRUE,
           overwrite = FALSE) {
    # initialize variable to avoid linter message
    scenario_name <- populationId <- NULL # nolint camelCase, variable is derived by column name esqlabR

    invisible(lapply(names(scenarioList), function(scenarioName) {
      updateParameterValues(
        scenarioName = scenarioName,
        scenario = scenarioList[[scenarioName]],
        dtPrior = dtList$prior,
        dtStartValues = dtList$startValues,
        dtMappedPaths = dtList$mappedPaths
      )
    }))

    for (scenarioName in names(scenarioList)) {
      newPopName <- paste0(scenarioList[[scenarioName]]$scenarioConfiguration$populationId, "_", runName, ".csv")
      if (file.exists(file.path(projectConfiguration$populationsFolder, newPopName)) &
        !overwrite) {
        message(paste("no export of", newPopName, "file already exists."))
      } else {
        message(paste("export", newPopName))
        ospsuite::exportPopulationToCSV(
          population = scenarioList[[scenarioName]]$population,
          filePath = file.path(
            projectConfiguration$populationsFolder,
            newPopName
          )
        )
      }
    }

    if (addAsNewScenarios) {
      wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
      dt <- xlsxReadData(wb, sheetName = "Scenarios")

      dtSc <- dt[scenario_name %in% names(scenarioList)]
      dtSc[, scenario_name := paste0(scenario_name, "_", runName)]
      dtSc[, populationId := paste0(populationId, "_", runName)]

      if (overwrite) {
        dt <- dt[!(scenario_name %in% dtSc$scenario_name)]
      } else {
        dtSc <- dtSc[!(scenario_name %in% dt$scenario_name)]
      }

      xlsxWriteData(wb, sheetName = "Scenarios", dt = rbind(dt, dtSc))

      openxlsx::saveWorkbook(wb = wb, file = projectConfiguration$scenariosFile, overwrite = TRUE)
    }
  }
#' Export Global and HyperParameter Parameters to Configuration Tables
#'
#' This function exports global parameters from the provided data table to a new sheet in the model parameters Excel file.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including the path to the model parameters file.
#' @param dtList A list of data.tables containing the prior values.
#' @param sheetName A string representing the name of exported sheet.
#' @param overwrite A boolean indicating whether to overwrite an existing sheet.
#'
#' @return NULL
#' @export
exportModelParametersToConfigTables <- function(projectConfiguration, dtList, sheetName, overwrite = FALSE) {
  tmp <- dtList$prior[useAsFactor == TRUE, c("name")] %>%
    unique()
  if (nrow(tmp) > 0) {
    warning(messages$warningFactorParametersNotExported(tmp$name))
  }

  dtExport <- dtList$prior[valueMode != PARAMETERTYPE$outputError &
    useAsFactor == FALSE]
  if (nrow(dtExport) == 0) {
    message("no parameters to export")
    return(invisible())
  }
  wbMP <- openxlsx::loadWorkbook(projectConfiguration$modelParamsFile)
  wbPop <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)


  for (covariate in unique(dtExport$categoricCovariate)) {
    exportSheets <- extractParameterValues(
      dtExport = dtExport[categoricCovariate == covariate],
      dtMappedPaths = dtList$mappedPaths[, c("name", "linkedParameters")]
    )

    wbMP <- addExportSheet(
      wb = wbMP,
      dt = exportSheets$global,
      covariate = covariate,
      overwrite = overwrite,
      suffix = "global",
      sheetName = sheetName
    )
    wbMP <- addExportSheet(
      wb = wbMP,
      dt = exportSheets$median,
      covariate = covariate,
      overwrite = overwrite,
      suffix = "median",
      sheetName = sheetName
    )
    wbPop <- addExportSheet(
      wb = wbPop,
      dt = exportSheets$population,
      covariate = covariate,
      overwrite = overwrite,
      suffix = "",
      sheetName = sheetName,
      toModelParameters = FALSE
    )
  }
  openxlsx::saveWorkbook(wb = wbMP, file = projectConfiguration$modelParamsFile, overwrite = TRUE)
  openxlsx::saveWorkbook(wb = wbPop, file = projectConfiguration$populationsFile, overwrite = TRUE)
}
# auxiliaries ------------
addExportSheet <- function(wb, dt, sheetName, covariate, overwrite, suffix, toModelParameters = TRUE) {
  if (nrow(dt) == 0) {
    return(wb)
  }

  sheetNameParts <- c(sheetName, covariate, suffix)
  sheetName <- paste(sheetNameParts[trimws(sheetNameParts) != ""], collapse = "_")

  if (sheetName %in% wb$sheet_names & !overwrite) {
    warning(messages$errorSheetAlreadyExists(sheetName))
    return(wb)
  }

  message(paste("export parameters to", sheetName))

  if (toModelParameters) {
    xlsxAddDataUsingTemplate(
      wb = wb,
      templateSheet = "Template",
      sheetName = sheetName,
      dtNewData = dt,
      templateXlsx = "ModelParameters.xlsx"
    )
  } else {
    xlsxAddDataUsingTemplate(
      wb = wb,
      templateSheet = "Template_Variability",
      sheetName = sheetName,
      dtNewData = dt,
      templateXlsx = "Populations.xlsx"
    )
  }

  return(wb)
}

#' Extract Parameter Values
#'
#' This function extracts the optimized  parameter values and merges them with mapped paths.
#'
#' @param dtExport A data.table containing identifier and parametertype of parameters to be exported
#' @param dtMappedPaths A data.tables containing mapped parameter paths.
#'
#' @return A list of data.table containing extracted sheet inputs for export
#' @keywords internal
#' @noRd
extractParameterValues <- function(dtExport, dtMappedPaths) {
  exportSheets <- list(global = data.table(), median = data.table(), population = data.table(), individuals = data.table())
  if (nrow(dtExport) == 0) {
    return(exportSheets)
  }

  dtExport <- addContainerAndParameterPath(dtExport = dtExport, dtMappedPaths = dtMappedPaths)

  # Rename 'unit' column to 'units' for consistency
  setnames(dtExport, old = c("unit"), new = c("units"))

  # filter global values
  exportSheets[["global"]] <-
    dtExport[valueMode == PARAMETERTYPE$global, c("container Path", "parameter Name", "value", "units")]

  # evaluate hyperParameters
  dtExport <- dtExport[
    valueMode != PARAMETERTYPE$global,
    c("container Path", "parameter Name", "name", "units", "hyperDistribution", "hyperParameter", "value")
  ]
  if (nrow(dtExport) > 0){
    dtExport[, index := seq(1, .N), by = c("container Path", "parameter Name", "name", "units", "hyperDistribution")]

    dtExport <- dcast(dtExport,
                      `container Path` + `parameter Name` + name + units + hyperDistribution ~ index,
                      value.var = c("hyperParameter", "value")
    ) %>%
      setnames(
        old = c("name", "hyperDistribution", paste("hyperParameter", seq(1, 3), sep = "_"), paste("value", seq(1, 3), sep = "_")),
        new = c("parameter Group", "distribution", paste0("p", seq(1, 3), "_type"), paste0("p", seq(1, 3), "_value")),
        skip_absent = TRUE
      )
    exportSheets[["population"]] <- copy(dtExport)

    dtExport[, value := apply(.SD, 1, calculateValueOfDistributionRow, value = 0.5, type = "Q", log = FALSE)]
    exportSheets[["median"]] <- dtExport[c("container Path", "parameter Name", "value", "units")]
  }

  return(exportSheets)
}
addFinalValue <- function(wb, sheetName, identifier, newTable) {
  dt <- xlsxReadData(wb, sheetName = sheetName)
  headers <- names(dt)
  dtHeaders <- dt[1]
  dt <- dt[-1]

  dt[, (identifier) := lapply(.SD, function(x) ifelse(is.na(x), "", x)), .SDcols = identifier]
  newTable[, (identifier) := lapply(.SD, function(x) ifelse(is.na(x), "", x)), .SDcols = identifier]


  dt <- dt %>%
    dplyr::select(-dplyr::any_of(c("startValue", "finalValue"))) %>%
    merge(
      newTable %>%
        dplyr::select(dplyr::all_of(c(identifier, "startValue", "value"))) %>%
        data.table::setnames("value", "finalValue"),
      by = identifier,
      sort = FALSE
    )

  dt <- rbind(dtHeaders, dt, fill = TRUE) %>%
    setcolorder(c(headers[seq(1, which(headers == "startValue"))], "finalValue"))

  return(dt)
}
" Add Container and Parameter Path
#"
#' This function merges a data.table containing parameter values with mapped paths to extract respective container and parameter names.
#'
#' @param dtExport A data.table containing identifier and parameter type of parameters to be merged with mapped paths.
#' @param dtMappedPaths A data.table containing mapped parameter paths.
#'
#' @return A data.table that includes the mapped container paths and parameter names.
#' @keywords internal
#' @noRd
addContainerAndParameterPath <- function(dtExport, dtMappedPaths) {
  dtExport <- merge(dtMappedPaths, dtExport, by = "name")

  dtExport[, `container Path` := sapply(strsplit(linkedParameters, "\\|"), function(x) paste(x[-length(x)], collapse = "|"))]
  dtExport[, `parameter Name` := sapply(strsplit(linkedParameters, "\\|"), function(x) tail(x, n = 1))]

  return(dtExport)
}
# exportPopulationWithVariability --------------
#' Export Population with Variability
#'
#' This function loads an existing population CSV file and generates new random parameter values
#' according to the distributions defined in a variability sheet. Parameters with the same
#' parameter group are strictly correlated.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details,
#'   including the path to the populations folder and populations file.
#' @param populationName A string representing the name of the population (without .csv extension).
#' @param variabilitySheetName A string representing the name of the variability sheet in the
#'   Populations.xlsx file.
#' @param newName An optional string for the new population name. If NULL, defaults to
#'   paste(populationName, variabilitySheetName, sep = '_').
#' @param overwrite A logical indicating whether to overwrite an existing population file with the same name.
#'   Default is FALSE.
#'
#' @return NULL (invisible). The function saves a new population CSV file.
#' @export
#' @family export
exportPopulationWithVariability <- function(projectConfiguration,
                                            populationName,
                                            variabilitySheetName,
                                            newName = NULL,
                                            overwrite = FALSE) {
  # Validate inputs
  checkmate::assertClass(projectConfiguration, "ProjectConfiguration")
  checkmate::assertString(populationName)
  checkmate::assertString(variabilitySheetName)
  checkmate::assertString(newName, null.ok = TRUE)
  checkmate::assertFlag(overwrite)

  # Set default newName if not provided
  if (is.null(newName)) {
    newName <- paste(populationName, variabilitySheetName, sep = "_")
  }

  # new population
  newPopulationFile <- file.path(projectConfiguration$populationsFolder, paste0(newName, ".csv"))

  # Check if file already exists
  if (file.exists(newPopulationFile) && !overwrite) {
    message(paste("Population file", newPopulationFile, "already exists. Use overwrite=TRUE to replace it."))
    return(invisible())
  }


  # Load existing population file
  populationFile <- file.path(projectConfiguration$populationsFolder, paste0(populationName, ".csv"))
  checkmate::assertFileExists(populationFile)

  message(paste("Loading population from:", populationFile))
  dtPopulation <- data.table::fread(populationFile)

  # Load variability sheet from Populations.xlsx
  wbPop <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)

  if (!(variabilitySheetName %in% wbPop$sheet_names)) {
    stop(paste("Variability sheet", variabilitySheetName, "not found in", projectConfiguration$populationsFile))
  }

  message(paste("Loading variability sheet:", variabilitySheetName))
  dtVariability <- xlsxReadData(wb = wbPop, sheetName = variabilitySheetName, skipDescriptionRow = TRUE)

  # Validate variability sheet has required columns
  requiredCols <- c("container Path", "parameter Name", "parameter Group", "distribution")
  missingCols <- setdiff(requiredCols, names(dtVariability))
  if (length(missingCols) > 0) {
    stop(paste("Variability sheet missing required columns:", paste(missingCols, collapse = ", ")))
  }

  # Create full parameter paths for matching
  dtVariability[, parameterPath := paste(`container Path`, `parameter Name`, sep = "|")]

  # Get number of individuals
  nIndividuals <- nrow(dtPopulation)
  message(paste("Generating variability for", nIndividuals, "individuals"))

  # Group parameters by parameter Group for correlation
  uniqueGroups <- unique(dtVariability$`parameter Group`)

  # Generate random values for each group
  for (group in uniqueGroups) {
    dtGroup <- dtVariability[`parameter Group` == group]
    nParams <- nrow(dtGroup)

    message(paste("Processing parameter group:", group, "with", nParams, "parameters"))

    # For strict correlation, generate one set of random quantiles (probabilities)
    # and apply to all parameters in the group
    randomQuantiles <- runif(nIndividuals)

    # Apply the same quantiles to each parameter in the group
    for (i in seq_len(nParams)) {
      row <- dtGroup[i, ]
      paramPath <- row$parameterPath

      # Generate new values using the quantile function for the distribution
      newValues <- sapply(randomQuantiles, function(q) {
        calculateValueOfDistributionRow(row = unlist(row[1]), type = "Q", value = q, log = FALSE)
      })

      if (any(is.na(newValues))) stop(paste("Parameter generation for", paramPath, "failed. Distributed parameter contains NA."))

      # Update population with new values
      dtPopulation[[paramPath]] <- newValues
    }
  }

  # Save new population
  message(paste("Saving new population to:", newPopulationFile))
  data.table::fwrite(dtPopulation, newPopulationFile)

  message(paste("Successfully created population with variability:", newName))
  return(invisible())
}
