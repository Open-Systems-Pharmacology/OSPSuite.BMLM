#' Save Final Values to Tables
#'
#' This function saves final values from a provided data table to specified sheets in an Excel workbook.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including paths to Excel files.
#' @param dtList A list of data.tables containing the final values to be saved.
#'
#' @return An updated workbook object.
#' @keywords internal
saveFinalValuesToTables <- function(projectConfiguration, dtList) {
  wb <- openxlsx::loadWorkbook(file = projectConfiguration$addOns$bMLMConfigurationFile)

  addFinalValue <- function(wb, sheetName, identifier, newTable) {
    dt <- xlsxReadData(wb, sheetName = sheetName)

    headers <- names(dt)
    dtHeaders <- dt[1]
    dt <- dt[-1]

    dt <- dt %>%
      dplyr::select(-dplyr::any_of(c("startValue", "finalValue"))) %>%
      merge(
        newTable %>%
          dplyr::select(dplyr::all_of(c(identifier, "startValue", "value"))) %>%
          data.table::setnames("value", "finalValue"),
        by = identifier,
        sort = FALSE
      )

    dt = rbind(dtHeaders,dt,fill = TRUE) %>%
      setcolorder(c(headers[seq(1, which(headers == "startValue"))], "finalValue"))

    xlsxWriteData(wb, sheetName = sheetName, dt)

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
exportOptimizedPopulation <-
  function(projectConfiguration,
           dtList,
           scenarioList,
           runName,
           addAsNewScenarios = TRUE,
           overwrite = FALSE) {

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

  for (scenarioName in names(scenarioList)) {
    newPopName <- paste0(scenarioList[[scenarioName]]$scenarioConfiguration$populationId, '_',runName, ".csv")
    if (file.exists( file.path(projectConfiguration$populationsFolder,newPopName)) &
        ! overwrite){
      message(paste('no export of',newPopName, 'file already exists.'))
    } else{
      message(paste('export',newPopName))
      ospsuite::exportPopulationToCSV(
        population = scenarioList[[scenarioName]]$population,
        filePath = file.path(
          projectConfiguration$populationsFolder,
          newPopName
        )
      )
    }

  }

  if (addAsNewScenarios){
    wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
    dt <- xlsxReadData(wb, sheetName = "Scenarios")

    dtSc <- dt[scenario_name %in% names(scenarioList)]
    dtSc[, scenario_name := paste0(scenario_name, '_',runName)]
    dtSc[, populationId := paste0(populationId, '_',runName)]

    if (overwrite){
      dt <- dt[!(scenario_name %in% dtSc$scenario_name)]
    } else {
      dtSc <- dtSc[!(scenario_name %in% dt$scenario_name)]
    }

    xlsxWriteData(wb, sheetName = "Scenarios", dt = rbind(dt, dtSc))

    openxlsx::saveWorkbook(wb = wb, file = projectConfiguration$scenariosFile, overwrite = TRUE)
  }
  }
#' Export Global Parameters to Configuration Tables
#'
#' This function exports global parameters from the provided data table to a new sheet in the model parameters Excel file.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including the path to the model parameters file.
#' @param dtList A list of data.tables containing the prior values.
#' @param runName A string representing the name of the run.
#' @param overwrite A boolean indicating whether to overwrite an existing sheet.
#'
#' @return NULL
#' @export
exportGlobalsParametersToConfigTables <- function(projectConfiguration, dtList,runName,overwrite = FALSE){

  if (!any(dtList$prior$valueMode == PARAMETERTYPE$global)){
    message('no global parameters available')
    return(invisible())
  }

  wb <- openxlsx::loadWorkbook(projectConfiguration$modelParamsFile)
  sheetName <- paste0(runName,'_global')
  if (sheetName %in% wb$sheet_names & !overwrite)
    stop(paste(sheetName,'already exists'))

  message('export global parameters')
  dtAdd <- extractParameterValues(
    dtNew = dtList$prior[valueMode == PARAMETERTYPE$global,c('name','value')],
    dtList = dtList,
    scenarioList = scenarioList)

  saveDataToWorkbook(wb = wb, sheetName = sheetName,
                     dt = dtdd,
                     templateSheet = "Template",
                     templateXlsx = "ModelParameters.xslx")

  openxlsx::saveWorkbook(wb = wb, file = projectConfiguration$modelParamsFile, overwrite = TRUE)

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
    message(paste('export values for', sheetName))

    dtInd <- dtList$startValues[sheetName == individualId]

    dt <- loadOrCreateSheetData(wb = wb, sheetName = sheetName)

    dtAdd <- extractParameterValues(dtNew = dtInd[, !("useAsFactor"), with = FALSE],
                                              dtList = dtList,
                                              scenarioList = scenarioList)

    dt <- rbind(dt, dtAdd)
    dt <- dt[!duplicated(dt[, c('container Path', 'parameter Name')], fromLast = TRUE)]

    saveDataToWorkbook(wb = wb, sheetName = sheetName, dt = dt)
  })

  openxlsx::saveWorkbook(wb = wb, file = projectConfiguration$individualsFile, overwrite = TRUE)

  return(invisible())
}

#' Extract Parameter Values
#'
#' This function extracts the optimized  parameter values and merges them with mapped paths.
#'
#' @param dtNew A data.table containing new parameter values to be extracted.
#' @param dtList A list of data.tables containing mapped paths and other relevant data.
#' @param scenarioList A list of scenarios, each containing simulation parameters.
#'
#' @return A data.table containing extracted parameter values along with their container paths and names.
#' @keywords internal
extractParameterValues <- function(dtNew, dtList, scenarioList) {
  # Merge the mapped paths with the individual data, excluding the 'useAsFactor' column
  dtAdd <- merge(dtList$mappedPaths, dtNew, by = 'name')

  # Extract 'container Path' and 'parameter Name' from 'linkedParameters'
  dtAdd[, `container Path` := sapply(strsplit(linkedParameters, "\\|"), function(x) paste(x[-length(x)], collapse = "|"))]
  dtAdd[, `parameter Name` := sapply(strsplit(linkedParameters, "\\|"), function(x) tail(x, n = 1))]

  # Rename 'unit' column to 'units' for consistency
  setnames(dtAdd, old = c('unit'), new = c('units'))

  # Loop through rows where 'useAsFactor' is TRUE
  for (iRow in which(as.logical(dtAdd$useAsFactor))) {
    # Select relevant columns and reshape the data from wide to long format
    tmp <- dtAdd[iRow] %>%
      dplyr::select(c('linkedParameters', 'value', names(scenarioList))) %>%
      tidyr::pivot_longer(cols = names(scenarioList), names_to = 'scenario', values_to = 'factor') %>%
      setDT()

    # Remove rows with NA factors
    tmp <- tmp[!is.na(factor)]

    # Check for consistency in factors; stop if any are duplicated
    if (any(duplicated(tmp$factor))) {
      stop(paste(tmp$linkedParameters[1], 'factors are not consistent for scenarios'))
    }

    # Keep only the first row (since factors should be consistent)
    tmp <- tmp[1]

    # Retrieve the parameter details using the linkedParameters
    p <- ospsuite::getParameter(tmp$linkedParameters, container = scenarioList[[tmp$scenario]]$simulation)

    # Update the value and units in dtAdd based on the retrieved parameter
    dtAdd$value[iRow] <- dtAdd$value[iRow] * tmp$factor
    dtAdd$units[iRow] <- p$unit
  }

  # Return the relevant columns for further processing
  return(dtAdd[, c("container Path", "parameter Name", "value", "units")])
}
#' Load or Create Sheet Data
#'
#' This function loads data from a specified sheet in an Excel workbook or creates an empty data.table if the sheet does not exist.
#'
#' @param wb An open workbook object.
#' @param sheetName A string representing the name of the sheet to load.
#'
#' @return A data.table containing the data from the specified sheet or an empty data.table if the sheet does not exist.
#' @keywords internal
loadOrCreateSheetData <- function(wb, sheetName) {
  if (sheetName %in% wb$sheet_names) {
    return(xlsxReadData(wb, sheetName = sheetName))
  } else {
    return(data.table())
  }
}
#' Save Data to Workbook
#'
#' This function saves a data.table to a specified sheet in an Excel workbook, creating the sheet from a template if it does not exist.
#'
#' @param wb An open workbook object.
#' @param sheetName A string representing the name of the sheet to save data to.
#' @param dt A data.table containing the data to be saved.
#' @param templateSheet A string representing the name of the template sheet to use if the sheet does not exist.
#' @param templateXlsx A string representing the name of the template Excel file.
#'
#' @return NULL
saveDataToWorkbook <- function(wb, sheetName, dt,templateSheet = 'template_Ind',
                               templateXlsx = "Individuals.xlsx") {
  if (sheetName %in% wb$sheet_names) {
    xlsxWriteData(wb = wb, sheetName = sheetName, dt = dt)
  } else {
    addDataAsTemplateToXlsx(wb = wb,
                            templateSheet = templateSheet,
                            sheetName = sheetName,
                            dtNewData = dt,
                            templateXlsx = templateXlsx)
  }
}
