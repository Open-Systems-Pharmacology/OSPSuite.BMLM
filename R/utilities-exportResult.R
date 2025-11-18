#' Save Final Values to Tables
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


    dtInd <- dtList$startValues[sheetName == individualId,c("name","categoricCovariate","finalValue")]
    dtAdd <- addContainerAndParameterPath(dtExport = dtInd,
                                          dtMappedPaths = dtList$mappedPaths) %>%
      setnames(old = c('unit'),
               new = c('units'))

    scenarios <- unique(dtList$data[individualId == sheetName]$scenario)

    dtAdd <-
      dtAdd[,!c('scenarios'),with = FALSE] %>%
      melt(value.name = 'multiplicator',variable.name = 'scenario',measure.vars = scenarios)
    dtAdd <- unique(dtAdd[,!c('scenario'),with = FALSE])
    if (any(duplicated(dtAdd$linkedParameters))){
      stop(messages$errorExportAmbiguousValues())
    }
    dtAdd[useAsFactor == 1,value := value*multiplicator]

    for (iRow in which(dtAdd$useAsFactor == 1)){
      p <- getParameter(container = scenarioList[[scenarios[[1]]]]$simulation,path = dtAdd$linkedParameters[iRow])
      dtAdd$units[iRow] = p$unit
    }

    if (sheetName %in% wb$sheet_names) {
      dt <- xlsxReadData(wb, sheetName = sheetName)
    } else {
      return(data.table('container Path' =  character(),
                        'parameter Name' = character(),
                        value	= numeric(),
                        units	= character()
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
exportModelParametersToConfigTables <- function(projectConfiguration,dtList,sheetName,overwrite = FALSE){

  tmp <- dtList$prior[useAsFactor == TRUE,c("name")] %>%
    unique()
  if (nrow(tmp) > 0){
    warning(messages$warningFactorParametersNotExported(tmp$name))
  }

  dtExport = dtList$prior[valueMode != PARAMETERTYPE$outputError &
                            useAsFactor == FALSE]
  if (nrow(dtExport) == 0){
    message('no parameters to export')
    return(invisible())
  }
  wbMP <- openxlsx::loadWorkbook(projectConfiguration$modelParamsFile)
  wbPop <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)


  for (covariate in unique(dtExport$categoricCovariate)){
    exportSheets <- extractParameterValues(
      dtExport = dtExport[categoricCovariate == covariate],
      dtMappedPaths = dtList$mappedPaths[,c('name','linkedParameters')]
    )

    wbMP <-  addExportSheet(wb = wbMP,
                            dt = exportSheets$global,
                            covariate = covariate,
                            overwrite = overwrite,
                            suffix = 'global',
                            sheetName = sheetName)
    wbMP <-  addExportSheet(wb = wbMP,
                            dt = exportSheets$median,
                            covariate = covariate,
                            overwrite = overwrite,
                            suffix = 'median',
                            sheetName = sheetName)
    wbPop <-  addExportSheet(wb = wbPop,
                             dt = exportSheets$population,
                             covariate = covariate,
                             overwrite = overwrite,
                             suffix = '',
                             sheetName = sheetName,
                             toModelParameters = FALSE)
  }
  openxlsx::saveWorkbook(wb = wbMP, file = projectConfiguration$modelParamsFile, overwrite = TRUE)
  openxlsx::saveWorkbook(wb = wbPop, file = projectConfiguration$populationsFile, overwrite = TRUE)

}
# auxiliaries ------------
addExportSheet <- function(wb,dt,sheetName,covariate,overwrite,suffix, toModelParameters= TRUE){
  if (nrow(dt) == 0) return(wb)

  sheetNameParts = c(sheetName,covariate,suffix)
  sheetName <- paste(sheetNameParts[trimws(sheetNameParts) !=''],collapse = '_')

  if (sheetName %in% wb$sheet_names & !overwrite) {
    warning(messages$errorSheetAlreadyExists(sheetName))
    return(wb)
  }

  message(paste("export parameters to",sheetName))

  if (toModelParameters){
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
extractParameterValues <- function(dtExport, dtMappedPaths ) {

  exportSheets = list(global = data.table(),median = data.table(),population = data.table(),individuals = data.table())
  if (nrow(dtExport) == 0) return(exportSheets)

  dtExport <- addContainerAndParameterPath(dtExport = dtExport,dtMappedPaths = dtMappedPaths)

  # Rename 'unit' column to 'units' for consistency
  setnames(dtExport, old = c("unit"), new = c("units"))

  # filter global values
  exportSheets[['global']] <-
    dtExport[valueMode == PARAMETERTYPE$global,c("container Path", "parameter Name", "value", "units")]

  # evaluate hyperParameters
  dtExport <- dtExport[valueMode != PARAMETERTYPE$global,
                       c("container Path","parameter Name","name","units","hyperDistribution","hyperParameter","value")]
  dtExport[,index := seq(1,.N),by = c("container Path","parameter Name","name","units","hyperDistribution")]

  dtExport <- dcast(dtExport,
                      `container Path` + `parameter Name` + name + units + hyperDistribution   ~ index,
                      value.var = c("hyperParameter","value")) %>%
    setnames(old = c('name','hyperDistribution',paste("hyperParameter",seq(1,3),sep = '_'),paste("value",seq(1,3),sep = '_')),
             new = c('parameter Group','distribution',paste0('p',seq(1,3),'_type'),paste0('p',seq(1,3),'_value')),
             skip_absent = TRUE)

  exportSheets[['population']] <- copy(dtExport)

  dtExport[,value := apply(.SD, 1, calculateValueOfDistributionRow,value = 0.5,type = 'Q',log = FALSE)]
  exportSheets[['median']] <- dtExport[c("container Path", "parameter Name", "value", "units")]

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
' Add Container and Parameter Path
#'
#' This function merges a data.table containing parameter values with mapped paths to extract respective container and parameter names.
#'
#' @param dtExport A data.table containing identifier and parameter type of parameters to be merged with mapped paths.
#' @param dtMappedPaths A data.table containing mapped parameter paths.
#'
#' @return A data.table that includes the mapped container paths and parameter names.
#' @keywords internal
#' @noRd
addContainerAndParameterPath <- function(dtExport,dtMappedPaths){

  dtExport <- merge(dtMappedPaths, dtExport, by = "name")

  dtExport[, `container Path` := sapply(strsplit(linkedParameters, "\\|"), function(x) paste(x[-length(x)], collapse = "|"))]
  dtExport[, `parameter Name` := sapply(strsplit(linkedParameters, "\\|"), function(x) tail(x, n = 1))]

  return(dtExport)
}
