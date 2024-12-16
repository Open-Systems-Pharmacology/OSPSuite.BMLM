evaluateBMLMResult <- function(projectConfiguration,scenarioList,runName){

dtList <- loadListsForRun(projectConfiguration,runName)

invisible(lapply(names(scenarioList), function(scenarioName) {
  updateParameterValues(
    scenarioName = scenarioName,
    scenario = scenarioList[[scenarioName]],
    dtPrior = dtList$prior,
    dtStartValues = dtList$startValues,
    dtMappedPaths = dtList$mappedPaths
  )
}))

exportOptimizedPopulation(projectConfiguration, scenarioList,runName)

}

#' Load Lists for Run
#'
#' This function loads various CSV files from a specified output directory for a given name of a BLMLM run,
#' processes them into data.tables, and returns them as a list.
#'
#' @param projectConfiguration An object of class projectConfiguration containing configuration settings, including
#'                             the output folder path where the CSV files are located.
#' @param runName A character string representing the name of the run, which is used to
#'                 construct the output directory path.
#'
#' @return A list of data.tables, each corresponding to one of the CSV files loaded. The
#'         list elements are named after the CSV files (without the '.csv' extension).
#'
#' @export
loadListsForRun <- function(projectConfiguration,runName){

  outputDir <- getOutputDirectoryForRun(projectConfiguration, runName)

  csvFiles <- c("data.csv","input.csv","mappedPaths.csv","prior.csv","startValues.csv")

  dtList = list()
  for (csvFile in csvFiles) {
    tmp <- data.table::fread(file.path(outputDir,csvFile))

    if ('individualId' %in% names(tmp)){
      tmp[,individualId := as.character(individualId)]
    }

    # add new column parameter as column name 'name' get confused in some applications
    if ('name' %in% names(tmp)){
      tmp[,parameter := name]
    }

    dtList[[gsub('.csv','',csvFile)]] <- tmp
  }


  return(dtList)

}


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

exportOptimizedPopulation <- function(projectConfiguration, scenarioList,runName) {
  #initialize variable to avoid linter message
  scenario_name <- populationId <- NULL #nolint camelCase, variable is derived by column name esqlabR

  for (scenarioName in names(scenarioList)) {
    ospsuite::exportPopulationToCSV(
      population = scenarioList[[scenarioName]]$population,
      filePath = file.path(
        projectConfiguration$populationsFolder,
        paste0(scenarioList[[scenarioName]]$scenarioConfiguration$populationId, '_',runName, ".csv")
      )
    )
  }

  wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  dt <- xlsxReadData(wb, sheetName = "Scenarios")

  dtSc <- dt[scenario_name %in% names(scenarioList)]
  dtSc[, scenario_name := paste0(scenario_name, '_',runName)]
  dtSc[, populationId := paste0(populationId, '_',runName)]

  dt <- dt[!(scenario_name %in% dtSc$scenario_name)]

  xlsxWriteData(wb, sheetName = "Scenarios", dt = rbind(dt, dtSc))

  openxlsx::saveWorkbook(wb = wb, file = projectConfiguration$scenariosFile, overwrite = TRUE)
}
