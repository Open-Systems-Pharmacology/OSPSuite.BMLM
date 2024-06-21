mergeIndividualScenarios <- function(projectConfig) {
  wb <- openxlsx::loadWorkbook(projectConfig$scenarioDefinitionFile)
  dtScenario <- setDT(openxlsx::read.xlsx(wb, sheet = "Scenarios"))


  # get columns by which the scenarios should be split into groups,
  # all simulation of one group are merged to apopulation, they should differ only in parameters
  colsToGroup <- c(
    "ModelFile", "SimulationTime", "SimulationTimeUnit",
    "SteadyState", "SteadyStateTime", "SteadyStateTimeUnit"
  )
  colsToGroup <-
    sapply(colsToGroup, function(x) {
      ifelse(all(!is.na(dtScenario[[x]])), x, NA)
    })
  colsToGroup <- colsToGroup[!is.na(colsToGroup)]

  dtScenarioPop <- unique(copy(dtScenario) %>%
    dplyr::select(all_of(colsToGroup)))

  for (d in split(dtScenarioPop, seq(nrow(dtScenarioPop)))) {
    dtScenarioG <- merge(dtScenario, d, by = colsToGroup)



    buildGroupPopulation(
      projectConfig = projectConfig,
      dtScenarioG = dtScenarioG,
      dataG = dataG
    )
  }
}


#' Title
#'
#' @param projectConfig
#' @param dtScenarioG
#' @param dataG
#'
#' @return
#' @export
#'
#' @examples
buildGroupPopulation <- function(projectConfig, dtScenarioG, dataG) {
  modelFile <- unique(dtScenarioG$ModelFile)
  checkmate::assertCharacter(modelFile, len = 1)
  checkmate::assertFileExists(file.path(projectConfig$modelFolder, modelFile))

  sim <- loadSimulation(filePath = file.path(file.path(projectConfig$modelFolder, modelFile)))


  # Read parameters
  # paramsModel <-
  #   getAllParameterForSheets(dtScenarioG = dtScenarioG,
  #                            sheetsColumn = 'ModelParameterSheets',
  #                            paramsXLSpath = projectConfig$paramsFile,
  #                            settingorder = 1)
  # paramsInd <-
  #   getAllParameterForSheets(sheets = unique(dtScenarioG$IndividualId),
  #                            paramsXLSpath = projectConfig$individualsFile,
  #                            sim = sim)
  # paramsApplication <-
  #   getAllParameterForSheets(sheets = unique(dtScenarioG$ApplicationProtocol),
  #                            paramsXLSpath = projectConfig$scenarioApplicationsFile,
  #                            sim = sim)
  #
  # # get default values for each parameter
  # defaultLine <- getDefaultPopulationLine(projectConfig = projectConfig,
  #                          params = c(paramsInd,paramsModel,paramsApplication),
  #                          modelFile = modelFile)
  #
  # for (sc in split(dtScenarioG, seq(nrow(dtScenarioG)))){
  #
  #   scParameters = list()
  #   tmp = rbind(defaultLine,
  #               rbindlist(paramsModel[sc$ModelParameterSheets]),
  #               rbindlist(paramsInd[sc$IndividualId]),
  #               rbindlist(paramsApplication[sc$ApplicationProtocol]))
  #   tmp <- tmp[!duplicated(tmp$paths,fromLast = TRUE)]
  #
  #
  # }
}

#' Title
#'
#' @param projectConfig
#' @param params
#' @param modelFile
#'
#' @return
#' @export
#'
#' @examples
getDefaultPopulationLine <- function(projectConfig, params, modelFile) {
  # parPaths = $paths %>%
  #   unique()
  #
  # parList <- getAllParametersMatching(parPaths,sim)
  #
  # values <- lapply(parList,getElement,'value')
  # names(values) <- lapply(parList,getElement,'path')
  #
  # defaultLine <- as.data.table(t(values %>%  unlist()))
  #
  # return(defaultLine)
}


#' Title
#'
#' @param sheets
#' @param paramsXLSpath
#'
#' @return
#' @export
#'
#' @examples
getAllParameterForSheets <- function(sheets, paramsXLSpath, sim) {
  sheets <- trimws(sheets)
  sheets <- sheets[!is.na(sheets) & sheets != ""]

  if (length(sheets) == 0) {
    return(list())
  }
  #
  # params <- lapply(sheets,
  #                       function(sheet){
  #                         params <- readParametersFromXLS(
  #                           paramsXLSpath = paramsXLSpath,
  #                           sheets = sheet) %>%
  #                           as.data.table()
  #                         values <- lapply(split(params,seq_len(nrow(params))), function(param){
  #                           p <- getParameter(param$paths,sim)
  #                           v = toBaseUnit(p$dimension,
  #                                                          values = param$values,
  #                                                          unit = param$units)
  #                           return(v)
  #                         })
  #                         names(values) <- params$paths
  #
  #                         return(as.data.table(t(values %>%  unlist())))
  #
  #
  #                       })
  # names(params) = sheets

  return(params)
}
