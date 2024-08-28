#' Title
#'
#' @param projectConfig
#'
#' @return
#' @export
#'
#' @examples
mergeIndividualScenarios <- function(projectConfig) {
  wb <- openxlsx::loadWorkbook(projectConfig$scenarioDefinitionFile)
  dtScenario <-  xlsxReadData(wb = wb,sheetName = "Scenarios")
  dtScenario <- dtScenario[is.na(PopulationId)]


  #Read all parameter sheets
  params <- mapply(
    function(sheet, file) {
      message(sheet)
      getAllParameterForSheets(
        projectConfig = projectConfig,
        sheets = cleanUpSheetList(dtScenario[[sheet]]),
        paramsXLSpath = file
      )
    },
    c(
      'ModelParameterSheets',
      'IndividualId',
      'ApplicationProtocol'
    ),
    c(
      projectConfig$paramsFile,
      projectConfig$individualsFile,
      projectConfig$scenarioApplicationsFile
    ),
    SIMPLIFY = FALSE
  )


  dtScenarioPop = getPopulationGroups(dtScenario[is.na(PopulationId)])

  popTableMatch = data.table()

  for (dPop in split(dtScenarioPop, seq(nrow(dtScenarioPop)))) {
    dtScenarioG <- merge(dtScenario[is.na(PopulationId)],
                         dPop %>% dplyr::select(all_of(attr(dtScenarioPop,'colsToGroup'))),
                         by = attr(dtScenarioPop,'colsToGroup'))

    poptable <- buildGroupPopulation(projectConfig = projectConfig,
                                     params = params,
                                     dtScenarioG = dtScenarioG
    )


    #"IndividualId","Gender","Population","Organism|Weight","Organism|BMI","Organism|BSA","Organism|Age","Organism|Gestational age","Organism|Height"

    # a population need a numeric individualId
    poptable[,IndividualId := .I-1]
    setcolorder(poptable,'IndividualId')

    popTableMatch <- rbind(popTableMatch,
                           poptable[, c('Scenario_name', 'IndividualId')] %>%
                             setnames(old = 'IndividualId', new = 'PopIndividualId') %>%
                             dplyr::mutate(PopulationId = dPop$PopulationId))

    write.csv(x = poptable,
              file = file.path(
                projectConfig$paramsFolder,
                'Populations',
                paste0(dPop$PopulationId, '.csv')
              ),
              fileEncoding = 'UTF8',
              row.names = FALSE
              )


  }

  xlsxWriteData(wb = wb, sheetName = "Scenarios", dt =  rbind(dtScenario,
                                                              dtScenarioPop))


  dtScenarioDataMatch <- xlsxReadData(wb,'MatchingTable') %>%
    merge(popTableMatch,
          by = 'Scenario_name',
          all = TRUE)

  xlsxWriteData(wb = wb, sheetName = "MatchingTable", dt =  dtScenarioDataMatch)


  openxlsx::saveWorkbook(wb, projectConfig$scenarioDefinitionFile, overwrite = TRUE)


  return(invisible())

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
buildGroupPopulation <- function(projectConfig,
                                 params,
                                 dtScenarioG) {


  defaultLine <- getDefaultPopulationLine(projectConfig = projectConfig,
                                         dtScenarioG = dtScenarioG,
                                         params = params)


  poptable = data.table()

  for (d in split(dtScenarioG,seq_len(nrow(dtScenarioG)))){

     popRow =  copy(defaultLine)

     for (parType in names(params)){

        sheets = cleanUpSheetList(d[[parType]])

        for (sheet in sheets){

          newValues = params[[parType]][[sheet]]

          popRow <- merge(popRow,
                          newValues,
                          by = 'paths',
                          all.x = TRUE,
                          suffixes = c('','.new'),
                          sort = FALSE)

          popRow[!is.na(values.new) &
                   units.new != units,
                 values.new := ospsuite::toBaseUnit(quantityOrDimension = dimensions,
                                                                          values = values.new,
                                                                          unit = units.new)]
          popRow[!is.na(values.new),values := values.new]

          popRow <- popRow %>%
            dplyr::select(all_of(names(defaultLine)))

        }

     }

     poptable <- rbind(poptable,
                       popRow %>%
                         dplyr::select(all_of(c('columnName','values'))) %>%
                         tidyr::pivot_wider(names_from = 'columnName',
                                            values_from = 'values') %>%
                         dplyr::mutate(IndividualID = d$index) %>%
                         dplyr::mutate(Scenario_name = d$Scenario_name) %>%
                         setDT()
     )



  }

  return(poptable)

}

#' Title
#'
#' @param projectConfig
#' @param dtScenarioG
#' @param params
#'
#' @return
getDefaultPopulationLine <- function(projectConfig,dtScenarioG, params) {

  # get default Values
  defaultLine <-
    rbindlist(lapply(names(params),
                     function(x){params[[x]][cleanUpSheetList(dtScenarioG[[x]])] %>%
                         rbindlist() %>%
                         dplyr::select(any_of('paths')) %>%
                         unique() })) %>%
    unique()

  modelFile <- unique(dtScenarioG$ModelFile)
  checkmate::assertCharacter(modelFile, len = 1)
  checkmate::assertFileExists(file.path(projectConfig$modelFolder, modelFile))

  sim <- loadSimulation(filePath = file.path(file.path(projectConfig$modelFolder, modelFile)))


  defaultLine[,c("values","units","dimensions") :=
              {p <- getParameter(paths,container = sim)
              list(value = p$value,
                   unit = p$unit,
                   dimensions = p$dimension)},
            by = seq_len(nrow(defaultLine))]

  #defaultLine[,columnName := ifelse(units != '',paste0(paths,' [',units,']'),paths)]
  defaultLine[,columnName := paths]


  return(defaultLine)

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
getAllParameterForSheets <- function(projectConfig,sheets,paramsXLSpath) {


  if (length(sheets) == 0) {
    return(list())
  }

  params = lapply(sheets,function(sheet){readParametersFromXLS(
    paramsXLSpath = paramsXLSpath,
    sheets = sheet) %>%
    as.data.table() })

  names(params) <- sheets

  return(params)
}



#' Title
#'
#' @param sheets
#'
#' @return
cleanUpSheetList<- function(sheets){
  sheet <- unique(sheets)
  sheets <- unlist(strsplit(sheets,','))
  sheets <- trimws(sheets)
  sheets <- sheets[!is.na(sheets) & sheets != ""]

  return(sheets)
}

#' Title
#'
#' @param dtScenario
#'
#' @return
#' @export
#'
#' @examples
getPopulationGroups <- function(dtScenario){

  # get columns by which the scenarios should be split into groups,
  # all simulation of one group are merged to a population, they should differ only in parameters
  colsToGroup <- c(
    "ModelFile", "SimulationTime", "SimulationTimeUnit",
    "SteadyState", "SteadyStateTime", "SteadyStateTimeUnit"
  )
  colsToGroup <-
    sapply(colsToGroup, function(x) {
      ifelse(all(!is.na(dtScenario[[x]])), x, NA)
    })
  colsToGroup <- colsToGroup[!is.na(colsToGroup)] %>%
    unname()

  dtScenarioPop <- unique(copy(dtScenario) %>%
                            dplyr::select(all_of(colsToGroup)))

  # scale group columns for filter
  setattr(dtScenarioPop,'colsToGroup',colsToGroup)

  # add columns
  dtScenarioPop[,Scenario_name := paste0('GroupScenario',.I)]
  dtScenarioPop[,PopulationId := Scenario_name]
  dtScenarioPop[,ReadPopulationFromCSV := TRUE]

  dtScenarioPop <- addMissingColumns(dtScenarioPop,names(dtScenario))



  return(dtScenarioPop)
}
