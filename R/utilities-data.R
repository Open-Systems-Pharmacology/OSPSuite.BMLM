#' Title
#'
#'
#'
#' @param projectConfig
#'
#' @return
#' @export
#'
#' @examples
readDataByDictionary <- function(projectConfig) {
  checkmate::assertFileExists(projectConfig$dataImporterConfigurationFile)

  dataList <- readExcel(projectConfig$dataImporterConfigurationFile, sheet = "DataFiles")
  # delete description line
  dataList <- dataList[-1, ]
  dataList <- dataList[rowSums(is.na(dataList)) < ncol(dataList), ]


  data <- data.table()
  dict <- list()
  for (d in split(dataList, seq(nrow(dataList)))) {
    tmpData <- fread(fs::path_abs(
      start = projectConfig$projectConfigurationDirPath,
      path = d$DataFile
    ))

    tmpdict <- readDataDictionary(
      dictionaryFile = projectConfig$dataImporterConfigurationFile,
      sheet = d$Dictionary,
      data = tmpData
    )

    data <- rbind(data,
      convertDataByDictionary(
        data = tmpData,
        dataFilter = d$DataFilter,
        dict = tmpdict,
        dictionaryName = d$Dictionary
      ),
      fill = TRUE
    )

    # get unique dictionary for columnType
    tmpdict <-
      tmpdict %>%
      dplyr::select(c("targetColumn", "type")) %>%
      unique()

    dict <- utils::modifyList(
      dict,
      as.list(tmpdict$type) %>%
        setNames(tmpdict$targetColumn)
    )
  }

  # check data validity
  colIdentifier <- c("individualId", "groupId", "outputPathId", "time")
  if (any(duplicated(data %>%
    dplyr::select(dplyr::all_of(colIdentifier))))) {
    warning(paste("data is not unique in columns", paste(colIdentifier, collapse = ", ")))
  }
  for (col in setdiff(names(data), c("lloq", "dvUnit"))) {
    if (any(is.na(data[[col]]) | data[[col]] == "")) {
      warning(paste("data contains NAs or empty values in column", col))
      print(paste("empty entries in", col))
      print(data[is.na(get(col)) | get(col) == ""])
    }
  }

  # set weighting variable
  data[, weighting := 1]


  # add dictionary as attributes
  dict[["timeUnit"]] <- "timeprofile"
  for (dc in names(dict)) {
    setattr(data[[dc]], "columnType", dict[[dc]])
  }

  return(data)
}


#' Title
#'
#' @param dictionaryFile
#' @param sheet
#' @param data
#'
#' @return
#' @export
#' @examples
readDataDictionary <- function(dictionaryFile, sheet, data) {
  dict <- readExcel(dictionaryFile, sheet = sheet) %>%
    setDT()
  dict <- dict[-1, ]
  dict <- dict[rowSums(is.na(dict)) < ncol(dict), ]

  checkmate::assertNames(
    dict$targetColumn,
    must.include = c(
      "individualId",
      "groupId",
      "outputPathId",
      "time",
      "dv",
      "dvUnit"
    ), .var.name = paste("Check for missing targetColumns in  dictionary", sheet)
  )

  tmp <- dict[is.na(sourceColumn) & is.na(filter), ]
  if (nrow(tmp) > 0) {
    stop(paste("Either sourceColumn or Filter on sourceColumn has to be filled in dictionary", sheet))
  }

  checkmate::assertNames(
    x = dict[!is.na(sourceColumn)]$sourceColumn,
    subset.of = names(data)
  )




  return(dict)
}


#' Title
#'
#' @param data
#' @param dict
#'
#' @return
#' @export
#'
#' @examples
convertDataByDictionary <- function(data,
                                    dataFilter,
                                    dict,
                                    dictionaryName) {
  # execute dataFilter
  if (!is.na(dataFilter) & dataFilter != "") data <- data[eval(parse(text = dataFilter))]

  # execute all filters
  dictFilters <- dict[!is.na(filter)]

  for (myFilter in split(dictFilters, seq(nrow(dictFilters)))) {
    data[
      eval(parse(text = myFilter$filter)),
      (myFilter$targetColumn) := eval(parse(text = myFilter$filterValue))
    ]
  }

  # Rename columns to target columns
  dictColumns <- dict[!is.na(sourceColumn) & sourceColumn != targetColumn]

  checkmate::assertCharacter(
    dictColumns$targetColumn,
    unique = TRUE,
    .var.name = paste("target columns with source columnsof", dictionaryName)
  )

  # Create new columns for duplicated old names, do not use setnames as source columns may not be unique
  for (iRow in seq_len(nrow(dictColumns))) {
    data[, (dictColumns$targetColumn[iRow]) := data[[dictColumns$sourceColumn[iRow]]]]
  }

  # reduce to defined columns
  data <- data %>%
    dplyr::select(unique(dict$targetColumn))

  # add time unit and convert biometrics to appropriate columns
  data[, timeUnit := dict[targetColumn == "time"]$sourceUnit]

  biometricUnits <- list(
    age = "year(s)",
    weight = "kg",
    height = "cm"
  )

  for (col in intersect(names(biometricUnits), dict$targetColumn)) {
    unitFactor <- toUnit(
      quantityOrDimension = getDimensionForUnit(biometricUnits[[col]]),
      values = 1,
      targetUnit = biometricUnits[[col]],
      sourceUnit = dict[targetColumn == col]$sourceUnit[1]
    )

    data[, (col) := get(col) * unitFactor]
  }

  return(data)
}


#
#
# loadDataForMatch(projectConfig,
#                  scenarios,
#                  dataSheet ='randomData'){
#
#
#   simulatedScenariosResults <- runScenarios(
#     scenarios = scenarios
#   )
#
#   simulatedResultsDt = rbindlist(lapply(names(simulatedScenariosResults),
#                        function(x){
#                          ospsuite::simulationResultsToDataFrame(simulatedScenariosResults[[x]]$results) %>%
#                            dplyr::mutate(individualId = x) %>%
#                            dplyr::mutate(dataGroupId = scenarios[[x]]$scenarioConfiguration$dataGroupId) %>%
#                            setDT()
#                        }
#   ))
#
#   # get data in display Units and replace OutputPathId by corresponding path
#   data = readExcel(projectConfig$dataFile,sheet = dataSheet) %>%
#     setDT()
#   OutputPaths = readExcel(projectConfig$scenarioDefinitionFile,sheet = 'OutputPaths') %>%
#     setDT()
#
#   data <- merge(data,OutputPaths,by = 'OutputPathId') %>%
#     setnames(old = c('OutputPath','Subject Id','Group Id'),
#              new = c('paths','IndividualId','dataGroupId'))
#
#   # convert data to units of simulation results
#   checkmate::assertCharacter(unique(data$`Time unit`),len = 1)
#
#   timefactor = ospsuite::toUnit(values = 1,
#                    quantityOrDimension = simulatedResultsDt$TimeDimension[1],
#                    targetUnit = simulatedResultsDt$TimeUnit[1],
#                    sourceUnit = data$`Time unit`[1])
#
#   data[,Time:= Time*timefactor]
#
#   dvUnitfactor <- simulatedResultsDt %>%
#     select('unit','dimension','molWeight','paths') %>%
#     unique() %>%
#     dplyr::mutate(factor = NA) %>%
#     merge(data %>%
#             dplyr::select('paths','Measurement unit') %>%
#             unique(),
#           by = 'paths')
#
#   for  (iRow in seq_len(nrow(dvUnitfactor))){
#     dvUnitfactor$factor[iRow] <- ospsuite::toUnit(values = 1,
#                                            quantityOrDimension = dvUnitfactor$dimension[iRow],
#                                          targetUnit = dvUnitfactor$unit[iRow],
#                                          sourceUnit = dvUnitfactor$`Measurement unit`[iRow],
#                                          molWeight = dvUnitfactor$molWeight,
#                                          molWeightUnit = 'g/mol')
#   }
#
#   data <- merge(data,
#                 dvUnitfactor %>%
#                   dplyr::select(c('paths','factor')),
#                 by = 'paths',
#                 all = TRUE) %>%
#     .[,Measurement:=Measurement*factor]
#
#
#   result <- data[simulatedResultsDt, on = .(paths,IndividualId,dataGroupId),
#                      .(Time, Measurement, Time, simulationValues = approx(Time = Time, Measurement = simulationValues, xout = Time)$y)]
#
#
#
# }
#
