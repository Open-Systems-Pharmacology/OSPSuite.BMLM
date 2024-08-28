## projectConfiguration ------------------

#' Title
#'
#' @param projectPath
#' @param snaphsotFile
#' @param PIName
#' @param modelFolder
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
createBMLMProjectConfiguration <- function(projectConfigurationFilePath,
                                                  bMLMConfigurationFile = 'BMLMConfiguration.xlsx') {

  projectConfiguration <-
    ProjectConfigurationBMLM$new(projectConfigurationFilePath = projectConfigurationFilePath)

  if (!file.exists(file.path(projectConfiguration$paramsFolder,bMLMConfigurationFile))){
    invisible(file.copy(from = system.file("templates", "BMLMConfiguration.xlsx",package = "ospsuite.bmlm"),
                        to = file.path(projectConfiguration$paramsFolder,bMLMConfigurationFile)))
  }

  projectConfiguration$BMLMConfigurationFile <- bMLMConfigurationFile

  return(projectConfiguration)
}




## read snapshot -------------------



#' Title
#'
#' @param snaphsotFile
#' @param PIName
#'
#' @return
#' @export
#'
#' @examples
readIdentificationParameterFromSnapshot <- function(snaphsotFile,
                                                    PIName,
                                                    projectConfiguration,
                                                    overwrite = FALSE) {
  checkmate::assertFileExists(fs::path_abs(snaphsotFile))

  snp <- jsonlite::fromJSON(fs::path_abs(snaphsotFile))
  checkmate::assertChoice(PIName, snp$ParameterIdentifications$Name)

  # load xls as workbook to conserve formating an data validation
  bMLMConfigurationFile <- file.path(projectConfiguration$BMLMConfigurationFile)
  checkmate::assertFileExists(fs::path_abs(bMLMConfigurationFile))
  wb <- openxlsx::loadWorkbook(bMLMConfigurationFile)


  # get Headers
  definitionDTHeader <- xlsxReadData(wb = wb,sheetName = BMLMSHEET$ParameterDefinition)

  # check if table already has entries
  if (nrow(definitionDTHeader) == 1 | overwrite) {
    selectedPI <- which(snp$ParameterIdentifications$Name == PIName)

    linkedParameter <- snp$ParameterIdentifications$IdentificationParameters[[selectedPI]]

    linkedParameterDT <- extractIdentificationParameter(
      linkedParameter = linkedParameter
    )

    if ('isFixed' %in% names(linkedParameterDT)){
      definitionDT <- linkedParameterDT[is.na(IsFixed) | IsFixed == FALSE ]
      definitionDT[, IsFixed := NULL]
    } else{
      definitionDT <- linkedParameterDT
    }

    if ('UseAsFactor' %in% names(definitionDT)){
      set(definitionDT, which(is.na(definitionDT$UseAsFactor)), "UseAsFactor", FALSE)
    } else {
      definitionDT[, UseAsFactor :=  FALSE]
    }

    definitionDT <- rbind(definitionDTHeader,
                                definitionDT,
                                fill = TRUE)

    xlsxWriteData(wb = wb, sheetName = BMLMSHEET$ParameterDefinition, dt = definitionDT)

    # add linked parameter
    dtMappedPaths <- rbind(xlsxReadData(wb = wb, sheetName = BMLMSHEET$ParameterMappedPaths),
                           extractMappedParameterPaths(linkedParameter),
                           fill = TRUE)

    xlsxWriteData(wb = wb, sheetName = BMLMSHEET$ParameterMappedPaths, dt = dtMappedPaths)

    # add outputMapping
    dtOutputMappings <- xlsxReadData(wb = wb,sheetName = BMLMSHEET$OutputDefinitions)

    dtOutputMappings <- extractOutputMappings(
      projectConfiguration = projectConfiguration,
      snp = snp,
      selectedPI = selectedPI,
      dtOutputMappingsHeader = dtOutputMappings
    )

    xlsxWriteData(wb = wb, sheetName = BMLMSHEET$OutputDefinitions, dt = dtOutputMappings)

    openxlsx::saveWorkbook(wb, projectConfiguration$BMLMConfigurationFile, overwrite = TRUE)


    # save fixedParameters as modelparameter
    if (any(linkedParameterDT$IsFixed)) {
      modelParameters <- extractFixedParameters(
        linkedParameterDT = linkedParameterDT,
        dtMappedPaths = dtMappedPaths
      )

      checkmate::assertFileExists(projectConfiguration$paramsFile)

      if (!(PIName %in% openxlsx::getSheetNames(projectConfiguration$paramsFile))) {
        wbP <- openxlsx::loadWorkbook(projectConfiguration$paramsFile)

        wbP <- xlsxCloneAndSet(wb = wbP, clonedSheet = "Template", sheetName = PIName,dt = modelParameters)

        openxlsx::saveWorkbook(wb = wb_P, projectConfiguration$paramsFile, overwrite = TRUE)
      } else {
        warning(paste("sheet", PIName, "exist already in", projectConfiguration$paramsFile))
      }
    }
  } else {
    warning(paste(projectConfiguration$BMLMConfigurationFile, "is already edited"))
  }
}


#' Title
#'
#' @param linkedParameter
#' @param definitionDTHeader
#'
#' @return
extractIdentificationParameter <- function(linkedParameter) {
  linkedParameterDT <-
    cbind(
      linkedParameter %>%
        dplyr::select(!c("Parameters", "LinkedParameters")) %>%
        setDT(),
      rbindlist(
        lapply(
          seq_len(nrow(linkedParameter)),
          function(iRow) {
            linkedParameter$Parameters[[iRow]] %>%
              tidyr::pivot_wider(names_from = Name, values_from = Value) %>%
              setDT() %>%
              setnames(
                old = c("Start value", "Start.value"),
                new = c("StartValue", "StartValue"),
                skip_absent = TRUE
              )
          }
        ),
        fill = TRUE
      )
    ) %>%
    dplyr::mutate(ValueMode = "global") %>%
    dplyr::mutate(Distribution = "uniform") %>%
    dplyr::mutate(HasHyperparameter = FALSE)
}



#' Title
#'
#' @param snp
#' @param selectedPI
#' @param headers
#'
#' @return
extractOutputMappings <- function(projectConfiguration,snp, selectedPI, dtOutputMappingsHeader) {


  dtOutputPathIds = getOutputPathIds(projectConfiguration)

  dtOutputMappings <- snp$ParameterIdentifications$OutputMappings[[selectedPI]]

  if (is.null(dtOutputMappings)){
    dtOutputMappings <- data.table(OutputPathId = dtOutputPathIds$outputPathId)
  } else {
    stop('compare to Plots.xslx')
    dtOutputMappings <- dtOutputMappings %>%
      setDT() %>%
      dplyr::select(c("Path", "Scaling")) %>%
      unique() %>%
      setnames(old = "Path", new = "OutputPath")

    dtOutputMappings[, OutputPath := replaceModelPath(OutputPath), by = "OutputPath"]

    dtOutputMappings <- dtOutputMappings[, .(
      ModelFile = paste(ModelFile, collapse = ", "),
      Scaling = unique(Scaling)[1]
    ),
    by = "OutputPath"
    ]


    dtOutputMappings$OutputPath <- factor(dtOutputMappings$OutputPath, levels = unique(dtOutputMappings$OutputPath))
    dtOutputMappings[, OutputPathId := paste("O", as.numeric(OutputPath), sep = "_"), by = "OutputPath"]

    dtOutputMappings[, ErrorModel := ifelse(Scaling == "Log", "relative", "absolute"),
                     by = "OutputPath"
    ]
    dtOutputMappings[, Scaling := NULL]

    dtOutputMappings[, ModelErrorId := paste0("sigma_", OutputPathId),
                     by = "OutputPath"
    ]

  }

  dtOutputMappings <-
    rbind(dtOutputMappingsHeader, dtOutputMappings, fill = TRUE)

  return(dtOutputMappings)
}



#' Title
#'
#' @param linkedParameterDT
#' @param dtMappedPaths
#'
#' @return
extractFixedParameters <- function(linkedParameterDT, dtMappedPaths) {
  setDT(dtMappedPaths)

  getContainerPath <- function(pathName) {
    x <- strsplit(x = pathName, split = "\\|")[[1]]
    return(paste(x[seq(2, length(x) - 1)], collapse = "|"))
  }
  dtMappedPaths[, ("Container Path") := getContainerPath(pathName = LinkedParameters),
                by = "LinkedParameters"
  ]
  dtMappedPaths[, ("Parameter Name") := tail(strsplit(x = LinkedParameters, split = "\\|")[[1]], 1),
                by = "LinkedParameters"
  ]

  dtMappedPaths <- dtMappedPaths %>%
    dplyr::select(c("Name", "Container Path", "Parameter Name")) %>%
    unique()

  modelParameters <- dtMappedPaths %>%
    merge(
      linkedParameterDT[IsFixed == TRUE] %>%
        dplyr::select(c("Name", "StartValue", "Unit")) %>%
        setnames(
          old = c("StartValue", "Unit"),
          new = c("Value", "Units")
        ),
      by = "Name"
    ) %>%
    dplyr::mutate(Name = NULL)

  return(modelParameters)
}


#' Title
#'
#' @param linkedParameter
#'
#' @return
#' @export
#'
#' @examples
extractMappedParameterPaths <- function(linkedParameter) {
  dtMappedPaths <- tidyr::unnest(linkedParameter %>%
                                   dplyr::select(c("Name", "LinkedParameters")), "LinkedParameters") %>%
    setDT()


  dtMappedPaths[, LinkedParameters := replaceModelPath(LinkedParameters), by = LinkedParameters]
  dtMappedPaths <- unique(dtMappedPaths)

  return(dtMappedPaths)
}

## configuratePriors ---------------------


#' Title
#'
#' @param projectConfiguration
#' @param data
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
configuratePriors <- function(projectConfiguration, dataObserved, overwrite = FALSE) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$BMLMConfigurationFile)

  dtPrior <- xlsxReadData(wb = wb,sheetName = BMLMSHEET$Prior)
  if (overwrite & nrow(dtPrior) > 1) {
    dtPrior <- dtPrior[1]
  }


  dtDefinition <-
    xlsxReadData(wb = wb,sheetName = BMLMSHEET$ParameterDefinition,skipDescriptionRow = TRUE)

  checkmate::assertDataTable(
    x = dtDefinition,
    min.rows = 1,
    .var.name = BMLMSHEET$ParameterDefinition
  )
  checkmate::assertCharacter(
    dtDefinition$Name,
    unique = TRUE,
    any.missing = FALSE,
    .var.name = patst("column Name in ", BMLMSHEET$ParameterDefinition)
  )


  dtDefinition[, Distribution := translateDistribution(Distribution), by = 1:nrow(dtDefinition)]
  checkmate::assertNames(
    dtDefinition$Distribution %>% unique(),
    subset.of = getAllDistributions(),
    .var.name = paste("column Distribution in", BMLMSHEET$ParameterDefinition)
  )

  isEdited <- FALSE

  if (nrow(dtPrior) == 1) {
    dtPrior <- addGlobalPriorParameter(dtPrior, dtDefinition)

    dtPrior <- addHyperPriorParameter(dtPrior, dtDefinition)

    dtOutputMappings <-
      xlsxReadData(wb = wb,sheetName = BMLMSHEET$OutputDefinitions,skipDescriptionRow = TRUE)
    dtPrior <- addModelErrorParameter(dtPrior, dtOutputMappings)

    dtPrior[, Distribution := reTranslateDistribution(Distribution), by = 1:nrow(dtPrior)]

    xlsxWriteData(wb = wb, sheetName = BMLMSHEET$Prior, dt = dtPrior)
    isEdited <- TRUE
  } else {
    warning(paste(BMLMSHEET$Prior, "is already edited"))
  }

  dtStartValuesHeaders <- xlsxReadData(wb = wb,sheetName = BMLMSHEET$IndividualStartValues)
  if (overwrite & nrow(dtStartValuesHeaders) > 1) {
    dtStartValuesHeaders <- dtStartValuesHeaders[1]
  }


  if (nrow(dtStartValuesHeaders) == 0) {
    individualIds <- unique(dataObserved$individualId)

    tmp <- dtDefinition[ValueMode == PARAMETERTYPE$individual] %>%
      dplyr::select(c("Name", "Unit"))

    dtStartValues <- tmp[rep(seq_len(nrow(tmp)), each = length(individualIds)), ]
    dtStartValues[, IndividualId := rep(individualIds, times = nrow(tmp))]

    dtStartValues <- dtStartValues %>%
      addMissingColumns(names(dtStartValuesHeaders)) %>%
      dplyr::select(all_of(names(dtStartValuesHeaders)))

    dtStartValues <- rbind(dtStartValuesHeaders,dtStartValues,fill = TRUE)

    xlsxWriteData(wb = wb, sheetName = BMLMSHEET$IndividualStartValues, dt = dtStartValues)

    isEdited <- TRUE
  } else {
    warning("StartValue sheet is already edited")
  }

  if (isEdited) {
    openxlsx::saveWorkbook(wb = wb, projectConfiguration$BMLMConfigurationFile, overwrite = TRUE)
  }
}
#' Title
#'
#' @param dtPrior
#' @param dtDefinition
#'
#' @return
addGlobalPriorParameter <- function(dtPrior, dtDefinition) {
  tmp <- dtDefinition[ValueMode == PARAMETERTYPE$global] %>%
    setnames(
      old = c("MinValue", "MaxValue", "Unit"),
      new = c("P1_value", "P2_value", "Unit_of_distributed_Parameter")
    ) %>%
    dplyr::mutate(P1_type = "min") %>%
    dplyr::mutate(P2_type = "max") %>%
    dplyr::mutate(P3_type = "") %>%
    addMissingColumns(names(dtPrior))

  tmp[Scaling == "Log", Distribution := "unif_log"]
  tmp[Scaling == "Log", P1_type := "min_log"]
  tmp[Scaling == "Log", P2_type := "max_log"]
  tmp[Scaling == "Log", P1_value := log(P1_value)]
  tmp[Scaling == "Log", P2_value := log(P2_value)]
  tmp[Scaling == "Log", StartValue := log(StartValue)]


  dtPrior <- rbind(
    dtPrior,
    tmp %>%
      dplyr::select(names(dtPrior))
  )

  return(dtPrior)
}




#' Title
#'
#' @param dtPrior
#' @param dtDefinition
#'
#' @return
addHyperPriorParameter <- function(dtPrior, dtDefinition) {
  # Create a template row for dtPrior
  templateRow <- data.table(matrix(NA, nrow = 1, ncol = length(dtPrior)))
  setnames(templateRow, colnames(dtPrior))
  charCols <- names(dtPrior)[sapply(dtPrior, is.character)]
  templateRow[, (charCols) := ""]
  templateRow$ValueMode <- PARAMETERTYPE$hyperParameter
  templateRow$Distribution <- "unif"
  templateRow$HyperParameter <- ""
  templateRow$P1_type <- "min"
  templateRow$P2_type <- "max"


  for (iRow in which(dtDefinition$ValueMode == "individual")) {
    par <- dtDefinition[iRow, ]

    if (!is.na(par$CategoricCovariate) & par$CategoricCovariate != "") {
      subGroups <- dataObserved[[par$CategoricCovariate]] %>% unique()
    } else {
      subGroups <- ""
    }

    for (sG in subGroups) {
      for (parType in getDistributionParameters(par$Distribution)) {
        tmp <- copy(templateRow)
        tmp$Name <- par$Name
        tmp$CategoricCovariate <- sG
        tmp$HyperParameter <- parType
        tmp$Unit_of_distributed_Parameter <- par$Unit

        if (parType %in% c("mean", "min", "max")) {
          tmp$StartValue <- par$StartValue
          tmp$P1_value <- par$MinValue
          tmp$P2_value <- par$MaxValue
        }
        if (parType == "meanlog") {
          tmp$StartValue <- log(par$StartValue)
          tmp$P1_value <- log(par$MinValue)
          tmp$P2_value <- log(par$MaxValue)
        }
        if (parType %in% c("sd", "sdlog")) {
          tmp$P1_value <- 0
        }


        dtPrior <- rbind(
          dtPrior,
          tmp
        )
      }
    }
  }
  return(dtPrior)
}



#' Title
#'
#' @param dtPrior
#' @param dtOutputMappings
#'
#' @return
addModelErrorParameter <- function(dtPrior, dtOutputMappings) {
  # Create a template row for dtPrior
  tmp <- data.table(
    Name = dtOutputMappings$ModelErrorId %>% unique(),
    ValueMode = PARAMETERTYPE$outputError,
    Distribution = "unif",
    P1_type = "min",
    P1_value = 0,
    P2_type = "max"
  ) %>%
    addMissingColumns(headers = names(dtPrior))

  dtPrior <- rbind(dtPrior, tmp)
}





## configData ----------------

#' Title
#'
#' @template projectConfiguration
#' @param data
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
addDataIdsToDictionary <- function(projectConfiguration, observedData, overwrite = FALSE) {

  # load xls as workbook to conserve formating an data validation
  bMLMConfigurationFile <- file.path(projectConfiguration$BMLMConfigurationFile)
  checkmate::assertFileExists(fs::path_abs(bMLMConfigurationFile))
  wb <- openxlsx::loadWorkbook(bMLMConfigurationFile)

  checkmate::assertDataFrame(data, null.ok = FALSE)

  # dictionary
  dtDictionaryHeader <- xlsxReadData(wb = wb,sheetName = BMLMSHEET$Dictionary)

  dataIdCols <- c(
    # "OutputPathId",
    # "StudyId",
    # "DataGroupId",
    # "ModelFile",
    "Covariate"
  )

  checkmate::assertNames(
    names(dtDictionaryHeader),
    must.include = dataIdCols
  )

  isEdited <- FALSE

  if (any(!is.na(dtDictionaryHeader %>% dplyr::select(all_of(dataIdCols)))) & !overwrite) {
    warning("Dictionary is already edited")
  } else {
    dtOutputMappings <- xlsxReadData(wb = wb,sheetName = BMLMSHEET$OutputDefinitions)

    dict <- list(
      OutputPathId = unique(data$outputPathId),
      StudyId = unique(data$studyId),
      DataGroupId = unique(data$groupId),
      ModelFile = strsplit(x = paste(unique(dtOutputMappings$ModelFile), collapse = ","), split = ",") %>%
        unlist() %>%
        trimws() %>%
        unique(),
      Covariate =
        names(data)[unlist(lapply(data, attr, "columnType")) %in% c("covariate", "biometrics")]
    )

    for (col in setdiff(names(dtDictionaryHeader), names(dict))) {
      dict[[col]] <- dtDictionaryHeader[[col]]
    }


    maxLength <- max(lengths(dict))

    dict <- lapply(dict, function(x) c(x, rep(NA, maxLength - length(x))))

    dtDictionary <- as.data.table(dict)

    xlsxWriteData(wb = wb, sheetName = BMLMSHEET$Dictionary, dt = dtDictionary)
    isEdited <- TRUE
  }


  # # datagroups to model file
  # dtDataGroupHeaders <- xlsxReadData(wb = wb,sheetName = BMLMSHEET$DataGroupToModelFile)
  #
  # if (any(!is.na(dtDataGroupHeaders)) & !overwrite) {
  #   warning(paste(BMLMSHEET$DataGroupToModelFile, "is already edited"))
  # } else {
  #   dtDataGroup <- data %>%
  #     dplyr::select(c("studyId", "groupId")) %>%
  #     unique() %>%
  #     setnames(
  #       old = c("studyId", "groupId"),
  #       new = c("StudyId", "DataGroupId")
  #     ) %>%
  #     addMissingColumns(headers = names(dtDataGroupHeaders))
  #
  #   xlsxWriteData(wb = wb, sheetName = BMLMSHEET$DataGroupToModelFile, dt = dtDataGroup)
  #   isEdited <- TRUE
  # }

  if (isEdited) {
    openxlsx::saveWorkbook(wb = wb, projectConfiguration$BMLMConfigurationFile, overwrite = TRUE)
  }
}
## auxiliaries -----------------

#' Title
#'
#' @param dt
#' @param headers
#'
#' @return
addMissingColumns <- function(dt, headers) {
  missingColumns <- setdiff(headers, names(dt))
  if (length(missingColumns) > 0) {
    setDT(dt)
    set(dt, j = missingColumns, value = NA)
  }
  setcolorder(dt, unique(headers))
  return(dt)
}

#' Title
#'
#' @param pathName
#'
#' @return
#' @export
replaceModelPath <- function(pathName) {
  x <- strsplit(x = pathName, split = "\\|")[[1]]
  return(paste(c("*", x[seq(2, length(x))]), collapse = "|"))
}


