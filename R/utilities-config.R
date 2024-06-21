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
createBMLMProjectConfiguration <- function(projectPath = ".",
                                           modelFolder = NULL,
                                           overwrite = FALSE) {
  if (!is.null(modelFolder)) {
    checkmate::assertDirectoryExists(fs::path_abs(path = modelFolder, start = projectPath))
  }

  # initialization of project
  initProject(
    destination = projectPath,
    sourceFolder = system.file(
      "extdata",
      "TemplateProject",
      package = "ospsuite.bmlm",
      mustWork = TRUE
    ),
    overwrite = FALSE
  )


  projectConfig <-
    ProjectConfigurationBMLM$new(projectConfigurationFilePath = file.path(projectPath, "projectConfiguration.xlsx")) #nolint


  return(projectConfig)
}



#'
#' @description
#'
#' Creates the default project folder structure with excels file templates in
#' the working directory.
#'
#' @param destination A string defining the path where to initialize the project.
#' default to current working directory.
#' @inheritParams fs::dir_copy
#' @export
initProject <-
  function(destination = ".",
           overwrite = FALSE,
           sourceFolder) {
    destination <- fs::path_abs(destination)

    checkmate::assertDirectoryExists(sourceFolder)


    dirsToCopy <- fs::path_rel(path = list.dirs(file.path(sourceFolder)), start = sourceFolder)

    for (d in dirsToCopy) {
      if (!dir.exists(file.path(destination, d))) {
        dir.create(file.path(destination, d), recursive = TRUE, showWarnings = FALSE)
      }

      fileList <- fs::path_rel(path = fs::dir_ls(file.path(sourceFolder, d), type = "file"), start = sourceFolder)

      for (f in fileList) {
        if (!file.exists(file.path(destination, f))) {
          file.copy(
            from = file.path(sourceFolder, f),
            to = file.path(destination, f),
            overwrite = overwrite
          )
        }
      }
    }




    return(invisible())
  }



#' Title
#'
#' @param configurationfile
#''
#' @return
#' @export
#'
#' @examples
readProjectConfiguration <- function(configurationfile,
                                     properties = list()) {
  checkmate::assertFileExists(configurationfile)
  checkmate::assertList(properties)

  # read current configuration
  configFileContent <- esqlabsR::readExcel(path = configurationfile)

  projectConfig <- as.list(configFileContent$Value)
  names(projectConfig) <- configFileContent$Property

  # add new properties
  properties <- properties[unlist(lapply(properties, function(x) {
    !is.null(x)
  }))]
  if (length(properties) > 0) {
    projectConfig <- utils::modifyList(projectConfig, properties)

    newContent <- merge(configFileContent %>% dplyr::select(!Value),
      data.table(
        Property = names(projectConfig),
        Value = unlist(projectConfig)
      ),
      by = "Property",
      sort = FALSE
    ) %>%
      setcolorder(c("Property", "Value"))

    writeExcel(
      data = list(newContent),
      path = configurationfile
    )
  }

  projectConfigurationDirPath <- fs::path_abs(dirname(configurationfile))


  withinParams <- names(projectConfig) %in% c(
    "BMLMConfigurationFile",
    "paramsFile",
    "individualsFile",
    "scenarioDefinitionFile",
    "scenarioApplicationsFile",
    "dataDictionary"
  )

  projectConfig[withinParams] <-
    lapply(projectConfig[withinParams], function(x) {
      fs::path_abs(file.path(parent = file.path(projectConfigurationDirPath, projectConfig$paramsFolder), path = x))
    })

  projectConfig[!withinParams] <-
    lapply(projectConfig[!withinParams], function(x) {
      fs::path_abs(file.path(parent = projectConfigurationDirPath, path = x))
    })

  projectConfig[["projectConfigurationDirPath"]] <- projectConfigurationDirPath

  return(projectConfig)
}

## configPriors ---------------------


#' Title
#'
#' @param projectConfig
#' @param data
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
configPriors <- function(projectConfig, data, overwrite = FALSE) {
  wb <- openxlsx::loadWorkbook(projectConfig$BMLMConfigurationFile)

  dtPrior <- setDT(openxlsx::read.xlsx(wb, sheet = BMLMSHEET$Prior))
  if (overwrite & nrow(dtPrior) > 0) {
    dtPrior <- dtPrior[-seq(1, nrow(dtPrior))]
  }


  dtDefinition <-
    setDT(openxlsx::read.xlsx(wb, sheet = BMLMSHEET$ParameterDefinition))

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

  if (nrow(dtPrior) == 0) {
    dtPrior <- addGlobalPriorParameter(dtPrior, dtDefinition)

    dtPrior <- addHyperPriorParameter(dtPrior, dtDefinition)

    dtOutputMappings <- setDT(openxlsx::read.xlsx(wb, sheet = BMLMSHEET$OutputDefinitions))
    dtPrior <- addModelErrorParameter(dtPrior, dtOutputMappings)

    dtPrior[, Distribution := reTranslateDistribution(Distribution), by = 1:nrow(dtPrior)]

    openxlsx::writeData(wb = wb, sheet = BMLMSHEET$Prior, x = dtPrior)
    isEdited <- TRUE
  } else {
    warning(paste(BMLMSHEET$Prior, "is already edited"))
  }

  dtStartValuesHeaders <- setDT(openxlsx::read.xlsx(wb, sheet = BMLMSHEET$IndividualStartValues))
  if (overwrite & nrow(dtStartValuesHeaders) > 0) {
    dtStartValuesHeaders <- dtStartValuesHeaders[-seq(1, nrow(dtStartValuesHeaders))]
  }


  if (nrow(dtStartValuesHeaders) == 0) {
    individualIds <- unique(data$individualId)

    tmp <- dtDefinition[ValueMode == PARAMETERTYPE$individual] %>%
      dplyr::select(c("Name", "Unit"))

    dtStartValues <- tmp[rep(seq_len(nrow(tmp)), each = length(individualIds)), ]
    dtStartValues[, IndividualId := rep(individualIds, times = nrow(tmp))]

    dtStartValues <- dtStartValues %>%
      addMissingColumns(names(dtStartValuesHeaders)) %>%
      dplyr::select(all_of(names(dtStartValuesHeaders)))

    openxlsx::writeData(wb = wb, sheet = BMLMSHEET$IndividualStartValues, x = dtStartValues)
    isEdited <- TRUE
  } else {
    warning("StartValue sheet is already edited")
  }

  if (isEdited) {
    openxlsx::saveWorkbook(wb = wb, projectConfig$BMLMConfigurationFile, overwrite = TRUE)
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
      subGroups <- data[[par$CategoricCovariate]] %>% unique()
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
                                                    projectConfig,
                                                    overwrite = FALSE) {
  checkmate::assertFileExists(fs::path_abs(snaphsotFile))

  snp <- jsonlite::fromJSON(fs::path_abs(snaphsotFile))
  checkmate::assertChoice(PIName, snp$ParameterIdentifications$Name)

  # load xls as workbook to conserve formating an data validation
  pIDefinitionFile <- file.path(projectConfig$BMLMConfigurationFile)
  checkmate::assertFileExists(fs::path_abs(pIDefinitionFile))
  wb <- openxlsx::loadWorkbook(pIDefinitionFile)


  # getHeaders
  definitionDTHeader <- openxlsx::read.xlsx(wb, sheet = BMLMSHEET$ParameterDefinition)

  # check if table already has entries
  if (nrow(definitionDTHeader) == 0 | overwrite) {
    selectedPI <- which(snp$ParameterIdentifications$Name == PIName)


    linkedParameter <- snp$ParameterIdentifications$IdentificationParameters[[selectedPI]]

    linkedParameterDT <- extractIdentificationParameter(
      linkedParameter = linkedParameter,
      headers = names(definitionDTHeader)
    )

    if (!("IsFixed" %in% names(linkedParameterDT))) {
      linkedParameterDT[, IsFixed := FALSE]
    } else {
      linkedParameterDT[is.na(IsFixed), IsFixed := FALSE]
    }

    definitionDT <- linkedParameterDT[IsFixed == FALSE]
    definitionDT[, IsFixed := NULL]

    definitionDT[, UseAsFactor := ifelse(is.na(UseAsFactor), FALSE, TRUE)]

    openxlsx::writeData(wb, sheet = BMLMSHEET$ParameterDefinition, x = definitionDT)

    # add linked parameter
    dtMappedPaths <- extractMappedParameterPaths(linkedParameter)

    openxlsx::writeData(wb, sheet = BMLMSHEET$ParameterMappedPaths, x = dtMappedPaths)

    # add outputMapping
    OutputMappingsDTHeader <- openxlsx::read.xlsx(wb, sheet = BMLMSHEET$OutputDefinitions)

    dtOutputMappings <- extractOutputMappings(
      snp = snp,
      selectedPI = selectedPI,
      headers = names(OutputMappingsDTHeader)
    )

    openxlsx::writeData(wb, sheet = BMLMSHEET$OutputDefinitions, x = dtOutputMappings, )


    openxlsx::saveWorkbook(wb, projectConfig$BMLMConfigurationFile, overwrite = TRUE)


    # save fixedParameters as modelparameter
    if (any(linkedParameterDT$IsFixed)) {
      modelParameters <- extractFixedParameters(
        linkedParameterDT = linkedParameterDT,
        dtMappedPaths = dtMappedPaths
      )

      checkmate::assertFileExists(projectConfig$paramsFile)

      sheets <- openxlsx::getSheetNames(projectConfig$paramsFile)
      if (!(PIName %in% sheets)) {
        wb_P <- openxlsx::loadWorkbook(projectConfig$paramsFile)

        openxlsx::cloneWorksheet(wb = wb_P, clonedSheet = "Template", sheetName = PIName)

        openxlsx::writeData(wb = wb_P, sheet = PIName, x = modelParameters)

        openxlsx::saveWorkbook(wb = wb_P, projectConfig$paramsFile, overwrite = TRUE)
      } else {
        warning(paste("sheet", PIName, "exist already in", projectConfig$paramsFile))
      }
    }
  } else {
    warning(paste(projectConfig$BMLMConfigurationFile, "is already edited"))
  }
}


#' Title
#'
#' @param linkedParameter
#' @param headers
#'
#' @return
extractIdentificationParameter <- function(linkedParameter,
                                           headers) {
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
    dplyr::mutate(HasHyperparameter = FALSE) %>%
    addMissingColumns(headers)
}



#' Title
#'
#' @param snp
#' @param selectedPI
#' @param headers
#'
#' @return
extractOutputMappings <- function(snp, selectedPI, headers) {
  OutputMappings <- snp$ParameterIdentifications$OutputMappings[[selectedPI]] %>%
    setDT() %>%
    dplyr::select(c("Path", "Scaling")) %>%
    unique() %>%
    setnames(old = "Path", new = "OutputPath")

  OutputMappings[, ModelFile := paste0(strsplit(x = OutputPath, split = "\\|")[[1]][1], ".pkml"),
    by = "OutputPath"
  ]

  OutputMappings[, OutputPath := replaceModelPath(OutputPath), by = "OutputPath"]

  dtOutputMappings <- OutputMappings[, .(
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

  dtOutputMappings <- dtOutputMappings %>%
    addMissingColumns(headers)

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
  dtMappedPaths <- unnest(linkedParameter %>%
    dplyr::select(c("Name", "LinkedParameters")), "LinkedParameters") %>%
    setDT()


  dtMappedPaths[, LinkedParameters := replaceModelPath(LinkedParameters), by = LinkedParameters]
  dtMappedPaths <- unique(dtMappedPaths)

  return(dtMappedPaths)
}



## config scenarios ----------

#' Title
#'
#' @param projectConfig
#' @param data
#'
#' @return
#' @export
configScenarios <- function(projectConfig, data, PIName = "", overwrite = FALSE) {
  wb <- openxlsx::loadWorkbook(projectConfig$scenarioDefinitionFile)

  dtScenario <- setDT(openxlsx::read.xlsx(wb, sheet = "Scenarios"))


  if (nrow(dtScenario) == 0 | overwrite) {
    # get sheets of other excel files, which can be used for scenarios
    modelParameterSheet <- intersect(openxlsx::getSheetNames(file = projectConfig$paramsFile), PIName)
    if (length(modelParameterSheet) == 0) modelParameterSheet <- ""
    ApplicationProtocolSheets <- openxlsx::getSheetNames(file = projectConfig$scenarioApplicationsFile)

    wbO <- openxlsx::loadWorkbook(projectConfig$BMLMConfigurationFile)
    dtDataGroups <- setDT(openxlsx::read.xlsx(wbO, sheet = BMLMSHEET$DataGroupToModelFile))

    modelFilesperGroup <- dtDataGroups$ModelFile
    names(modelFilesperGroup) <- dtDataGroups$DataGroupId

    for (gId in as.character(unique(data$groupId))) {
      applicationProtocol <- intersect(ApplicationProtocolSheets, gId)
      if (length(applicationProtocol) == 0) applicationProtocol <- ""
      modelFile <- modelFilesperGroup[[gId]]


      for (indId in unique(data[groupId == gId]$individualId)) {
        dtScenario <- rbind(
          dtScenario,
          data.table(
            Scenario_name = paste("G", gId, "I", indId, sep = "_"),
            IndividualId = indId,
            ModelParameterSheets = modelParameterSheet,
            ApplicationProtocol = applicationProtocol,
            ModelFile = modelFile
          ),
          fill = TRUE
        )
      }
    }

    openxlsx::writeData(wb = wb, sheet = "Scenarios", x = dtScenario)

    openxlsx::saveWorkbook(wb = wb, projectConfig$scenarioDefinitionFile, overwrite = TRUE)
  } else {
    warning("Scenario sheet is already edited")
  }
}



## configData ----------------

addDataIdsToDictionary <- function(projectConfig, data, overwrite = FALSE) {
  # load xls as workbook to conserve formating an data validation
  pIDefinitionFile <- file.path(projectConfig$BMLMConfigurationFile)
  checkmate::assertFileExists(fs::path_abs(pIDefinitionFile))
  wb <- openxlsx::loadWorkbook(pIDefinitionFile)

  checkmate::assertDataFrame(data, null.ok = FALSE)

  # dictionary
  dtDictionaryHeader <- openxlsx::read.xlsx(wb, sheet = BMLMSHEET$Dictionary)

  dataIdCols <- c(
    "OutputPathId",
    "StudyId",
    "DataGroupId",
    "ModelFile",
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
    dtOutputMappings <- openxlsx::read.xlsx(wb, sheet = BMLMSHEET$OutputDefinitions)


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

    openxlsx::writeData(wb, sheet = BMLMSHEET$Dictionary, x = dtDictionary, )
    isEdited <- TRUE
  }


  # datagroups to model file
  dtDataGroupHeaders <- openxlsx::read.xlsx(wb, sheet = BMLMSHEET$DataGroupToModelFile)

  if (any(!is.na(dtDataGroupHeaders)) & !overwrite) {
    warning(paste(BMLMSHEET$DataGroupToModelFile, "is already edited"))
  } else {
    dtDataGroup <- data %>%
      dplyr::select(c("studyId", "groupId")) %>%
      unique() %>%
      setnames(
        old = c("studyId", "groupId"),
        new = c("StudyId", "DataGroupId")
      ) %>%
      addMissingColumns(headers = names(dtDataGroupHeaders))

    openxlsx::writeData(wb, sheet = BMLMSHEET$DataGroupToModelFile, x = dtDataGroup)
    isEdited <- TRUE
  }

  if (isEdited) {
    openxlsx::saveWorkbook(wb = wb, projectConfig$BMLMConfigurationFile, overwrite = TRUE)
  }
}


#' Title
#'
#' @param projectConfig
#' @param data
#'
#' @return
#' @export
#'
#' @examples
saveBiometricsToConfig <- function(projectConfig, data, overwrite = FALSE) {
  wb <- openxlsx::loadWorkbook(projectConfig$individualsFile)

  sheets <- openxlsx::getSheetNames(projectConfig$individualsFile)

  dtIndividualBiometrics <- setDT(openxlsx::read.xlsx(wb, sheet = "IndividualBiometrics", sep.names = " "))

  biometrics <-
    data %>%
    dplyr::select(
      c(
        "individualId",
        names(data)[unlist(lapply(data, attr, "columnType")) == "biometrics"]
      )
    ) %>%
    unique()

  for (col in names(biometrics)) {
    newName <- grep(col, names(dtIndividualBiometrics), ignore.case = TRUE, value = TRUE)
    if (newName != "") {
      setnames(biometrics, old = col, new = newName)
    }
  }

  if (!("Species" %in% names(biometrics))) biometrics[["Species"]] <- ospsuite::Species$Human
  biometrics[, Gender := toupper(Gender)]
  biometrics[, Gender := ifelse(Gender == "M", "MALE", Gender)]
  biometrics[, Gender := ifelse(Gender == 1, "MALE", Gender)]
  biometrics[, Gender := ifelse(Gender == "F", "FEMALE", Gender)]
  biometrics[, Gender := ifelse(Gender == 2, "FEMALE", Gender)]

  if (!("biometrics_overview" %in% sheets | overwrite)) {
    biometrics <-
      addMissingColumns(biometrics, names(dtIndividualBiometrics))

    openxlsx::cloneWorksheet(wb, sheetName = "biometrics_overview", clonedSheet = "IndividualBiometrics")
    openxlsx::writeData(wb = wb, sheet = "biometrics_overview", x = biometrics)
  } else {
    warning("biometrics_overview already edited")
  }

  # Create a progress bar
  pb <- utils::txtProgressBar(min = 1, max = nrow(biometrics), style = 3)


  for (indId in biometrics$IndividualId) {
    utils::setTxtProgressBar(pb, indId) # Update the progress bar


    if (!(indId %in% sheets) | overwrite) {
      if (!(indId %in% sheets)) openxlsx::cloneWorksheet(wb, sheetName = indId, clonedSheet = "template_Indiv1")

      biomForInd <- biometrics[IndividualId == indId, ]


      # Create ontogenies for the proteins
      moleculeOntogenies <- esqlabsR:::.readOntongeniesFromXLS(biomForInd)

      # Create the IndividualCharacteristics object
      individualCharacteristics <- ospsuite::createIndividualCharacteristics(
        species = biomForInd$Species, population = biomForInd$Population,
        gender = data$Gender,
        weight = biomForInd$`Weight [kg]`,
        height = biomForInd$`Height [cm]`,
        age = biomForInd$`Age [year(s)]`,
        moleculeOntogenies = moleculeOntogenies
      )

      ## copy from esqlab::writeIndividualToXLS bud does not write to xlsx

      individual <- createIndividual(individualCharacteristics)

      columnNames <- c("Container Path", "Parameter Name", "Value", "Units")

      containerPaths <- vector("character", length(individual$distributedParameters$paths))
      paramNames <- vector("character", length(individual$distributedParameters$paths))
      values <- vector("numeric", length(individual$distributedParameters$paths))
      units <- vector("character", length(individual$distributedParameters$paths))

      for (i in seq_along(individual$distributedParameters$paths)) {
        fullPathParts <- strsplit(individual$distributedParameters$paths[[i]], split = "|", fixed = TRUE)[[1]]
        containerPath <- paste(fullPathParts[seq_along(fullPathParts) - 1], collapse = "|")
        paramName <- fullPathParts[[length(fullPathParts)]]

        containerPaths[i] <- containerPath
        paramNames[i] <- paramName
        values[i] <- individual$distributedParameters$values[[i]]
        units[i] <- individual$distributedParameters$units[[i]]
      }

      output <- data.frame(
        unlist(containerPaths, use.names = FALSE),
        unlist(paramNames, use.names = FALSE),
        unlist(as.numeric(values), use.names = FALSE),
        unlist(units, use.names = FALSE)
      )
      colnames(output) <- columnNames


      openxlsx::writeData(wb = wb, sheet = indId, x = output)
    } else {
      warning(paste("Individual sheet for", indId, "already edited"))
    }
  }

  close(pb)

  openxlsx::saveWorkbook(wb, projectConfig$individualsFile, overwrite = TRUE)
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


## auf Halde ---------------

#' Title
#'
#' @param projectConfig
#' @param data
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
addDataToConfig <- function(projectConfig, data, overwrite = FALSE) {
  columnTypes <- lapply(data, function(x) {
    attributes(x)$columnType
  })

  biometrics <- data %>%
    dplyr::select(c("individualId", names(columnTypes)[columnTypes == "biometrics"])) %>%
    unique()


  addIndividualAndPopulations(projectConfig, biometrics)
  # generate biometric overview
  biometricsTemplate <- names(readExcel(projectConfig$individualsFile, "IndividualBiometrics"))


  exportData <- list(
    IndividualBiometrics = copy(biometrics) %>%
      setnames(
        old = names(biometrics),
        new = lapply(names(biometrics), function(x) {
          tmp <- grep(x,
            biometricsTemplate,
            value = TRUE,
            ignore.case = TRUE
          )
          ifelse(length(tmp) == 0, x, tmp)
        }) %>%
          unlist()
      ) %>%
      addMissingColumns(headers = biometricsTemplate)
  )

  unlist(lapply(names(data), function(x) {
    tmp <- grep(x,
      biometricsTemplate,
      value = TRUE,
      ignore.case = TRUE
    )
    ifelse(length(tmp) == 0, x, tmp)
  }))


  # generate individualParameters Tables
}
