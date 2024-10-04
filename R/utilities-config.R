## projectConfiguration ------------------

#' Create BMLM Project Configuration
#'
#' This function creates and initializes a project configuration for BMLM (Biologically-Based Modeling and Learning Model).
#' It sets up the necessary configuration table and updates parameters based on a provided snapshot file.
#'
#' @param projectConfiguration An object of class `ProjectConfiguration`, which contains configuration details for the project.
#' This object should include paths to necessary folders and files for the BMLM project.
#'
#' @param bMLMConfigurationFile A string representing the name of the BMLM configuration file.
#' Default is "BMLMConfiguration.xlsx". This file contains various settings and parameters for the BMLM model.
#'
#' @param snapshotFile A string representing the path to the snapshot file. This file contains parameter identifications
#' that will be read and potentially used to update the project configuration. If NULL, no updates from a snapshot will occur.
#'
#' @param nameOfParameterIdentfication A string representing the name of the parameter identification to be read from the snapshot.
#' This should match one of the names specified in the snapshot file. If NULL, the function will not attempt to read any parameters.
#'
#' @param overwrite A logical indicating whether to overwrite existing data in the configuration file if it already exists.
#' Default is FALSE, meaning existing data will not be overwritten unless specified.
#'
#' @return An object of class `ProjectConfigurationBMLM`, which includes the updated configuration details after processing.
#'
#' @export
#'
addBMLMPConfiguration <- function(projectConfiguration,
                                  bMLMConfigurationFile = "BMLMConfiguration.xlsx",
                                  snapshotFile = NULL,
                                  nameOfParameterIdentfication = NULL,
                                  overwrite = FALSE) {
  projectConfiguration <-
    ProjectConfigurationBMLM$new(
      projectConfigurationFilePath = projectConfiguration$projectConfigurationFilePath) # nolint identation

  if (!file.exists(file.path(projectConfiguration$configurationsFolder, bMLMConfigurationFile))) {
    invisible(file.copy(
      from = system.file("templates", "BMLMConfiguration.xlsx", package = "ospsuite.bmlm"),
      to = file.path(projectConfiguration$configurationsFolder, bMLMConfigurationFile)
    ))
  }

  projectConfiguration$BMLMConfigurationFile <- bMLMConfigurationFile


  # Update DataGroups and Outputs in Plot configuration sheet
  if (!is.null(snapshotFile) && !is.null(nameOfParameterIdentfication)) {
    readIdentificationParameterFromSnapshot(
      snapshotFile = file.path(projectConfiguration$modelFolder, "DrugX.json"),
      nameOfParameterIdentfication = nameOfParameterIdentfication,
      projectConfiguration = projectConfiguration,
      overwrite = overwrite
    )
  }

  return(projectConfiguration)
}

## read snapshot -------------------

#' Read Identification Parameter From Snapshot
#'
#' This function reads specified identification parameters from a snapshot file and updates the BMLM project configuration accordingly.
#' It performs checks to ensure the validity of the snapshot and the specified parameter identification.
#'
#' @param snapshotFile A string representing the path to the snapshot file. This file should be in JSON format and contain
#' parameter identifications that can be extracted and used to update the project configuration.
#'
#' @param nameOfParameterIdentfication A string representing the name of the parameter identification to be read from the snapshot.
#' This should correspond to a valid entry under `ParameterIdentifications` in the snapshot file.
#'
#' @param projectConfiguration An object of class `ProjectConfigurationBMLM`, which contains configuration details for the project.
#' This object will be modified to incorporate the new identification parameters extracted from the snapshot.
#'
#' @param overwrite A logical indicating whether to overwrite existing data in the ParameterDefinition sheet of the configuration file.
#' Default is FALSE. If TRUE, existing data will be replaced with the new values from the snapshot.
#'
#' @return NULL (invisible), as the function performs side effects by modifying the configuration file directly.
#'
#' @export
#'
readIdentificationParameterFromSnapshot <- function(snapshotFile,
                                                    nameOfParameterIdentfication,
                                                    projectConfiguration,
                                                    overwrite = FALSE) {
  bMLMConfigurationFile <- file.path(projectConfiguration$BMLMConfigurationFile)
  checkmate::assertFileExists(fs::path_abs(bMLMConfigurationFile))
  wb <- openxlsx::loadWorkbook(bMLMConfigurationFile)

  definitionDTHeader <- xlsxReadData(wb = wb, sheetName = "ParameterDefinition") # nolint

  if (nrow(definitionDTHeader) == 1 || overwrite) {
    checkmate::assertFileExists(fs::path_abs(snapshotFile))
    snp <- jsonlite::fromJSON(fs::path_abs(snapshotFile))
    checkmate::assertChoice(nameOfParameterIdentfication, snp$ParameterIdentifications$Name)

    selectedPI <- which(snp$ParameterIdentifications$Name == nameOfParameterIdentfication)

    linkedParameter <- snp$ParameterIdentifications$IdentificationParameters[[selectedPI]]
    linkedParameterDT <- extractIdentificationParameter(linkedParameter)

    updateDefinitionDT(
      linkedParameterDT = linkedParameterDT,
      definitionDTHeader = definitionDTHeader,
      wb = wb
    )
    updateMappedPaths(
      wb = wb,
      linkedParameter = linkedParameter,
      projectConfiguration = projectConfiguration
    )
    updateOutputMappings(
      projectConfiguration = projectConfiguration,
      snp = snp,
      selectedPI = selectedPI,
      wb = wb
    )

    openxlsx::saveWorkbook(wb, projectConfiguration$BMLMConfigurationFile, overwrite = TRUE)

    if (any(linkedParameterDT$isFixed)) {
      updateFixedParameters(
        linkedParameterDT = linkedParameterDT, # nolint identation
        projectConfiguration = projectConfiguration,
        nameOfParameterIdentfication = nameOfParameterIdentfication
      )
    }

    message("Add snapshot to Configurationfile")
  }

  return(invisible())
}

#' Update Definition Data Table
#'
#' This helper function prepares and updates the definition data table by filtering and modifying the linked parameter data.
#'
#' @param linkedParameterDT A data.table containing linked parameters, including their fixed status and values.
#' @param definitionDTHeader A data.table containing the header for the definition data.
#' @param wb A workbook object to write the updated data table.
#'
#' @return NULL (invisible), as the function performs side effects by writing to a workbook.
#' @keywords internal
updateDefinitionDT <- function(linkedParameterDT, definitionDTHeader, wb) {
  # Initialize variables to NULL to avoid linter messages
  isFixed <- useAsFactor <- NULL

  if ("isFixed" %in% names(linkedParameterDT)) {
    definitionDT[, isFixed := as.logical(isFixed)]
    definitionDT <- linkedParameterDT[is.na(isFixed) | isFixed == FALSE]
    definitionDT[, isFixed := NULL]
  } else {
    definitionDT <- linkedParameterDT
  }

  if ("useAsFactor" %in% names(definitionDT)) {
    set(definitionDT, which(is.na(definitionDT$useAsFactor)), "useAsFactor", 0)
  } else {
    definitionDT$useAsFactor <- 0
  }
  definitionDT[, useAsFactor := as.numeric(useAsFactor)]


  xlsxWriteData(
    wb = wb,
    sheetName = "ParameterDefinition",
    dt = rbind(definitionDTHeader, definitionDT, fill = TRUE)
  )

  return(invisible(NULL))
}

#' Update Mapped Paths
#'
#' This helper function updates the mapped paths in the workbook based on the linked parameters.
#'
#' @param wb The workbook object to be updated.
#' @param linkedParameter A list of linked parameters.
#' @param projectConfiguration An object of class `ProjectConfigurationBMLM`, which contains configuration details for the project.
#'
#' @keywords internal
updateMappedPaths <- function(wb, linkedParameter, projectConfiguration) {
  # Initialize variables to NULL to avoid linter messages
  LinkedParameters <- NULL # nolint object_name

  dtMappedPaths <- tidyr::unnest(linkedParameter %>%
    dplyr::select(c("Name", "LinkedParameters")), "LinkedParameters") %>%  # nolint identation
    data.table::setDT()

  replaceModelPath <- function(pathName) {
    x <- strsplit(x = pathName, split = "\\|")[[1]]
    return(paste(x[seq(2, length(x))], collapse = "|"))
  }

  dtMappedPaths[, LinkedParameters := replaceModelPath(LinkedParameters), by = LinkedParameters]
  dtMappedPaths <- unique(dtMappedPaths)

  dtMappedPaths <-
    rbind(xlsxReadData(wb = wb, sheetName = "ParameterMappedPaths", convertHeaders = FALSE), # nolint identation
      dtMappedPaths, # nolint identation
      fill = TRUE)

  xlsxWriteData(wb = wb, sheetName = "ParameterMappedPaths", dt = dtMappedPaths) # nolint

  return(invisible())
}

#' Update Output Mappings
#'
#' This helper function updates the output mappings in the workbook based on the project configuration and snapshot.
#'
#' @param projectConfiguration An object of class `ProjectConfigurationBMLM`, which contains configuration details for the project.
#' @param snp A list representing the snapshot data.
#' @param selectedPI An integer representing the selected parameter identification index.
#' @param wb The workbook object to be updated.
#'
#' @keywords internal
updateOutputMappings <- function(projectConfiguration, snp, selectedPI, wb) {
  # Initialize variables to NULL to avoid linter messages
  outputPath <- errorModel <- scaling <- modelErrorId <- outputPathId <- NULL

  dtOutputPathIds <- getOutputPathIds(projectConfiguration)

  dtOutputMappings <- snp$ParameterIdentifications$OutputMappings[[selectedPI]]

  if (is.null(dtOutputMappings)) {
    dtOutputMappings <- data.table(
      outputPathId = dtOutputPathIds$outputPathId,
      scaling = "Log"
    )
  } else {
    stop("link per path to outputPathId")

    dtOutputMappings <- dtOutputMappings %>%
      setDT() %>%
      dplyr::select(c("path", "scaling")) %>%
      unique() %>%
      setnames(old = "path", new = "outputPath")

    dtOutputMappings[, outputPath := replaceModelPath(outputPath), by = "outputPath"]
  }

  dtOutputMappings[, errorModel := ifelse(scaling == "Log", "relative", "absolute"),
                   by = "outputPathId"] # nolint identation

  dtOutputMappings[, scaling := NULL]

  dtOutputMappings[, modelErrorId := paste0("sigma_", outputPathId),
                   by = "outputPathId"] # nolint identation

  dtOutputMappingsHeader <- xlsxReadData(wb = wb, sheetName = "ModelError")
  dtOutputMappings <-
    rbind(dtOutputMappingsHeader, dtOutputMappings, fill = TRUE)

  xlsxWriteData(wb = wb, sheetName = "ModelError", dt = dtOutputMappings) # nolint
}

#' Update Fixed Parameters
#'
#' This helper function updates fixed parameters in the project configuration by writing them to the appropriate sheet.
#'
#' @param linkedParameterDT A data.table containing linked parameters, including their fixed status and values.
#' @param projectConfiguration An object of class `ProjectConfigurationBMLM`, which contains configuration details for the project.
#' @param nameOfParameterIdentfication A string representing the name of the parameter identification.
#'
#' @keywords internal
updateFixedParameters <- function(linkedParameterDT, projectConfiguration, nameOfParameterIdentfication) {
  # Initialize variables to NULL to avoid linter messages
  linkedParameters <- isFixed <- NULL

  dtMappedPaths <- xlsxReadData(wb = projectConfiguration$BMLMConfigurationFile, sheetName = "ParameterMappedPaths")

  getContainerPath <- function(pathName) {
    x <- strsplit(x = pathName, split = "\\|")[[1]]
    return(paste(x[seq(2, length(x) - 1)], collapse = "|"))
  }

  dtMappedPaths[, ("container Path") := getContainerPath(pathName = linkedParameters), by = "linkedParameters"]
  dtMappedPaths[, ("parameter Name") := tail(strsplit(x = linkedParameters, split = "\\|")[[1]], 1), by = "linkedParameters"]

  dtMappedPaths <- dtMappedPaths %>%
    dplyr::select(c("name", "container Path", "parameter Name")) %>%
    unique()

  modelParameters <- dtMappedPaths %>%
    merge(
      linkedParameterDT[isFixed == TRUE] %>%
        dplyr::select(c("name", "startValue", "unit")) %>%
        setnames(
          old = c("startValue", "unit"),
          new = c("value", "units")
        ),
      by = "name"
    ) %>%
    dplyr::mutate(name = NULL)

  checkmate::assertFileExists(projectConfiguration$paramsFile)

  if (!(nameOfParameterIdentfication %in% openxlsx::getSheetNames(projectConfiguration$paramsFile))) {
    wbP <- openxlsx::loadWorkbook(projectConfiguration$paramsFile)
    wbP <- xlsxCloneAndSet(wb = wbP, clonedSheet = "Template", sheetName = nameOfParameterIdentfication, dt = modelParameters)
    openxlsx::saveWorkbook(wb = wbP, projectConfiguration$paramsFile, overwrite = TRUE)
  } else {
    warning(paste("Sheet", nameOfParameterIdentfication, "exists already in", projectConfiguration$paramsFile))
  }
}

#' Extract Identification Parameter
#'
#' This function extracts identification parameters from linked parameters.
#'
#' @param linkedParameter A list of linked parameters.
#'
#' @return A data.table containing the extracted parameters, including columns for parameter names, values, and their configurations.
#' @keywords internal
extractIdentificationParameter <- function(linkedParameter) {
  # Initialize variables to NULL to avoid linter messages
  Name <- Value <- NULL #nolint object_name
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
                new = c("startValue", "startValue"),
                skip_absent = TRUE
              )
          }
        ),
        fill = TRUE
      )
    ) %>%
    dplyr::mutate(valueMode = "global") %>%
    dplyr::mutate(distribution = "unif") %>%
    dplyr::mutate(hasHyperparameter = FALSE)

  data.table::setDT(linkedParameterDT)

  # nolint start
  data.table::setnames(linkedParameterDT,
                       sapply(names(linkedParameterDT),
                              function(x)
                                paste0(tolower(substring(x, 1, 1)), substring(x, 2))
                              )
                       )
  # nolint end

  return(linkedParameterDT)
}



## configuratePriors ---------------------

#' Configure Priors
#'
#' This function configures priors based on the project configuration and observed data.
#'
#' @param projectConfiguration An object of class `ProjectConfigurationBMLM`.
#' @param dataObserved A data.table of observed data.
#' @param overwrite A logical indicating whether to overwrite existing data. Default is FALSE.
#'
#' @return NULL (invisible).
#' @export
configurePriors <- function(projectConfiguration, dataObserved, overwrite = FALSE) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$BMLMConfigurationFile)
  dtPrior <- loadPriorData(wb, overwrite)
  dtDefinition <- xlsxReadData(wb = wb, sheetName = "ParameterDefinition", skipDescriptionRow = TRUE) # nolint

  validateParameterDefinition(dtDefinition)

  isEdited <- FALSE

  if (nrow(dtPrior) == 1 | overwrite) {
    dtPrior <- updatePriorParameters(dtPrior = dtPrior, dtDefinition = dtDefinition, wb = wb)
    isEdited <- TRUE
  } else {
    warning("sheet 'Prior' is already edited") # nolint
  }

  dtStartValuesHeaders <- loadStartValuesHeader(wb, overwrite)

  if (nrow(dtStartValuesHeaders) == 1 | overwrite) {
    dtStartValues <- createStartValues(dtDefinition, dataObserved)
    dtStartValues <- rbind(dtStartValuesHeaders, dtStartValues, fill = TRUE)

    xlsxWriteData(wb = wb, sheetName = "IndividualStartValues", dt = dtStartValues) # nolint
    isEdited <- TRUE
  } else {
    warning("StartValue sheet is already edited")
  }

  if (isEdited) {
    openxlsx::saveWorkbook(wb = wb, projectConfiguration$BMLMConfigurationFile, overwrite = TRUE)
  }

  return(invisible())
}

#' Load Prior Data
#'
#' This function loads the prior data from the specified workbook and handles
#' the logic for overwriting the existing data if necessary.
#'
#' @param wb An open workbook object from which to read the prior data.
#' @param overwrite A logical indicating whether to overwrite existing data.
#'                  Default is FALSE.
#'
#' @return A data.table containing the prior data.
#' @keywords internal
loadPriorData <- function(wb, overwrite) {
  dtPrior <- xlsxReadData(wb = wb, sheetName = "Prior") # nolint
  if (overwrite & nrow(dtPrior) > 1) {
    dtPrior <- dtPrior[1]
  }
  return(dtPrior)
}


#' Validate Parameter Definition
#'
#' This function validates the parameter definitions to ensure that the
#' required columns are present and contain valid data.
#'
#' @param dtDefinition A data.table containing parameter definitions to be validated.
#'
#' @return invisible(NULL). This function is called for its side effects (validation).
#' @keywords internal
validateParameterDefinition <- function(dtDefinition) {

  checkmate::assertCharacter(
    dtDefinition$name,
    unique = TRUE,
    any.missing = FALSE,
    .var.name = paste("column Name in", "ParameterDefinition")
  )

  checkmate::assertNames(
    unique(dtDefinition$distribution),
    subset.of = getAllDistributions(),
    .var.name = paste("column Distribution in", "ParameterDefinition")
  )

  return(invisible())
}

#' Update Prior Parameters
#'
#' This function updates the prior parameters by adding global and hyper prior
#' parameters and model error parameters, then writes the updated data back
#' to the workbook.
#'
#' @param dtPrior A data.table containing the existing prior parameters.
#' @param dtDefinition A data.table containing parameter definitions.
#' @param wb An open workbook object where the updated prior parameters will be written.
#'
#' @return A data.table containing the updated prior parameters.
#' @keywords internal
updatePriorParameters <- function(dtPrior, dtDefinition, wb) {
  dtPrior <- addGlobalPriorParameter(dtPrior = dtPrior, dtDefinition = dtDefinition)
  dtPrior <- addHyperPriorParameter(dtPrior = dtPrior, dtDefinition = dtDefinition)

  dtPrior <- addModelErrorParameter(dtPrior, wb)

  xlsxWriteData(wb = wb, sheetName = "Prior", dt = dtPrior)

  return(dtPrior)
}

#' Load Start Values Header
#'
#' This function loads the start values header from the specified workbook and
#' handles the logic for overwriting the existing header if necessary.
#'
#' @param wb An open workbook object from which to read the start values header.
#' @param overwrite A logical indicating whether to overwrite existing data.
#'                  Default is FALSE.
#'
#' @return A data.table containing the start values header.
loadStartValuesHeader <- function(wb, overwrite) {
  dtStartValuesHeaders <- xlsxReadData(wb = wb, sheetName = "IndividualStartValues")
  if (overwrite & nrow(dtStartValuesHeaders) > 1) {
    dtStartValuesHeaders <- dtStartValuesHeaders[1]
  }

  return(dtStartValuesHeaders)
}

#' Create Start Values
#'
#' This function generates start values for individual parameters based on the
#' parameter definitions and unique individual IDs from the observed data.
#'
#' @param dtDefinition A data.table containing parameter definitions, including
#'                     names and units for individual parameters.
#' @param dataObserved A data.table of observed data.
#'
#' @return A data.table containing start values for each individual parameter.
#' @keywords internal
createStartValues <- function(dtDefinition, dataObserved) {
  dtStartValues <- data.table()

  for (iRow in which(dtDefinition$valueMode == PARAMETERTYPE$individual)) {
    covariates <- dtDefinition$categoricCovariate[iRow]
    if (is.na(covariates)) covariates <- NULL

    tmpStartValues <- dataObserved %>%
      dplyr::select(dplyr::any_of(c("individualId", covariates))) %>%
      unique() %>%
      dplyr::mutate(name = dtDefinition$name[iRow]) %>%
      dplyr::mutate(unit = dtDefinition$unit[iRow])

    dtStartValues <- rbind(
      dtStartValues,
      tmpStartValues
    )
  }

  return(dtStartValues)
}

#' Add Global Prior Parameter
#'
#' This function adds global prior parameters based on the parameter definition.
#'
#' @param dtPrior A data.table containing prior parameters.
#' @param dtDefinition A data.table containing parameter definitions.
#'
#' @return A data.table containing updated prior parameters.
#' @keywords internal
addGlobalPriorParameter <- function(dtPrior, dtDefinition) {
  # Initialize variables to NULL to avoid linter messages
  valueMode <- scaling <- p1_type <- p2_type <- startValue <- p1_value <- p2_value <- NULL

  globalParams <- dtDefinition[valueMode == PARAMETERTYPE$global] %>% # nolint identation
    setnames(
      old = c("minValue", "maxValue", "unit"),
      new = c("p1_value", "p2_value", "unit_of_distributed_Parameter")
    ) %>%
    dplyr::mutate(p1_type = "min", p2_type = "max", p3_type = "")

  dtPrior <-
    rbind(
      dtPrior,
      globalParams %>%
        dplyr::select(dplyr::any_of(names(dtPrior))),
      fill = TRUE
    )

  return(dtPrior)
}

#' Add Hyper Prior Parameter
#'
#' This function adds hyper prior parameters based on the parameter definition and observed data.
#'
#' @param dtPrior A data.table containing prior parameters.
#' @param dtDefinition A data.table containing parameter definitions.
#' @param dataObserved A data.frame of observed data.
#'
#' @return A data.table containing updated prior parameters.
#' @keywords internal
addHyperPriorParameter <- function(dtPrior, dtDefinition, dataObserved) {
  # Initialize a list to store new rows
  newRows <- list()

  # Process each parameter row
  for (iRow in which(dtDefinition$valueMode == "individual")) {
    par <- dtDefinition[iRow, ]

    # Determine subgroups
    subGroups <- ifelse(!is.na(par$categoricCovariate) && par$categoricCovariate != "",
      unique(dataObserved[[par$categoricCovariate]]), # nolint identation
      ""
    )

    # Create rows for each subgroup
    for (sG in subGroups) {
      for (parType in getDistributionParameters(distributionName = par$distribution)) {
        # Prepare common values to pass
        commonValues <- list(
          name = par$name,
          categoricCovariate = sG,
          hyperParameter = parType,
          unit_of_distributed_Parameter = par$unit
        )

        # Prepare specific values based on parameter type
        distributionRow <- getDistributionRow(
          distributionName = par$distribution,
          distributionParameter = parType
        )

        specificValues <- list()
        specificValues[c("p1_value", "p2_value", "startValue", "scaling")] <-
          mapply(
            function(value) {
              if (value %in% names(par)) {
                return(par[[value]])
              } else {
                return(value)
              }
            },
            c(
              distributionRow$minValue,
              distributionRow$maxValue,
              distributionRow$defaultValue,
              distributionRow$scaling
            ),
            SIMPLIFY = TRUE
          )

        # Create the hyperparameter row with common and specific values
        newRows[[length(newRows) + 1]] <-
          stats::setNames(as.list(rep(NA, ncol(dtPrior))), names(dtPrior)) %>%
          utils::modifyList(list(
            valueMode = PARAMETERTYPE$hyperParameter,
            distribution = "unif",
            hyperParameter = "",
            p1_type = "min",
            p2_type = "max"
          )) %>%
          utils::modifyList(commonValues) %>%
          utils::modifyList(specificValues)
      }
    }
  }

  # Combine all new rows into dtPrior
  dtPrior <- rbind(dtPrior, rbindlist(newRows))

  return(dtPrior)
}

#' Add Model Error Parameter
#'
#' This function adds model error parameters based on output mappings.
#'
#' @param dtPrior A data.table containing prior parameters.
#' @param wb the open workkbook with the BMLM configurations
#'
#' @return A data.table containing updated prior parameters.
#' @keywords internal
addModelErrorParameter <- function(dtPrior, wb) {
  # Initialize variables to NULL to avoid linter messages
  errorModel <-  NULL

  dtOutputMappings <- xlsxReadData(wb = wb, sheetName = "ModelError", skipDescriptionRow = TRUE)

  modelErrorParams <-
    dtOutputMappings[, .(
      valueMode = PARAMETERTYPE$outputError,
      distribution = "unif",
      scaling = ifelse(errorModel == ERRORMODEL$log_absolute, "Log", "Linear"),
      p1_type = "min",
      p1_value = 0,
      p2_type = "max",
      p2_value = "Inf"
    ),
    by = "modelErrorId"
    ] %>%
    data.table::setnames("modelErrorId", "name")

  dtPrior <- rbind(dtPrior, modelErrorParams, fill = TRUE)

  return(dtPrior)
}

