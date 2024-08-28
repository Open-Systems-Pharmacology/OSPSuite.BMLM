## config scenarios ----------

#' Title
#'
#' @param projectConfiguration
#' @param data
#'
#' @return
#' @export
configScenarios <- function(projectConfiguration, data, PIName = "", overwrite = FALSE) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$scenarioDefinitionFile)

  dtScenario <- xlsxReadData(wb = wb,sheetName = 'Scenarios')

  # initialize matching table
  dtScenarioDataMatch = data.table()

  if (nrow(dtScenario) == 0 | overwrite) {
    # get sheets of other excel files, which can be used for scenarios
    modelParameterSheet <- intersect(openxlsx::getSheetNames(file = projectConfiguration$paramsFile), PIName)
    if (length(modelParameterSheet) == 0) modelParameterSheet <- ""
    ApplicationProtocolSheets <- openxlsx::getSheetNames(file = projectConfiguration$scenarioApplicationsFile)

    wbO <- openxlsx::loadWorkbook(projectConfiguration$BMLMConfigurationFile)
    dtDataGroups <- xlsxReadData(wb = wbO,sheetName = BMLMSHEET$DataGroupToModelFile)

    modelFilesperGroup <- dtDataGroups$ModelFile
    names(modelFilesperGroup) <- dtDataGroups$DataGroupId

    for (gId in as.character(unique(data$groupId))) {
      applicationProtocol <- intersect(ApplicationProtocolSheets, gId)
      if (length(applicationProtocol) == 0) applicationProtocol <- ""
      modelFile <- modelFilesperGroup[[gId]]


      for (indId in unique(data[groupId == gId]$individualId)) {
        scenarioName <- paste("G", gId, "I", indId, sep = "_")
        dtScenario <- rbind(
          dtScenario,
          data.table(
            Scenario_name = scenarioName,
            IndividualId = indId,
            ModelParameterSheets = modelParameterSheet,
            ApplicationProtocol = applicationProtocol,
            ModelFile = modelFile
          ),
          fill = TRUE
        )
        dtScenarioDataMatch <- rbind(
          dtScenarioDataMatch,
          data.table(
            Scenario_name = scenarioName,
            IndividualId = indId,
            dataGroupID = gId
          )
        )
      }
    }

    xlsxWriteData(wb = wb, sheetName = "Scenarios", dt = dtScenario)
    xlsxAddSheet(wb,sheetName = 'MatchingTable',dt = dtScenarioDataMatch)

    openxlsx::saveWorkbook(wb = wb, projectConfiguration$scenarioDefinitionFile, overwrite = TRUE)
  } else {
    warning("Scenario sheet is already edited")
  }
}



#' Title
#'
#' @param projectConfiguration
#' @param data
#'
#' @return
#' @export
#'
#' @examples
saveBiometricsToConfig <- function(projectConfiguration, data, overwrite = FALSE) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$individualsFile)

  dtIndividualBiometrics <- xlsxReadData(wb = wb,sheetName = "IndividualBiometrics")

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

  if (!("biometrics_overview" %in% wb$sheet_names | overwrite)) {
    biometrics <-
      addMissingColumns(biometrics, names(dtIndividualBiometrics))

    xlsxCloneAndSet(wb = wb,clonedSheet = "IndividualBiometrics",sheetName =  "biometrics_overview",dt = biometrics)

  } else {
    warning("biometrics_overview already edited")
  }

  # Create a progress bar
  pb <- utils::txtProgressBar(min = 1, max = nrow(biometrics), style = 3)


  for (indId in biometrics$IndividualId) {
    utils::setTxtProgressBar(pb, indId) # Update the progress bar


    if (!(indId %in% wb$sheet_names) | overwrite) {

      biomForInd <- biometrics[IndividualId == indId, ]


      # Create ontogenies for the proteins
      moleculeOntogenies <- esqlabsR:::.readOntongeniesFromXLS(biomForInd)

      # Create the IndividualCharacteristics object
      individualCharacteristics <- ospsuite::createIndividualCharacteristics(
        species = biomForInd$Species,
        population = biomForInd$Population,
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

      Wwb <- xlsxCloneAndSet(wb,
                             clonedSheet = "template_Indiv1",
                             sheetName = indId,
                             dt = output)
    } else {
      warning(paste("Individual sheet for", indId, "already edited"))
    }
  }

  close(pb)

  openxlsx::saveWorkbook(wb, projectConfiguration$individualsFile, overwrite = TRUE)
}
