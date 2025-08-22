# setup create test setup with projectconfiguration


# Unit tests for addBMLMPConfiguration
test_that("addBMLMPConfiguration works correctly with out snapshotFile", {
  result <- addBMLMPConfiguration(
    projectConfiguration = projectConfiguration,
    nameOfParameterIdentfication = "TestIdentification",
    snapshotFile = NULL,
    overwrite = TRUE
  )

  expect_s3_class(result, "ProjectConfiguration")
  expect_true("bMLMConfigurationFile" %in% names(result$addOns))
  expect_true(file.exists(result$addOns$bMLMConfigurationFile))
})

# Unit tests for addBMLMPConfiguration
test_that("addBMLMPConfiguration works correctly with out snapshotFile", {
  addBMLMPConfiguration(
    projectConfiguration = projectConfiguration,
    nameOfParameterIdentification = "PI",
    snapshotFile = system.file(
      package = "ospsuite.bmlm",
      "extdata",
      "ParameterIdentificationSetup.json",
      mustWork = TRUE
    )
  )


  expect_s3_class(result, "ProjectConfiguration")
  expect_true("bMLMConfigurationFile" %in% names(result$addOns))

  # Check if the configuration file was updated
  wb <- openxlsx::loadWorkbook(projectConfiguration$addOns$bMLMConfigurationFile)
  definitionDT <- xlsxReadData(wb = wb, sheetName = "ParameterDefinition")
  expect_true(nrow(definitionDT) > 0)
})

mockManualEditingsUpdateParameterDefinition(projectConfiguration)


# Unit tests for configurePriors
test_that("configurePriors updates prior parameters", {
  expect_invisible(configurePriors(projectConfiguration, dataObserved, overwrite = TRUE))

  wb <- openxlsx::loadWorkbook(projectConfiguration$addOns$bMLMConfigurationFile)
  dtPrior <- xlsxReadData(wb = wb, sheetName = "Prior")
  expect_length(nrow(dtPrior), 8)
})

# Unit tests for updateFixedParameters
test_that("updateFixedParameters adds fixed parameters correctly", {
  # Ensure the initial state
  initial_sheets <- openxlsx::getSheetNames(projectConfiguration$modelParamsFile)

  updateFixedParameters(
    linkedParameterDT = data.table(name = "FixedParam", isFixed = TRUE, startValue = 1, unit = "unit"),
    projectConfiguration = projectConfiguration,
    nameOfParameterIdentfication = "TestIdentification"
  )

  # Check if the new sheet was created
  new_sheets <- openxlsx::getSheetNames(projectConfiguration$modelParamsFile)
  expect_true(any(new_sheets == "TestIdentification"))
})
