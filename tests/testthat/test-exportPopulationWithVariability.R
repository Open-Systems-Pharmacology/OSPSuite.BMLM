test_that("exportPopulationWithVariability creates new population file", {
  skip_if_not_installed("ospsuite.reportingframework")
  
  # Build test data
  testData <- buildTestData()
  projectConfiguration <- testData$projectConfiguration
  
  # Skip if required files don't exist
  skip_if_not(file.exists(projectConfiguration$populationsFile))
  
  # Get population name from test data
  populationName <- "testPopulation"
  populationFile <- file.path(projectConfiguration$populationsFolder, paste0(populationName, ".csv"))
  skip_if_not(file.exists(populationFile))
  
  # Create a simple variability sheet for testing
  # First, check if we can add a test variability sheet
  wbPop <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)
  
  # Use Template_Variability if it exists, otherwise skip
  variabilitySheetName <- "Template_Variability"
  skip_if_not(variabilitySheetName %in% wbPop$sheet_names)
  
  # Read template variability sheet
  dtVariability <- xlsxReadData(wb = wbPop, sheetName = variabilitySheetName)
  
  # Skip if template is empty or doesn't have required structure
  skip_if(nrow(dtVariability) == 0)
  skip_if(!all(c("container Path", "parameter Name", "parameter Group", "distribution") %in% names(dtVariability)))
  
  # Create a test variability sheet with some parameters
  testVariabilitySheetName <- "test_variability"
  
  # Read the original population to get some parameter names
  dtOriginalPop <- data.table::fread(populationFile)
  
  # Select a few parameters that exist in the population
  paramNames <- names(dtOriginalPop)
  # Skip the first column (usually IndividualId or similar)
  paramNames <- paramNames[2:min(5, length(paramNames))]
  
  # Create test variability data with normal distributions
  testVarData <- data.table::data.table(
    `container Path` = sapply(paramNames, function(p) {
      parts <- strsplit(p, "\\|")[[1]]
      if (length(parts) > 1) {
        paste(parts[-length(parts)], collapse = "|")
      } else {
        ""
      }
    }),
    `parameter Name` = sapply(paramNames, function(p) {
      parts <- strsplit(p, "\\|")[[1]]
      parts[length(parts)]
    }),
    `parameter Group` = paste0("Group_", seq_along(paramNames)),
    distribution = "norm",
    p1_type = "mean",
    p1_value = 1.0,
    p2_type = "sd",
    p2_value = 0.2,
    units = "",
    stringsAsFactors = FALSE
  )
  
  # Add the test sheet to workbook
  xlsxAddDataUsingTemplate(
    wb = wbPop,
    templateSheet = "Template_Variability",
    sheetName = testVariabilitySheetName,
    dtNewData = testVarData,
    templateXlsx = "Populations.xlsx"
  )
  
  openxlsx::saveWorkbook(wbPop, projectConfiguration$populationsFile, overwrite = TRUE)
  
  # Test the function
  newPopName <- "testPopulation_with_variability"
  
  expect_message(
    exportPopulationWithVariability(
      projectConfiguration = projectConfiguration,
      populationName = populationName,
      variabilitySheetName = testVariabilitySheetName,
      newName = newPopName,
      overwrite = TRUE
    ),
    "Successfully created population with variability"
  )
  
  # Check that new file was created
  newPopFile <- file.path(projectConfiguration$populationsFolder, paste0(newPopName, ".csv"))
  expect_true(file.exists(newPopFile))
  
  # Read the new population and verify structure
  dtNewPop <- data.table::fread(newPopFile)
  
  # Should have same number of rows as original
  expect_equal(nrow(dtNewPop), nrow(dtOriginalPop))
  
  # Should have same columns
  expect_equal(names(dtNewPop), names(dtOriginalPop))
  
  # Values should be different (with high probability) due to randomization
  # Check at least one of the varied parameters
  if (length(paramNames) > 0) {
    param <- paramNames[1]
    # Values should not be identical (allowing for very small chance they are)
    expect_false(all(dtNewPop[[param]] == dtOriginalPop[[param]]))
  }
  
  # Clean up test file
  if (file.exists(newPopFile)) {
    file.remove(newPopFile)
  }
})

test_that("exportPopulationWithVariability handles missing files gracefully", {
  skip_if_not_installed("ospsuite.reportingframework")
  
  testData <- buildTestData()
  projectConfiguration <- testData$projectConfiguration
  
  # Test with non-existent population
  expect_error(
    exportPopulationWithVariability(
      projectConfiguration = projectConfiguration,
      populationName = "nonexistent_population",
      variabilitySheetName = "Template_Variability"
    ),
    "does not exist"
  )
})

test_that("exportPopulationWithVariability handles missing variability sheet", {
  skip_if_not_installed("ospsuite.reportingframework")
  
  testData <- buildTestData()
  projectConfiguration <- testData$projectConfiguration
  
  populationName <- "testPopulation"
  populationFile <- file.path(projectConfiguration$populationsFolder, paste0(populationName, ".csv"))
  skip_if_not(file.exists(populationFile))
  
  # Test with non-existent variability sheet
  expect_error(
    exportPopulationWithVariability(
      projectConfiguration = projectConfiguration,
      populationName = populationName,
      variabilitySheetName = "nonexistent_sheet"
    ),
    "not found"
  )
})

test_that("exportPopulationWithVariability uses default newName", {
  skip_if_not_installed("ospsuite.reportingframework")
  
  testData <- buildTestData()
  projectConfiguration <- testData$projectConfiguration
  
  populationName <- "testPopulation"
  populationFile <- file.path(projectConfiguration$populationsFolder, paste0(populationName, ".csv"))
  skip_if_not(file.exists(populationFile))
  
  wbPop <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)
  variabilitySheetName <- "Template_Variability"
  skip_if_not(variabilitySheetName %in% wbPop$sheet_names)
  
  dtVariability <- xlsxReadData(wb = wbPop, sheetName = variabilitySheetName)
  skip_if(nrow(dtVariability) == 0)
  
  # Create minimal test variability sheet
  dtOriginalPop <- data.table::fread(populationFile)
  paramNames <- names(dtOriginalPop)[2:min(3, length(names(dtOriginalPop)))]
  
  testVariabilitySheetName <- "test_var2"
  testVarData <- data.table::data.table(
    `container Path` = sapply(paramNames, function(p) {
      parts <- strsplit(p, "\\|")[[1]]
      if (length(parts) > 1) paste(parts[-length(parts)], collapse = "|") else ""
    }),
    `parameter Name` = sapply(paramNames, function(p) {
      parts <- strsplit(p, "\\|")[[1]]
      parts[length(parts)]
    }),
    `parameter Group` = paste0("Group_", seq_along(paramNames)),
    distribution = "norm",
    p1_type = "mean",
    p1_value = 1.0,
    p2_type = "sd",
    p2_value = 0.1,
    units = ""
  )
  
  xlsxAddDataUsingTemplate(
    wb = wbPop,
    templateSheet = "Template_Variability",
    sheetName = testVariabilitySheetName,
    dtNewData = testVarData,
    templateXlsx = "Populations.xlsx"
  )
  openxlsx::saveWorkbook(wbPop, projectConfiguration$populationsFile, overwrite = TRUE)
  
  # Test without providing newName
  expect_message(
    exportPopulationWithVariability(
      projectConfiguration = projectConfiguration,
      populationName = populationName,
      variabilitySheetName = testVariabilitySheetName
    )
  )
  
  # Check default name was used
  defaultNewName <- paste(populationName, testVariabilitySheetName, sep = "_")
  defaultNewFile <- file.path(projectConfiguration$populationsFolder, paste0(defaultNewName, ".csv"))
  expect_true(file.exists(defaultNewFile))
  
  # Clean up
  if (file.exists(defaultNewFile)) {
    file.remove(defaultNewFile)
  }
})

test_that("exportPopulationWithVariability respects overwrite flag", {
  skip_if_not_installed("ospsuite.reportingframework")
  
  testData <- buildTestData()
  projectConfiguration <- testData$projectConfiguration
  
  populationName <- "testPopulation"
  populationFile <- file.path(projectConfiguration$populationsFolder, paste0(populationName, ".csv"))
  skip_if_not(file.exists(populationFile))
  
  wbPop <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)
  variabilitySheetName <- "Template_Variability"
  skip_if_not(variabilitySheetName %in% wbPop$sheet_names)
  
  dtVariability <- xlsxReadData(wb = wbPop, sheetName = variabilitySheetName)
  skip_if(nrow(dtVariability) == 0)
  
  # Create minimal test variability sheet
  dtOriginalPop <- data.table::fread(populationFile)
  paramNames <- names(dtOriginalPop)[2:min(3, length(names(dtOriginalPop)))]
  
  testVariabilitySheetName <- "test_var3"
  testVarData <- data.table::data.table(
    `container Path` = sapply(paramNames, function(p) {
      parts <- strsplit(p, "\\|")[[1]]
      if (length(parts) > 1) paste(parts[-length(parts)], collapse = "|") else ""
    }),
    `parameter Name` = sapply(paramNames, function(p) {
      parts <- strsplit(p, "\\|")[[1]]
      parts[length(parts)]
    }),
    `parameter Group` = paste0("Group_", seq_along(paramNames)),
    distribution = "norm",
    p1_type = "mean",
    p1_value = 1.0,
    p2_type = "sd",
    p2_value = 0.1,
    units = ""
  )
  
  xlsxAddDataUsingTemplate(
    wb = wbPop,
    templateSheet = "Template_Variability",
    sheetName = testVariabilitySheetName,
    dtNewData = testVarData,
    templateXlsx = "Populations.xlsx"
  )
  openxlsx::saveWorkbook(wbPop, projectConfiguration$populationsFile, overwrite = TRUE)
  
  newPopName <- "testPop_overwrite_test"
  newPopFile <- file.path(projectConfiguration$populationsFolder, paste0(newPopName, ".csv"))
  
  # First call - should create the file
  expect_message(
    exportPopulationWithVariability(
      projectConfiguration = projectConfiguration,
      populationName = populationName,
      variabilitySheetName = testVariabilitySheetName,
      newName = newPopName,
      overwrite = TRUE
    ),
    "Successfully created"
  )
  
  expect_true(file.exists(newPopFile))
  
  # Second call with overwrite=FALSE - should not overwrite
  expect_message(
    exportPopulationWithVariability(
      projectConfiguration = projectConfiguration,
      populationName = populationName,
      variabilitySheetName = testVariabilitySheetName,
      newName = newPopName,
      overwrite = FALSE
    ),
    "already exists"
  )
  
  # Third call with overwrite=TRUE - should overwrite
  expect_message(
    exportPopulationWithVariability(
      projectConfiguration = projectConfiguration,
      populationName = populationName,
      variabilitySheetName = testVariabilitySheetName,
      newName = newPopName,
      overwrite = TRUE
    ),
    "Successfully created"
  )
  
  # Clean up
  if (file.exists(newPopFile)) {
    file.remove(newPopFile)
  }
})
