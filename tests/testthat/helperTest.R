buildTestData <- function(rootDirectory = NULL,
                          writeTestData = FALSE) {

  # Initialize class to build test project
  pBuilder <- TestProjectBuilder$new()

  # get projectConfiguration
  projectConfiguration <- pBuilder$iniTestProject(rootDirectory = rootDirectory)

  # Clear existing data from relevant Excel sheets
  pBuilder$mockManualEditingsCleanup(projectConfiguration)

  # Define virtual populations within biometric ranges
  randomPops <- data.table(
    populationName = c("testPopulation"),
    species = "Human",
    population = "European_ICRP_2002",
    numberOfIndividuals = 6,
    proportionOfFemales = 50,
    ageMin = c(20),
    ageMax = c(40),
    weightUnit = "kg",
    heightUnit = "cm",
    bMIUnit = "kg/m\u00B2",
    protein = "CYP3A4,UGT1A4",
    ontogeny = "CYP3A4,UGT1A4"
  )
  pBuilder$mockManualEditingsPopulation(projectConfiguration,
                                        randomPops = randomPops
  )

  modelFiles <- list.files(file.path(system.file(
    package = "ospsuite.reportingframework",
    "extdata",
    mustWork = TRUE
  ), "Models"))
  names(modelFiles) <- substr(modelFiles, 1, 2)

  scenarioNames <- c("testScenario_iv", "optimScenario")
  pBuilder$mockManualEditingsScenario(
    projectConfiguration,
    dtTestScenarios = data.table(
      scenario_name = scenarioNames,
      populationId = c("testPopulation", "1234_adults_iv"),
      readPopulationFromCSV = 1,
      modelFile = modelFiles["iv"],
      outputPathsIds = "Plasma",
      shortName = scenarioNames,
      longName = scenarioNames
    ),
    pKParameter = NULL
  )

  #
  pBuilder$setupRandomPopulations(
    projectConfiguration = projectConfiguration,
    populationNames = unique(randomPops$populationName),
    writeTestData = writeTestData,
    templateDir = system.file(
      package = "ospsuite.bmlm",
      "extdata",
      mustWork = TRUE
    )
  )

  scenarioList <- createScenarios.wrapped(
    projectConfiguration = projectConfiguration,
    scenarioNames = scenarioNames[1]
  )

  # Run scenarios and calculate PK
  scenarioResults <- pBuilder$setupSimulations(
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioList,
    writeTestData = writeTestData,
    templateDir = instDirectory <- system.file(
      package = "ospsuite.bmlm",
      "extdata",
      mustWork = TRUE
    )
  )

  pBuilder$addRandomTPData(
    projectConfiguration = projectConfiguration,
    scenarioResults = scenarioResults,
    outputPathIds = "Plasma",
    ids = seq(0, 6)
  )

  dataObserved <- readObservedDataByDictionary(projectConfiguration)

  exportVirtualTwinPopulations(
    projectConfiguration = projectConfiguration,
    modelFile = list.files(projectConfiguration$modelFolder, pattern = ".pkml")[1],
    overwrite = TRUE
  )

  scenarioList <- createScenarios.wrapped(
    projectConfiguration = projectConfiguration,
    scenarioNames = scenarioNames[2]
  )

  mockManualEditingsUpdateDefaultSCenario(projectConfiguration)

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

  mockManualEditingsUpdateParameterDefinition(projectConfiguration)
  configurePriors(
    projectConfiguration = projectConfiguration,
    dataObserved = dataObserved,
    overwrite = TRUE
  )
  mockManualEditingsUpdatePriorDefinition(projectConfiguration)


  myTestRun <- BMLMOptimization$new(
    projectConfiguration = projectConfiguration,
    runName = "myTestRun",
    scenarioList = scenarioList,
    dataObserved = dataObserved
  )

  myTestRun$evaluateInitialValues()

  return(invisible(list(
    projectConfiguration = projectConfiguration,
    dataObserved = dataObserved,
    scenarioList = scenarioList,
    myTestRun = myTestRun
  )))
}

mockManualEditingsUpdateParameterDefinition <- function(projectConfiguration) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$addOns$bMLMConfigurationFile)
  dt <- xlsxReadData(wb = wb, sheetName = "ParameterDefinition", skipDescriptionRow = FALSE)
  dt[grep("Ontogeny", name), `:=`(
    valueMode = PARAMETERTYPE$individual,
    distribution = "lnorm_geomean",
    useAsFactors = 0
  )]

  xlsxWriteData(wb = wb, sheetName = "ParameterDefinition", dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$addOns$bMLMConfigurationFile, overwrite = TRUE)

  return(invisible())
}

mockManualEditingsUpdatePriorDefinition <- function(projectConfiguration) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$addOns$bMLMConfigurationFile)
  dt <- xlsxReadData(wb = wb, sheetName = "Prior", skipDescriptionRow = FALSE)
  dt <- dt[!grep("geomean", hyperParameter)]
  dt[grep("geosd", hyperParameter), `:=`(
    maxValue = 1.4
  )]

  xlsxWriteData(wb = wb, sheetName = "Prior", dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$addOns$bMLMConfigurationFile, overwrite = TRUE)

  return(invisible())
}

mockManualEditingsUpdateDefaultSCenario <- function(projectConfiguration) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  dt <- xlsxReadData(wb = wb, sheetName = "DataGroups", skipDescriptionRow = FALSE)
  dt[grep("1234_adults_iv", group), `:=`(
    defaultScenario = "optimScenario"
  )]

  xlsxWriteData(wb = wb, sheetName = "DataGroups", dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  return(invisible())
}
