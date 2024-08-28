# Initialization  ----------------------------------------------------------
# load libraries and source project specific code
library(ospsuite.reportingframework)
library(ospsuite.plots)
library(esqlabsR)
library(ospsuite.bmlm)

# set graphic all defaults
# (see vignette TODO)
ospsuite.plots::setDefaults()
theme_update(panel.background = element_rect(linetype = 'solid'))

# set options to enable watermarks
# (see vignette TODO)
setOspsuite.plots.option(
  optionKey = OptionKeys$watermark_enabled,
  value = TRUE
)

# Setup project structure -------------------------------------------------
# creates project directory (see vignette TODO Esqlabs)
# and help initProject for source Folder Selection
projectPath <- initProject(
  projectPath = "..",
  overwrite = FALSE,
  sourceFolder = templateDirectory()
)

# initialize log file
initLogfunction(projectPath)

logCatch({

  # get paths of all relevant project files
  projectConfiguration <-
    createBMBLProjectConfiguration(
      path = file.path(projectPath, "ProjectConfiguration.xlsx"),
      bMLMConfigurationFile = 'BMLMConfiguration.xlsx',
    )

  # Read observedData -------------------------------------------------------
  # (see vignette('data_import_by_dictionary'))

  # read data as data.table, exclude addBiometricsToConfigFlag,
  # as for BMLM thy are added as individual parameters
  dataDT <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,
                                         addBiometricsToConfigFlag = FALSE)



  # finalize workflow---------------------
  addMessageToLog("finalize workflow")

})
# save Session Infos including the loaded packages and R version, into a log file
saveSessionInfo()
