#' List of functions and strings used to signal error and warning messages
#' @description 
#' This module contains all error and warning messages used throughout the package.
#' Messages are organized as functions that return formatted strings.
#' @export
messages <- list()

# Data Preparation Messages ----

messages$errorScenariosNotDefaultScenario <- function(missingScenarios) {
  paste(
    'There are scenarios which are not selected as "DefaultScenario" in sheet "DataGroups" "Plots.xlsx".',
    "This is mandatory to connect data and simulations:",
    paste(missingScenarios, collapse = ", ")
  )
}

messages$errorNonIndividualDataClass <- function(dataGroups) {
  paste(
    'Please select only scenarios which are matched as "DefaultScenarios" in sheet "DataGroups" "Plots.xlsx" to individual data.',
    "Check dataGroup",
    paste(dataGroups, collapse = ", ")
  )
}

messages$errorValuesNotAllowedForErrorModel <- function() {
  "values <= 0 not allowed for this error model"
}

messages$errorEmptyPriorSheet <- function() {
  "empty Prior sheet"
}

messages$errorProbabilityOfStartValueNA <- function(parameterNames) {
  paste(
    "Probability of startvalue is NA, check priors",
    paste(parameterNames, collapse = ", ")
  )
}

messages$errorStartValueOutsideDistribution <- function(parameterNames) {
  paste(
    "Start value outside distribution range, check",
    paste(parameterNames, collapse = ", ")
  )
}

messages$errorRandomStartValuesNotPossible <- function(groupName) {
  paste("Not possible to generate random startValues in boundarys for", groupName)
}

messages$errorStartValuesOutsideDistributionRange <- function() {
  "There are start values outside the distribution range"
}

messages$errorValuesNotSatisfyCondition <- function() {
  "Some values do not satisfy the condition minValue <= value <= maxValue"
}

messages$errorColumnsNotPositiveForLogScaling <- function() {
  "Columns 'value', 'minValue', and 'maxValue' must be greater than 0, for Scaling log"
}

messages$errorDuplicateNames <- function(sheetName, identifierCols, duplicateNames) {
  paste0(
    "Sheet ", sheetName, " must be unique in columns '",
    paste(identifierCols, collapse = "', '"), "' ",
    "\nCheck parameters with name: '",
    paste(duplicateNames, collapse = "', '"), "'"
  )
}

messages$warningDataBeforeSimulationRange <- function() {
  "data with time < 0 is outside simulation range will be ignored"
}

messages$warningDataOutsideSimulationRange <- function() {
  "data with time outside simulation range will be ignored"
}

messages$warningDataBelowLLOQ <- function() {
  "Set Data Values below lloq to lloq/2"
}

messages$warningNoIndividualStartValues <- function() {
  "No individual start values available"
}

messages$warningInvalidProbability <- function(parameterNames) {
  paste0(
    "Invalid probability for parameters: ",
    paste(parameterNames, collapse = ", "),
    ". Probability must be between 0 and 1"
  )
}

messages$warningInconsistentDefinition <- function(tableName, col, parameterNames) {
  paste0(
    "Sheet '", tableName, "' is not consistent with sheet 'ParameterDefinition' for column '",
    col, "' for parameter(s): '", paste(parameterNames, collapse = "', '"), ".",
    " Settings defined in 'ParameterDefinition' are ignored!"
  )
}

# Convergence Plot Messages ----

messages$errorConvergenceTableMissingColumns <- function(requiredCols) {
  paste("Convergence table must contain the following columns:", paste(requiredCols, collapse = ", "))
}

messages$errorUnknownSectionMode <- function() {
  "unknown sectionMode"
}

# Export Result Messages ----

messages$errorExportAmbiguousValues <- function() {
  "Export not possible. There are ambiguous values."
}

messages$errorSheetAlreadyExists <- function(sheetName) {
  paste(sheetName, "already exists. Please set overwrite to `TRUE` if you want to overwrite existing values.")
}

messages$warningFactorParametersNotExported <- function(parameterNames) {
  paste("Parameters defined with `useAsFactors = TRUE` are not exported. Please check", paste(parameterNames, collapse = ', '))
}

# BMLM Optimization Messages ----

messages$errorNoBMLMConfiguration <- function() {
  "Project configuration has no BMLM Configuration attached!"
}

messages$errorOnlyVirtualTwinPopulations <- function(invalidScenarios) {
  paste("Please use only scenarios for virtual twin populations! Check",
        paste(invalidScenarios, collapse = ", "))
}

messages$errorStatusIsRunning <- function() {
  "Status is running!
             Please check if a background job is still running, otherwise reset status with 'cleanUpStatus()'"
}

messages$errorExecutionStoppedByUser <- function() {
  "Execution stopped by user."
}

messages$errorStrangeLikelihood <- function() {
  "strange loglikelihood"
}

messages$errorFirstLikelihoodEvaluationFailed <- function() {
  "First likelihood evaluation must not fail"
}

# Scaling Messages ----

messages$errorUnknownScalingMethod <- function() {
  "unknown scaling method"
}

# Config Messages ----

messages$errorUnsupportedModelParametersExtension <- function(fileExtension, supportedExtensions) {
  paste0(
    "Model parameters file extension '", fileExtension, 
    "' is not supported. Supported extensions are: ", 
    paste(supportedExtensions, collapse = ", ")
  )
}

messages$errorMissingOutputIdentifier <- function(paths) {
  paste0(
    "Missing identifier for path(s): ",
    paste(paths, collapse = ","),
    ". Please update Plotconfiguration 'Outputs'"
  )
}

messages$warningSheetExists <- function(sheetName, fileName) {
  paste("Sheet", sheetName, "exists already in", fileName)
}

messages$warningPriorSheetAlreadyEdited <- function() {
  "sheet 'Prior' is already edited"
}

messages$warningStartValueSheetAlreadyEdited <- function() {
  "StartValue sheet is already edited"
}

# Correlation Plot Messages ----

messages$errorNoDistributedParameters <- function() {
  "No distributed parameters available"
}

# Parameter Plot Messages ----

messages$errorNoParametersWithPrior <- function() {
  "No parameters with prior information available"
}

messages$errorMinMaxValuesMustBeUnique <- function() {
  "min and max Values must be unique for each group in startValues"
}

messages$errorNoParametersForLogScale <- function() {
  "No parameters left for log-scale display."
}

messages$warningSkippingParametersForLogScale <- function(parameterNames) {
  sprintf('Skipping parameters with display ranges less than or equal to zero for log-scale display: %s',
          paste(parameterNames, collapse = ', '))
}
