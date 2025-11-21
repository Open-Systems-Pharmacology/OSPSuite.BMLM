#!/usr/bin/env Rscript

# Script to convert test data files from scaled to unscaled parameters
# This should be run once to update the test data files in inst/extdata/BMLMTestResult

library(data.table)
library(ospsuite.bmlm)

# Path to test result files
testResultDir <- file.path("inst", "extdata", "BMLMTestResult")

# We need the prior and startValues CSV files to perform the conversion
# These should come from a test run
# For now, we'll use the buildTestData to get them

# Build test data to get the proper CSV files
source("tests/testthat/helperTest.R")
testData <- buildTestData(writeTestData = FALSE)

# Get the output directory from myTestRun
outputDir <- testData$myTestRun$outputDir

# Copy the CSV files we need for conversion
file.copy(
  from = file.path(outputDir, "prior.csv"),
  to = file.path(testResultDir, "prior.csv"),
  overwrite = TRUE
)

file.copy(
  from = file.path(outputDir, "startValues.csv"),
  to = file.path(testResultDir, "startValues.csv"),
  overwrite = TRUE
)

# Now convert the optimStatus files
convertScaledToUnscaledParams(testResultDir)

# Clean up - remove the CSV files as they're not part of the test data
file.remove(file.path(testResultDir, "prior.csv"))
file.remove(file.path(testResultDir, "startValues.csv"))

# Update version file
writeLines("0.2.0", file.path(testResultDir, "package_version.txt"))

message("Test data conversion complete!")
