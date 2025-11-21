# Testing Guide for Version 0.2.0 Changes

## Overview
Version 0.2.0 changes how parameters are stored in optimStatus files. This guide helps verify that the changes work correctly.

## Pre-Testing Setup

### 1. Update Test Data Files
The test data files need to be converted from v0.1.9 format to v0.2.0 format:

```r
# From the package root directory
source("update_test_data.R")
```

This will:
- Generate test data with proper CSV files
- Convert the RDS files to unscaled format
- Update the version file to 0.2.0

Alternatively, you can regenerate test data from scratch by:
1. Running a short optimization
2. Copying the result files to `inst/extdata/BMLMTestResult/`

## Unit Tests to Run

### 1. Basic Package Tests
```r
# Install the package in development mode
devtools::load_all()

# Run all tests
devtools::test()
```

Expected: All tests should pass, particularly:
- `test-BMLMOptimization.R`: All tests
- `test-utilities-config.R`: convertVersionBMLM tests (if any)

### 2. Specific Functionality Tests

#### Test: New Run with Version Tracking
```r
library(ospsuite.bmlm)

# Create a new optimization run
myRun <- BMLMOptimization$new(
  projectConfiguration = projectConfiguration,
  runName = "test_version_tracking",
  scenarioList = scenarioList,
  dataObserved = dataObserved
)

# Verify version file exists
version_file <- file.path(myRun$outputDir, "package_version.txt")
stopifnot(file.exists(version_file))

# Verify version is 0.2.0
version <- readLines(version_file)[1]
stopifnot(version == "0.2.0")
```

#### Test: Loading Existing Run (Auto-Conversion)
```r
# Simulate an old project by:
# 1. Create a test directory with old optimStatus files
# 2. Create a version file with "0.1.9"
# 3. Load the run
# 4. Verify conversion happened

# Create old-style test directory
old_run_dir <- file.path(tempdir(), "test_old_run")
dir.create(old_run_dir, showWarnings = FALSE)

# Copy old test data
file.copy(
  from = system.file("extdata", "BMLMTestResult", package = "ospsuite.bmlm"),
  to = dirname(old_run_dir),
  recursive = TRUE
)

# Set old version
writeLines("0.1.9", file.path(old_run_dir, "package_version.txt"))

# Now try to load this run... (would need complete setup)
# Check logs for conversion messages
```

#### Test: Manual Conversion
```r
# Create a test project structure
test_project_dir <- file.path(tempdir(), "test_project")
dir.create(file.path(test_project_dir, "Output", "BMLM", "test_run"), 
           recursive = TRUE)

# Copy old test data
# ... setup code ...

# Run manual conversion
projectConfig <- list(outputFolder = file.path(test_project_dir, "Output"))
convertVersionBMLM(projectConfig)

# Verify version was updated
version <- readLines(file.path(test_project_dir, "Output", "BMLM", "test_run", 
                                "package_version.txt"))[1]
stopifnot(version == "0.2.0")
```

## Integration Tests

### 1. Full Optimization Workflow

```r
# Create new run
myRun <- BMLMOptimization$new(
  projectConfiguration = projectConfiguration,
  runName = "integration_test",
  scenarioList = scenarioList,
  dataObserved = dataObserved
)

# Evaluate initial values (creates first optimStatus.RDS)
myRun$evaluateInitialValues()

# Verify optimStatus.RDS contains unscaled parameters
status <- readRDS(file.path(myRun$outputDir, "optimStatus.RDS"))
# Check that params look reasonable (not all in [0,1] range for hardBounds)
print(summary(status$params))

# Run short optimization
myRun$startOptimization(
  projectConfiguration = projectConfiguration,
  method = "SANN",
  control = list(maxit = 2),
  scalingMethod = "hardBounds",
  startInBackground = FALSE
)

# Verify bestOptimStatus.RDS contains unscaled parameters
bestStatus <- readRDS(file.path(myRun$outputDir, "bestOptimStatus.RDS"))
print(summary(bestStatus$params))

# Test plotting functions (they load optimStatus)
plot1 <- myRun$checkConvergence()
plot2 <- myRun$checkParameterLimits()

# Test export functions (they load optimStatus)
myRun$getCurrentConfigTable(projectConfiguration)
```

### 2. Backward Compatibility Test

```r
# This requires having an actual old project from version 0.1.9
# Steps:
# 1. Create optimization run with version 0.1.9 (if available)
# 2. Upgrade to version 0.2.0
# 3. Load the old run
# 4. Verify it still works

# If you have an old project:
old_run <- BMLMOptimization$new(
  projectConfiguration = old_projectConfiguration,
  runName = "old_run_name",  # existing run from 0.1.9
  scenarioList = old_scenarioList,
  dataObserved = old_dataObserved
)

# Check log for conversion messages
old_run$openLogFile()

# Verify functionality
old_run$checkConvergence()
old_run$checkCorrelations()
```

## Manual Verification Checklist

### Version Tracking
- [ ] New runs create `package_version.txt` with "0.2.0"
- [ ] Version file is logged in `optimization_log.txt`
- [ ] Loading existing runs checks version file
- [ ] Older versions trigger automatic conversion
- [ ] Conversion is logged in `optimization_log.txt`

### Parameter Storage
- [ ] New optimStatus.RDS files contain unscaled parameters
- [ ] Parameters in optimStatus files are in reasonable value ranges (not all [0,1])
- [ ] bestOptimStatus.RDS contains unscaled parameters
- [ ] failedOptimStatus.RDS (if generated) contains unscaled parameters

### Conversion Logic
- [ ] `convertVersionBMLM()` runs without errors
- [ ] Conversion is idempotent (running twice doesn't break things)
- [ ] Old test data can be converted with `update_test_data.R`
- [ ] Converted files work correctly in all functions

### Functionality Preservation
- [ ] Optimization runs complete successfully
- [ ] All plotting functions work (checkConvergence, checkCorrelations, etc.)
- [ ] All export functions work (exportResultAsPopulation, etc.)
- [ ] Parameter values make sense (not corrupted during conversion)

## Common Issues and Solutions

### Issue: Test failures after update
**Solution**: Make sure test data has been updated to v0.2.0 format using `update_test_data.R`

### Issue: Parameters look wrong after loading
**Solution**: Check if conversion was applied correctly. Old files should have been detected and converted.

### Issue: Conversion not happening automatically
**Solution**: Verify `package_version.txt` exists and contains correct version number. Check that `checkAndConvertVersion()` is being called.

### Issue: Tests expect specific file format
**Solution**: Update tests to expect unscaled parameters in optimStatus files.

## Debugging Tips

### Check if parameters are scaled or unscaled
```r
status <- readRDS("path/to/optimStatus.RDS")

# For hardBounds scaling method:
# Scaled: all values in (0, 1)
# Unscaled: values in actual parameter ranges

if (status$scalingMethod == "hardBounds") {
  if (all(status$params > 0 & status$params < 1)) {
    message("Parameters appear to be SCALED (old format)")
  } else {
    message("Parameters appear to be UNSCALED (new format)")
  }
}
```

### Check version in log file
```r
# Look for these messages in optimization_log.txt:
# - "Saved package version: 0.2.0"
# - "Detected older version: 0.1.9 -> Updating to: 0.2.0"
# - "Version update completed: 0.1.9 -> 0.2.0"

logfile <- file.path(myRun$outputDir, "optimization_log.txt")
cat(readLines(logfile), sep = "\n")
```

### Verify conversion happened
```r
# Check prior.csv and startValues.csv exist (needed for conversion)
list.files(myRun$outputDir, pattern = "\\.csv$")

# Check for conversion messages
grep("Converted scaled to unscaled", 
     readLines(file.path(myRun$outputDir, "optimization_log.txt")),
     value = TRUE)
```

## Expected Test Results

### Before 0.2.0
- optimStatus.RDS: params in [0,1] for hardBounds or [-20,20] for logsig
- No package_version.txt file
- setParameterToTables() used to unscale when loading

### After 0.2.0
- optimStatus.RDS: params in actual value ranges
- package_version.txt contains "0.2.0"
- setUnscaledParameterToTables() used when loading (no unscaling needed)
- Old projects automatically converted on first load

## Regression Testing

Run the full test suite on several scenarios:
1. New project from scratch
2. Loading old project (if available)
3. Continuing stopped optimization
4. Restarting finalized optimization
5. All plotting functions
6. All export functions

All should work without errors and produce consistent results with previous versions (behavior should be identical, only storage format changed).
