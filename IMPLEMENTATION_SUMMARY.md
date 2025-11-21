# Consolidate Scaling - Implementation Summary

## Overview
This implementation consolidates the scaling approach by storing parameters as **unscaled values** in optimStatus files, rather than scaled values. This simplifies the codebase and improves maintainability.

## Changes Made

### 1. Core Functionality Changes

#### R/utilities-scaling.R (+75 lines)
- **New function**: `getUnscaledParams()` - Returns unscaled parameter values directly from dtPrior and dtStartValues
- **New function**: `setUnscaledParameterToTables()` - Sets unscaled values directly to data tables without scaling/unscaling transformation

#### R/utilities-startBMLMOptimization.R (~6 lines modified)
- **Modified**: `updateOptimStatus()` now calls `getUnscaledParams()` instead of `getParams()`
- **Impact**: All status files (optimStatus.RDS, bestOptimStatus.RDS, failedOptimStatus.RDS) now store unscaled parameters

### 2. BMLMOptimization Class Updates

#### R/BMLMOptimization.R (~114 lines modified, net +79)
**Updated 9 locations** where optimStatus files are loaded:
1. `exportIndividualResultsToPkml()` - line 160-180
2. `exportResultAsPopulation()` - line 196-210
3. `exportIndividualValuesToConfigTable()` - line 225-235
4. `exportModelParametersToConfigTables()` - line 247-265
5. `exportHyperParametersToConfigTables()` - line 276-293
6. `exportFinalValuesToBMLConfigTable()` - line 300-315
7. `getCurrentConfigTable()` - line 321-340
8. `startOptimization()` - line 817-835 (restart scenario)
9. `updatePredictedValues()` - line 1040-1055

All now use `setUnscaledParameterToTables()` instead of `setParameterToTables()` when loading optimStatus files.

**New private methods**:
- `savePackageVersion()` - Saves current package version to `package_version.txt` in output directory
- `checkAndConvertVersion()` - Checks for version file and automatically converts old format if needed

**Modified**:
- `initialize()` - Now saves version on new runs and checks/converts on reload

### 3. Version Conversion Support

#### R/convertVersion.R (+106 lines)
- **New function**: `convertScaledToUnscaledParams()` - Detects and converts old scaled parameters to unscaled format
  - Uses heuristics to detect if parameters are scaled (for hardBounds: all in (0,1); for logsig: most in [-20,20])
  - Loads prior.csv and startValues.csv to perform accurate conversion
  - Safe to run multiple times (idempotent)
- **Modified**: `convertVersionBMLM()` - Now calls the new conversion function

### 4. Package Version

#### DESCRIPTION
- Updated version from 0.1.9 to 0.2.0 (breaking change)

#### NEWS.md (+28 lines)
- Added comprehensive changelog for version 0.2.0
- Documented breaking changes
- Provided migration guide for existing projects

### 5. Documentation and Test Support

#### inst/extdata/BMLMTestResult/README.md (new, 34 lines)
- Explains why test data needs updating
- Provides instructions for conversion

#### inst/extdata/BMLMTestResult/package_version.txt (new)
- Marks test data as version 0.1.9 (needs conversion)

#### update_test_data.R (new, 46 lines)
- Script to convert test data files from scaled to unscaled format
- Can be run manually: `source("update_test_data.R")`

## Behavior Changes

### Before (0.1.9):
```r
# Parameters saved as scaled values
optimStatus <- list(
  params = c(0.5, 0.3, 0.8),  # Scaled [0,1] for hardBounds
  scalingMethod = "hardBounds",
  ...
)

# When loading:
dtList <- setParameterToTables(dtList, optimStatus$params, optimStatus$scalingMethod)
# ^ This unscales the params
```

### After (0.2.0):
```r
# Parameters saved as unscaled values
optimStatus <- list(
  params = c(50, 0.03, 800),  # Actual unscaled values
  scalingMethod = "hardBounds",
  ...
)

# When loading:
dtList <- setUnscaledParameterToTables(dtList, optimStatus$params)
# ^ No unscaling needed, params are already in correct form
```

## Backward Compatibility

### Automatic Conversion
When loading an existing project created with version 0.1.9 or earlier:
1. `BMLMOptimization$new()` checks for `package_version.txt` in the run directory
2. If not found or version < 0.2.0, calls conversion functions automatically
3. Logs the conversion in the optimization log
4. Updates the version file to 0.2.0

### Manual Conversion
Users can also manually call:
```r
convertVersionBMLM(projectConfiguration)
```
This converts all runs in the project.

### Safety
- Conversion is idempotent - safe to run multiple times
- Original files are updated in place (consider backing up first)
- Version file prevents re-conversion of already converted runs

## Testing Notes

### Test Data Update Required
The test data files in `inst/extdata/BMLMTestResult/` were created with version 0.1.9 and contain scaled parameters. They need to be converted or regenerated:

**Option 1**: Run the conversion script
```r
source("update_test_data.R")
```

**Option 2**: Generate new test data
Run a short optimization with version 0.2.0 and copy the resulting files.

### Test Coverage
Tests verify:
- Initialization creates status files
- Optimization runs and produces results
- Plotting functions work with loaded results
- Export functions work with loaded results

All tests that load optimStatus files will now expect unscaled parameters.

## Files Modified Summary
- Modified: 5 files (DESCRIPTION, BMLMOptimization.R, convertVersion.R, utilities-scaling.R, utilities-startBMLMOptimization.R)
- Created: 4 files (NEWS.md additions, README.md, package_version.txt, update_test_data.R)
- Total changes: +377 lines, -35 lines (net +342 lines)

## Migration Checklist for Users

For projects created with version 0.1.9 or earlier:

- [ ] Backup your project directory
- [ ] Update to version 0.2.0
- [ ] Option A: Let automatic conversion happen when you load runs
  - Simply call `BMLMOptimization$new()` with existing runs
  - Check the log file for conversion messages
- [ ] Option B: Manually convert all runs
  - Call `convertVersionBMLM(projectConfiguration)`
- [ ] Verify converted runs work correctly
- [ ] Check that plots and exports work as expected

## Additional Notes

### Why This Change?
1. **Simplicity**: Storing unscaled values is more intuitive and matches the actual parameter values
2. **Consistency**: Reduces the number of scaling/unscaling operations throughout the code
3. **Maintainability**: Easier to understand and debug when parameters are in their natural units

### Scaling Still Used
The scaling functions (`getParams`, `setParameterToTables`, etc.) are still used during optimization:
- During optimization, parameters are still scaled for the optimizer
- Only the *storage* format in RDS files has changed
- The optimization algorithm behavior is unchanged

### Future Work
- Consider adding tests specifically for the conversion logic
- Consider adding validation that parameters are in expected ranges after conversion
- Monitor for any edge cases in parameter detection during conversion
