# ospsuite.bmlm NEWS

## [0.1.10.9000] - 2025-11-21

### Breaking Changes
- **Parameter Storage Format Changed**: Parameters in optimStatus.RDS, bestOptimStatus.RDS, and failedOptimStatus.RDS are now stored as **unscaled values** instead of scaled values.
  - This improves consistency and simplifies the codebase.
  - Automatic conversion is provided for existing projects via `convertVersionBMLM()`.
  
### New Features
- **Automatic Version Detection and Conversion**: 
  - Package version is now saved in each run's output directory as `package_version.txt`.
  - When loading existing runs, version is checked automatically.
  - If an older version is detected, conversion functions are called automatically to update file formats.
  - Version changes are logged in the optimization log file.

### Internal Changes
- Added `getUnscaledParams()` function to retrieve unscaled parameter values.
- Added `setUnscaledParameterToTables()` function to set unscaled parameters directly.
- Modified `updateOptimStatus()` to save unscaled parameters.
- Updated all locations loading optimStatus files to handle unscaled parameters.
- Enhanced `convertVersionBMLM()` with `convertScaledToUnscaledParams()` function.
- Added `savePackageVersion()` and `checkAndConvertVersion()` private methods to `BMLMOptimization` class.

### Migration Guide
For existing projects created with version 0.1.9 or earlier:
1. The conversion happens automatically when you create a `BMLMOptimization` object for an existing run.
2. Alternatively, you can manually call `convertVersionBMLM(projectConfiguration)` to convert all runs in a project.
3. The conversion is idempotent - running it multiple times is safe.

## [0.1.8] - 20-Jun-2025
- Bug fixes for run with no individual parameters

## [0.1.7] - 18-Jun-2025
- Bug fix log likelihood calculation off cutoff parameter

## [0.1.5] - 2025-02-24
- Add function evaluateAtInitialValues, openLogfile
- Add filter on outputPathid and scenario to checkfunctions

## [0.1.4] - 2025-02-10
- Allow flat distribution for individual parameters

## [0.1.3] - 2025-02-10
- Bug fixes in update parameters, set default Values of missing individuals 

## [0.1.2] - 2025-02-06
- Bug fixes in update parameters, only the parameter of the first scenario are updated

## [0.1.1] - 2025-02-04
- Bug fixes in checkProgress functions and export functions 
  Add publix functions for class BMLMOptimization exportIndividualResultsToPkml, getCurrentConfigTable


## [0.1.0] - 2025-01-14
- Initial setup of beta version
