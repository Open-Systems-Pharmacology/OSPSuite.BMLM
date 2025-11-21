# Update Test Data for Version 0.1.10

The test data files in `inst/extdata/BMLMTestResult/` need to be updated to contain unscaled parameters (version 0.1.10 format) instead of scaled parameters (version 0.1.9 format).

## Why?

Version 0.1.10 changes the storage format:
- **Old (0.1.9)**: Parameters stored as scaled values in optimStatus files
- **New (0.1.10)**: Parameters stored as unscaled values in optimStatus files

## How to Update

Run the conversion script from the package root directory:

```r
source("update_test_data.R")
```

This script will:
1. Build test data to get the current prior.csv and startValues.csv
2. Use these to convert the optimStatus.RDS and bestOptimStatus.RDS files
3. Update the package_version.txt file to 0.1.10

## Alternative: Manual Test Data Generation

If the script doesn't work, you can generate new test data by:

1. Running a short optimization with the test data
2. Copying the resulting bestOptimStatus.RDS, optimStatus.RDS, and bestPrediction.RDS files to inst/extdata/BMLMTestResult/
3. Making sure they have the 0.1.10 format (unscaled parameters)

## Note on Tests

Tests in `test-BMLMOptimization.R` copy files from `inst/extdata/BMLMTestResult/` to test the plotting and export functions. These files must contain unscaled parameters for the tests to work with version 0.1.10+.
