# OSPSuite.BMLM

Bayesian Multilevel Model Parameter Identification for OSPSuite

## Overview

The `{ospsuite.bmlm}` package provides tools for fitting Bayesian multilevel models within the OSPSuite framework. It leverages the capabilities of the `{ospsuite.reportingframework}` package for project configuration, enabling efficient parameter identification using individual study data.

Bayesian multilevel models are particularly useful for:
- Analyzing hierarchical data structures
- Estimating individual-specific parameters
- Characterizing population variability
- Identifying global parameters (e.g., compound properties)

## Key Features

- **Integration with OSPSuite**: Seamless workflow integration with the reporting framework
- **Flexible Configuration**: Excel-based configuration files for easy setup
- **Multiple Optimization Methods**: Support for various optimization algorithms (BFGS, Nelder-Mead, etc.)
- **Internal Optimization**: Optional nested optimization for improved efficiency
- **Comprehensive Diagnostics**: Built-in plotting functions for monitoring optimization progress
- **Prior Distributions**: Support for various prior distributions on parameters
- **Hierarchical Structure**: Hyperparameter modeling for population-level parameters

## Installation

```r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("Open-Systems-Pharmacology/OSPSuite.BMLM")
```

## Documentation

The package includes comprehensive vignettes to help you get started:

1. **[Introduction to {ospsuite.bmlm}](https://www.open-systems-pharmacology.org/OSPSuite.BMLM/articles/01-introduction.html)** - Package overview, workflow diagram, integration with reporting framework, and quick start guide

2. **[BMLM Configuration](https://www.open-systems-pharmacology.org/OSPSuite.BMLM/articles/02-configuration.html)** - Detailed guidance on setting up configuration files, parameter definitions, mappings, model errors, and priors

3. **[Running BMLM Optimization](https://www.open-systems-pharmacology.org/OSPSuite.BMLM/articles/03-optimization.html)** - How to initialize optimization, run analyses, monitor progress with diagnostic plots, and export results

4. **[Likelihood Components and Error Models](https://www.open-systems-pharmacology.org/OSPSuite.BMLM/articles/04-likelihood-components.html)** - Mathematical foundations, objective function formulation, and error models

## Quick Start

```r
library(ospsuite.bmlm)

# 1. Add BMLM configuration to your reporting framework project
projectConfiguration <- addBMLMPConfiguration(
  projectConfiguration,
  snapshotFile = file.path(projectConfiguration$modelFolder, "mySnapshot.json"),
  nameOfParameterIdentification = "myParameterIdentification"
)

# 2. Configure priors
configurePriors(
  projectConfiguration = projectConfiguration,
  dataObserved = dataObserved
)

# 3. Initialize optimization
myRun <- BMLMOptimization$new(
  projectConfiguration = projectConfiguration,
  runName = "myRun",
  scenarioList = scenarioList,
  dataObserved = dataObserved
)

# 4. Start optimization
myRun$startOptimization(
  method = "BFGS",
  control = list(maxit = 1000),
  withInternalOptimization = TRUE
)

# 5. Monitor progress
myRun$checkConvergence()
myRun$checkPredictedVsObserved()

# 6. Export results
myRun$exportResultAsPopulation(projectConfiguration)
```

## RStudio Add-in

The package includes an **Insert BMLM Template** add-in that allows you to easily insert a complete workflow template into your R scripts. Access it from the RStudio **Addins** menu.

## Getting Help

- **Configuration questions**: See the [Configuration vignette](https://www.open-systems-pharmacology.org/OSPSuite.BMLM/articles/02-configuration.html)
- **Optimization issues**: See the [Optimization vignette](https://www.open-systems-pharmacology.org/OSPSuite.BMLM/articles/03-optimization.html)
- **Mathematical background**: See the [Likelihood Components vignette](https://www.open-systems-pharmacology.org/OSPSuite.BMLM/articles/04-likelihood-components.html)
- **Function documentation**: Use `?BMLMOptimization` or `?configurePriors` in R
- **Package issues**: Visit the [GitHub repository](https://github.com/Open-Systems-Pharmacology/OSPSuite.BMLM)

## Contributing

We welcome contributions! Please feel free to submit issues or pull requests on [GitHub](https://github.com/Open-Systems-Pharmacology/OSPSuite.BMLM).

## License

GPL-2
