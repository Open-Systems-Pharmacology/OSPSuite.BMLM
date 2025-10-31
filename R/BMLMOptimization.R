#' @title BMLMOptimization
#' @docType class
#' @description An object storing configuration used project-wide
#' This class encapsulates the functionality for performing BMLM optimization.
#' It manages the configuration, output directories, logging, and optimization processes.
#'
#' @export
BMLMOptimization <- R6::R6Class(
  "BMLMOptimization",
  cloneable = FALSE,
  # active---------------
  active = list(
    #' @field runName Identifier of optimization run.
    runName = function() {
      return(private$.runName)
    },
    #' @field outputDir Identifier of optimization run.
    outputDir = function(value) {
      if (missing(value)) {
        value <- private$.outputDir
      }
      private$.outputDir <- value
      return(value)
    }
  ),
  # public---------------
  public = list(

    #' Initialize BMLMOptimization
    #'
    #' This method initializes the BMLMOptimization object with the provided project configuration,
    #' run name, scenario list, observed data, and random seed.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
    #' @param runName A character string representing the name of the current run.
    #' @param scenarioList A list of scenarios to be optimized.
    #' @param dataObserved A data.table containing the observed data to be fitted.
    #' @param seed A numeric value to set the random seed for start value generation (default is 1234).
    #'
    #' @return An invisible reference to the BMLMOptimization object.
    initialize = function(projectConfiguration,
                          runName,
                          scenarioList,
                          dataObserved,
                          seed = 1234) {
      # Check BMLM Configuration
      if (is.null(projectConfiguration$addOns$bMLMConfigurationFile)) {
        stop("Project configuration has no BMLM Configuration attached!")
      }
      checkmate::assertFileExists(projectConfiguration$addOns$bMLMConfigurationFile)

      # check and set Properties
      checkmate::assertString(runName)

      private$.runName <- runName

      # Create output directory and handle user confirmation
      asReload <- private$manageOutputDirectory(projectConfiguration)

      # Initialize data list
      private$scenarioList <- scenarioList
      if (any(lapply(scenarioList, "getElement", "scenarioType") == "Individual")) {
        stop(paste("Please use only scenarios for virtual twin populations! Check",
          paste(names(scenarioList)[lapply(scenarioList, "getElement", "scenarioType") == "Individual"]),
          collapse = ", "
        ))
      }


      if (asReload) {
        dtList <- loadListsForRun(self$outputDir, self$runName)
      } else {
        callDetails <- paste("Configuration Call:\n",
          "      BMLM Configuration file: ", basename(projectConfiguration$addOns$bMLMConfigurationFile), "\n",
          "      scenarios: ", paste(names(scenarioList), collapse = ", "), "\n",
          "      dataObserved: ", deparse(substitute(dataObserved)), "\n",
          "      seed: ", seed, "\n",
          sep = ""
        )
        logAndDisplayOptimization(message = callDetails, outputDir = self$outputDir)

        dtList <- createDtList(projectConfiguration, scenarioList, dataObserved, seed)
        saveDataTablesAsCSV(dtList = dtList, outputDir = self$outputDir)

        private$setStatus(RUNSTATUS$initialized)
      }

      for (iList in names(dtList)) {
        if ("categoricCovariate" %in% names(dtList[[iList]])) {
          dtList[[iList]][, categoricCovariate := as.character(categoricCovariate)]
          dtList[[iList]][is.na(categoricCovariate), categoricCovariate := ""]
        }
      }
      private$dtList <- dtList
    },
    #' Clean up the status of the optimization run
    #'
    #' This method checks the status of the optimization run and updates it accordingly.
    #'
    #' @return An invisible reference to the BMLMOptimization object.
    cleanUpStatus = function() {
      # initialized ?
      necessaryFiles <- c("data.csv", "mappedPaths.csv", "prior.csv", "startValues.csv")
      if (any(!file.exists(file.path(self$outputDir, necessaryFiles)))) {
        message("Run is corrupt, as configuration is incomplete!")
        private$setStatus("corrupt")
        return(invisible())
      }

      # does it run in the background ?
      if (!is.null(private$jobId)) {
        jobStatus <- rstudioapi::jobGetState(private$jobId)
        if (jobStatus %in% c("running", "queued")) {
          message("Run is running as background job")
          private$setStatus(RUNSTATUS$running)
          return(invisible())
        }
      }

      # check if final result exist
      if (file.exists(file.path(self$outputDir, "result.RDS"))) {
        message('Status of run is "finalized".')
        private$setStatus("finalized")
        return(invisible())
      }


      # check if temporary result exist
      if (file.exists(file.path(self$outputDir, "convergence.csv"))) {
        message("Run has temporary results but was not finalized!")
        message("Before restarting the optimization please ensure that the job is not still running.")
        private$setStatus(RUNSTATUS$stopped)
        return(invisible())
      }

      # if nothing else it mus be Initialized
      message('Status of run is "initialized".')
      private$setStatus(RUNSTATUS$initialized)
      return(invisible())
    },
    #' Print properties and status of the BMLMOptimization object
    #'
    #' @description  This method prints the current properties and status of the BMLMOptimization object to the console.
    print = function() {
        ospsuite.utils::ospPrintClass(self)
        ospsuite.utils::ospPrintItems(list(
          Run = self$runName,
          Status = self$status
        ))
      invisible(self)
    },
    #' This function exports individual results to a PKML file for a specified individual ID across scenarios.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including paths for saving PKML files.
    #' @param individualId A string representing the ID of the individual whose results will be exported.
    exportIndividualResultsToPkml = function(projectConfiguration, individualId) {
      individualId <- as.character(individualId)
      checkmate::assertCharacter(individualId, len = 1)
      checkmate::assertNames(individualId, subset.of = unique(private$dtList$data$individualId))


      statusList <- private$loadOptimStatusList(statusTypes = "best")
      if (is.null(statusList)) {
        message("Export is done with initial values.")
        scalingMethod <- SCALINGMETHOD$hardBounds
        params <- getParams(
          dtPrior = private$dtList$prior,
          dtStartValues = private$dtList$startValues,
          optimizationGroup = "both",
          scalingMethod = scalingMethod
        )
      } else {
        params <- statusList$best$params
        scalingMethod <- statusList$best$scalingMethod
      }
      private$dtList <- setParameterToTables(
        dtList = private$dtList,
        params = params,
        scalingMethod = scalingMethod
      )


      exportIndividualResultsToPkml(
        projectConfiguration = projectConfiguration,
        scenarioList = private$scenarioList,
        dtList = private$dtList,
        outputDir = self$outputDir,
        individualId = individualId
      )

      return(invisible())
    },
    #' This function exports optimized population data to CSV files for each scenario in the scenario list.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including paths for saving population files.
    #' @param addAsNewScenarios A boolean indicating whether to add new scenarios to the workbook.
    #' @param overwrite A boolean indicating whether to overwrite existing files.
    exportResultAsPopulation = function(projectConfiguration, addAsNewScenarios = TRUE, overwrite = FALSE) {
      statusList <- private$loadOptimStatusList(statusTypes = "best")
      if (is.null(statusList)) {
        return(invisible())
      }

      private$dtList <- setParameterToTables(
        dtList = private$dtList,
        params = statusList$best$params,
        scalingMethod = statusList$best$scalingMethod
      )

      exportOptimizedPopulation(
        projectConfiguration = projectConfiguration,
        dtList = private$dtList,
        scenarioList = private$scenarioList,
        runName = self$runName,
        addAsNewScenarios = addAsNewScenarios,
        overwrite = overwrite
      )
    },
    #' This function exports individual values from a given data table to a specified
    #' configuration table in an Excel workbook.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including the path to the Excel file.
    exportIndividualValuesToConfigTable = function(projectConfiguration) {
      statusList <- private$loadOptimStatusList(statusTypes = "best")
      if (is.null(statusList)) {
        return(invisible())
      }

      private$dtList <- setParameterToTables(
        dtList = private$dtList,
        params = statusList$best$params,
        scalingMethod = statusList$best$scalingMethod
      )

      exportIndividualValuesToConfigTable(
        projectConfiguration = projectConfiguration,
        scenarioList = private$scenarioList,
        dtList = private$dtList
      )
    },
    #' This function exports global parameters from the provided data table to a new sheet in the model parameters Excel file.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including the path to the model parameters file.
    #' @param sheetName A string representing the name of the exported sheets. If NULL The name of the run (myRun$runName) is sued
    #' @param overwrite A boolean indicating whether to overwrite an existing sheet.
    exportModelParametersToConfigTables = function(projectConfiguration,
                                                   sheetName = NULL,
                                                   overwrite = FALSE) {
      statusList <- private$loadOptimStatusList(statusTypes = "best")
      if (is.null(statusList)) {
        return(invisible())
      }

      private$dtList <- setParameterToTables(
        dtList = private$dtList,
        params = statusList$best$params,
        scalingMethod = statusList$best$scalingMethod
      )

      exportModelParametersToConfigTables(
        projectConfiguration = projectConfiguration,
        dtList = private$dtList,
        sheetName = ifelse(is.null(sheetName),self$runName,sheetName),
        overwrite = overwrite
      )

      return(invisible(NULL))
    },
    #' This function exports global parameters from the provided data table to a new sheet in the model parameters Excel file.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including the path to the model parameters file.
    #' @param dtList A list of data.tables containing the prior values.
    #' @param runName A string representing the name of the run.
    #' @param overwrite A boolean indicating whether to overwrite an existing sheet.
    exportHyperParametersToConfigTables = function(projectConfiguration, overwrite = FALSE) {
      statusList <- private$loadOptimStatusList(statusTypes = "best")
      if (is.null(statusList)) {
        return(invisible())
      }

      private$dtList <- setParameterToTables(
        dtList = private$dtList,
        params = statusList$best$params,
        scalingMethod = statusList$best$scalingMethod
      )

      exportHyperParametersToConfigTables(
        projectConfiguration = projectConfiguration,
        dtList = private$dtList,
        runName = self$runName,
        overwrite = overwrite
      )

      return(invisible(NULL))
    },
    #' This function saves final values from a provided data table to specified sheets in an Excel workbook.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including paths to Excel files.
    exportFinalValuesToBMLConfigTable = function(projectConfiguration) {
      statusList <- private$loadOptimStatusList(statusTypes = "best")
      if (is.null(statusList)) {
        return(invisible())
      }

      private$dtList <- setParameterToTables(
        dtList = private$dtList,
        params = statusList$best$params,
        scalingMethod = statusList$best$scalingMethod
      )

      saveFinalValuesToTables(
        projectConfiguration = projectConfiguration,
        dtList = private$dtList
      )
    },
    #' This function retrieves the configuration table from the specified sheet in the Excel workbook and add the finalValues
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including paths to Excel files.
    #' @param sheetName A character string specifying the name of the sheet to retrieve data from. Options are 'Prior' or 'IndividualStartValues'.
    getCurrentConfigTable = function(projectConfiguration, sheetName = c("Prior", "IndividualStartValues")) {
      sheetName <- match.arg(sheetName)

      statusList <- private$loadOptimStatusList(statusTypes = "best")
      if (is.null(statusList)) {
        return(invisible())
      }

      private$dtList <- setParameterToTables(
        dtList = private$dtList,
        params = statusList$best$params,
        scalingMethod = statusList$best$scalingMethod
      )

      dt <- getCurrentConfigTable(
        projectConfiguration = projectConfiguration,
        dtList = private$dtList,
        sheetName = sheetName
      )

      return(dt)
    },
    #' Check Residuals as QQ Plot
    #'
    #' This method generates a QQ plot to assess the normality of the residuals.
    #'
    #' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 2)
    #' @param filteroutputPathId vector with outputpathsIds to plot, if NULL (default) all are selected
    #' @param filterScenarioName vector with scenarios to plot, if NULL (default) all are selected
    #' @param excludeCensored A logical value indicating whether to exclude censored data (default is FALSE).
    #' @param ... Additional arguments passed to the plot function.
    #'
    #' @return An invisible reference to the BMLMOptimization object.
    checkResidualsAsQQ = function(nCols = 2,
                                  excludeCensored = TRUE,
                                  filteroutputPathId = NULL,
                                  filterScenarioName = NULL,
                                  ...) {
      dtRes <-
        private$updatePredictedValues(
          filteroutputPathId = filteroutputPathId,
          filterScenarioName = filterScenarioName
        )
      if (is.null(dtRes)) {
        return(invisible(list()))
      }

      plotList <- plotResidualLoop(dtRes = dtRes,
                                   plotFunction = plotResidualsAsQQ,
                                   excludeCensored = excludeCensored,
                                   nCols = nCols,
                                   titeltxt = self$runName, ...)

      print(plotList)
      return(invisible(plotList))
    },
    #' Check Residuals vs Time
    #'
    #' This method generates residuals vs time plots to visualize the residuals over time.
    #'
    #' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 2)
    #' @param filteroutputPathId vector with outputpathsIds to plot, if NULL (default) all are selected
    #' @param filterScenarioName vector with scenarios to plot, if NULL (default) all are selected
    #' @param excludeCensored A logical value indicating whether to exclude censored data (default is FALSE).
    #' @param ... Additional arguments passed to the plot function.
    #'
    #' @return An invisible reference to the BMLMOptimization object.
    checkResidualsVsTime = function(nCols = 2,
                                    filteroutputPathId = NULL,
                                    filterScenarioName = NULL,
                                    excludeCensored = TRUE,
                                    ...) {
      dtRes <-
        private$updatePredictedValues(
          filteroutputPathId = filteroutputPathId,
          filterScenarioName = filterScenarioName
        )
      if (is.null(dtRes)) {
        return(invisible(list()))
      }

      plotList <- plotResidualLoop(dtRes = dtRes,
                                   plotFunction = plotResidualsVsTime,
                                   excludeCensored = excludeCensored,
                                   nCols = nCols,
                                   titeltxt = self$runName, ...)

      print(plotList)
      return(invisible(plotList))
    },
    #' Check Residuals as Distribution
    #'
    #' This method generates histogram plots of the residuals to visualize their distribution.
    #'
    #' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 2)
    #' @param filteroutputPathId vector with outputpathsIds to plot, if NULL (default) all are selected
    #' @param filterScenarioName vector with scenarios to plot, if NULL (default) all are selected
    #' @param excludeCensored A logical value indicating whether to exclude censored data (default is FALSE).
    #' @param ... Additional arguments passed to the plot function.
    #'
    #' @return An invisible reference to the BMLMOptimization object.
    checkResidualDistribution = function(nCols = 2,
                                         filteroutputPathId = NULL,
                                         filterScenarioName = NULL,
                                         excludeCensored = TRUE,
                                         ...) {
      dtRes <-
        private$updatePredictedValues(
          filteroutputPathId = filteroutputPathId,
          filterScenarioName = filterScenarioName
        )
      if (is.null(dtRes)) {
        return(invisible(list()))
      }

      plotList <- plotResidualLoop(dtRes = dtRes,
                                   plotFunction = plotResidualsDistribution,
                                   excludeCensored = excludeCensored,
                                   nCols = nCols,
                                   titeltxt = self$runName, ...)

      print(plotList)
      return(invisible(plotList))
    },
    #' Check Residuals as Histogram
    #'
    #' This method generates histogram plots of the residuals to visualize their distribution.
    #'
    #' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 2)
    #' @param filteroutputPathId vector with outputpathsIds to plot, if NULL (default) all are selected
    #' @param filterScenarioName vector with scenarios to plot, if NULL (default) all are selected
    #' @param excludeCensored A logical value indicating whether to exclude censored data (default is FALSE).
    #' @param ... Additional arguments passed to the plot function.
    #'
    #' @return An invisible reference to the BMLMOptimization object.
    checkResidualsAsHistogram = function(nCols = 2,
                                         filteroutputPathId = NULL,
                                         filterScenarioName = NULL,
                                         excludeCensored = TRUE,
                                         ...) {
      dtRes <-
        private$updatePredictedValues(
          filteroutputPathId = filteroutputPathId,
          filterScenarioName = filterScenarioName
        )
      if (is.null(dtRes)) {
        return(invisible(list()))
      }

      plotList <- plotResidualLoop(dtRes = dtRes,
                                   plotFunction = plotResidualsAsHistogram,
                                   excludeCensored = excludeCensored,
                                   nCols = nCols,
                                   titeltxt = self$runName, ...)

      print(plotList)
      return(invisible(plotList))
    },
    #' Create and Print Predicted vs Observed of best result
    #'
    #' This function generates a plot for each unique outputPathId in the provided dataset.
    #' Each plot displays predicted vs observed values and includes facets for scenario and group.
    #'
    #' @param addRegression A logical value indicating whether to add regression lines to the plot.
    #' @param xyScale A character string specifying the scale type for the x and y axes (default "log").
    #' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 2)
    #' @param filteroutputPathId vector with outputpathsIds to plot, if NULL (default) all are selected
    #' @param filterScenarioName vector with scenarios to plot, if NULL (default) all are selected
    #' @param excludeCensored A logical value indicating whether to exclude censored data (default is FALSE).
    #' @param ... additional arguments passed on to ospsuite.plots::plotPredVsObs
    checkPredictedVsObserved = function(addRegression = TRUE, xyScale = unlist(SCALING), nCols = 2,
                                        filteroutputPathId = NULL,
                                        filterScenarioName = NULL,
                                        excludeCensored = FALSE,
                                        ...) {
      dtRes <- private$updatePredictedValues(
        filteroutputPathId = filteroutputPathId,
        filterScenarioName = filterScenarioName
      )
      if (is.null(dtRes)) {
        return(invisible(list()))
      }

      plotList <- plotResidualLoop(
        dtRes = dtRes,
        plotFunction = plotPredictedVsObserved,
        addRegression = addRegression,
        xyScale = xyScale,
        nCols = nCols,
        titeltxt = self$runName,
        excludeCensored = excludeCensored,
        ...
      )

      print(plotList)
      return(invisible(plotList))
    },
    #' Check Predicted vs Time
    #'
    #' This method generates plots to compare predicted values against time.
    #'
    #' @param dtRes A data frame containing the data to be plotted.
    #' @param yScale A character string specifying the scale for the y-axis (default is "log").
    #' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 4)
    #' @param filteroutputPathId vector with outputpathsIds to plot, if NULL (default) all are selected
    #' @param filterScenarioName vector with scenarios to plot, if NULL (default) all are selected
    #' @param ... Additional arguments passed to the plot function.
    #'
    #' @return An invisible reference to the BMLMOptimization object.
    checkPredictedVsTime = function(yScale = unlist(SCALING),
                                    nCols = 4,
                                    filteroutputPathId = NULL,
                                    filterScenarioName = NULL,
                                    ...) {
      dtRes <- private$updatePredictedValues(
        filteroutputPathId = filteroutputPathId,
        filterScenarioName = filterScenarioName
      )
      if (is.null(dtRes)) {
        return(invisible(list()))
      }
      plotList <- plotPredictedVsTime(
        dtRes = dtRes,
        yScale = yScale,
        nCols = nCols,
        titeltxt = self$runName,
        ...
      )

      print(plotList)

      return(invisible(plotList))
    },
    #' Plot Correlations
    #' This function generates a correlation matrix plot for the specified status and checks
    #' for relevant columns in the population data.
    #'
    #' @param dtList A list containing prior and start values data tables.
    #' @param statusList A list containing the current and best parameter values.
    #' @param titeltxt A string to include in the plot title.
    #' @param method A character string specifying the correlation method to use (default is 'spearman').
    #' @param statusToShow A character string indicating which status to show. Options are 'best', 'current', and 'start'.
    #' @param scenarioList A list of scenarios to analyze.
    #' @param corCut A numeric value for the correlation cutoff threshold. Default is 0.5.
    #' @param pValueCut A numeric value for the Kruskal-Wallis test cutoff threshold. Default is 0.1.
    #' @param nPlotsPopulation Number of plots in one figure for correlations with population
    checkCorrelations = function(method = "spearman",
                                 statusToShow = c("best", "current", "start"),
                                 corCut = 0.5,
                                 pValueCut = 0.1,
                                 nPlotsPopulation = 12) {
      statusList <- private$loadOptimStatusList()
      if (is.null(statusList)) {
        return(invisible(list()))
      }

      plotList <- plotCorrelations(
        dtList = private$dtList,
        statusList = statusList,
        titeltxt = self$runName,
        statusToShow = statusToShow,
        scenarioList = private$scenarioList,
        corCut = corCut,
        pValueCut = pValueCut
      )

      if (is.null(sys.call(-1))) {
        print(plotList[[1]])
        iPlot <- 2
        while (iPlot <= length(plotList)) {
          maxPlot <- min(length(plotList), iPlot + nPlotsPopulation - 1)
          print(addWatermark(cowplot::plot_grid(plotlist = plotList[seq(iPlot, maxPlot)])))
          iPlot <- iPlot + nPlotsPopulation
        }
      }
      return(invisible(plotList))
    },
    #' This function creates ggplot objects to display the parameter Values vs the prior distributions.
    #'
    #' @param xScale character 'linear' or 'log', scale of x axis
    #' @param ... additional arguments passed on to plotDistributions
    checkParameterValuesVsPrior = function( xScale = unlist(SCALING), ...) {
      xScale <- match.arg(xScale)

      statusList <- private$loadOptimStatusList()
      if (is.null(statusList)) {
        return(invisible(list()))
      }

      plotObject <- plotParameterValuesVsPrior(
        dtList = private$dtList[c('prior','startValues')],
        statusList,
        xScale = xScale,
        titeltxt = self$runName,
        ...
      )

      print(plotObject)
      return(invisible(plotObject))
    },
    #' This function creates ggplot objects to display the parameter Values vs the prior distributions.
    #'
    #' @param nCols An integer specifying the number of columns for faceting. Default is 2.
    #' @param nRows An integer specifying the maximum number of rows for faceting. Default is 3.
    #' @param xyScale A character string specifying the scale of the x- and y-axis. Default is 'log'.
    #' @param parameterFilter A character vector for filtering parameter names. Default is NULL.
    #' @param ... additional arguments passed on to plotBestVsStartParameter
    checkBestVsStartParameter = function( xyScale = unlist(SCALING),
                                          nCols = 2,
                                          nRows = 3,
                                          parameterFilter,
                                          ...) {
      xyScale <- match.arg(xyScale)

      statusList <- private$loadOptimStatusList()
      if (is.null(statusList)) {
        return(invisible(list()))
      }

      plotObject <- plotBestVsStartParameter(
        dtList = private$dtList[c('prior','startValues')],
        statusList,
        titeltxt = self$runName,
        nCols = nCols,
        nRows = nRows,
        xyScale = xyScale,
        parameterFilter = parameterFilter,
        ...
      )

      print(plotObject)
      return(invisible(plotObject))
    },
    #' This function creates ggplot objects to display the current best and start values of the fitted parameter.
    #' all values a display as relative between min and max value using defined scaling
    #'
    #' @param nCols An integer specifying the number of columns for the plot layout.
    #' @param nRows An integer specifying the number of rows for the plot layout.
    #' @param ...  arguments passed on to function plotParameterLimits
    checkParameterLimits = function(nCols = 2, nRows = 3, ...) {
      statusList <- private$loadOptimStatusList()
      if (is.null(statusList)) {
        return(invisible(list()))
      }

      plotList <- plotParameterLimits(
        dtList = private$dtList[c('prior','startValues')],
        statusList = statusList,
        titeltxt = self$runName,
        nCols = nCols,
        nRows = nRows,
        ...
      )

      print(plotList)
      return(invisible(plotList))
    },
    #' This function creates ggplot objects to display the individual Values vs the fitted distributions.
    #'
    #' @param nCols An integer specifying the number of columns for the plot layout.
    #' @param nRows An integer specifying the number of rows for the plot layout.
    #' @param parameterFilter A character vector for filtering parameter names. Default is NULL.
    #' @param zoomOnData A logical value indicating whether to zoom in on data. Default is FALSE.
    #' @param xScale character 'linear' or 'log', scale of x axis
    #' @param ... additional arguments passed on to plotDistributions
    checkDistributions = function(nCols = 2, nRows = 3, xScale = unlist(SCALING),
                                  parameterFilter = NULL,
                                  zoomOnData = FALSE,
                                  ...) {
      xScale <- match.arg(xScale)

      statusList <- private$loadOptimStatusList()
      if (is.null(statusList)) {
        return(invisible(list()))
      }

      plotList <- plotDistributions(
        dtList = private$dtList[c('prior','startValues')],
        statusList = statusList,
        nCols = nCols,
        nRows = nRows,
        xScale = xScale,
        titeltxt = self$runName,
        parameterFilter = parameterFilter,
        zoomOnData = zoomOnData,
        ...
      )

      print(plotList[[1]])
      return(invisible(plotList))
    },

    #' Check Convergence of Model Parameters
    #'
    #' This function checks the convergence of model parameters by reading a CSV file
    #' containing convergence data and visualizing the results using ggplot2.
    #'
    #' @param nPoints An integer specifying the number of points to select for plotting.
    #' Default is 200.
    #' @param selectionMode A character string indicating the mode of selection for points.
    #' Options are 'last'  'random', and 'first' (the first nPoints). Default is 'last'.
    #' @param displayVariablesIndx An integer vector specifying which variables to display.
    #' 1 = 'value of objective function: -loglikelihood',
    #' 2 = '- loglikelihood TimeProfile', 3 = '- loglikelihood HyperParameter',
    #' 4 ='- loglikelihood Prior',  5 = 'percentage of failed iterations' and
    #' 6 = 'percentage of iterations with parameters outside range',
    #' if NULL (default) the columns with entries != 0 are selected.
    checkConvergence = function(displayVariablesIndx = NULL,
                                nPoints = 200,
                                selectionMode = c("last", "random", "first")) {
      # Check if the convergence CSV file exists
      if (!file.exists(file.path(self$outputDir, "convergence.csv"))) {
        message(paste("convergence.csv does not exist yet, please wait"))
        return(invisible(list()))
      }

      # Read the convergence data from the CSV file
      dtConvergence <- fread(file.path(self$outputDir, "convergence.csv"),
        colClasses = c("integer", "double", "double", "double", "integer", "integer", "character")
      )

      # Print the first few rows of the sorted convergence data
      nPointsAvailable <- nrow(dtConvergence[event == "best"])
      setorderv(dtConvergence, "iteration", -1)
      print(head(dtConvergence[event == "best"] %>%
        dplyr::select(!any_of("event")), 5))

      plotList <- plotConvergence(
        dtConvergence,
        titletxt = self$runName,
        nPoints = nPoints,
        selectionMode = selectionMode,
        displayVariablesIndx = displayVariablesIndx
      )

      print(plotList)
      return(invisible(plotList))
    },
    #' Check Initial Values for Project Configuration
    #'
    #' This function evaluates initial values for a given project configuration by running simulations for specified scenarios.
    #' It logs the progress and results to both the console and a designated log file.
    #'
    #' @param failValue A numeric value to set if evaluation of the objective function fails.
    #'
    #' @return NULL This function does not return any value but logs information to the console and a log file.
    evaluateInitialValues = function() {
      evaluateInitialValues(
        dtList = private$dtList,
        outputDir = self$outputDir,
        scenarioList = private$scenarioList
      )
    },
    #' This method initiates the optimization process using the specified method and control parameters.
    #' Internally, the function `optim` is used, so please check the help for more details.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
    #' @param method A character string specifying the optimization method to be used (default is "BFGS").
    #' @param control A list of control parameters for the optimization process.
    #' @param scalingMethod A character string specifying the scaling method.
    #' @param simulationRunOptions Optional additional simulation run options.
    #' @param failValue A numeric value to set if evaluation of the objective function fails (default is 1e+10).
    #' @param lastStatusSavingIntervalInSecs An integer specifying the interval for saving the last status (default is 60 seconds).
    #' @param startInBackground A logical indicating whether to start the optimization in the background (default is TRUE).
    #' @param withInternalOptimization A logical indicating whether to perform internal optimization (default is FALSE).
    #' @param ... Additional arguments to be passed to the optimization function `optim`.
    #'
    #' @return An invisible reference to the BMLMOptimization object.
    startOptimization = function(projectConfiguration,
                                 method = "BFGS",
                                 control = list(),
                                 simulationRunOptions = NULL,
                                 failValue = 1e+10,
                                 scalingMethod = unlist(SCALINGMETHOD),
                                 lastStatusSavingIntervalInSecs = 60,
                                 withInternalOptimization = FALSE,
                                 startInBackground = TRUE,
                                 ...) {
      if (private$status == RUNSTATUS$running) {
        stop("Status is running!
             Please check if a background job is still running, otherwise reset status with 'cleanUpStatus()'")
      }
      scalingMethod <- match.arg(scalingMethod)
      scalingMethod <- unname(unlist(scalingMethod))

      # Log start time
      startTime <- Sys.time()
      logAndDisplayOptimization("Optimization started.", outputDir = self$outputDir)

      private$dtList[["iteration"]] <- 0
      private$dtList[["bestValue"]] <- Inf
      private$dtList[["NAcounter"]] <- 0
      private$dtList[["outsideRangeCounter"]] <- 0
      private$dtList$scalingMethod <- scalingMethod
      if (private$status %in% c("stopped", "finalized")) {
        private$archivePreviousResults()
        optimStatus <- readRDS(file = file.path(self$outputDir, "bestOptimStatus.RDS"))
        private$dtList <- setParameterToTables(
          dtList = private$dtList,
          params = optimStatus$params,
          scalingMethod = optimStatus$scalingMethod
        )
        private$dtList$iteration <- optimStatus$iteration
        private$dtList$NAcounter <- optimStatus$NAcounter
        private$dtList$outsideRangeCounter <- optimStatus$outsideRangeCounter
        private$dtList$bestValue <- -1 * sum(optimStatus$loglikelihoods)

        fwrite(
          as.data.table(as.list(
            c(
              iteration = optimStatus$iteration,
              c(
                logTimeProfile = NA,
                logHyperParameter = NA,
                logPrior = NA
              ),
              NAcounter = NA,
              outsideRangeCounter = NA,
              event = "restart"
            )
          )),
          file = file.path(self$outputDir, "convergence.csv"),
          append = TRUE
        )
        logAndDisplayOptimization(paste("Restart at best Result at iteration", optimStatus$iteration),
          outputDir = self$outputDir
        )
      }
      private$setStatus(RUNSTATUS$running)


      # Capture additional arguments
      additionalArgs <- list(...)
      additionalArgsExpr <- sapply(names(additionalArgs), function(arg) {
        if (is.null(arg)) {
          return(deparse(additionalArgs[[arg]]))
        } else {
          return(paste(arg, "=", deparse(additionalArgs[[arg]]), sep = ""))
        }
      })

      logAndDisplayOptimization(
        paste("Function call:\n",
          "      method: ", method, "\n",
          "      control: ", deparse(substitute(control)), "\n",
          "      failValue: ", failValue, "\n",
          "      scalingMethod: ", scalingMethod,
          sep = ""
        ),
        outputDir = self$outputDir
      )
      logAndDisplayOptimization(
        ifelse(length(additionalArgsExpr) > 0,
          paste("      Additional arguments: ", paste(additionalArgsExpr, collapse = ", "), "\n",
            sep = ""
          ),
          "\n"
        ),
        outputDir = self$outputDir,
        prependTimestamp = FALSE
      )



      # Perform optimization
      argListForJob <- c(
        list(
          dtList = private$dtList,
          scenarioList = private$scenarioList,
          simulationRunOptions = simulationRunOptions,
          method = method,
          control = control,
          outputDir = self$outputDir,
          startTime = startTime,
          lastStatusSavingIntervalInSecs = lastStatusSavingIntervalInSecs,
          withInternalOptimization = withInternalOptimization,
          failValue = failValue
        ),
        list(...)
      )

      # Perform optimization
      if (startInBackground && interactive()) {
        private$setStatus(RUNSTATUS$running)

        # Assign argList to the global environment
        assign("argListForJob", argListForJob, envir = .GlobalEnv)

        # Run the script in the background
        private$jobId <- rstudioapi::jobRunScript(
          path = system.file("templates/runAsBackgroundJob.R", package = "ospsuite.bmlm"),
          name = self$runName,
          encoding = "UTF-8",
          workingDir = projectConfiguration$projectConfigurationDirPath,
          importEnv = TRUE
        )
        # cleanup
        rm(argListForJob, envir = .GlobalEnv)
      } else {
        private$setStatus(RUNSTATUS$running)
        # execute local
        do.call(what = optimizeParameters, args = argListForJob)

        self$cleanUpStatus()
      }

      return(invisible(self))
    },
    #' Opens logfile of the BMLMOptimization object
    #'
    #' @description  This method opens the current logfile
    openLogFile = function() {
      logfile <- file.path(self$outputDir, "optimization_log.txt")
      if (file.exists(logfile)) {
        file.edit(logfile)
      } else {
        message("no Log file exists")
      }
    }
  ),
  # private-----------------
  private = list(
    dtList = NULL,
    scenarioList = NULL,
    status = NULL,
    .runName = NULL,
    logFile = NULL,
    .outputDir = NULL,
    jobId = NULL,
    manageOutputDirectory = function(projectConfiguration) {
      self$outputDir <- file.path(projectConfiguration$outputFolder, "BMLM", self$runName)

      if (dir.exists(self$outputDir)) {
        if (!file.exists(file.path(self$outputDir, "status.RDS"))) {
          # Prompt the user for confirmation to reset the run directory
          response <- readline(prompt = paste(
            "Output directory exists already but does not contain all mandatory files.",
            "\nDid it crash during the last initialisation?",
            "\nDo you want to reset everything in the run directory? (Yes/No): "
          ))
          if (tolower(response) == "yes") {
            logAndDisplayOptimization(paste("Reset Run", self$runName),
              outputDir = self$outputDir
            )
            asReload <- FALSE
            return(asReload)
          } else {
            stop("Execution stopped by user.")
          }
        }

        private$loadStatus()

        message("Alert: The output folder for ", self$runName, " already exists! Status: ", private$status)
        if (private$status == RUNSTATUS$running) {
          message("Please check if a background job is still running, otherwise reset status with 'cleanUpStatus()'")
        }
        asReload <- TRUE
      } else {
        dir.create(self$outputDir, recursive = TRUE)
        message("Output directory created: ", self$outputDir)
        asReload <- FALSE
        logAndDisplayOptimization(message = paste("Initialize Run", self$runName), outputDir = self$outputDir)
      }

      return(asReload)
    },
    setStatus = function(status) {
      private$status <- status
      saveRDS(object = status, file = file.path(self$outputDir, "status.RDS"))
    },
    loadStatus = function(status) {
      status <- readRDS(file = file.path(self$outputDir, "status.RDS"))
      private$status <- status
    },
    archivePreviousResults = function() {
      archiveDir <- file.path(self$outputDir, paste0("archive_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S")))

      filesToCopy <- c("result.RDS", "bestOptimStatus.RDS", "convergence.csv")
      if (any(file.exists(file.path(self$outputDir, filesToCopy)))) {
        dir.create(archiveDir)
      }

      for (fileToCopy in filesToCopy) {
        if (file.exists(file.path(self$outputDir, fileToCopy))) {
          file.copy(
            from = file.path(self$outputDir, fileToCopy),
            to = file.path(archiveDir, fileToCopy)
          )
        }
      }
    },
    loadOptimStatusList = function(statusTypes = c("best", "current")) {
      cat("load results for ", self$runName, "\n")

      statusTypes <- match.arg(statusTypes, several.ok = TRUE)

      statusFiles <- c(best = "bestOptimStatus.RDS", current = "optimStatus.RDS")

      if (any(!file.exists(file.path(self$outputDir, statusFiles[statusTypes])))) {
        message("No results yet, did you start the job?")
        return(invisible())
      }

      statusList <- list()

      for  (type in statusTypes) {
        statusList[[type]] <- readRDS(file.path(self$outputDir, statusFiles[type]))
        private$printStatus(statusList[[type]], type)
      }

      return(statusList)
    },
    updatePredictedValues = function(filteroutputPathId = NULL,
                                     filterScenarioName = NULL) {
      if (!file.exists(file.path(self$outputDir, c("bestPrediction.RDS")))) {
        message("No results yet.")
        return(invisible())
      }
      bestStatus <- readRDS(file.path(self$outputDir, "bestOptimStatus.RDS"))
      private$printStatus(bestStatus, "best")

      private$dtList <- setParameterToTables(
        dtList = private$dtList,
        params = bestStatus$params,
        scalingMethod = bestStatus$scalingMethod
      )
      dtRes <- readRDS(file.path(self$outputDir, "bestPrediction.RDS"))
      if (!is.data.frame(dtRes)) {
        dtRes <- rbindlist(dtRes)
      }
      dtRes <- updateModelError(dtPrior = private$dtList$prior, dtRes = dtRes)

      dtRes[, isCensored := !is.na(lloq) & lloq > yValues]

      # Apply the function to calculate likelihood
      dtRes[, resNorm := mapply(calculateResidual, yValues, predicted, errorModel, sigma, isCensored, lloq)]

      if (!is.null(filteroutputPathId)) {
        checkmate::assertNames(filteroutputPathId, subset.of = dtRes$outputPathId)
      }
      if (!is.null(filterScenarioName)) {
        checkmate::assertNames(filterScenarioName, subset.of = dtRes$scenarioName)
        dtRes <- dtRes[scenarioName %in% filterScenarioName]
      }
      if (!is.null(filteroutputPathId)) {
        dtRes <- dtRes[outputPathId %in% filteroutputPathId]
      }

      return(dtRes)
    },
    printStatus = function(statusObject, statusName) {
      cat(
        sprintf(
          paste("%s:\n",
                "    iteration: %d\n",
                "    objective function value: %.2f\n",
                "    percentage of failed function evaluations: %.2f\n",
                "    percentage of iterations with parameter outside range: %.2f\n"),
          statusName,
          statusObject$iteration,
          -sum(
            statusObject$loglikelihood
          ),
          statusObject$NAcounter /
            statusObject$iteration * 100,
          statusObject$outsideRangeCounter /
            statusObject$iteration * 100

        )
      )
    }
  )
)
