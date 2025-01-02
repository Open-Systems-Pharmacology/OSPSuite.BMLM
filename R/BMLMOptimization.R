#' @title BMLMOptimization
#' @docType class
#' @description An object storing configuration used project-wide
#' This class encapsulates the functionality for performing BMLM optimization.
#' It manages the configuration, output directories, logging, and optimization processes.
#' @export
BMLMOptimization <-  R6::R6Class(
  "BMLMOptimization",
  inherit = ospsuite.utils::Printable,
  cloneable = FALSE,
  # active---------------
  active = list(
    #' @field runName Identifier of optimization run.
    runName = function() {
        return(private$.runName)
    }
  ),
  # public---------------
  public = list(
    #' Initialize BMLMOptimization
    #' @param projectConfiguration A list containing project configuration details.
    #' @param runName A character string representing the name of the current run.
    #' @param scenarioList A list of scenarios to be optimized.
    #' @param dataObserved A data.table containing the observed data to be fitted.
    #' @param seed numeric value set random seed for startValue generation
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
      if (asReload){
        dtList <- loadListsForRun(private$outputDir,self$runName)
        private$logMessage('Reload Run')

      } else {

        callDetails <- paste("Configuration Call:\n",
                             "      BMLM Configuration file: ", basename(projectConfiguration$addOns$bMLMConfigurationFile), "\n",
                             "      scenarios: ", paste(names(scenarioList), collapse = ", "), "\n",
                             "      dataObserved: ", deparse(substitute(dataObserved)), "\n",
                             "      seed: ", seed, "\n",
                             sep = ""
        )
        private$logMessage(callDetails)

        dtList <- createDtList(projectConfiguration, scenarioList, dataObserved, seed)
        saveDataTablesAsCSV(dtList = dtList,outputDir = private$outputDir)

        private$setStatus(RUNSTATUS$initialized)
      }

      for (iList in names(dtList)){
        if ('categoricCovariate' %in% names(dtList[[iList]])){
          dtList[[iList]][,categoricCovariate := as.character(categoricCovariate)]
          dtList[[iList]][is.na(categoricCovariate),categoricCovariate := '']
        }
      }
      private$dtList <- dtList

    },
    #' Start the optimization process
    #'
    #' This method initiates the optimization process using the specified method and control parameters.
    #' Internally function `optim` is used, so please check help for more details
    #'
    #' @param projectConfiguration A list containing project configuration details.
    #' @param method A character string specifying the optimization method to be used (default is "BFGS").
    #' @param control A list of control parameters for the optimization process.
    #' @param simulationRunOptions Optional additional simulation run options.
    #' @param failValue A numeric value to set if evaluation of the objective function fails (default is 1e+10).
    #' @param lastStatusSavingIntervalInSecs An integer specifying the interval for saving the last status (default is 60 seconds).
    #' @param startInBackground A logical indicating whether to start the optimization in the background (default is TRUE).
    #' @param ... Additional arguments to be passed to the optimization function `optim`.
    #'
    #' @return An invisible reference to the BMLMOptimization object.
    startOptimization = function(projectConfiguration,
                                 method = "BFGS",
                                 control = list(),
                                 simulationRunOptions = NULL,
                                 failValue = 1e+10,
                                 lastStatusSavingIntervalInSecs = 60,
                                 startInBackground = TRUE,
                                 ...){

      if (private$status == RUNSTATUS$running) stop('Status is running, check for background jobs.')

      # Log start time
      startTime <- Sys.time()
      private$logMessage("Optimization started.")

      private$dtList[["iteration"]] <- 0
      private$dtList[["bestValue"]] <- Inf
      private$dtList[["NAcounter"]] <- 0
      if (private$status %in% c('stopped','finalized')) {
        optimStatus <- readRDS(file = file.path(private$outputDir, "bestOptimStatus.RDS"))
        private$dtList$input$param <- optimStatus$params
        private$dtList$iteration <- optimStatus$iteration
        private$dtList$NAcounter <- optimStatus$NAcounter
        private$dtList$bestValue <- -1 * sum(optimStatus$loglikelihoods)

        private$logMessage("Restart at best Result")
      }

      private$archivePreviousResults()


      # Capture additional arguments
      additionalArgs <- list(...)
      additionalArgsExpr <- sapply(names(additionalArgs), function(arg) {
        if (is.null(arg)) {
          return(deparse(additionalArgs[[arg]]))
        } else {
          return(paste(arg, "=", deparse(additionalArgs[[arg]]), sep = ""))
        }
      })

      callDetails <- paste("Function call:\n",
                           "      method: ", method, "\n",
                           "      control: ", deparse(substitute(control)), "\n",
                           "      failValue: ", failValue, "\n",
                           "      Additional arguments: ", paste(additionalArgsExpr, collapse = ", "), "\n",
                           sep = ""
      )
      private$logMessage(callDetails)


      # Perform optimization
      argListForJob <- c(
        list(
          dtList = private$dtList,
          scenarioList = private$scenarioList,
          simulationRunOptions = simulationRunOptions,
          method = method,
          control = control,
          outputDir = private$outputDir,
          startTime = startTime,
          lastStatusSavingIntervalInSecs = lastStatusSavingIntervalInSecs,
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
        rm(argListForJob,envir = .GlobalEnv)

      } else {
        private$setStatus(RUNSTATUS$running)
        # execute local
        do.call(what = optimizeParameters, args = argListForJob)

        private$cleanUpStatus()
      }

      return(invisible(self))
    },
    #' Clean up the status of the optimization run
    #'
    #' This method checks the status of the optimization run and updates it accordingly.
    #'
    #' @return An invisible reference to the BMLMOptimization object.
    cleanUpStatus = function(){
      # initialized ?
      necessaryFiles <- c("data.csv","mappedPaths.csv","prior.csv","startValues.csv")
      if (any(!file.exists(file.path(private$outputDir,necessaryFiles)))){
        message('Run is corrupt, as configuration is incomplete!')
        private$setStatus('corrupt')
        return(invisible())
      }

      # does it run in the background ?
      if (!is.null(private$jobId)) {
        jobStatus <- rstudioapi::jobGetState(private$jobId)
        if (jobStatus %in% c('running','queued')){
          message('Run is running as background job')
          private$setStatus(RUNSTATUS$running)
          return(invisible())
        }
      }

      # check if final result exist
      if (file.exists(file.path(private$outputDir,'result.RDS'))){
        message('Status of run is "finalized".')
        private$setStatus('finalized')
        return(invisible())
      }


      # check if temporary result exist
      if (file.exists(file.path(private$outputDir,'convergence.csv'))){
        message('Run has temporary results but was not finalized!')
        message('Before restarting the optimization please ensure that the job is not still running.')
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
        private$printClass()
        private$printLine("runName", self$runName)
        private$printLine("status", self$status)
        invisible(self)
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
    checkConvergence = function(nPoints = 200,
                                selectionMode = c('last','random','first')){

      # Check if the convergence CSV file exists
      if (!file.exists(file.path(private$outputDir,'convergence.csv'))) {
        message(paste('convergence.csv does not exist yet, please wait'))
        return(NULL)
      }

      # Read the convergence data from the CSV file
      dtConvergence <- fread(file.path(private$outputDir,'convergence.csv'),
                             colClasses = c('integer','double','double','double'))

      plotConvergence(dtConvergence,titletxt = private$runName,nPoints = nPoints,selectionMode = selectionMode)

    },
    #' This function creates ggplot objects to display the current best and start values of the fitted parameter.
    #' all values a display as relative between min and max value using defined scaling
    #'
    #' @param nCols An integer specifying the number of columns for the plot layout.
    #' @param nRows An integer specifying the number of rows for the plot layout.
    #' @param useInteractivePlots A Boolean, if TRUE plots will be generated with mouse overlay for absolute values.
    #' @param ...  arguments passed on to function plotParameterLimits
    checkParameterLimits = function(nCols = 2, nRows = 3,useInteractivePlots = FALSE,...){
      statusList <- private$updateParameterValues()
      if (is.null(statusList)) return(invisible())

      plotData <- preparePlotDataParameterValues(dtList <- private$dtList,
                                                 currentStatus = statusList$current,
                                                 bestStatus = statusList$best)

      plotParameterLimits(plotData, titeltxt = self$runName,
                                 nCols = nCols,
                                 nRows = nRows,
                                 useInteractivePlots = useInteractivePlots,
                                 ...)
    },
    #' This function creates ggplot objects to display the individual Values vs the fitted distributions.
    #'
    #' @param nCols An integer specifying the number of columns for the plot layout.
    #' @param nRows An integer specifying the number of rows for the plot layout.
    #' @param xScale character 'Linear' or 'Log', scale of x axis
    #' @param ... additionla arguments passed on to plotDistributions
    checkDistributions = function(nCols = 2, nRows = 3,xScale = unlist(SCALING),...){

      xScale <- match.arg(xScale)

      statusList <- private$updateParameterValues()
      if (is.null(statusList)) return(invisible())

      plotData <- preparePlotDataParameterValues(dtList <- private$dtList,
                                                 currentStatus = statusList$current,
                                                 bestStatus = statusList$best)

      plotDistributions(
        plotData = plotData,
        nCols = nCols,
        nRows = nRows,
        xScale = xScale,
        ...
      )

    },
    #' Create and Print Predicted vs Observed of best result
    #'
    #' This function generates a plot for each unique outputPathId in the provided dataset.
    #' Each plot displays predicted vs observed values and includes facets for scenario and group.
    #'
    #' @param addRegression A logical value indicating whether to add regression lines to the plot.
    #' @param xyScale A character string specifying the scale type for the x and y axes (default "Log").
    #' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 2)
    #' @param ... additional arguments passed on to ospsuite.plots::plotPredVsObs
    checkPredictedVsObserved = function(addRegression = TRUE, xyScale = unlist(SCALING), nCols = 2,...){
      dtRes <- updatePredictedValues()
      if (is.null(statusList)) return(invisible())


    }

  ),
  #private-----------------
  private = list(
    dtList = NULL,
    scenarioList = NULL,
    status = NULL,
    .runName = NULL,
    logFile = NULL,
    outputDir = NULL,
    jobId = NULL,
    manageOutputDirectory = function(projectConfiguration) {
      private$outputDir <- file.path(projectConfiguration$outputFolder, 'BMLM',self$runName)

      if (dir.exists( private$outputDir)) {
        private$loadStatus()

        message("Alert: The output folder for ", self$runName, " already exists!")
        if (private$status == RUNSTATUS$running){
          stop("Status is Running.
               Please check if a background job is still running, otherwise reset status with 'cleanUpStatus()'")
        }
        asReload = TRUE

      } else {
        dir.create( private$outputDir, recursive = TRUE)
        message("Output directory created: ",  private$outputDir)
        asReload = FALSE
        private$logMessage(paste('Initialize Run',self$runName))
      }

      return(asReload)
    },
    #' Logs a message to the specified log file with a timestamp.
    logMessage = function(message) {
      cat(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        message,
        "\n",
        file = file.path(private$outputDir, "optimization_log.txt"),
        append = TRUE
      )
    },
    setStatus = function(status){
      private$status = status
      saveRDS(object = status,file = file.path(private$outputDir,'status.RDS'))

    },
    loadStatus = function(status){
      status = readRDS(file = file.path(private$outputDir,'status$RDS'))
      private$status = status
    },
    archivePreviousResults = function(){
      archiveDir <- file.path(private$outputDir,paste0('archive_',format(Sys.time(), "%Y-%m-%d_%H-%M-%S")))

      filesToCopy <- c('result.RDS','bestOptimStatus.RDS','convergence.csv')
      if (any(file.exists(file.path(private$outputDir,filesToCopy))))
        dir.create(archiveDir)

      for (fileToCopy in filesToCopy){
        if (file.exists(file.path(private$outputDir,fileToCopy)))
          file.copy(from = file.path(private$outputDir,fileToCopy),
                    to = file.path(archiveDir,fileToCopy))
      }

    },
    updateParameterValues = function(){

      if (any(!file.exists(file.path(private$outputDir, c('bestOptimStatus.RDS','optimStatus.RDS'))))){
        message('No results yet, did you start the job?')
        return(invisible())
      }
      currentStatus <- readRDS(file.path(private$outputDir, 'optimStatus.RDS'))
      private$printStatus(currentStatus,'current')

      bestStatus <- readRDS(file.path(private$outputDir, 'bestOptimStatus.RDS'))
      private$printStatus(bestStatus,'best')


      return(list(current = currentStatus,
                  best = bestStatus))

    },
    updatePredictedValues = function(){

      if (!file.exists(file.path(private$outputDir, c('bestPrediction.RDS')))){
        message('No results yet.')
        return(invisible())
      }
      dtRes <- readRDS(file.path(private$outputDir, 'bestPrediction.RDS'))

      return(dtRes)

    },
    printStatus = function(statusObject,statusName){
      cat(
        sprintf(
          '%s:\n    iteration: %d\n    objective function value: %.2f\n    percentage of failed function evaluations: %.2f\n',
          statusName,
          statusObject$iteration,
          -sum(
            statusObject$loglikelihood),
          statusObject$naCounter /
            statusObject$iteration * 100

        )
      )
    }
  )
)


