#' Start BMLM Optimization
#'
#' This function initiates the BMLM optimization process based on the provided configuration and parameters.
#' The core of the optimization is performed using the `optim` function, so for more help on additional parameters
#' and control options, users should refer to the documentation for `optim`.
#' The optimized parameters and the final value of the objective function are saved to an RDS file, along with
#' relevant log information in a text file.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details.
#' @param runName A character string representing the name of the current run.
#' @param scenarioList A list of scenarios to be optimized.
#' @param dataObserved A data.table containing the observed data to be fitted.
#' @param seed An integer for random number generation (default is 1234).
#' @param method A character string specifying the optimization method (default is "L-BFGS-B").
#' @param control A list of control parameters for the optimization function passed to `optim`.
#' @param simulationRunOptions A list of options for simulation runs. Specify the parameters for the simulation here.
#' @param restartAtBestValues A logical indicating whether to restart from the best values found (default is FALSE).
#' @param failValue A numeric value to set if evaluation of the objective function fails (default is -1e+10).
#' @param lastStatusSavingInterval An integer specifying the interval for saving the last status (default is 60 seconds).
#' @param startInBackground A logical indicating whether to start the optimization in the background (default is FALSE).
#' @param ... Additional arguments passed to the `optim`.
#'
#' @return Returns NULL invisibly after the optimization process is complete. This indicates that the function is primarily used for its side effects, such as saving results and logging.
#' @export
startBMLMOptimization <- function(projectConfiguration,
                                  runName,
                                  scenarioList,
                                  dataObserved,
                                  seed = 1234,
                                  method = "L-BFGS-B",
                                  control = list(),
                                  simulationRunOptions = NULL,
                                  restartAtBestValues = FALSE,
                                  failValue = 1e+10,
                                  lastStatusSavingInterval = 60,
                                  startInBackground = FALSE,
                                  ...) {
  # Check BMLM Configuration
  checkBMLMConfiguration(projectConfiguration)

  # Create output directory and handle user confirmation
  outputDir <- manageOutputDirectory(projectConfiguration, runName, restartAtBestValues)
  if (is.null(outputDir)) {
    return(invisible())
  }
  runName <- basename(outputDir)

  # Create log file
  logFile <- file.path(outputDir, "optimization_log.txt")

  # Log start time
  startTime <- Sys.time()
  # Save function call details
  logFunctionCall(
    logFile = logFile,
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioList,
    seed = seed,
    dataObservedExpr = deparse(substitute(dataObserved)),
    method = method,
    controlExpr = deparse(substitute(control)),
    restartAtBestValues = restartAtBestValues,
    failValue = failValue,
    startInBackground = startInBackground,
    ...
  )

  # Initialize data list
  dtList <- createDtList(projectConfiguration, scenarioList, dataObserved, seed)

  # Initialize optimization variables
  dtList <- initializeOptimization(restartAtBestValues, outputDir, dtList)

  # Perform optimization
  argList <- c(
    list(
      dtList = dtList,
      scenarioList = scenarioList,
      simulationRunOptions = simulationRunOptions,
      method = method,
      control = control,
      outputDir = outputDir,
      logFile = logFile,
      startTime = startTime,
      lastStatusSavingInterval = lastStatusSavingInterval,
      failValue = failValue
    ),
    list(...)
  )

  # Perform optimization
  if (startInBackground && interactive()) {
    # Assign argList to the global environment
    assign("argList", argList, envir = .GlobalEnv)

    # Run the script in the background
    rstudioapi::jobRunScript(
      path = system.file("templates/runAsBackgroundJob.R", package = "ospsuite.bmlm"),
      name = runName,
      encoding = "UTF-8",
      workingDir = ".",
      importEnv = TRUE
    )
  } else {
    # execute local
    do.call(what = optimizeParameters, args = argList)
  }

  return(invisible())
}


#' Check BMLM Configuration
#'
#' Validates the presence of the BMLM configuration file in the project configuration.
#'
#' @param projectConfiguration A list containing project configuration details.
#'
#' @return NULL if the configuration is valid; stops execution if invalid.
#' @keywords internal
checkBMLMConfiguration <- function(projectConfiguration) {
  if (is.null(projectConfiguration$addOns$bMLMConfigurationFile)) {
    stop("Project configuration has no BMLM Configuration attached!")
  }
  checkmate::assertFileExists(projectConfiguration$addOns$bMLMConfigurationFile)
}

#' Manage Output Directory
#'
#' This function manages the output directory for a specific run.
#' If `restartAtBestValues` is FALSE and the directory exists, a new directory
#' will be created with a timestamp appended to the run name.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing project configuration details.
#' @param runName A character string specifying the name of the run, which will
#'        be used to create the output folder.
#' @param restartAtBestValues A logical value indicating whether to restart at the
#'        best values. If FALSE and the directory exists, a new directory will be created
#'        with a timestamp appended to the run name.
#'
#' @return A character string containing the path to the output directory, or NULL
#'         if the operation is cancelled by the user.
#'
#' @keywords internal
manageOutputDirectory <- function(projectConfiguration, runName, restartAtBestValues) {
  outputDir <- getOutputDirectoryForRun(projectConfiguration, runName)

  if (dir.exists(outputDir)) {
    if (!restartAtBestValues) {
      message("Alert: The output folder for ", runName, " already exists!")

      newRunName <- paste0(runName, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
      cat("runName was changed to: ", newRunName, "\n")
      outputDir <- getOutputDirectoryForRun(projectConfiguration, newRunName)

      if (dir.exists(outputDir)) stop(paste("please check", outputDir))

      dir.create(outputDir, recursive = TRUE)
    }
  } else {
    dir.create(outputDir, recursive = TRUE)
    message("Output directory created: ", outputDir)
  }

  return(outputDir)
}


#' Log Message
#'
#' Logs a message to the specified log file with a timestamp.
#'
#' @param logFile A character string representing the path to the log file.
#' @param message A character string containing the message to log.
#'
#' @return NULL
#' @keywords internal
logMessage <- function(logFile, message) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message, "\n", file = logFile, append = TRUE)
}

#' Log Function Call
#'
#' Logs the details of the function call to the specified log file.
#'
#' @param logFile A character string representing the path to the log file.
#' @param projectConfiguration A list containing project configuration details.
#' @param scenarioList A list of scenarios to be optimized.
#' @param dataObservedExpr An expression representing the observed data; useful for logging purposes.
#' @param seed An integer for random number generation.
#' @param method A character string specifying the optimization method.
#' @param controlExpr A list of control parameters for the optimization function.
#' @param restartAtBestValues A logical indicating whether to restart from the best values found.
#' @param failValue A numeric value to set if evaluation of the objective function fails.
#' @param ... Additional arguments passed to the optimization function.
#'
#' @return NULL
#' @keywords internal
logFunctionCall <-
  function(logFile,
           projectConfiguration,
           scenarioList,
           dataObservedExpr,
           seed,
           method,
           controlExpr,
           restartAtBestValues,
           startInBackground,
           failValue,
           ...) {
    logMessage(logFile, "Optimization started.")

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
      "      BMLM Configuration file: ", basename(projectConfiguration$addOns$bMLMConfigurationFile), "\n",
      "      scenarios: ", paste(names(scenarioList), collapse = ", "), "\n",
      "      dataObserved: ", dataObservedExpr, "\n",
      "      seed: ", seed, "\n",
      "      method: ", method, "\n",
      "      control: ", controlExpr, "\n",
      "      restartAtBestValues: ", restartAtBestValues, "\n",
      "      failValue: ", failValue, "\n",
      "      startInBackground: ", startInBackground, "\n",
      "      Additional arguments: ", paste(additionalArgsExpr, collapse = ", "), "\n",
      sep = ""
    )

    logMessage(logFile, callDetails)
  }

#' Initialize Optimization
#'
#' Initializes optimization variables and loads previous results if restarting from the best values.
#'
#' @param restartAtBestValues A logical indicating whether to restart from the best values found.
#' @param outputDir A character string representing the path to the output directory.
#' @param dtList A list containing data tables for optimization.
#'
#' @return dtList with two added entries iteration adn bestValue
#'
#' @keywords internal
initializeOptimization <- function(restartAtBestValues, outputDir, dtList) {
  dtList[["iteration"]] <- 0
  dtList[["bestValue"]] <- Inf

  if (restartAtBestValues) {
    optimStatus <- readRDS(file = file.path(outputDir, "bestOptimStatus.RDS"))
    dtList$input$param <- optimStatus$params
    dtList$iteration <- optimStatus$iteration
    dtList$bestValue <- -1 * sum(optimStatus$loglikelihoods)
  }
  saveDataTablesAsCSV(dtList = dtList, outputDir = outputDir)

  return(dtList) # Initialize bestValue to Inf
}

#' Optimize Parameters
#'
#' Performs the optimization of parameters using the specified method and control settings.
#'
#' @param dtList A list containing data tables for optimization.
#' @param scenarioList A list of scenarios to be optimized.
#' @param simulationRunOptions A list of options for simulation runs.
#' @param method A character string specifying the optimization method.
#' @param control A list of control parameters for the optimization function.
#' @param outputDir A character string representing the path to the output directory.
#' @param lastStatusSavingInterval An integer specifying the interval for saving the last status.
#' @param bestValue A numeric value representing the current best value of optimization.
#' @param logFile A character string representing the file path for the log file.
#' @param startTime A POSIXct object representing the start time of the optimization process.
#' @param failValue A numeric value to set if evaluation of the objective function fails.
#' @param ... Additional arguments passed to the optimization function.
#'
#' @return The result of the optimization process.
#' @export
optimizeParameters <-
  function(dtList,
           scenarioList,
           simulationRunOptions,
           method,
           control,
           outputDir,
           lastStatusSavingInterval,
           logFile,
           startTime,
           failValue,
           ...) {
    # Create the optim environment
    optimEnv <- new.env()

    optimEnv$iteration <- dtList$iteration
    dtList$iteration <- NULL
    optimEnv$bestValue <- dtList$bestValue
    dtList$bestValue <- NULL
    optimEnv$lastSaveTime <- Sys.time()
    optimEnv$failValue <- failValue

    optimStatus <- list()

    result <- optim(
      par = dtList$input$param,
      fn = function(params) {
        optimEnv$iteration <- optimEnv$iteration + 1

        tryCatch(
          {
            # Save the current values to a global variable
            optimStatus <- list(
              iteration = optimEnv$iteration,
              params = params
            )

            loglikelihoods <- getLogLikelihood(params, scenarioList, dtList, simulationRunOptions)
            optimStatus[["loglikelihoods"]] <- loglikelihoods

            if (any(is.na(loglikelihoods))){
              currentValue <- optimEnv$failValue
            } else {
              currentValue <- -1 * sum(loglikelihoods)
            }

            # Check if enough time has passed since the last save of current values
            if (difftime(Sys.time(), optimEnv$lastSaveTime, units = "secs") >= lastStatusSavingInterval) {
              # Write the current status to a file
              saveRDS(optimStatus, file = file.path(outputDir, "optimStatus.RDS"))
              optimEnv$lastSaveTime <- Sys.time()
            }

            # Check if objective function value has improved
            if (currentValue < optimEnv$bestValue) {
              optimEnv$bestValue <- currentValue
              fwrite(
                as.data.table(as.list(c(
                  iteration = optimEnv$iteration, loglikelihoods
                ))),
                file = file.path(outputDir, "convergence.csv"),
                append = TRUE
              )
              saveRDS(optimStatus, file = file.path(outputDir, "bestOptimStatus.RDS"))
            }
          },
          error = function(err) {
            saveRDS(optimStatus, file = file.path(outputDir, "failedOptimStatus.RDS"))
            logMessage(logFile = logFile, message = conditionMessage(err))
            stop(conditionMessage(err))
          }
        )

        return(currentValue)
      },
      method = method,
      control = control
    )

    finalizeOptim(
      result = result,
      outputDir = outputDir,
      logFile = logFile,
      startTime = startTime,
      dtList = dtList
    )

    return(invisible())
  }

#' Finalize Optimization Results
#'
#' This function saves the results of an optimization process to an RDS file and logs details of the optimization.
#' It also saves any relevant data tables as CSV files.
#'
#' @param result A list containing the results of the optimization, typically returned by the `optim` function.
#'               It should include components such as `par`, `value`, `convergence`, and `message`.
#' @param outputDir A character string specifying the directory where the results and data tables will be saved.
#' @param logFile A character string specifying the file path for the log file where optimization details will be recorded.
#' @param startTime A POSIXct object representing the start time of the optimization process, used for calculating total duration.
#' @param dtList A list containing data tables to be saved.
#'
#' @return Returns `invisible(NULL)` after logging the results. The function is primarily used for side effects (saving files and logging).
#'
#' @keywords internal
finalizeOptim <- function(result, outputDir, logFile, startTime, dtList) {
  # save result
  saveRDS(result, file = file.path(outputDir, "result.RDS"))

  # Save data tables as CSV
  saveDataTablesAsCSV(dtList = dtList, outputDir = outputDir, params = result$par)

  duration <- Sys.time() - startTime

  resultDetails <- paste("Optimization finalized:\n",
    "      Final value: ", result$value, "\n",
    "      Counts: ", paste(names(result$counts), result$counts, collapse = ", "), "\n",
    "      Convergence: ", result$convergence, "\n",
    "      Message: ", result$message, "\n",
    "      Total duration: ", signif(duration, 3), attr(duration, "units"), "\n",
    sep = ""
  )

  logMessage(logFile, resultDetails)

  return(invisible())
}


#' Save Data Tables as CSV
#'
#' Saves the provided data tables to CSV files in the specified output directory.
#'
#' @param dtList A list containing data tables to be saved.
#' @param outputDir A character string representing the path to the output directory.
#' @param params Optional; a numeric vector of parameters to set in the data tables (default is NULL).
#'
#' @keywords internal
saveDataTablesAsCSV <- function(dtList, outputDir, params = NULL) {
  if (!is.null(params)) {
    dtList <- setParameterToTables(
      dtList = dtList,
      params = params
    )
  }

  for (name in names(dtList)) {
    if (is.data.table(dtList[[name]])) {
      filePath <- file.path(outputDir, paste0(name, ".csv"))
      write.csv(dtList[[name]], file = filePath, row.names = FALSE)
    }
  }
}

#' Get Output Directory for Run
#'
#' This function constructs the output directory path for a specific run
#' based on the project configuration and the run name.
#'
#' @param projectConfiguration A list containing project configuration details, including output folder.
#' @param runName A character string representing the name of the run.
#' @return A character string representing the full path to the output directory for the specified run.
#' @export
getOutputDirectoryForRun <- function(projectConfiguration, runName) {
  file.path(projectConfiguration$outputFolder, "BMLM", runName)
}
