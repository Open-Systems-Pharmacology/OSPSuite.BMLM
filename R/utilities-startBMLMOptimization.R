
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
           lastStatusSavingIntervalInSecs,
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
    optimEnv$NAcounter <- dtList$NAcounter
    optimEnv$Residuals <- data.table()

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
              params = params,
              naCounter = optimEnv$NAcounter
            )

            loglikelihoods <-
              getLogLikelihood(
                params = params,
                scenarioList = scenarioList,
                dtList = dtList,
                simulationRunOptions = simulationRunOptions,
                optimEnv = optimEnv
              )
            optimStatus[["loglikelihoods"]] <- loglikelihoods

            if (any(is.null(loglikelihoods)) | length(loglikelihoods) < 3) stop('strange loglikelihood')

            if (any(is.na(loglikelihoods))){
              if (optimEnv$iteration == 1) stop('First likelihood evaluation must not fail')
              optimEnv$NAcounter <- optimEnv$NAcounter +1
              optimStatus[["naCounter"]] <-  optimEnv$NAcounter
              currentValue <- optimEnv$failValue
            } else {
              currentValue <- -1 * sum(loglikelihoods)
            }

            # Check if enough time has passed since the last save of current values
            if (difftime(Sys.time(), optimEnv$lastSaveTime, units = "secs") >= lastStatusSavingIntervalInSecs) {
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
              saveRDS(optimEnv$Residuals, file = file.path(outputDir, "bestPrediction.RDS"))
            }
          },
          error = function(err) {
            saveRDS(optimStatus, file = file.path(outputDir, "failedOptimStatus.RDS"))
            stop(conditionMessage(err))
          }
        )

        return(currentValue)
      },
      method = method,
      control = control
    )

    # save result
    saveRDS(result, file = file.path(outputDir, "result.RDS"))

    return(invisible())
  }
