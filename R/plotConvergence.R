#' Check Convergence of Model Parameters
#'
#' This function checks the convergence of model parameters by reading a data.table
#' containing convergence data and visualizing the results using ggplot2.
#'
#' @param dtConvergence A data.table containing the convergence data.
#' @param displayVariablesIndx An integer vector specifying which variables to display.
#' @param titletxt A string to include in the plot title.
#' @param nPoints An integer specifying the number of points to select for plotting. Default is 200.
#' @param selectionMode A character string indicating the mode of selection for points.
#' Options are 'last', 'random', and 'first'. Default is 'last'.
#'
#' @return A ggplot object visualizing the convergence of model parameters.
#' @export
#' @family plotting

# Main function
plotConvergence <- function(dtConvergence,
                            displayVariablesIndx = NULL,
                            titletxt = NULL,
                            nPoints = 200,
                            selectionMode = c("last", "random", "first")) {
  # Validate inputs
  checkmate::assertDataTable(dtConvergence,min.rows = 1,.var.name = 'convergence table')
  checkmate::assertIntegerish(displayVariablesIndx, lower = 1, upper = 6, unique = TRUE, any.missing = FALSE, null.ok = TRUE)
  checkmate::assertString(titletxt, null.ok = TRUE)
  checkmate::assertCount(nPoints, positive = TRUE)
  selectionMode <- match.arg(selectionMode)

  setorderv(dtConvergence, "iteration", 1)

  dtConvergenceList = split(dtConvergence,by = 'event')

  # Calculate metrics
  dtConvergenceList$best <- calculateConvergenceMetrics(dtConvergenceList$best)

  nPointsAvailable <- nrow(dtConvergenceList$best)
  if (nPointsAvailable == 1) {
    message(paste("only one point available, please wait for plots"))
    return(NULL)
  }

  columnheaders <- getConvergenceColumnHeaders(
    dt = dtConvergenceList$best,
    displayVariablesIndx = displayVariablesIndx
  )

  # Select points based on the specified selection mode
  selectionMode <- match.arg(selectionMode)
  dtConvergenceList <- selectIterations(dtConvergenceList, nPointsAvailable, nPoints, selectionMode)

  # Reshape the data for plotting
  plotData <- data.table::melt(dtConvergenceList$best,
    measure.vars = names(columnheaders),
    variable.name = "summand",
    value.name = "value"
  )

  plotData$summand <- factor(plotData$summand,
    levels = names(columnheaders)
  )
  levels(plotData$summand) <- columnheaders[levels(plotData$summand)]

  # Create the plot using ggplot2
  plotObject <- ggplot(plotData, mapping = aes(x = iteration, y = value)) +
    geom_step(mapping = aes(color = "current", linetype = "current")) +
    geom_hline(
      mapping = aes(yintercept = value, color = "start", linetype = "start"),
      data = plotData[iteration == 1]
    ) +
    facet_wrap(~summand, ncol = 1, scales = "free_y") +
    scale_color_manual(values = c(current = "black", start = "darkred")) +
    scale_linetype_manual(values = c(current = "solid", start = "dotted")) +
    labs(
      y = "", color = "", linetype = "",
      title = titletxt
    ) +
    theme(legend.position = "none") +
    layerWatermark()

  # Add restart points as vertical lines
  if (nrow(dtConvergenceList$restart)>0) {
    plotObject <- plotObject +
      geom_vline(data = dtConvergenceList$restart, mapping = aes(xintercept = iteration)) +
      labs(caption = "vertical lines indicate restart of algorithm")
  }

  return(invisible(list(convergence = plotObject)))
}
# auxiliaries -------------
#' Calculate Convergence Metrics
#'
#' This function calculates the objective values and percentages for the convergence data.
#'
#' @param dt A data.table containing convergence data with necessary columns.
#'
#' @return A data.table with additional columns for objective values and percentages of failure and outside range.
#' @keywords internal
#' @noRd
calculateConvergenceMetrics <- function(dt) {

  requiredCols <- c("logTimeProfile", "logHyperParameter", "logPrior", "NAcounter", "outsideRangeCounter", "iteration")
  if (!all(requiredCols %in% names(dt))) {
    stop("Convergence table must contain the following columns: ", paste(requiredCols, collapse = ", "))
  }

  dt[, objectiveValue := -(logTimeProfile + logHyperParameter + logPrior)]
  dt[, `:=`(
    logTimeProfile = -logTimeProfile,
    logHyperParameter = -logHyperParameter,
    logPrior = -logPrior
  )]
  dt[, percentageOfFailure := NAcounter / iteration * 100]
  dt[, percentageOutsideRange := outsideRangeCounter / iteration * 100]
  return(dt)
}

#' Get Column Headers for Display Variables
#'
#' This function retrieves the column headers for the specified display variable indices.
#'
#' @param dt A data.table containing convergence data.
#' @param displayVariablesIndx An integer vector specifying which variables to display.
#'
#' @return A named character vector of column headers for the display variables.
#' @keywords internal
#' @noRd
getConvergenceColumnHeaders <- function(dt, displayVariablesIndx) {
  columnheaders <- c(
    objectiveValue = "value of objective function: -loglikelihood",
    logTimeProfile = "- loglikelihood TimeProfile",
    logHyperParameter = "- loglikelihood HyperParameter",
    logPrior = "- loglikelihood Prior",
    percentageOfFailure = "percentage of failed iterations",
    percentageOutsideRange = "percentage of iterations with parameters outside range"
  )

  if (is.null(displayVariablesIndx)) {
    displayVariablesIndx <- which(sapply(names(columnheaders), function(col) any(dt[[col]] != 0)))
  }

  return(columnheaders[displayVariablesIndx])
}

#' Select Points Based on Selection Mode
#'
#' This function selects points from the convergence data based on the specified selection mode.
#'
#' @param dt A data.table containing convergence data.
#' @param nPointsAvailable An integer specifying the number of available points.
#' @param nPoints An integer specifying the number of points to select.
#' @param selectionMode A character string indicating the mode of selection for points.
#'
#' @return A data.table containing the selected points.
#' @keywords internal
#' @noRd
selectIterations <- function(dtConvergenceList, nPointsAvailable, nPoints, selectionMode) {

  if (nPointsAvailable > nPoints) {
    dtConvergenceList$best <- dtConvergenceList$best[switch(selectionMode,
                    first = seq(1, nPoints) ,
                    random = sort(c(1,
                                    sample(seq(2, nPointsAvailable - 1),
                                           size = nPoints - 2,
                                           replace = FALSE),
                                    nPointsAvailable)),
                    last = seq(1, nPoints) + nPointsAvailable - nPoints,
                    stop('unknown sectionMode'))]
    dtConvergenceList$restart <- dtConvergenceList$restart[
                     iteration >= min(dtConvergenceList$best$iteration) &
                     iteration <= max(dtConvergenceList$best$iteration)]
  }
  return(dtConvergenceList)
}
