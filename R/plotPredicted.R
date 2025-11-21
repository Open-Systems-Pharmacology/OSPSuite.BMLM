#' Create and Print Predicted vs Time Plots
#'
#' This function generates plots to compare predicted values against time.
#'
#' @param dtRes A data frame containing the data to be plotted. It should include columns for
#'              outputPathId, scenarioName, individualId, predicted, yValues, and yUnit.
#' @param yScale A character string specifying the scale for the y-axis (default is "log").
#' @param nCols An integer specifying the number of columns for the facet wrap (default is 4).
#' @param titeltxt A string to include in the plot title.
#' @param ... Additional arguments passed on to ospsuite.plots::plotTimeProfile.
#'
#' @return A list of ggplot objects for each unique outputPathId and scenarioName combination.
#' @export
#' @family plotting
plotPredictedVsTime <- function(
    dtRes,
    yScale = unlist(SCALING),
    nCols = 4,
    titeltxt = NULL,
    ...) {
  # Input validation
  checkmate::assertDataTable(dtRes, null.ok = FALSE, min.rows = 1)
  checkmate::assertCharacter(titeltxt, len = 1, null.ok = TRUE)
  checkmate::assertCount(nCols, positive = TRUE)
  yScale <- tolower(match.arg(yScale))
  plotList <- list()

  for (dtResGroup in split(dtRes, by = c("outputPathId", "scenarioName"))) {
    # for inidividuals with only one measurement plot predcition as stright line
    dtIndCount <- dtResGroup[, .N, by = individualId]
    plotData <- rbind(
      dtResGroup,
      dtResGroup[individualId %in% dtIndCount[N > 1]$individualId] %>%
        .[, dataType := "simulated"]
    )
    plotDataSingleValue <- dtResGroup[individualId %in% dtIndCount[N == 1]$individualId]

    yUnit <- dtResGroup$yUnit[1]

    plotObject <-
      ospsuite_plotTimeProfile(
        plotData = plotData,
        mapping = aes(y = predicted, groupby = outputPathId),
        observedMapping = aes(
          y = yValues,
          groupby = outputPathId
        ),
        yscale = tolower(yScale),
        xscale.args = list(limits = c(NA, NA)),
        ...
      ) +
      facet_wrap(vars(individualId), ncol = min(nCols, dplyr::n_distinct(dtResGroup$individualId))) +
      labs(
        title = titeltxt,
        subtitle = dtResGroup$scenario[1],
        y = dtResGroup$outputPathId[1],
        caption = "time raster of predicted is matched to time raster of observed"
      ) +
      theme(legend.position = "none")

    if (nrow(plotDataSingleValue) > 0) {
      plotObject <- plotObject +
        geom_point(
          mapping = aes(x = xValues, y = predicted),
          data = plotDataSingleValue, shape = "plus"
        )
    }

    plotName <- paste(dtResGroup$outputPathId[1], dtResGroup$scenarioName[1], sep = "_")
    plotList[[plotName]] <- plotObject
  }

  return(invisible(plotList))
}
#' Plot Residual Loop Function
#'
#' This function encapsulates the common logic for generating residual plots.
#'
#' @param dtRes A data frame containing the data to be plotted. It should include columns for
#'              scenario, outputPathId, group, and the residuals.
#' @param plotFunction A function that defines the specific plotting logic for each residual plot.
#' @param nCols An integer specifying the number of columns for the facet wrap (default is 2).
#' @param titeltxt A string to include in the plot title.
#' @param excludeCensored A logical value indicating whether to exclude censored data (default is FALSE).
#' @param ... Additional arguments passed to the specific plotting function.
#'
#' @return NULL This function does not return a value; it prints the plots directly.
#' @keywords internal
#' @noRd
plotResidualLoop <- function(dtRes, plotFunction, nCols = 2, titeltxt = NULL,
                             excludeCensored = FALSE, ...) {
  # Input validation
  checkmate::assertDataTable(dtRes, min.rows = 1)
  checkmate::assertFunction(plotFunction)
  checkmate::assertCount(nCols, positive = TRUE)
  checkmate::assertCharacter(titeltxt, null.ok = TRUE, len = 1)
  checkmate::assertLogical(excludeCensored, null.ok = FALSE)

  # Get unique outputPathIds
  outputPathIds <- unique(dtRes$outputPathId)

  # Loop through each outputPathId and create a plot
  plotList <- list()
  for (id in outputPathIds) {
    # Filter data for the current outputPathId
    filteredData <- dtRes[dtRes$outputPathId == id, ]
    if (excludeCensored) {
      filteredData <- filteredData[isCensored == FALSE]
    }

    # Call the specific plot expression passed as an argument
    plotObject <- plotFunction(filteredData, ...) +
      labs(
        subtitle = id,
        title = titeltxt
      )

    # Add facet wrapping by scenario and group
    plotObject <- plotObject +
      facet_wrap(vars(scenario, group), ncol = nCols) +
      scale_shape_manual(values = c("FALSE" = "circle", "TRUE" = "circle open")) +
      theme(legend.direction = "horizontal")

    if (excludeCensored) {
      plotObject <- plotObject +
        guides(shape = "none", fill = "none", color = "none")
    }
    plotList[[id]] <- plotObject
  }

  return(invisible(plotList))
}
#' Create and Print Predicted vs Observed Plots
#'
#' This function generates a plot to compare predicted values against time for a single outputPathId and scenario.
#'
#' @param filteredData A data frame containing the data to be plotted. It should include columns for
#'                     scenario, outputPathId, group, predicted, yValues, and isCensored.
#' @param addRegression A logical value indicating whether to add regression lines to the plot (default is TRUE).
#' @param xyScale A character string specifying the scale type for the x and y axes (default is "log").
#' @param ... Additional arguments passed on to ospsuite.plots::plotPredVsObs.
#'
#' @return An invisible ggplot object for the predicted vs observed plot.
#' @export
#' @family plotting
plotPredictedVsObserved <- function(
    filteredData,
    addRegression = TRUE,
    xyScale = unlist(SCALING),
    ...) {
  # Input validation
  checkmate::assertDataTable(filteredData, min.rows = 1)
  checkmate::assertLogical(addRegression, null.ok = FALSE)
  xyScale <- tolower(match.arg(xyScale))

  # Create the  plot
  plotObject <- ospsuite_plotPredictedVsObserved(
    plotData = filteredData,
    mapping = aes(lloq = lloq),
    addRegression = addRegression,
    comparisonLineVector = getFoldDistanceList(folds = c()),
    xyscale = xyScale,
    groupAesthetics = c(),
    geomPointAttributes = list(shape = 21, fill = "blue"),
    ...
  )

  return(invisible(plotObject))
}

#' Create and Print Residuals vs Time Plots
#'
#' This function generates a plot for residuals versus observed values for a single outputPathId.
#'
#' @param filteredData A data frame containing the data to be plotted. It should include columns for
#'                     scenario, outputPathId, group, and the residuals.
#' @param ... Additional arguments passed on to ospsuite.plots::plotResVsCov.
#'
#' @return A ggplot object for the residuals vs time plot.
#' @export
#' @family plotting
plotResidualsVsTime <- function(filteredData, ...) {
  # Input validation
  checkmate::assertDataTable(filteredData, min.rows = 1)

  # Create the base plot for residuals vs observed
  plotObject <- ospsuite_plotResidualsVsTime(filteredData,
    mapping = aes(
      y = resNorm,
      shape = isCensored
    ),
    groupAesthetics = c()
  ) +
    labs(y = getErrormodelLabel(filteredData$errorModel[1]))

  return(plotObject)
}

#' Plot Residual Distribution
#'
#' This function creates a plot to visualize the distribution of residuals from the model predictions.
#'
#' @param filteredData A data.table containing the residuals and other relevant data.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return A ggplot object visualizing the residual distribution.
#' @export
#' @family plotting
plotResidualsDistribution <- function(filteredData, ...) {
  # initialize variable used in data.table syntax
  isCensored <- resNorm <- NULL

  # Input validation
  checkmate::assertDataTable(filteredData, min.rows = 1)

  setorderv(filteredData, "resNorm")
  filteredData[, ecdf := seq_len(.N) / .N, by = c("scenario", "group")]

  plotObject <- ggplotWithWatermark(filteredData) +
    geom_point(aes(x = resNorm, y = ecdf, shape = isCensored)) +
    geom_function(fun = pnorm) +
    labs(
      x = getErrormodelLabel(filteredData$errorModel[1]),
      y = "cumulative proportion"
    )

  return(invisible(plotObject))
}
#' Create and Print Residuals as Histogram Plots
#'
#' This function generates histogram plots of the residuals to visualize their distribution for a single outputPathId.
#'
#' @param filteredData A data frame containing the data to be plotted. It should include columns for
#'                     scenario, outputPathId, group, and the residuals.
#' @param ... Additional arguments passed on to ospsuite.plots::plotHistogram.
#'
#' @return A ggplot object for the histogram plot.
#' @export
#' @family plotting
plotResidualsAsHistogram <- function(filteredData, ...) {
  # Input validation
  checkmate::assertDataTable(filteredData, min.rows = 1)

  # Create the base plot for residuals vs observed
  plotObject <- ospsuite.plots::plotHistogram(filteredData,
    mapping = aes(x = resNorm, groupby = isCensored),
    plotAsFrequency = TRUE,
    distribution = "none",
    geomHistAttributes = list(position = "stack"),
    ...
  ) +
    stat_function(
      fun = dnorm, args = list(mean = 0, sd = 1),
      color = "black", linewidth = 1
    ) +
    geom_vline(xintercept = 0, linewidth = 0.5) +
    # theme(legend.position = "none") +
    labs(
      x = getErrormodelLabel(filteredData$errorModel[1]),
    )
  return(plotObject)
}
#' Create and Print Residuals as QQ Plot
#'
#' This function generates a QQ plot to assess the normality of the residuals for a single outputPathId.
#'
#' @param filteredData A data frame containing the data to be plotted. It should include columns for
#'                     scenario, outputPathId, group, and the residuals.
#' @param ... Additional arguments passed on to ospsuite.plots::plotQQ.
#'
#' @return A ggplot object for the QQ plot.
#' @export
#' @family plotting
plotResidualsAsQQ <- function(filteredData, ...) {
  # Input validation
  checkmate::assertDataTable(filteredData, min.rows = 1)

  # Create the base plot for residuals vs observed
  plotObject <- ospsuite.plots::plotQQ(
    data = filteredData,
    mapping = aes(sample = resNorm, groupby = isCensored)
  ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed")
  labs(
    y = getErrormodelLabel(filteredData$errorModel[1])
  )
  return(invisible(plotObject))
}
# auxiliaries -------------------
getErrormodelLabel <- function(errorModel) {
  return(paste(
    "normalized residual\n",
    switch(errorModel,
      absolute = "(data - predicted) / sigma",
      proportional = "(data - predicted) / (predicted * sigma)",
      log_absolute = "(log(data) - log(predicted)) / sigma"
    )
  ))
}
