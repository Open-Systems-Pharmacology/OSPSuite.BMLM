#' Create and Print Parameter Limits Plots
#'
#' This function creates ggplot objects to display the current best and start values
#' of the fitted parameters.
#'
#' @param dtList A list containing prior and start values data tables.
#' @param statusList A list containing the current and best parameter values.
#' @param titeltxt A string to include in the plot title.
#' @param nCols An integer specifying the number of columns for the plot layout.
#' @param nRows An integer specifying the number of rows for the plot layout.
#' @param colorScalingVector A named vector of colors for different statuses.
#' Default is set by `defaultColorsParameterPlots()`.
#'
#' @return invisible list of ggplot objects.
#' @export
plotParameterLimits <-
  function(dtList,
           statusList,
           titeltxt,
           nCols,
           nRows,
           colorScalingVector = defaultColorsParameterPlots()) {
    # Validate inputs
    checkmate::assertList(dtList, types = "data.table")
    checkmate::assertList(statusList,types = 'list',min.len = 1,names = 'named')
    checkmate::assertNames(names(statusList),subset.of = c('best','current'))
    checkmate::assertCharacter(titeltxt, null.ok = TRUE)
    checkmate::assertCount(nCols, positive = TRUE)
    checkmate::assertCount(nRows, positive = TRUE)
    checkmate::assertVector(colorScalingVector, names = "named")
    checkmate::assertNames(names(colorScalingVector), permutation.of = c("best", "current", "start"))

    dataList <- prepareDataForParameterLimits(
      dtList = dtList,
      statusList = statusList
    )

    mapping <- aes(y = statusParam, color = status, shape = status)

    # limits of global parameter
    plotObject <- ggplot(dataList$globals) +
      suppressWarnings(geom_point(utils::modifyList(mapping, aes(x = name)))) +
      facet_wrap(vars(valueMode), ncol = 2, scales = "free_y")
    plotObject <- adjustLimitsPlot(plotObject = plotObject,
                                   colorScalingVector = colorScalingVector,
                                   titeltxt = titeltxt)

    plotList <- list(global =plotObject)

    # limits of individual parameters
    facetToPlotList <- getFacetToPlotList(
      labelVector = unique(dataList$individuals$label),
      nCols = nCols, nRows = nRows
    )

    for (facetsToPlot in facetToPlotList) {
      # Create the plot using the subset
      plotObject <- ggplot(dataList$individuals[label %in% facetsToPlot]) +
        suppressWarnings(geom_point(utils::modifyList(mapping, aes(x = xlabel)))) +
        facet_wrap(vars(label), ncol = nCols, scales = "free_y")

      plotObject <- adjustLimitsPlot(plotObject = plotObject,
                                     colorScalingVector = colorScalingVector,
                                     titeltxt = titeltxt)

      plotList[[paste("individual", length(plotList), sep = "_")]] <- plotObject
    }

    return(invisible(plotList))
  }

#' Plot Distributions
#'
#' This function creates a series of plots to visualize the cumulative distribution of individual values and hyperparameters.
#'
#' @param dtList A data.table containing the data to be plotted.
#' @param statusList A list containing the current and best parameter values.
#' @param nCols An integer specifying the number of columns for faceting. Default is 2.
#' @param nRows An integer specifying the maximum number of rows for faceting. Default is 3.
#' @param xScale A character string specifying the scale of the x-axis. Default is 'log'.
#' @param titeltxt A string to include in the plot title.
#' @param parameterFilter A character vector for filtering parameter names. Default is NULL.
#' @param zoomOnData A logical value indicating whether to zoom in on data. Default is FALSE.
#' @param colorScalingVector A named vector of colors for different statuses.
#' Default is set by `defaultColorsParameterPlots()`.
#'
#' @return Returns an invisible NULL after printing the plots.
#' @export
plotDistributions <- function(dtList,
                              statusList,
                              nCols = 2,
                              nRows = 3,
                              xScale = unlist(SCALING),
                              titeltxt = NULL,
                              parameterFilter = NULL,
                              zoomOnData = FALSE,
                              colorScalingVector = defaultColorsParameterPlots()) {
  # Validate inputs
  checkmate::assertList(dtList, types = "data.table")
  checkmate::assertList(statusList,types = 'list',min.len = 1,names = 'named')
  checkmate::assertNames(names(statusList),subset.of = c('best','current'))
  checkmate::assertCount(nCols, positive = TRUE)
  checkmate::assertCount(nRows, positive = TRUE)
  checkmate::assertVector(colorScalingVector, names = "named")
  checkmate::assertNames(names(colorScalingVector), permutation.of = c("best", "current", "start"))
  checkmate::assertCharacter(titeltxt, null.ok = TRUE)
  checkmate::assertCharacter(parameterFilter, null.ok = TRUE)
  checkmate::assertLogical(zoomOnData, len = 1)
  xScale <- tolower(match.arg(xScale))

  if (nrow(dtList$startValues) == 0) {
    stop("No distributed parameters available")
  }

  plotData <- prepareDataForDistributionPlot(
    dtList = dtList,
    statusList = statusList,
    parameterFilter = parameterFilter,
    zoomOnData = zoomOnData,
    xScale = xScale
  )

  dtValues <- plotData[valueMode == PARAMETERTYPE$individual] %>%
    setorderv(c("label", "status", "statusValue"))

  dtValues[, ecdf := seq_len(.N) / .N, by = c("status", "label")]

  hyperParameter <- setlogTruncationOffset(
    dtPrior = plotData[valueMode == PARAMETERTYPE$hyperParameter] %>%
      merge(
        dtList$prior[, c("name", "categoricCovariate", "hyperDistribution")] %>%
          unique(),
        by = c("name", "categoricCovariate")
      ) %>%
      setnames("statusValue", "value"),
    dtStartValues = dtValues,
    identifier = c("name", "categoricCovariate", "status"),
    colsToKeep = c(
      "hyperParameter","hyperDistribution","logTruncationOffset",
      "value","minValue","maxValue","displayMin","displayMax",
      "rangeMin","rangeMax","scaling","label")
  )

  facetsToPlotList <- getFacetToPlotList(dtValues$label,nCols = nCols,nRows = nRows)

  plotList <- list()
  for (facetsToPlot in facetsToPlotList) {
    plotObject <-
      generateParameterDistributionPlot(
        dtValuesSubset = dtValues[label %in% facetsToPlot],
        hyperParameterSubset = hyperParameter[label %in% facetsToPlot],
        titeltxt = titeltxt,
        xScale = xScale,
        colorScalingVector = colorScalingVector,
        nCols = nCols
      )

    plotList[[paste0("distributions_", length(plotList) + 1)]] <- plotObject
  }

  plotList <- createDistributionTables(
    dtPrior = dtList$prior,
    hyperParameter = hyperParameter,
    dtValues = dtValues,
    plotList = plotList
  )


  return(invisible(plotList))
}
#' Plot Best Values vs Start Values
#'
#' This function generates a plot comparing the best values against the start values.
#'
#' @param dtList A list containing prior and start values data tables.
#' @param statusList A list containing the current and best parameter values.
#' @param nCols An integer specifying the number of columns for faceting. Default is 2.
#' @param nRows An integer specifying the maximum number of rows for faceting. Default is 3.
#' @param xyScale A character string specifying the scale of the x- and y-axis. Default is 'log'.
#' @param titeltxt A string to include in the plot title.
#' @param parameterFilter A character vector for filtering parameter names. Default is NULL.
#'
#' @return An invisible ggplot object visualizing the best values vs start values.
#' @export
plotBestVsStartParameter <- function(dtList,
                            statusList,
                            nCols = 2,
                            nRows = 3,
                            xyScale = unlist(SCALING),
                            parameterFilter = NULL,
                            titeltxt = NULL,
                            ...) {
  # Validate inputs
  checkmate::assertList(dtList, types = "data.table")
  checkmate::assertList(statusList, types = 'list', min.len = 1, names = 'named')
  checkmate::assertNames(names(statusList), subset.of = c('best', 'current'))
  checkmate::assertCharacter(titeltxt, null.ok = TRUE)
  checkmate::assertCount(nCols, positive = TRUE)
  checkmate::assertCount(nRows, positive = TRUE)
  xyScale <- tolower(match.arg(xyScale))


  if (nrow(dtList$startValues) == 0) {
    stop("No distributed parameters available")
  }

  # Prepare data
  plotData <- preparePlotDataParameterValues(dtList = dtList, statusList = statusList)
  plotData <- plotData[valueMode == PARAMETERTYPE$individual]
  plotData <- addLabel(plotData = plotData, dtPrior = dtList$prior,
                       unitSep = ' ', identifier = "name")

  facetsToPlotList <- getFacetToPlotList(plotData$label,nCols = nCols,nRows = nRows)

  # build a data table to plot line of identity (use this approach instaed of geom_abline
  # to generate squareplots)
  identitylineData <- rbind(plotData[,.(x = min(c(startValue,bestValue))),by = label],
                    plotData[,.(x = max(c(startValue,bestValue))),by = label]) %>%
    .[,y:=x]

  plotList <- list()
  for (facetsToPlot in facetsToPlotList) {
    # Create the  plot
    plotObject <- ggplot(data  = plotData[label %in% facetsToPlot],
                         mapping = aes(x = startValue, y = bestValue)) +
     geom_point(shape = 'circle') +
      geom_line(data = identitylineData,mapping = aes(x = x, y = y)) +
      facet_wrap(vars(label), scales = "free", ncol = nCols) +
      labs(
        x = "Start Value",
        y = "Best Value",
        title = titeltxt
      ) +
      theme(aspect.ratio = 1)

  }

  return(invisible(plotObject))
}
#' Create and Print Parameter Values vs Prior Plot
#'
#' This function generates a plot comparing parameter values against their prior distributions.
#'
#' @param dtList A list containing prior and start values data tables.
#' @param statusList A list containing the current and best parameter values.
#' @param xScale A character string specifying the scale of the x-axis. Default is 'log'.
#' @param titeltxt A string to include in the plot title.
#' @param colorScalingVector A named vector of colors for different statuses.
#' Default is set by `defaultColorsParameterPlots()`.
#'
#' @return An invisible ggplot object visualizing the parameter values vs prior.
#' @export
plotParameterValuesVsPrior <- function(dtList,
                                       statusList,
                                       xScale = unlist(SCALING),
                                       titeltxt = NULL,
                                       colorScalingVector = defaultColorsParameterPlots()) {
  # Validate inputs
  checkmate::assertList(dtList, types = "data.table")
  checkmate::assertList(statusList,types = 'list',min.len = 1,names = 'named')
  checkmate::assertNames(names(statusList),subset.of = c('best','current'))
  checkmate::assertVector(colorScalingVector, names = "named")
  checkmate::assertNames(names(colorScalingVector), permutation.of = c("best", "current", "start"))
  checkmate::assertCharacter(titeltxt, null.ok = TRUE)
  xScale <- tolower(match.arg(xScale))

  dtPrior <- dtList$prior[distribution != "flat"]
  if (nrow(dtPrior) == 0) {
    stop("No parameters with prior information available")
  }

  plotData <- preparePlotDataParameterValues(
    dtList = dtList,
    statusList = statusList
  )
  plotData <- reshapePlotDataParameterValues(plotData)

  functionLines <- createPriorLineData(dtPrior)

  plotObject <-
    ggplot() +
    geom_line(data = functionLines, aes(x = x, y = y)) +
    geom_vline(
      data = merge(plotData,
                   unique(functionLines[, c("id", "label")]),
                   by = "id"
      ),
      mapping = aes(xintercept = statusValue, color = status)
    ) +
    facet_wrap(vars(label), scales = "free", strip.position = "bottom") +
    labs(
      y = "density",
      y = titeltxt
    ) +
    theme(
      legend.direction = "horizontal",
      legend.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      strip.placement = "outside"
    )

  if (xScale == SCALING$log) {
    plotObject <- plotObject + scale_x_log10()
  }


  return(invisible(plotObject))
}

# auxiliaries ----------------
#' Prepare Data for Parameter Limits
#'
#' This function prepares data for plotting parameter limits.
#'
#' @param dtList A list containing prior and start values data tables.
#' @param statusList A list containing the current and best parameter values.
#'
#' @return A list containing processed data for individuals and globals.
#' @keywords internal
prepareDataForParameterLimits <- function(dtList,
                                          statusList) {
  plotData <- preparePlotDataParameterValues(
    dtList = dtList,
    statusList = statusList
  )
  # label for xlabel (!plot will displayed with coord_flip)
  plotData[, xlabel := ""]
  plotData[valueMode == PARAMETERTYPE$individual, xlabel := individualId]
  plotData[valueMode == PARAMETERTYPE$hyperParameter, xlabel := hyperParameter, by = .I]

  plotData$xlabel <- factor(plotData$xlabel, levels = unique(plotData$xlabel), ordered = TRUE)

  plotData <- reshapePlotDataParameterValues(plotData)
  plotData <- addLabel(plotData = plotData, dtPrior = dtList$prior, unitSep = NULL, identifier = "name")


  return(list(
    individuals = plotData[!(valueMode %in% c(PARAMETERTYPE$global, PARAMETERTYPE$outputError))],
    globals = plotData[(valueMode %in% c(PARAMETERTYPE$global, PARAMETERTYPE$outputError))]
  ))
}
#' Adjust Limits for a ggplot Object
#'
#' This function modifies a ggplot object by adding horizontal lines at y=0 and y=1,
#' customizing the color scale, and adjusting the labels and theme.
#'
#' @param plotObject A ggplot object that will be modified.
#' @param colorScalingVector A named vector of colors for different statuses.
#' This vector will be used to customize the color scale of the plot.
#' @param titeltxt A string to include in the plot title.
#'
#' @return A modified ggplot object with adjusted limits, colors, and labels.
#' @keywords internal
adjustLimitsPlot <- function(plotObject,colorScalingVector,titeltxt){
  plotObject <-
    plotObject +
    geom_hline(yintercept = c(0, 1)) +
    scale_color_manual(
      values = colorScalingVector
    ) +
    coord_flip() +
    labs(
      x = "",
      y = "",
      color = "",
      shape = "",
      title = titeltxt
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, by = 0.25),
      labels = c("min", rep("", 3), "max")
    ) +
    layerWatermark() +
    theme(legend.direction = "horizontal")
}
#' Prepare Data for Distribution Plot
#'
#' This function prepares data for distribution plots.
#'
#' @param dtList A list containing prior and start values data tables.
#' @param statusList A list containing the current and best parameter values.
#' @param zoomOnData A logical value indicating whether to zoom in on data. Default is FALSE.
#' @param xScale display scale of x-axis.
#'
#' @return A data.table containing prepared data for distribution plots.
#' @keywords internal
prepareDataForDistributionPlot <- function(dtList, statusList, parameterFilter, zoomOnData,xScale) {
  plotData <- preparePlotDataParameterValues(
    dtList = dtList,
    statusList = statusList
  )
  plotData <- reshapePlotDataParameterValues(plotData)
  plotData <- addLabel(plotData = plotData, dtPrior = dtList$prior)


  # Filter parameters based on parameterFilter
  if (!is.null(parameterFilter) && length(parameterFilter) > 0) {
    plotData <- plotData[name %in% parameterFilter]
  }

  # Add rangeLimits
  rangeLimits <- unique(dtList$startValues[, c("name", "categoricCovariate", "minValue", "maxValue")]) %>%
    setnames(old = c("minValue", "maxValue"), new = c("rangeMin", "rangeMax"))
  if (any(duplicated(rangeLimits[, c("name", "categoricCovariate")]))) {
    stop("min and max Values must be unique for each group in startValues")
  }
  plotData <- plotData %>%
    merge(rangeLimits,by = c("name", "categoricCovariate"))


  if (zoomOnData) {
    # Adjust displayMin and displayMax based on statusValue
    plotData[, `:=`(
      displayMin = 0.9 * min(statusValue, na.rm = TRUE),
      displayMax = 1.1 * max(statusValue, na.rm = TRUE)
    ), by = label]
  } else {
    plotData[, `:=`(
      displayMin = rangeMin,
      displayMax = rangeMax
    )]
  }

  if (xScale == SCALING$log){
    if (any(plotData$displayMin <= 0) || any(plotData$displayMax <= 0)){
      skippedParameters <- unique(plotData[displayMin <= 0 | displayMax <= 0]$name)
      plotData <- plotData[displayMin >0 & displayMax > 0,]
      warning(sprintf('Skipping parameters with display ranges less than or equal to zero for log-scale display: %s',
                      paste(skippedParameters, collapse = ", ")))
      if (nrow(plotData) == 0) stop('No parameters left for log-scale display.')
    }
  }

  return(plotData)
}
#' Generate Parameter Distribution Plot
#'
#' This function generates a ggplot object for parameter distributions.
#'
#' @param dtValuesSubset A data.table containing the subset of values to plot.
#' @param hyperParameterSubset A data.table containing hyperparameter information.
#' @param titeltxt A string to include in the plot title.
#' @param xScale A character string specifying the scale of the x-axis.
#' @param colorScalingVector A named vector of colors for different statuses.
#' @param nCols An integer specifying the number of columns for faceting.
#'
#' @return A ggplot object representing the parameter distribution.
#' @keywords internal
generateParameterDistributionPlot <- function(dtValuesSubset, hyperParameterSubset,
                                              titeltxt, xScale, colorScalingVector,nCols) {
  lineData <- createLineData(
    hyperParameter = hyperParameterSubset,
    xScale = xScale,
    dtValues = dtValuesSubset
  )

  # Create the plot for the current subset
  plotObject <- ggplot(data = dtValuesSubset) +
    geom_point(mapping = aes(x = statusValue, y = ecdf, fill = status, shape = status)) +
    labs(
      x = "parameter values",
      y = "cumulative proportion",
      title = titeltxt
    )

  if (xScale == SCALING$log) {
    plotObject <- plotObject + scale_x_log10()
  }

  plotObject <- plotObject +
    geom_line(data = lineData, aes(x = x, y = value, color = status, linetype = status), linewidth = 1) +
    facet_wrap(vars(label), scales = "free_x", ncol = nCols) +
    layerWatermark()

  plotObject <- customizeLegend(plotObject, colorScalingVector)

  return(plotObject)
}
#' Create Line Data for Hyperparameters
#'
#' This function generates line data for hyperparameters based on their distributions.
#'
#' @param hyperParameter A data.table containing hyperparameter information.
#' @param xScale A character string specifying the scale of the x-axis ('linear' or 'log').
#' @param dtValues A data.table containing values for plotting.
#'
#' @return A data.table containing the line data for hyperparameters.
#' @keywords internal
createLineData <- function(hyperParameter, xScale, dtValues = NULL) {
  lineData <- data.table()

  for (dtHyperPar in split(hyperParameter, by = c("label", "status"))) {
    x <- if (xScale == SCALING$log) {
      exp(seq(log(dtHyperPar$displayMin[1]),
              log(dtHyperPar$displayMax[1]),
              length.out = 100
      ))
    } else {
      seq(dtHyperPar$displayMin[1],
          dtHyperPar$displayMax[1],
          length.out = 100
      )
    }

    argList <- stats::setNames(as.numeric(dtHyperPar[["value"]]), as.character(dtHyperPar[["hyperParameter"]]))
    y <- do.call(paste0("p", dtHyperPar$hyperDistribution[1]), args = c(list(q = x), argList))
    # Renormalizationf or truncation
    ylimits <- do.call(paste0("p", dtHyperPar$hyperDistribution[1]), args = c(list(q = c(dtHyperPar$rangeMin, dtHyperPar$rangeMax)), argList))
    y <- (y - ylimits[1]) / diff(range(ylimits))

    lineData <- rbind(
      lineData,
      data.table(
        x = x,
        value = y,
        status = dtHyperPar$status[1],
        label = dtHyperPar$label[1]
      )
    )
  }

  return(lineData)
}
#' Create Distribution Tables
#'
#' This function generates tables for hyperparameters and their distributions.
#'
#' @param dtPrior A data.table containing prior information.
#' @param hyperParameter A data.table containing hyperparameter information.
#' @param plotList A list of plots to be generated.
#' @param dtValues A data.table containing values for plotting.
#'
#' @return A list of distribution tables.
#' @keywords internal
createDistributionTables <- function(dtPrior, hyperParameter, plotList,dtValues) {
  dtPrior <- copy(dtPrior)
  dtPrior[, priorDescription :=
            trimws(paste(
              distribution,
              ifelse(distribution == "flat", "",
                     paste0(
                       "(",
                       ifelse(is.na(p1_type), "", paste0(p1_type, ": ", p1_value)),
                       ifelse(is.na(p2_type), "", paste0(" ", p2_type, ": ", p2_value)),
                       ifelse(is.na(p3_type), "", paste0(" ", p3_type, ": ", p3_value)), ")"
                     )
              )
            ))]

  hyperParameter <- hyperParameter %>%
    merge(dtPrior[, c("name", "hyperParameter", "categoricCovariate", "priorDescription")],
          by = c("name", "hyperParameter", "categoricCovariate")
    )

  for (dtHyper in split(hyperParameter, by = "label")) {
    tmpHyper <- dcast(dtHyper[, c("hyperParameter", "status", "value", "minValue", "maxValue", "priorDescription")],
                      ... ~ status,
                      value.var = "value"
    )

    tmpLog <- rbind(stats::setNames(lapply(unique(dtHyper$status), function(testStatus) {
      getLikelihoodForIndividualGroup(
        copy(dtValues)[label == dtHyper$label[1] &
                         status == testStatus] %>%
          setnames("statusValue", "value"),
        dtHyper[status == testStatus]
      )
    }), unique(dtHyper$status))) %>% as.data.table()
    tmpLog[, hyperParameter := "loglikelihood"]

    dtHyper[, truncationOffset := 1 - exp(logTruncationOffset)]
    tmpTrunc <- dcast(dtHyper[, c("truncationOffset", "status", "rangeMin", "rangeMax")] %>% unique(),
                      ... ~ status,
                      value.var = "truncationOffset"
    )
    tmpTrunc[, hyperParameter := paste0("likelihood outside range (", rangeMin, "-", rangeMax, ")")]
    tmpTrunc[, rangeMin := NULL]
    tmpTrunc[, rangeMax := NULL]

    tmp <- rbind(tmpHyper,
                 tmpLog,
                 tmpTrunc,
                 fill = TRUE
    ) %>%
      setnames("hyperParameter", ".")
    print(knitr::kable(tmp, caption = dtHyper$label[1]))

    plotList[[dtHyper$label[1]]] <- tmp
  }

  return(plotList)
}
#' Create Prior Line Data
#'
#' This function generates line data for prior distributions.
#'
#' @param dtPrior A data.table containing prior information.
#'
#' @return A data.table containing the line data for prior distributions.
#' @keywords internal
createPriorLineData <- function(dtPrior) {
  dtPrior[, label := paste(
    name,
    categoricCovariate,
    ifelse(valueMode == PARAMETERTYPE$hyperParameter, hyperParameter, "")
  )]

  functionLines <- data.table()
  for (priorRow in split(dtPrior, by = "id")) {
    # Extract parameters
    paramTypes <-
      priorRow[, grep("_type$", names(priorRow), value = TRUE), with = FALSE] # Get all type columns
    paramValues <-
      priorRow[, grep("_value$", names(priorRow), value = TRUE), with = FALSE] # Get all value columns
    if (priorRow$scaling == SCALING$log) {
      xValues <- exp(seq(log(priorRow$minValue), log(priorRow$maxValue), length.out = 100))
    } else {
      xValues <- seq(priorRow$minValue, priorRow$maxValue, length.out = 100)
    }
    yValues <- computeStatFunction(
      values = paramValues,
      parameters = paramTypes,
      distribution = priorRow$distribution,
      v = xValues,
      type = "D",
      log = FALSE
    )
    functionLines <- data.table(x = xValues, y = yValues, id = priorRow$id, label = priorRow$label)
  }
  return(functionLines)
}
#' Customize Legend for ggplot Objects
#'
#' This function customizes the legend for ggplot objects by setting specific colors and shapes for different statuses.
#'
#' @param plotObject A ggplot object to which the legend will be added.
#' @param colorScalingVector A named vector of colors for different statuses.
#'
#' @return A ggplot object with a customized legend.
#' @keywords internal
customizeLegend <- function(plotObject, colorScalingVector,
                            aesthetics = c("color", "fill", "shape", "linetype")) {
  legendTitleShape <- "Individual Values"
  legendTitleLine <- "Distribution"

  if ("linetype" %in% aesthetics) {
    plotObject <- plotObject +
      scale_linetype_manual(
        values = c("dotted", "solid", "twodash"),
        breaks = names(colorScalingVector)
      )
  }
  if ("shape" %in% aesthetics) {
    plotObject <- plotObject +
      scale_shape_manual(
        values = c("square filled", "triangle filled", "circle filled"),
        breaks = names(colorScalingVector)
      )
  }
  if ("color" %in% aesthetics) {
    plotObject <- plotObject +
      scale_color_manual(
        values = colorScalingVector,
        breaks = names(colorScalingVector)
      )
  }
  if ("fill" %in% aesthetics) {
    plotObject <- plotObject +
      scale_fill_manual(
        values = colorScalingVector,
        breaks = names(colorScalingVector)
      )
  }

  plotObject <- plotObject +
    guides(
      shape = guide_legend(title = legendTitleShape, order = 1),
      fill = guide_legend(title = legendTitleShape, order = 1),
      color = guide_legend(title = legendTitleLine, order = 2),
      linetype = guide_legend(title = legendTitleLine, order = 2)
    )

  return(plotObject)
}
#' Get Default Colors for Parameter Plots
#'
#' This function retrieves the default color settings for parameter plots.
#' It checks the global option `ospsuite.BMLM.defaultColors` and returns the
#' specified colors. If no option is set, it returns a predefined set of colors.
#'
#' @return A named vector of colors for different statuses. The default colors
#' are:
#' \itemize{
#'   \item current: "darkgreen"
#'   \item start: "lightblue"
#'   \item best: "orange"
#' }
#'
#' @examples
#' \dontrun{
#' # Set custom colors for parameter plots
#' options(ospsuite.BMLM.defaultColors = c(current = "blue", start = "yellow", best = "red"))
#' customColors <- defaultColorsParameterPlots()
#' print(customColors)
#' }
#' @export
defaultColorsParameterPlots <- function() {
  return(getOption("ospsuite.BMLM.defaultColors", default =
                     c(current = "darkgreen", start = "lightblue", best = "orange")))
}
#' Get Facet to Plot List
#'
#' This function generates a list of facets to be plotted based on the provided
#' label vector and the specified number of columns and rows for each plot.
#'
#' @param labelVector A character or factor vector containing the facet labels.
#' @param nCols An integer specifying the number of columns in each plot.
#' @param nRows An integer specifying the number of rows in each plot.
#'
#' @return A list where each element is a vector of facets that should be plotted
#'         together in a single plot.
#' @keywords internal
getFacetToPlotList <- function(labelVector, nCols, nRows) {
  # Determine unique facets
  uniqueFacets <- unique(labelVector)
  totalFacets <- length(uniqueFacets)

  # Calculate how many plots are needed
  totalPlots <- ceiling(totalFacets / (nCols * nRows))

  facetToPlotList <- lapply(seq_len(totalPlots), function(iPlot) {
    # Determine the facets for one plot
    startIndex <- (iPlot - 1) * nCols * nRows + 1
    endIndex <- min(startIndex + (nCols * nRows) - 1, totalFacets)

    facetsToPlot <- uniqueFacets[startIndex:endIndex]
  })

  return(facetToPlotList)
}
#' Prepare Plot Data for Parameter Values
#'
#' This function prepares the data for plotting by loading the necessary status files
#' and transforming the data into a suitable format for visualization.
#'
#' @param dtList A list containing prior and start values data tables.
#' @param statusList A list which contains the current and best values.
#'
#' @return A data.table containing the prepared plot data.
#' @keywords internal
preparePlotDataParameterValues <- function(dtList, statusList) {
  # startValue
  columnsToSelect <- c("id","name","categoricCovariate",
                     "startValue","minValue","maxValue","scaling",
                     "valueMode","hyperParameter",
                     "individualId")
  plotData <-
    dtList$prior %>% dplyr::select(dplyr::any_of(c(columnsToSelect))) %>%
    dplyr::mutate(individualId = NA)
  if (nrow(dtList$startValues) > 0) {
    plotData <- rbind(
      plotData,
      dtList$startValues %>% dplyr::select(dplyr::any_of(c(columnsToSelect))) %>%
        dplyr::mutate(
          valueMode = PARAMETERTYPE$individual,
          hyperParameter = ""
        )
    )
  }
  plotData <- copy(plotData)

  # set param for start
  plotData[
    , startParam :=
      scaleWithinBounds(
        value = startValue,
        minValue = minValue,
        maxValue = maxValue,
        scaling = tolower(scaling)
      ),
    .I
  ]

  # set param and value for statusList
  for (statusName in names(statusList)) {
    status <- statusList[[statusName]]

    if (!is.null(status)) {
      unscaleParamToValue <- getUnscaleFunction(status$scalingMethod)
      scaleValueToParam <- getScaleFunction(status$scalingMethod)

      plotData[, param := status$params[id]]

      # Use a dynamic column name based on the status
      plotData[, paste0(statusName, "Value") := unscaleParamToValue(
        param = param,
        minValue = minValue,
        maxValue = maxValue,
        scaling = tolower(scaling)
      ), .I]

      plotData[, paste0(statusName, "Param") := scaleWithinBounds(value = get(paste0(statusName, "Value")),
                                                                  minValue = minValue,
                                                                  maxValue = maxValue,
                                                                  scaling = tolower(scaling)), .I]
    }
  }

  return(plotData)
}
#' Reshape Plot Data for Visualization
#'
#' This function reshapes the data.table and adds additional columns to the plot data
#' for labeling and categorization.
#'
#' @param plotData A data.table containing the initial plot data.
#'
#' @return A data.table with enhanced plot data suitable for visualization.
#' @keywords internal
reshapePlotDataParameterValues <- function(plotData) {
  # reshapes the plot data for visualization by melting the data.table.
  plotData[, param := NULL]
  # Assuming plotData is already a data.table
  plotData <- melt(plotData,
                   measure.vars = intersect(
                     names(plotData),
                     c("startValue", "currentValue", "bestValue",
                       "startParam", "currentParam", "bestParam"
                     )),
                   variable.name = "status",
                   value.name = "statusValue"
  )

  # Create the 'type' column and modify the 'status' column
  plotData[, type := ifelse(grepl("Param", status), "statusParam", "statusValue")]
  plotData[, status := gsub("Value", "", gsub("Param", "", status))]

  # Reshape the data to wide format
  plotData <- dcast(plotData, ... ~ type, value.var = "statusValue")

  return(plotData)
}

#' Add Labels to Plot Data
#'
#' This function merges the plot data with prior data to add labels for better visualization.
#'
#' @param plotData A data.table containing the initial plot data.
#' @param dtPrior A data.table containing prior values for merging.
#' @param unitSep A string to separate the name and unit in the label. Default is a space.
#' If `unitSep` is NULL, unit will be skipped
#'
#' @return A data.table with labels added for each parameter.
#' @keywords internal
addLabel <- function(plotData, dtPrior, unitSep = " ", identifier = c("name", "categoricCovariate")) {
  plotData <- merge(plotData,
                    dtPrior[, c(..identifier, "unit")] %>%
                      unique(),
                    by = identifier
  )

  plotData[, label := ifelse(is.na(unit) | is.null(unitSep), name, paste0(name, unitSep, "[", unit, "]"))]
  if ("categoricCovariate" %in% identifier) {
    plotData[, label := ifelse(is.na(categoricCovariate) | categoricCovariate == "",
                               label,
                               paste0(label, " (", categoricCovariate, ")")
    )]
  }

  return(plotData)
}
