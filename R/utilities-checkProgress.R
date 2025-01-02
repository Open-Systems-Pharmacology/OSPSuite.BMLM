#' Check Convergence of Model Parameters
#'
#' This function checks the convergence of model parameters by reading a CSV file
#' containing convergence data and visualizing the results using ggplot2.
#'
#' @param dtConvergence A data.table containing the loglikelihood vsplot titlename of the run.
#' @param nPoints An integer specifying the number of points to select for plotting.
#' Default is 200.
#' @param selectionMode A character string indicating the mode of selection for points.
#' Options are 'last', 'random', and 'first'. Default is 'last'.
#'
#' @return A ggplot object visualizing the convergence of model parameters.
#' The plot shows the values of logTimeProfile, logHyperParameter, logPrior,
#' and objectiveValue over iterations.
#'
#' @keywords internal
plotConvergence <- function(dtConvergence,
                             titletxt = NULL,
                             nPoints = 200,
                             selectionMode = c('last','random','first')){

  # Match the selection mode to ensure it's one of the allowed values
  selectionMode <- match.arg(selectionMode)


  # Calculate the objective value as the negative sum of log likelihoods
  dtConvergence[, objectiveValue := -(logTimeProfile + logHyperParameter + logPrior)]

  # Print the first few rows of the sorted convergence data
  nPointsAvailable <- nrow(dtConvergence)
  print(head(setorderv(dtConvergence,'iteration',-1), 5))

  if (nPointsAvailable == 1){
    message(paste('only one points available, please wait for plots'))
    return(NULL)
  }

  # Select points based on the specified selection mode
  # Attention sis sorted decending for print
  if (nPointsAvailable > nPoints) {
    ixSelected <- switch(selectionMode,
                         first = seq(1, nPoints) + nPointsAvailable - nPoints,
                         random = sort(c(1, sample(seq(2, nPointsAvailable - 1), size = nPoints - 2, replace = FALSE), nPointsAvailable)),
                         last = seq(1, nPoints)
    )
    dtConvergence <- dtConvergence[ixSelected]
  }

  # Reshape the data for plotting
  dtConvergence <- data.table::melt(dtConvergence,
                                    measure.vars = c('logTimeProfile', 'logHyperParameter', 'logPrior', 'objectiveValue'),
                                    variable.name = 'summand',
                                    value.name = 'value')
  dtConvergence$summand <- factor(dtConvergence$summand,
                                  levels = c('logTimeProfile', 'logHyperParameter', 'logPrior', 'objectiveValue'),
                                  labels = c('loglikelihood TimeProfile', 'loglikelihood HyperParameter',
                                             'loglikelihood Prior', 'value of objective function: -sum(loglikelihood)'))

  # Create the plot using ggplot2
  plotObject <- ggplot(dtConvergence, mapping = aes(x = iteration, y = value)) +
    geom_step(mapping = aes(color = 'current', linetype = 'current')) +
    geom_hline(mapping = aes(yintercept = value, color = 'start', linetype = 'start'),
               data = dtConvergence[iteration == 1]) +
    facet_wrap(~summand, ncol = 1, scales = 'free_y') +
    scale_color_manual(values = c(current = 'black', start = 'darkred')) +
    scale_linetype_manual(values = c(current = 'solid', start = 'dotted')) +
    labs(y = '', color = '', linetype = '',
         title = titletxt) +
    theme(legend.title = element_blank(), legend.direction = 'horizontal') +
    layerWatermark()

  # Print the plot to the console
  print(plotObject)

  return(invisible(plotObject))
}


#' Create Plots
#'
#' This function creates ggplot objects based on the prepared plot data.
#' If the number of plots exceeds the specified grid size, it splits the plots into multiple sets.
#'
#' @param plotData A data.table containing the reshaped plot data.
#' @param titeltxt A string  to include in the plot title.
#' @param nCols An integer specifying the number of columns for the plot layout.
#' @param nRows An integer specifying the number of rows for the plot layout.
#' @return A list of ggplot objects representing the plots for the parameter values.
#' @keywords internal
plotParameterLimits <-
  function(plotData,
           titeltxt,
           nCols,
           nRows,
           useInteractivePlots = FALSE,
           colorScalingVector =  c(
             start = 'lightblue',
             current = 'darkgreen',
             best = 'orange'
           )) {

    # Get the unique combinations for the current set of plots
    uniqueCombinations <- unique(plotData[!(valueMode %in% c(PARAMETERTYPE$global, PARAMETERTYPE$outputError)), .(categoricCovariate, name)])
    setorderv(uniqueCombinations,c('categoricCovariate','name'))
    nPlots <- nrow(uniqueCombinations)

    # Determine how many sets of plots are needed
    totalPlots <- ceiling(nPlots / (nCols * nRows))

    mapping <- aes(y = statusParam, color = status, shape = status)
    if (useInteractivePlots)
      mapping <- utils::modifyList(mapping,aes(text = paste("Value:", signif(statusValue,3))))

    for (setIndex in seq(0,totalPlots)) {
      if (setIndex == 0) {
        plotObject <- ggplot(plotData[valueMode %in% c(PARAMETERTYPE$global, PARAMETERTYPE$outputError)]) +
          suppressWarnings(geom_point(utils::modifyList(mapping,aes(x = name)))) +
          facet_wrap(vars(valueMode), ncol = 2, scales = 'free_y')
      } else {
        # Subset the data for the current set of plots
        startIndex <- (setIndex - 1) * (nCols * nRows) + 1
        endIndex <- min(startIndex + (nCols * nRows) - 1, nPlots)

        # Determine the rows to plot for the current set
        rowsToPlot <- seq(startIndex, endIndex)
        plotDataSubset <- uniqueCombinations[rowsToPlot]

        # Create the plot using the subset
        plotObject <- ggplot(plotData[.(plotDataSubset), on = .(categoricCovariate, name)]) +
          suppressWarnings(geom_point(utils::modifyList(mapping,aes(x = label)))) +
          facet_wrap(vars(categoricCovariate, name), ncol = nCols, scales = 'free_y')

      }

      plotObject <-
        plotObject +
        geom_hline(yintercept = c(0, 1)) +
        scale_color_manual(
          values = colorScalingVector
        ) +
        coord_flip() +
        labs(
          x = '',
          y = '',
          color = '',
          shape = '',
          title = titeltxt
        ) +
        scale_y_continuous(
          breaks = seq(0, 1, by = 0.25),
          labels = c('min', rep('', 3), 'max')
        ) +
        layerWatermark()

      if (useInteractivePlots){
        # Convert ggplot to plotly for interactivity
        plotlyPlot <- plotly::ggplotly(plotObject, tooltip = "text")
        plotlyPlot <- plotly::config(plotlyPlot, displayModeBar = FALSE)
        print(plotlyPlot)
      } else {
        plotObject <- plotObject +
          theme(legend.direction = 'horizontal')
        print(plotObject)
      }

    }

    return(invisible())
  }

#' Plot Distributions
#'
#' This function creates a series of plots to visualize the cumulative distribution of individual values and hyperparameters.
#'
#' @param plotData A data.table containing the data to be plotted.
#' @param nCols An integer specifying the number of columns for faceting. Default is 2.
#' @param nRows An integer specifying the maximum number of rows for faceting. Default is 3.
#' @param xScale A character string specifying the scale of the x-axis. Default is 'Log'
#' @param colorScalingVector A named vector of colors for different statuses. Default includes light blue, dark green, and orange.
#'
#' @return Returns an invisible NULL after printing the plots.
#'
#' @export
plotDistributions <- function(plotData,
                              nCols = 2,
                              nRows = 3,
                              xScale = unlist(SCALING),
                              colorScalingVector =  c(
                                start = 'lightblue',
                                current = 'darkgreen',
                                best = 'orange'
                              )) {

  xScale <- match.arg(xScale)

  dtValues <- plotData[valueMode == PARAMETERTYPE$individual] %>%
    setorderv(c('name', 'categoricCovariate', 'status', 'statusValue'))

  dtValues[, ecdf := cumsum(statusValue) / sum(statusValue), by = c('status', 'name', 'categoricCovariate')]

  # Determine unique facets
  uniqueFacets <- unique(dtValues[, .(categoricCovariate, name)])
  totalFacets <- nrow(uniqueFacets)

  # Calculate how many plots are needed
  totalPlots <- ceiling(totalFacets / (nCols * nRows))

  # Create a list to store individual plots
  plotList <- vector("list", totalPlots)

  for (i in seq_len(totalPlots)) {

    # Determine the facets for the current plot
    startIndex <- (i - 1) * nCols * nRows + 1
    endIndex <- min(startIndex + (nCols * nRows) - 1, totalFacets)

    facetsToPlot <- uniqueFacets[startIndex:endIndex]

    # Subset the data for the current plot
    dtValuesSubset <- dtValues[categoricCovariate %in% facetsToPlot$categoricCovariate &
                                 name %in% facetsToPlot$name]

    lineData <- createLineData(plotData = plotData[categoricCovariate %in% facetsToPlot$categoricCovariate &
                                                     name %in% facetsToPlot$name],
                               dtValues = dtValuesSubset,
                              xScale = xScale)

    # Create the plot for the current subset
    plotSubset <- ggplot(data = dtValuesSubset) +
      geom_point(mapping = aes(x = statusValue, y = ecdf, fill = status, shape = status)) +
      labs(x = '', y = 'cumulative distribution')

    if (xScale == SCALING$log) {
      plotSubset <- plotSubset + scale_x_log10()
    }

    plotSubset <- plotSubset +
      geom_line(data = lineData, aes(x = x, y = value, color = status, linetype = status), size = 1)

    plotSubset <- customizeLegend(plotSubset, colorScalingVector)

    # Add facetting to the plot
    plotSubset <- plotSubset +
      facet_wrap(vars(categoricCovariate, name), scales = 'free_x', ncol = nCols) +
      layerWatermark()

    # Store the plot in the list
    plotList[[i]] <- plotSubset
  }

  # Print all plots
  for (plot in plotList) {
    print(plot)
  }

  return(invisible())
}

#' Create and Print Predicted vs Observed
#'
#' This function generates a plot for each unique outputPathId in the provided dataset.
#' Each plot displays predicted vs observed values and includes facets for scenario and group.
#'
#' @param dtRes A data frame containing the data to be plotted. It should include columns for
#'              scenario, outputPathId, and group.
#' @param addRegression A logical value indicating whether to add regression lines to the plot.
#' @param xyScale A character string specifying the scale type for the x and y axes (default "Log").
#' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 2)
#' @param ... additional arguments passed on to ospsuite.plots::plotPredVsObs
#'
#' @return NULL This function does not return a value; it prints the plots directly.
plotPredictedVsObserved <- function(dtRes, addRegression = TRUE, xyScale = unlist(SCALING), nCols = 2,...) {

  xyScale <- tolower(match.arg(xyScale))

  # Get unique outputPathIds
  outputPathIds <- unique(dtRes$outputPathId)

  # Loop through each outputPathId and create a plot
  for (id in outputPathIds) {
    # Filter data for the current outputPathId
    filteredData <- dtRes[dtRes$outputPathId == id, ]

    # Create the base plot
    basePlot <- ospsuite_plotPredictedVsObserved(
      plotData = filteredData,
      addRegression = addRegression,
      comparisonLineVector = getFoldDistanceList(folds = c()),
      xyscale = xyScale,
      groupAesthetics = c(),
      ...
    ) +
      labs(title = outputPathIds)

    # Add facet wrapping by scenario and group
    finalPlot <- basePlot +
      facet_wrap(vars(scenario, group), ncol = nCols)

    # Print the plot
    print(finalPlot)
  }
}
#' Create and Print Residuals vs Observed Plots for Each Output Path ID
#'
#' This function generates a plot for residuals versus observed values from the provided dataset.
#' Each plot includes facets for scenario and group, and is created for each unique outputPathId.
#' The facets are arranged in a grid defined by the number of columns and rows.
#'
#' @param dtRes A data frame containing the data to be plotted. It should include columns for
#'              scenario, outputPathId, group, and the residuals.
#' @param xscale A character string specifying the scale type for the x-axis (e.g., "log").
#' @param nCols An integer specifying the number of columns for the facet wrap.
#' @param nRows An integer specifying the number of rows for the facet wrap.
#'
#' @return NULL This function does not return a value; it prints the plots directly.
#'
#' @examples
#' createResidualsPlot(dtRes, "log", 3, 2)
#'
#' @import ggplot2
plotResidualsVsTime <- function(dtRes, nCols = 2,...) {
  # Get unique outputPathIds
  outputPathIds <- unique(dtRes$outputPathId)

  # Loop through each outputPathId and create a plot
  for (id in outputPathIds) {
    # Filter data for the current outputPathId
    filteredData <- dtRes[dtRes$outputPathId == id, ]

    # Create the base plot for residuals vs observed
    basePlot <- ospsuite_plotResidualsVsTime(filteredData)

    # Add facet wrapping by scenario and group
    finalPlot <- basePlot +
      facet_wrap(vars(scenario, group), ncol = nCols)

    # Print the plot
    print(finalPlot)
  }
}

# auxiliary ------------

#' Create Line Data for Hyperparameters
#'
#' This function generates the line data for hyperparameters based on their distributions.
#'
#' @param plotData A data.table containing the data to be processed.
#' @param dtValues A data.table with ECDF calculated.
#' @param xScale A character string specifying the scale of the x-axis.
#' @return A data.table containing the line data for hyperparameters.
#' @keywords internal
createLineData <- function(plotData, dtValues, xScale) {
  dtHyper <- plotData[valueMode == PARAMETERTYPE$hyperParameter]
  lineData <- data.table()

  for (dtHyperPar in split(dtHyper, by = c('name', 'categoricCovariate', 'status'))) {
    dtHyperPar <- dtHyperPar %>% setDT()

    x <- if (xScale == SCALING$log) {
      exp(seq(log(dtValues[name == dtHyperPar$name[1]]$minValue[1]),
              log(dtValues[name == dtHyperPar$name[1]]$maxValue[1]),
              length.out = 100))
    } else {
      seq(dtValues[name == dtHyperPar$name[1]]$minValue[1],
          dtValues[name == dtHyperPar$name[1]]$maxValue[1],
          length.out = 100)
    }

    argList <- stats::setNames(as.numeric(dtHyperPar[['statusValue']]), as.character(dtHyperPar[['label']]))

    y <- do.call(paste0('p', dtHyperPar$hyperDistribution[1]), args = c(list(q = x), argList))

    lineData <- rbind(lineData,
                      data.table(x = x,
                                 value = y,
                                 status = dtHyperPar$status[1],
                                 name = dtHyperPar$name[1],
                                 categoricCovariate = dtHyperPar$categoricCovariate[1]))

  }

  return(lineData)
}

#' Customize Legend
#'
#' This function customizes the legend for the plot.
#'
#' @param plotObject The ggplot object to which the legend will be added.
#' @param colorScalingVector A named vector of colors for different statuses.
#' @return A ggplot object with a customized legend.
#' @keywords internal
customizeLegend <- function(plotObject, colorScalingVector) {
  legendTitleShape <- 'Individual Values'
  legendTitleLine <- 'Distribution'

  plotObject +
    scale_linetype_manual(values = c('dotted', 'solid', 'twodash'),
                          breaks = names(colorScalingVector)) +
    scale_shape_manual(values = c('square filled', 'triangle filled', 'circle filled'),
                       breaks = names(colorScalingVector)) +
    scale_color_manual(values = colorScalingVector,
                       breaks = names(colorScalingVector)) +
    scale_fill_manual(values = colorScalingVector,
                      breaks = names(colorScalingVector)) +
    guides(shape = guide_legend(title = legendTitleShape, order = 1),
           fill = guide_legend(title = legendTitleShape, order = 1),
           color = guide_legend(title = legendTitleLine, order = 2),
           linetype = guide_legend(title = legendTitleLine, order = 2))
}



#' Prepare Plot Data
#'
#' This function prepares the data for plotting by loading the necessary status files and
#' transforming the data into a suitable format for visualization.
#'
#' @param dtList A list containing prior and start values data tables.
#' @param currentStatus A list which contains the current values.
#' @param bestStatus A list which contains the best values.
#' @return A data.table containing the prepared plot data.
preparePlotDataParameterValues <- function(dtList, currentStatus,bestStatus) {
  # startValue
  plotData <- prepareInputData(dtPrior = dtList$prior,
                               dtStartValues = dtList$startValues,
                               valueColumn = 'startValue') %>%
    setnames(old = 'value',new = 'startValue')

  plotData[, startParam := plogis(transformToUnbounded(value = startValue,minValue = minValue,maxValue = maxValue,scaling = scaling)), .I]

  # current
  plotData$param <- currentStatus$params
  plotData[, currentValue := inverseTransformParams(param = param,minValue = minValue,maxValue = maxValue,scaling = scaling), .I]
  plotData[, currentParam := plogis(param), .I]

  # best
  plotData$param <- bestStatus$params
  plotData[, bestParam := plogis(param), .I]
  plotData[, bestValue := inverseTransformParams(param = param,minValue = minValue,maxValue = maxValue,scaling = scaling), .I]

  plotData <- enhancePlotDataParameterValues(plotData, dtList)

  return(plotData)
}



#' Enhance Plot Data
#'
#' This function adds additional columns to the plot data for labeling and categorization.
#'
#' @param plotData A data.table containing the initial plot data.
#' @param dtList A list containing prior and start values data tables.
#' @return A data.table with enhanced plot data.
enhancePlotDataParameterValues <- function(plotData, dtList) {

  # Create the plotData data.table
  plotData <- cbind(plotData,
                    rbindlist(list(dtList$prior[, .(name, valueMode, label = hyperParameter, hyperDistribution, categoricCovariate)],
                                   dtList$startValues[, .(
                                     name,
                                     valueMode = PARAMETERTYPE$individual,
                                     label = individualId,
                                     hyperDistribution = '',
                                     categoricCovariate
                                   )])))

  plotData$label <- factor(plotData$label, levels = unique(plotData$label), ordered = TRUE)

  #reshapes the plot data for visualization by melting the data.table.
  plotData[,param:= NULL]
  # Assuming plotData is already a data.table
  plotData <- melt(plotData,
                   measure.vars = c('startValue', 'currentValue', 'bestValue',
                                    'startParam', 'currentParam', 'bestParam'),
                   variable.name = 'status',
                   value.name = 'statusValue')

  # Create the 'type' column and modify the 'status' column
  plotData[, type := ifelse(grepl('Param', status), 'statusParam', 'statusValue')]
  plotData[, status := gsub('Value', '', gsub('Param', '', status))]

  # Reshape the data to wide format
  plotData <- dcast(plotData, ... ~ type, value.var = 'statusValue')

  return(plotData)
}


# debug -------------------
evaluateLogLikelihoodAtFailedStatus <- function(projectConfiguration,runName,scenarioList){
  outputDir <- getOutputDirectoryForRun(projectConfiguration, runName)
  dtList <- loadListsForRun(projectConfiguration, runName)

  status <- readRDS(file.path(outputDir, 'failedOptimStatus.RDS'))

  loglikelihoods <- getLogLikelihood(status$params, scenarioList, dtList, simulationRunOptions = NULL)



}
