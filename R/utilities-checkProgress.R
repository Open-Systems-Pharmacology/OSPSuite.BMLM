#' Check Convergence of Model Parameters
#'
#' This function checks the convergence of model parameters by reading a CSV file
#' containing convergence data and visualizing the results using ggplot2.
#'
#' @param projectConfiguration A list containing configuration settings for the project,
#' including the output folder path.
#' @param runName A character string specifying the name of the run.
#' @param nPoints An integer specifying the number of points to select for plotting.
#' Default is 200.
#' @param selectionMode A character string indicating the mode of selection for points.
#' Options are 'last', 'random', and 'first'. Default is 'last'.
#'
#' @return A ggplot object visualizing the convergence of model parameters.
#' The plot shows the values of logTimeProfile, logHyperParameter, logPrior,
#' and objectiveValue over iterations.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' plot <- checkConvergence(projectConfiguration, "run1")
#' print(plot)
#' }
#'
#' @export
checkConvergence <- function(projectConfiguration,
                             runName,
                             nPoints = 200,
                             selectionMode = c('last','random','first')){

  # Match the selection mode to ensure it's one of the allowed values
  selectionMode <- match.arg(selectionMode)

  # Construct the output directory path
  outputDir <- getOutputDirectoryForRun(projectConfiguration, runName)

  # Check if the convergence CSV file exists
  if (!file.exists(file.path(outputDir,'convergence.csv'))) {
    message(paste('convergence.csv does not exist yet, please wait'))
    return(NULL)
  }

  # Read the convergence data from the CSV file
  dtConvergence <- fread(file.path(outputDir,'convergence.csv'),
                         colClasses = c('integer','double','double','double'))

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
  if (nPointsAvailable > nPoints) {
    ixSelected <- switch(selectionMode,
                         last = seq(1, nPoints) + nPointsAvailable - nPoints,
                         random = sort(c(1, sample(seq(2, nPointsAvailable - 1), size = nPoints - 2, replace = FALSE), nPointsAvailable)),
                         first = seq(1, nPoints)
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
         title = runName) +
    theme(legend.title = element_blank(), legend.direction = 'horizontal')

  # Print the plot to the console
  print(plotObject)

  return(invisible(plotObject))
}
# ParameterValues ------------
#' Check Parameter Values and Create Plots
#'
#' This function checks the parameter values for a given run, prepares the data for plotting,
#' and creates visualizations of the current and best optimization statuses.
#'
#' @param projectConfiguration A list containing project configuration details.
#' @param runName A string indicating the name of the run to check.
#' @return A list of ggplot objects representing the plots for the parameter values.
#' @export
checkParameterValues <- function(projectConfiguration, runName,nCols = 2,nRows = 10, useInteractivePlots = FALSE,
                                 colorScalingVector =  c(
                                   start = 'lightblue',
                                   current = 'darkgreen',
                                   best = 'orange'
                                 )) {
  outputDir <- getOutputDirectoryForRun(projectConfiguration, runName)

  if (!checkStatusFilesExist(outputDir)) {
    message('Status files do not exist yet, please wait')
    return(NULL)
  }

  dtList <- loadListsForRun(projectConfiguration, runName)
  plotData <- preparePlotDataParameterValues(dtList, outputDir)

  createPlotsParameterValues(plotData, runName,nCols = nCols, nRows = nRows,useInteractivePlots,colorScalingVector)

  return(invisible())
}

#' Check if Status Files Exist
#'
#' This function checks if the required optimization status files exist in the specified output directory.
#'
#' @param outputDir A string representing the output directory path.
#' @return A logical value indicating whether the status files exist.
checkStatusFilesExist <- function(outputDir) {
  return(file.exists(file.path(outputDir, 'bestOptimStatus.RDS')) &&
           file.exists(file.path(outputDir, 'optimStatus.RDS')))
}

#' Prepare Plot Data
#'
#' This function prepares the data for plotting by loading the necessary status files and
#' transforming the data into a suitable format for visualization.
#'
#' @param dtList A list containing prior and start values data tables.
#' @param outputDir A string representing the output directory path.
#' @return A data.table containing the prepared plot data.
preparePlotDataParameterValues <- function(dtList, outputDir) {
  plotData <- prepareInputData(dtPrior = dtList$prior,
                               dtStartValues = dtList$startValues,
                               valueColumn = 'startValue') %>%
    setnames(old = 'value',new = 'startValue')

  plotData[, startParam := plogis(param), .I]

  currentStatus <- readRDS(file.path(outputDir, 'optimStatus.RDS'))
  plotData$param <- currentStatus$params
  plotData[, currentParam := plogis(param), .I]
  plotData[, currentValue := inverseTransformParams(param, maxValue, minValue, scaling), .I]
  printOptimizationStatus('current', currentStatus)

  bestStatus <- readRDS(file.path(outputDir, 'bestOptimStatus.RDS'))
  plotData$param <- bestStatus$params
  plotData[, bestParam := plogis(param), .I]
  plotData[, bestValue := inverseTransformParams(param, maxValue, minValue, scaling), .I]
  printOptimizationStatus('best', bestStatus)

  plotData <- enhancePlotDataParameterValues(plotData, dtList)

  return(plotData)
}

#' Print Optimization Status
#'
#' This function prints the optimization status information for the current or best status.
#'
#' @param statusType A string indicating whether the status is 'current' or 'best'.
#' @param status A list containing the optimization status details.
printOptimizationStatus <- function(statusType, status) {
  cat(sprintf('%s:\n    iteration: %d\n    objective function value: %.2f\n',
              statusType, status$iteration, -sum(status$loglikelihood)))
}

#' Enhance Plot Data
#'
#' This function adds additional columns to the plot data for labeling and categorization.
#'
#' @param plotData A data.table containing the initial plot data.
#' @param dtList A list containing prior and start values data tables.
#' @return A data.table with enhanced plot data.
enhancePlotDataParameterValues <- function(plotData, dtList) {
  plotData$name <- c(dtList$prior$name, dtList$startValues$name)
  plotData$valueMode <- c(dtList$prior$valueMode, rep('individualValue', nrow(dtList$startValues)))
  plotData$label <- c(dtList$prior$hyperParameter, dtList$startValues$individualId)
  plotData$categoricCovariate <- c(dtList$prior$categoricCovariate, dtList$startValues$categoricCovariate)

  plotData[, categoricCovariate := as.character(categoricCovariate)]
  plotData[is.na(categoricCovariate), categoricCovariate := '']

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
#' Create Plots
#'
#' This function creates ggplot objects based on the prepared plot data.
#' If the number of plots exceeds the specified grid size, it splits the plots into multiple sets.
#'
#' @param plotData A data.table containing the reshaped plot data.
#' @param runName A string indicating the name of the run to include in the plot title.
#' @param nCols An integer specifying the number of columns for the plot layout.
#' @param nRows An integer specifying the number of rows for the plot layout.
#' @return A list of ggplot objects representing the plots for the parameter values.
createPlotsParameterValues <- function(plotData, runName, nCols, nRows,useInteractivePlots,colorScalingVector) {

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
      p <- ggplot(plotData[valueMode %in% c(PARAMETERTYPE$global, PARAMETERTYPE$outputError)]) +
        geom_point(utils::modifyList(mapping,aes(x = name))) +
        facet_wrap(vars(valueMode), ncol = 2, scales = 'free_y')
    } else {
      # Subset the data for the current set of plots
      startIndex <- (setIndex - 1) * (nCols * nRows) + 1
      endIndex <- min(startIndex + (nCols * nRows) - 1, nPlots)

      # Determine the rows to plot for the current set
      rowsToPlot <- seq(startIndex, endIndex)
      plotDataSubset <- uniqueCombinations[rowsToPlot]

      # Create the plot using the subset
      p <- ggplot(plotData[.(plotDataSubset), on = .(categoricCovariate, name)]) +
        geom_point(utils::modifyList(mapping,aes(x = label))) +
        facet_wrap(vars(categoricCovariate, name), ncol = nCols, scales = 'free_y')

    }

    p <- customizePlotAppearance(plot = p, runName = runName,colorScalingVector = colorScalingVector)

    if (useInteractivePlots){
      # Convert ggplot to plotly for interactivity
      plotlyPlot <- plotly::ggplotly(p, tooltip = "text")
      plotlyPlot <- plotly::config(plotlyPlot, displayModeBar = FALSE)
      print(plotlyPlot)
    } else {
      print(p)
    }

  }

  return(invisible())
}

#' Customize Plot Appearance
#'
#' This function customizes the appearance of a ggplot object.
#'
#' @param plot A ggplot object to be customized.
#' @param runName A string indicating the name of the run to include in the plot title.
#' @return A customized ggplot object.
customizePlotAppearance <- function(plot, runName,colorScalingVector) {
  return(
    plot +
      geom_hline(yintercept = c(0, 1)) +
      theme(legend.direction = 'horizontal') +
      scale_color_manual(
        values = colorScalingVector
      ) +
      coord_flip() +
      labs(
        x = '',
        y = '',
        color = '',
        shape = '',
        title = runName
      ) +
      scale_y_continuous(
        breaks = seq(0, 1, by = 0.25),
        labels = c('min', rep('', 3), 'max')
      )
  )
}
# debug -------------------
evaluateLogLikelihoodAtFailedStatus <- function(projectConfiguration,runName,scenarioList){
  outputDir <- getOutputDirectoryForRun(projectConfiguration, runName)
  dtList <- loadListsForRun(projectConfiguration, runName)

  status <- readRDS(file.path(outputDir, 'failedOptimStatus.RDS'))

  loglikelihoods <- getLogLikelihood(status$params, scenarioList, dtList, simulationRunOptions = NULL)



}
