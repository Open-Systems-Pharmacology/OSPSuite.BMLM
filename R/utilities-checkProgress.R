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
plotConvergence <- function(dtConvergence,
                            displayVariablesIndx = seq(1,5),
                            titletxt = NULL,
                            nPoints = 200,
                            selectionMode = c('last', 'random', 'first')) {

  columnheaders <- c(objectiveValue = 'value of objective function: -loglikelihood',
                     logTimeProfile = '- loglikelihood TimeProfile',
                     logHyperParameter = '- loglikelihood HyperParameter',
                     logPrior = '- loglikelihood Prior',
                     percentageOfFailure = 'percentage of failed iterations')

  # Match the selection mode to ensure it's one of the allowed values
  selectionMode <- match.arg(selectionMode)
  displayVariablesIndx <- as.integer(displayVariablesIndx)
  checkmate::assertInteger(displayVariablesIndx,lower = 1,upper = length(columnheaders),unique = TRUE,any.missing = FALSE)

  displayVariables <- names(columnheaders[displayVariablesIndx])


  # Calculate the objective value as the negative sum of log likelihoods
  dtConvergence[, objectiveValue := -(logTimeProfile + logHyperParameter + logPrior)]
  dtConvergence[,  `:=` (logTimeProfile = - logTimeProfile,
                         logHyperParameter = -logHyperParameter,
                         logPrior = -logPrior)]
  dtConvergence[,percentageOfFailure := NAcounter/iteration*100]

  nPointsAvailable <- nrow(dtConvergence[event == 'best'])
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
  plotData <- data.table::melt(dtConvergence[event == 'best'],
                               measure.vars = displayVariables,
                               variable.name = 'summand',
                               value.name = 'value')
  plotData$summand <- factor(plotData$summand,
                             levels = displayVariables)
  levels(plotData$summand) <- columnheaders[levels(plotData$summand)]

  # Create the plot using ggplot2
  plotObject <- ggplot(plotData, mapping = aes(x = iteration, y = value)) +
    geom_step(mapping = aes(color = 'current', linetype = 'current')) +
    geom_hline(mapping = aes(yintercept = value, color = 'start', linetype = 'start'),
               data = plotData[iteration == 1]) +
    facet_wrap(~summand, ncol = 1, scales = 'free_y') +
    scale_color_manual(values = c(current = 'black', start = 'darkred')) +
    scale_linetype_manual(values = c(current = 'solid', start = 'dotted')) +
    labs(y = '', color = '', linetype = '',
         title = titletxt) +
    theme(legend.position = 'none') +
    layerWatermark()

  # add restart
  if (any(dtConvergence$event == 'restart')){
    plotObject <- plotObject +
      geom_vline(data = dtConvergence[event == 'restart'],mapping = aes(xintercept = iteration) ) +
      labs(caption = 'vertical lines indicate restart of algorithm')
  }

  # Print the plot to the console
  print(plotObject)

  return(invisible(plotObject))
}
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
#' Default includes dark green, light blue, and orange.
#'
#' @return NULL This function does not return a value; it prints the plots directly.
#' @export
plotParameterLimits <-
  function(dtList,
           statusList,
           titeltxt,
           nCols,
           nRows,
           colorScalingVector =  c(
             current = 'darkgreen',
             start = 'lightblue',
             best = 'orange'
           )) {

    plotData <- preparePlotDataParameterValues(dtList =  dtList,
                                               currentStatus = statusList$current,
                                               bestStatus = statusList$best)
    plotData[,xlabel := '']
    plotData[valueMode ==  PARAMETERTYPE$individual,xlabel :=individualId]
    plotData[valueMode ==  PARAMETERTYPE$hyperParameter, xlabel :=hyperParameter,by = .I]

    plotData$xlabel <- factor(plotData$xlabel, levels = unique(plotData$xlabel), ordered = TRUE)

    plotData <- reshapePlotDataParameterValues(plotData)


    plotData[,label := ifelse(is.na(categoricCovariate),name,paste0(name,' (',categoricCovariate,')'))]
    plotData$label <- factor(plotData$label, levels = unique(plotData$label), ordered = TRUE)

    plotDataInd <- plotData[!(valueMode %in% c(PARAMETERTYPE$global, PARAMETERTYPE$outputError))]
    plotDataNonInd <- plotData[(valueMode %in% c(PARAMETERTYPE$global, PARAMETERTYPE$outputError))]

    # Determine unique facets
    uniqueFacets <- unique(plotDataInd$label)
    totalFacets <- length(uniqueFacets)

    # Calculate how many plots are needed
    totalPlots <- ceiling(totalFacets / (nCols * nRows))

    mapping <- aes(y = statusParam, color = status, shape = status)

    for (iPlot in seq(0,totalPlots)) {
      if (iPlot == 0) {
        plotObject <- ggplot(plotDataNonInd) +
          suppressWarnings(geom_point(utils::modifyList(mapping,aes(x = name)))) +
          facet_wrap(vars(valueMode), ncol = 2, scales = 'free_y')
      } else {
        # Determine the facets for the current plot
        startIndex <- (iPlot - 1) * nCols * nRows + 1
        endIndex <- min(startIndex + (nCols * nRows) - 1, totalFacets)

        facetsToPlot <- uniqueFacets[startIndex:endIndex]

        # Create the plot using the subset
        plotObject <- ggplot( plotDataInd[label %in% facetsToPlot]) +
          suppressWarnings(geom_point(utils::modifyList(mapping,aes(x = xlabel)))) +
          facet_wrap(vars(label), ncol = nCols, scales = 'free_y')

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
        layerWatermark() +
        theme(legend.direction = 'horizontal')

      print(plotObject)

    }

    return(invisible())
  }

#' Plot Distributions
#'
#' This function creates a series of plots to visualize the cumulative distribution of individual values and hyperparameters.
#'
#' @param dtList A data.table containing the data to be plotted.
#' @param currentStatus A data.table with the current parameter values.
#' @param bestStatus A data.table with the best parameter values.
#' @param nCols An integer specifying the number of columns for faceting. Default is 2.
#' @param nRows An integer specifying the maximum number of rows for faceting. Default is 3.
#' @param xScale A character string specifying the scale of the x-axis. Default is 'log'.
#' @param titeltxt A string to include in the plot title.
#' @param colorScalingVector A named vector of colors for different statuses. Default includes light blue, dark green, and orange.
#'
#' @return Returns an invisible NULL after printing the plots.
#' @export
plotDistributions <- function(dtList,
                              currentStatus = NULL,
                              bestStatus,
                              nCols = 2,
                              nRows = 3,
                              xScale = unlist(SCALING),
                              titeltxt = NULL,
                              colorScalingVector =  c(
                                start = 'lightblue',
                                current = 'darkgreen',
                                best = 'orange'
                              )) {

  xScale <- tolower(match.arg(xScale))

  plotData <- preparePlotDataParameterValues(dtList = dtList,
                                             currentStatus = currentStatus,
                                             bestStatus = bestStatus)
  plotData <- reshapePlotDataParameterValues(plotData)

  plotData <- addLabel(plotData = plotData,dtPrior =  dtList$prior)

  dtValues <- plotData[valueMode == PARAMETERTYPE$individual] %>%
    setorderv(c('label', 'status', 'statusValue'))

  dtValues[, ecdf := seq_len(.N) / .N, by = c('status', 'label')]


  hyperParameter <- setlogTruncationOffset(dtPrior = plotData[valueMode == PARAMETERTYPE$hyperParameter]  %>%
                                             merge(dtList$prior[,c("name","categoricCovariate",'hyperDistribution')] %>%
                                                     unique() ,
                                                   by = c("name","categoricCovariate")) %>%
                                             setnames('statusValue','value'),
                                           dtStartValues =  dtValues,
                                           identifier = c('name', 'categoricCovariate','status'),
                                           colsToKeep = c(
                                             "hyperParameter",
                                             "value",
                                             "hyperDistribution",
                                             "scaling",
                                             'logTruncationOffset',
                                             'minValue.indValues',
                                             'maxValue.indValues',
                                             'minValue',
                                             'maxValue',
                                             'label')
  )
  # Determine unique facets
  uniqueFacets <- unique(dtValues$label)
  totalFacets <- length(uniqueFacets)

  # Calculate how many plots are needed
  totalPlots <- ceiling(totalFacets / (nCols * nRows))

  for (iPlot in seq_len(totalPlots)) {

    # Determine the facets for the current plot
    startIndex <- (iPlot - 1) * nCols * nRows + 1
    endIndex <- min(startIndex + (nCols * nRows) - 1, totalFacets)

    facetsToPlot <- uniqueFacets[startIndex:endIndex]

    # Subset the data for the current plot
    dtValuesSubset <- dtValues[label %in% facetsToPlot]

    lineData <- createLineData(hyperParameter = hyperParameter[label %in% facetsToPlot],
                               xScale = xScale)

    # Create the plot for the current subset
    plotObject <- ggplot(data = dtValuesSubset) +
      geom_point(mapping = aes(x = statusValue, y = ecdf, fill = status, shape = status)) +
      labs(x = 'parameter values',
           y = 'cumulative proportion',
           title = titeltxt)

    if (xScale == SCALING$log) {
      plotObject <- plotObject + scale_x_log10()
    }

    plotObject <- plotObject +
      geom_line(data = lineData, aes(x = x, y = value, color = status, linetype = status), linewidth = 1)

    plotObject <- customizeLegend(plotObject, colorScalingVector)

    # Add facetting to the plot
    plotObject <- plotObject +
      facet_wrap(vars(label), scales = 'free_x', ncol = nCols) +
      layerWatermark()

    # Store the plot in the list
    print(plotObject)
  }

  for (dtHyper in split(hyperParameter, by = 'label')){
    dtHyper[,truncationOffset := 1-exp(logTruncationOffset) ]
    tmp <- rbind(dcast(dtHyper[,c('hyperParameter','status','value','minValue','maxValue')],
                       ... ~ status , value.var = 'value') ,
                 dcast(dtHyper[,c('truncationOffset','status','minValue.indValues','maxValue.indValues')] %>% unique(),
                       +       ... ~ status , value.var = 'truncationOffset') %>%
                   setnames(c('minValue.indValues','maxValue.indValues'),
                            c('minValue','maxValue')) %>%
                   dplyr::mutate(hyperParameter = 'likelihood outside range'),
                 fill = TRUE) %>%
      setnames('hyperParameter','.')
    print(knitr::kable(tmp,caption = dtHyper$label[1]))
  }


  return(invisible())
}
#' Plot Correlations
#'
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
#' @param pValueCut A numeric value for the Chi-squared cutoff threshold. Default is 0.1.
#'
#' @return NULL This function does not return a value; it prints the correlation plot directly.
#' @export
plotCorrelations <- function(dtList,
                             statusList,
                             titeltxt = NULL,
                             method = 'spearman',
                             statusToShow = c('best', 'current', 'start'),
                             scenarioList,
                             corCut = 0.5,
                             pValueCut = 0.1,
                             nPlotsPopulation = 12){

  statusToShow = match.arg(statusToShow)

  plotData <- preparePlotDataParameterValues(dtList =  dtList,
                                             currentStatus = statusList$current,
                                             bestStatus = statusList$best)
  plotData <- reshapePlotDataParameterValues(plotData[valueMode == PARAMETERTYPE$individual])
  plotData <- plotData[status == statusToShow]
  plotData <- addLabel(plotData = plotData,dtPrior =  dtList$prior,unitSep = '\n',identifier = 'name')

  labels <- unique(plotData$label)

  plotData <- dcast(plotData[,c('statusValue','label','individualId')], ... ~ label, value.var = 'statusValue')
  pm <- GGally::ggpairs(plotData,
                        columns = labels,
                        switch = 'y',
                        diag = list(continuous = GGally::wrap("barDiag",bins = 20,na.rm = TRUE)),
                        upper = list(continuous = GGally::wrap("cor", method = method,use = "complete.obs")),
                        lower = list(continuous = GGally::wrap("points", na.rm = TRUE)),
                        title = titeltxt,
                        mapping = aes(shape = 'circle')) +
    theme(strip.placement = 'outside')

  print(pm)

  checkForRelevantColumnsOfPopulation(plotData = plotData,
                                      labels = labels,
                                      scenarioList = scenarioList,
                                      corCut = corCut,
                                      pValueCut = pValueCut,
                                      method = method,
                                      nPlotsPopulation = nPlotsPopulation,
                                      dtMappedPaths = dtList$mappedPaths)




  return(invisible())
}
#' Create and Print Predicted vs Observed Plots
#'
#' This function generates a plot for each unique outputPathId in the provided dataset.
#' Each plot displays predicted vs observed values and includes facets for scenario and group.
#'
#' @param dtRes A data frame containing the data to be plotted. It should include columns for
#'              scenario, outputPathId, and group.
#' @param addRegression A logical value indicating whether to add regression lines to the plot.
#' @param xyScale A character string specifying the scale type for the x and y axes (default "log").
#' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 2)
#' @param titeltxt A string to include in the plot title.
#' @param ... Additional arguments passed on to ospsuite.plots::plotPredVsObs.
#'
#' @return NULL This function does not return a value; it prints the plots directly.
#' @export
plotPredictedVsObserved <- function(
    dtRes,
    addRegression = TRUE,
    xyScale = unlist(SCALING),
    nCols = 2,
    titeltxt = NULL,
    ...) {

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
      labs(subtitle = id,
           title = titeltxt)

    # Add facet wrapping by scenario and group
    finalPlot <- basePlot +
      facet_wrap(vars(scenario, group), ncol = nCols)

    # Print the plot
    print(finalPlot)
  }
}
#' Create and Print Predicted vs Time Plots
#'
#' This function generates plots to compare predicted values against time.
#'
#' @param dtRes A data frame containing the data to be plotted.
#' @param yScale A character string specifying the scale for the y-axis (default is "log").
#' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 4)
#' @param titeltxt A string to include in the plot title.
#' @param ... Additional arguments passed on to ospsuite.plots::plotTimeProfile.
#'
#' @return NULL This function does not return a value; it prints the plots directly.
#' @export
plotPredictedVsTime <- function(
    dtRes,
    yScale = unlist(SCALING),
    nCols = 4,
    titeltxt = NULL,
    ...) {

  yScale <- tolower(match.arg(yScale))

  for (dtResGroup in split(dtRes, by = c('outputPathId','scenarioName'))){

    dtResGroup <- setDT(dtResGroup)

    # for inidividuals with only one measurement plot predcition as stright line
    dtIndCount <- dtResGroup[,.N,by = individualId]

    plotData = rbind(dtResGroup,
                     dtResGroup[individualId %in% dtIndCount[N>1]$individualId] %>%
                       .[,dataType := 'simulated'])

    plotDataSingleValue <- dtResGroup[individualId %in% dtIndCount[N==1]$individualId]

    yUnit <- dtResGroup$yUnit[1]

    plotObject <-
      ospsuite_plotTimeProfile(plotData = plotData,
                               mapping = aes(y = predicted,groupby = outputPathId),
                               observedMapping = aes(y = yValues,
                                                     groupby = outputPathId),
                               yscale = tolower(yScale),
                               xscale.args = list(limits = c(NA,NA)),
                               ...) +
      facet_wrap(vars(individualId),ncol = min(nCols,dplyr::n_distinct(dtResGroup$individualId))) +
      labs(title = titeltxt,
           subtitle = dtResGroup$scenario[1],
           y = dtResGroup$outputPathId[1]) +
      theme(legend.position = 'none')

    if (nrow(plotDataSingleValue) > 1) {
      plotObject <- plotObject +
        geom_point(mapping = aes(x = xValues, y = predicted),
                   data = plotDataSingleValue, shape = 'plus')
    }

    print(plotObject)
  }

}
#' Create and Print Residuals vs Time Plots
#'
#' This function generates a plot for residuals versus observed values from the provided dataset.
#' Each plot includes facets for scenario and group, and is created for each unique outputPathId.
#'
#' @param dtRes A data frame containing the data to be plotted. It should include columns for
#'              scenario, outputPathId, group, and the residuals.
#' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 2)
#' @param titeltxt A string to include in the plot title.
#' @param ... Additional arguments passed on to ospsuite.plots::plotResVsCov.
#'
#' @return NULL This function does not return a value; it prints the plots directly.
#' @export
plotResidualsVsTime <- function(dtRes, nCols = 2,titeltxt = NULL,...) {

  # Get unique outputPathIds
  outputPathIds <- unique(dtRes$outputPathId)

  # Loop through each outputPathId and create a plot
  for (id in outputPathIds) {
    # Filter data for the current outputPathId
    filteredData <- dtRes[dtRes$outputPathId == id, ]

    # Create the base plot for residuals vs observed
    plotObject <- ospsuite_plotResidualsVsTime(filteredData,
                                               mapping = aes(y =resNorm),
                                               groupAesthetics = c()) +
      facet_wrap(vars(scenario, group), ncol = nCols) +
      labs(y = 'Residuals',
           subtitle = id,
           title = titeltxt)

    # Print the plot
    print(plotObject)
  }
}
#' Create and Print Residuals as Histogram Plots
#'
#' This function generates histogram plots of the residuals to visualize their distribution.
#'
#' @param dtRes A data frame containing the data to be plotted. It should include columns for
#'              scenario, outputPathId, group, and the residuals.
#' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 2)
#' @param titeltxt A string to include in the plot title.
#' @param ... Additional arguments passed on to ospsuite.plots::plotHistogram.
#'
#' @return NULL This function does not return a value; it prints the plots directly.
#' @export
plotResidualsAsHistogram <- function(dtRes, nCols = 2,titeltxt = NULL,...) {

  # Get unique outputPathIds
  outputPathIds <- unique(dtRes$outputPathId)

  # Loop through each outputPathId and create a plot
  for (id in outputPathIds) {
    # Filter data for the current outputPathId
    filteredData <- dtRes[dtRes$outputPathId == id, ]

    # Create the base plot for residuals vs observed
    plotObject <- ospsuite.plots::plotHistogram(filteredData,mapping = aes(x = resNorm, color = 'blue'),
                                                distribution = 'normal',plotAsFrequency = TRUE) +
      stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                    color = "black", linewidth = 1) +
      geom_vline(xintercept = 0,linewidth = 0.5) +
      facet_wrap(vars(scenario, group), ncol = nCols) +
      theme(legend.position = 'none') +
      labs(x = 'Residuals',
           subtitle = id,
           title = titeltxt)

    # Print the plot
    print(plotObject)
  }
}
#' Create and Print Residuals as QQ Plot
#'
#' This function generates a QQ plot to assess the normality of the residuals.
#'
#' @param dtRes A data frame containing the data to be plotted. It should include columns for
#'              scenario, outputPathId, group, and the residuals.
#' @param nCols An integer specifying the number of columns for the facet wrap. (Default = 2)
#' @param titeltxt A string to include in the plot title.
#' @param ... Additional arguments passed on to ospsuite.plots::plotQQ.
#'
#' @return NULL This function does not return a value; it prints the plots directly.
#' @export
plotResidualsAsQQ <- function(dtRes, nCols = 2,titeltxt = NULL,self...) {

  # Get unique outputPathIds
  outputPathIds <- unique(dtRes$outputPathId)

  # Loop through each outputPathId and create a plot
  for (id in outputPathIds) {
    # Filter data for the current outputPathId
    filteredData <- dtRes[dtRes$outputPathId == id, ]

    # Create the base plot for residuals vs observed
    plotObject <- ospsuite.plots::plotQQ(data = filteredData,mapping = aes(sample = resNorm)) +
      facet_wrap(vars(scenario, group), ncol = nCols) +
      theme(legend.position = 'none') +
      labs(y = 'Residuals',
           subtitle = id,
           title = titeltxt)

    # Print the plot
    print(plotObject)
  }
}
#' Get Current Configuration Table
#'
#' This function retrieves the current configuration table from the specified sheet in the Excel workbook.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including paths to Excel files.
#' @param dtList A list of data.tables containing the values.
#' @param sheetName A character string specifying the name of the sheet to retrieve data from. Options are 'Prior' or 'IndividualStartValues'.
#'
#' @return A data.table containing the current configuration values.
#' @export
getCurrentConfigTable <- function(projectConfiguration, dtList,sheetName = c('Prior','IndividualStartValues')) {

  sheetName <- match.arg(sheetName)

  identifier <- switch(sheetName,
                       Prior = c("name", "hyperParameter", "categoricCovariate"),
                       IndividualStartValues = c("name", "individualId", "categoricCovariate")
  )

  dtOld <- switch(sheetName,
                  Prior = dtList$prior,
                  IndividualStartValues = dtList$startValues
  )


  wb <- openxlsx::loadWorkbook(file = projectConfiguration$addOns$bMLMConfigurationFile)

  dtNew <- addFinalValue(wb,
                         sheetName = sheetName,
                         identifier = identifier,
                         newTable = dtOld
  )

  return(dtNew)
}

# auxiliary ------------
#' Check for Relevant Columns in Population Data
#'
#' This function checks for relevant columns in the population data by merging it with
#' the plot data and assessing correlations and Chi-squared tests.
#'
#' @param plotData A data.table containing the plot data with individual values.
#' @param labels A character vector of labels to analyze for correlation.
#' @param scenarioList A list of scenarios to analyze for potential correlations.
#' @param corCut A numeric value for the correlation cutoff threshold. Default is 0.5.
#' @param pValueCut A numeric value for the Chi-squared cutoff threshold. Default is 0.1.
#' @param method A character string specifying the correlation method to use (default is 'spearman').
#'
#' @return NULL This function does not return a value; it prints correlation plots directly if relevant columns are found.
#' @keywords internal
checkForRelevantColumnsOfPopulation <- function(plotData,
                                           labels,
                                       scenarioList,
                                       corCut,
                                       pValueCut,
                                       method,
                                       nPlotsPopulation,
                                       dtMappedPaths){

  dtPop <- preparePopulationForCorrelationCheck(scenarioList,dtMappedPaths)

  mergedData <- merge(plotData,
                     dtPop,
                     by.x = 'individualId',
                     by.y = 'ObservedIndividualId')

  # Loop through each label to find correlation
  plotList = list()
  maxCorrelation = 0
  minPvalue = 1
  for (label in labels) {
    for (popCol in setdiff(names(dtPop), c('ObservedIndividualId'))) {
      iNonNans = which(!is.na(mergedData[[popCol]]) &
                         !is.na(mergedData[[label]]))
      if (length(iNonNans) > 3){
        plotData <- copy(mergedData) %>%
          setnames(old = c(label,popCol),
                   new = c('label','popCol')) %>%
          dplyr::select(c('label','popCol'))

      if (is.numeric(mergedData[[popCol]])) {
        # Calculate correlation for numeric columns
        correlationValue <-
          cor(mergedData[[label]],
              mergedData[[popCol]],
              use = "complete.obs",
              method = method)
        maxCorrelation <- max(maxCorrelation,abs(correlationValue))
        if (abs(correlationValue) > corCut){
          plotList[[paste(label,popCol)]] <-
            ggplot(data = plotData,
                   mapping = aes(y = label,x = popCol)) +
            geom_point(fill = 'black',na.rm = TRUE) +
            geom_smooth(method = 'lm',formula = y ~ x,na.rm = TRUE) +
            labs(y = label,
                 x = popCol,
                 title = paste('Cor:',round(correlationValue,2)))
        }
      } else if (is.factor(mergedData[[popCol]])) {
        kruskalTestResult <- kruskal.test(label ~ popCol, data = plotData)
        if (!is.na(kruskalTestResult$p.value)){
          minPvalue <- min(minPvalue,kruskalTestResult$p.value)
          if (kruskalTestResult$p.value < pValueCut)
            plotList[[paste(label,popCol)]] <-
              ospsuite.plots::plotBoxWhisker(data = plotData,
                                             mapping = aes(y =label,x = popCol)) +
              geom_jitter(fill = 'black',na.rm = TRUE) +
              labs(y = label,
                   x = popCol,
                   title = paste('pValue:',signif(kruskalTestResult$p.value,2))) +
              theme(axis.text.x = element_text(angle = 45,hjust = 1))

        }
      }
      }
    }
  }

  if (length(plotList) > 0){
    iPlot <- 1
    while(iPlot <= length(plotList)){
      maxPlot <- min(length(plotList),iPlot + nPlotsPopulation -1)
      print(cowplot::plot_grid(plotlist = plotList[seq(iPlot,maxPlot)]))
      iPlot <- iPlot + nPlotsPopulation
    }

  } else{
    message(paste0('no correlated columns found in population.\n',
                  'maximal Correlation: ',round(maxCorrelation,2), '(cut: ',corCut,')\n',
                  'minimal pValue of Chisquare test: ',signif(minPvalue,2),'(cut: ',pValueCut,')'))
  }

  return(invisible())

}
#' Prepare Population Data for Correlation Check
#'
#' This function processes a list of scenarios and a data.table of mapped paths
#' to prepare a population dataset for correlation analysis. It combines populations
#' from multiple scenarios, removes unnecessary columns, converts character columns
#' to factors, and excludes identical factor columns that lead to the same patient groups.
#'
#' @param scenarioList A list of scenarios, where each scenario contains a population
#'                     that can be converted to a data.frame.
#' @param dtMappedPaths A data.table containing mapped paths, which includes linked
#'                      parameters to be excluded from the final dataset.
#'
#' @return A data.table containing the processed population data, with unique individuals
#'         and factors prepared for correlation analysis.
#'
#' @export
preparePopulationForCorrelationCheck <- function(scenarioList, dtMappedPaths) {

  dtPop <- rbindlist(lapply(scenarioList, function(scenario){
    ospsuite::populationToDataFrame(scenario$population) %>%
      setDT() }),
    fill = TRUE) %>%
    unique()

  # delete simulated individual ID to not confuse with individualId of observedData
  dtPop[, IndividualId := NULL]

  # Exclude unique columns
  uniqueColumns <- sapply(dtPop, function(col) length(unique(col[!is.na(col)])) == 1)
  dtPop <- dtPop[, !uniqueColumns, with = FALSE]

  # Exclude columns mapped with fit parameters
  dtPop <- dtPop %>% dplyr::select(!any_of(dtMappedPaths$linkedParameters))

  # convert characters to factor
  numericColumns <- names(dtPop)[sapply(dtPop, function(col) is.numeric(col))]
  for (col in setdiff(names(dtPop), c('ObservedIndividualId', numericColumns))) {
    dtPop[[col]] <- factor(dtPop[[col]])
  }

  # Check for identical factors
  excludedFactors = c()
  factorColumns <- names(dtPop)[sapply(dtPop, is.factor)]
  for (i in seq_len(length(factorColumns) - 1)) {
    if (!(factorColumns[i] %in% excludedFactors)) {
      nGroupedData = nrow(dtPop[,.N, by = c(factorColumns[i])])

      for (j in seq(i + 1, length(factorColumns))) {
        nGroupedData2 = nrow(dtPop[,.N, by = c(factorColumns[i], factorColumns[j])])

        if (nGroupedData == nGroupedData2) {
          message(paste("Factors", factorColumns[i], "and", factorColumns[j], "leads to the same patient groups.
                      Ignore", factorColumns[j], "for analysis."))
          excludedFactors = factorColumns[j]
        }
      }
    }
  }

  if (length(excludedFactors) > 1)
  dtPop <- dplyr::select(!any_of(excludedFactors))

  return(dtPop)
}

#' Create Line Data for Hyperparameters
#'
#' This function generates line data for hyperparameters based on their distributions.
#'
#' @param hyperParameter A data.table containing hyperparameter information.
#' @param xScale A character string specifying the scale of the x-axis ('linear' or 'log').
#'
#' @return A data.table containing the line data for hyperparameters.
#' @keywords internal
createLineData <- function(hyperParameter, xScale) {
  lineData <- data.table()

  for (dtHyperPar in split(hyperParameter, by = c('label', 'status'))) {

    x <- if (xScale == SCALING$log) {
      exp(seq(log(dtHyperPar$minValue.indValues[1]),
              log(dtHyperPar$maxValue.indValues[1]),
              length.out = 100))
    } else {
      seq(dtHyperPar$minValue.indValues[1],
          dtHyperPar$maxValue.indValues[1],
          length.out = 100)
    }

    argList <- stats::setNames(as.numeric(dtHyperPar[['value']]), as.character(dtHyperPar[['hyperParameter']]))

    y <- do.call(paste0('p', dtHyperPar$hyperDistribution[1]), args = c(list(q = x), argList))
    y <- (y - y[1])/diff(range(y))

    lineData <- rbind(lineData,
                      data.table(x = x,
                                 value = y,
                                 status = dtHyperPar$status[1],
                                 label = dtHyperPar$label[1]))

  }

  return(lineData)
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
customizeLegend <- function(plotObject, colorScalingVector) {
  legendTitleShape <- 'Individual Values'
  legendTitleLine <- 'Cumulative Distribution'

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
#' Prepare Plot Data for Parameter Values
#'
#' This function prepares the data for plotting by loading the necessary status files
#' and transforming the data into a suitable format for visualization.
#'
#' @param dtList A list containing prior and start values data tables.
#' @param currentStatus A list which contains the current values.
#' @param bestStatus A list which contains the best values.
#'
#' @return A data.table containing the prepared plot data.
#' @keywords internal
preparePlotDataParameterValues <- function(dtList, currentStatus,bestStatus) {
  # startValue
  plotData <- rbind(
    dtList$prior[, c(
      'id',
      'name',
      'categoricCovariate',
      'startValue',
      'minValue',
      'maxValue',
      'scaling',
      'valueMode',
      'hyperParameter'
    )] %>%
      dplyr::mutate(individualId = NA),
    dtList$startValues[, c('id',
                           'startValue',
                           'name',
                           'categoricCovariate',
                           'minValue',
                           'maxValue',
                           'scaling',
                           'individualId')] %>%
      dplyr::mutate(valueMode = PARAMETERTYPE$individual,
                    hyperParameter = '')
  )

  plotData[, startParam := plogis(
    transformToUnbounded(
      value = startValue,
      minValue = minValue,
      maxValue = maxValue,
      scaling = tolower(scaling)
    )),
    .I]

  # current
  plotData[ ,param := currentStatus$params[id]]
  plotData[, currentValue := inverseTransformParams(
    param = param,
    minValue = minValue,
    maxValue = maxValue,
    scaling = tolower(scaling)
  ),
  .I]
  plotData[, currentParam := plogis(param), .I]

  # best
  plotData[ ,param := bestStatus$params[id]]
  plotData[, bestParam := plogis(param), .I]
  plotData[, bestValue := inverseTransformParams(
    param = param,
    minValue = minValue,
    maxValue = maxValue,
    scaling = tolower(scaling)
  ), .I]

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
#' Calculate Residuals from Observed and Predicted Values
#'
#' This function calculates the residuals based on the observed and predicted values,
#' applying different models for the calculation depending on whether the data is censored.
#'
#' @param yValue A numeric value representing the observed value.
#' @param predicted A numeric value representing the predicted value.
#' @param model A character string specifying the model to use for calculating the residuals
#'               (options are "absolute", "proportional", "log_absolute").
#' @param sigma A numeric value representing the standard deviation of the residuals.
#' @param isCensored A logical value indicating whether the data is censored.
#' @param lloq A numeric value representing the lower limit of quantification.
#'
#' @return A numeric value representing the calculated residual.
#' @keywords internal
calculateResidual <- function(yValue, predicted, model, sigma, isCensored, lloq) {

  # Validate that all inputs are of the correct type and length
  checkmate::assertNumeric(yValue, len = 1)  # yValue should be a single numeric value
  checkmate::assertNumeric(predicted, len = 1)  # predicted should be a single numeric value
  checkmate::assertChoice(model, c("absolute", "proportional", "log_absolute"))  # model must be one of the specified choices
  checkmate::assertNumeric(sigma, len = 1)  # sigma should be a single numeric value
  checkmate::assertLogical(isCensored, len = 1)  # isCensored should be a single logical value
  checkmate::assertNumeric(lloq, len = 1)  # lloq should be a single numeric value

  # If predicted value is NA or below the lower bound, return log(0) (indicating a very low likelihood)
  if (is.na(predicted)) {
    return(NA)
  }


  # If the data is censored, calculate the probability of being above the lower limit of quantification (lloq)
  if (isCensored) {
    res <- switch(model,
                  absolute = (lloq - predicted)/sigma,
                  proportional = (lloq - predicted)/ (sigma * predicted),
                  log_absolute = (log(lloq) - log(predicted))/ sigma
    )
  } else {
    # If the data is notored, calculate the log likelihood based on the chosen model
    res <- switch(model,
                  absolute = (yValue - predicted)/sigma,
                  proportional =  (yValue - predicted)/ (sigma * predicted),
                  log_absolute =(log(yValue) - log(predicted))/ sigma
    )
  }


  return(res)  # Return the final log likelihood value
}
#' Add Labels to Plot Data
#'
#' This function merges the plot data with prior data to add labels for better visualization.
#'
#' @param plotData A data.table containing the initial plot data.
#' @param dtPrior A data.table containing prior values for merging.
#' @param unitSep A string to separate the name and unit in the label. Default is a space.
#'
#' @return A data.table with labels added for each parameter.
#' @keywords internal
addLabel <- function(plotData,dtPrior,unitSep = ' ',identifier = c('name', 'categoricCovariate')){

  plotData <- merge(plotData,
                    dtPrior[,c(..identifier,'unit')] %>%
                      unique(),
                    by = identifier)

  plotData[,label := ifelse(is.na(unit),name,paste0(name,unitSep,'[',unit,']'))]
  if ('categoricCovariate' %in% identifier){
    plotData[,label := ifelse(is.na(categoricCovariate) | categoricCovariate == '',
                              label,
                              paste0(label,' (',categoricCovariate,')'))]
  }

  return(plotData)

}
