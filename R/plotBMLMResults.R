plotBMLMResults <- function(projectConfiguration, subfolder, runName,
                            dataObserved,
                            scenarioResults,
                            nFacetColumns,
                            nMaxFacetRows,
                            referenceScaleVector = list(
                              'Final' = c(NA, NA),
                              'Start' = c(NA, NA)
                            )) {


  # initialize Container for RMD generation for .Rmd generation
  rmdContainer <-
    RmdContainer$new(
      rmdfolder = file.path(projectConfiguration$outputFolder),
      subfolder =  paste(subfolder,runName,sep = '_'))

  # ad intro
  rmdContainer$addHeader("runName", level = 1)

  dtList <- loadListsForRun(projectConfiguration,runName)

  # plotconvergence ! should be accesible during run

  # list fit parameters ! should be accesible during run


  rmdContainer <- plotIndividualValues(rmdContainer = rmdContainer,
                                       dtList = dtList,
                                       nFacetColumns = nFacetColumns,
                                       nMaxFacetRows = nMaxFacetRows,
                                       referenceScaleVector = referenceScaleVector)

  # plot timeprofiles
  # check if scenarios exists
  # simulate if necessary
  rmdContainer <- createPanelPlotsForPlotName(onePlotConfig = onePlotConfig,
                                          rmdContainer = rmdContainer,
                                          projectConfiguration = projectConfiguration,
                                          dataObserved = dataObserved,
                                          scenarioResults = scenarioResults,
                                          nFacetColumns = nFacetColumns,
                                          nMaxFacetRows = nMaxFacetRows,
                                          facetAspectRatio = TP.nMaxFacetRows,
                                          aggregationFun = NULL,
                                          referenceScaleVector = referenceScaleVector)

return(rmdContainer)
}


plotIndividualValues <- function(rmdContainer,dtList,maxPlots = 12,colorVector){

  # add  section headers
  rmdContainer$addHeader("Individual Values", level = 2)

  dStartValues <-split(dtList$startValues,by = c('parameter','categoricCovariate'))

  for (plotCount in seq(1, length(dStartValues), by = maxPlots)){

    # Get the current set of parameters
    currentStartValues <- rbindlist(dStartValues[plotCount:min(plotCount + maxPlots - 1, length(dStartValues))]) %>%
      setnames(old = c('startValue', 'value'),
               new = c('start', 'final'))
    currentPrior = dtList$prior %>% merge(currentStartValues[, c('parameter', 'categoricCovariate')], by = c('parameter', 'categoricCovariate')) %>%
      setnames(old = c('startValue', 'value'),
               new = c('start', 'final'))

    for (xScale in SCALING){

      plotObject <- plotOverlayHistogramDistribution(
        dtPrior = currentPrior,
        dtDefinition = dtList$definition[parameter %in% unique(currentStartValues$parameter)],
        dtStartValues = currentStartValues,
        xScale = xScale,
        colorVector = colorVector)

      rmdContainer$addAndExportFigure(
        plotObject = plotObject,
        caption ='Comaprison of individual start and final values',
        footNoteLines = NULL,
        figureKey = paste('individualValues',plotCount,xScale,sep = '_')
      )


    }

    for (hyperDist in unique(currentPrior$hyperDistribution)){
      # Create a data frame for the table
      tableData <- data.table::melt(currentPrior[hyperDist == hyperDistribution], id.vars = "hyperParameter", measure.vars = c("start", "final"),
                        variable.name = "status", value.name = "value")[, dcast(.SD, hyperParameter ~ status, value.var = "value")]


      rmdContainer$addAndExportTable(dt = dt,
                                     path = resultDirectory,
                                     caption = paste('Comparison of distributions parameters for ',hyperDistribution),
                                     figureKey = paste('individualValues',plotCount,xScale,sep = '_')
      )


    }


    dtPrior = currentPrior[,c('parameter','hyperParameter')]





  }
  return(rmdContainer)

}


# Define the function
plotOverlayHistogramDistribution <- function(dtPrior,
                                             dtStartValues,
                                             dtDefinition,
                                             colorVector = c(start = 'red', final = 'blue'),
                                             xScale = SCALING) {

  xScale <- match.arg(xScale)

  # Prepare the data for the histogram
  dtValues <- data.table::melt(dtStartValues[, .(start, final, individualId,parameter,categoricCovariate)] ,
                   measure.vars = c("start", "final"),
                   variable.name = "status",
                   value.name = "value") %>%
    dtValues[is.na(categoricCovariate),categoricCovariate:= '']

  setorderv(dtValues,c('parameter','status','value'))

  dtValues[,ecdf := cumsum(value)/sum(value), by = c('status','parameter','categoricCovariate')]

  # Create the plot
  plotObject <- ggplot(data = dtValues) +
    geom_point(aes(x = value, y = ecdf, fill = status, shape = status)) +
    facet_wrap(vars(parameter,categoricCovariate),scales = 'free_x') +
    theme(aspect.ratio = 1) +
    labs(x = parName,
         y = 'cumulative distribution')

  if (xScale == SCALING$log){
    plotObject <- plotObject +scale_x_log10()
  }


  # Create a data frame for the lines
  lineData <- data.table()
  for (dtPriorPar in split(dtPrior,by = c('parameter','categoricCovariate'))){

    dtPriorPar <- dtPriorPar %>%
      setDT()
    dtPriorPar[scaling == SCALING$log,start := log(start)]
    dtPriorPar[scaling == SCALING$log,final := log(final)]

    x <- seq(dtDefinition[parameter == dtPriorPar$parameter[1]]$minValue,
             dtDefinition[parameter == dtPriorPar$parameter[1]]$maxValue,
             length.out = 100)

    for (status in c('start', 'final')) {

      argList <- stats::setNames(
        as.numeric(dtPriorPar[[status]]),
        as.character(dtPriorPar[['hyperParameter']])
      )

      y <-
        do.call(paste0('p', dtPriorPar$hyperDistribution[1]),
                args = c(list(q = x),
                         argList))

      lineData <- rbind(lineData,
                        data.table(x = x,
                                   value = y,
                                   status = status,
                                   parameter =  dtPriorPar$parameter[1],
                                   categoricCovariate = dtPriorPar$categoricCovariate))

    }
  }

  # Add lines to the plot using geom_line
  plotObject <- plotObject +
    geom_line(data = lineData, aes(x = x, y = value, color = status,linetype = status), size = 1)

  # Update legend
  legendTitleShape <- 'Individual Values'
  legendTitleLine <- 'Distribution'
  plotObject <-
    plotObject +
    scale_linetype_manual(values = c('twodash', 'solid'),
                          breaks = c('start', 'final')) +
    scale_shape_manual(values = c('square filled', 'triangle filled'),
                       breaks = c('start', 'final')) +
    scale_color_manual(values = colorVector,
                       breaks = c('start', 'final')) +
    guides(shape = guide_legend(title = legendTitleShape,order = 1),
           fill = guide_legend(title = legendTitleShape,order = 1),
           color = guide_legend(title = legendTitleLine,order = 2),
           linetype = guide_legend(title = legendTitleLine,order = 2))

  return(plotObject)
}
