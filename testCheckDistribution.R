checkDistributionsForParameter <- function(dtList, bestStatus, currentStatus) {

  plotData <- preparePlotDataParameterValues(dtList = dtList,
                                             currentStatus = currentStatus,
                                             bestStatus = bestStatus)
  plotData <- reshapePlotDataParameterValues(plotData)
  plotData <- addLabel(plotData = plotData, dtPrior = dtList$prior)

  dtValues <- prepareDtValues(plotData)
  dtHyperValues <- prepareDtHyperValues(plotData)

  plotListAll = list()

  for (dHyperValues in split(dtHyperValues, by = 'label')) {
    plotListLabel = list()

    dValues <- dtValues[label == dHyperValues$label[[1]]]
    dPrior <- merge(dtList$prior, dHyperValues[, c("id","status","statusValue")], by = 'id')

    dHyperValues <- computeHyperValues(dHyperValues, dPrior, dValues)
    dValues <- merge(dValues, unique(dHyperValues[, c('label', 'densityNorm', 'pUB', 'pLB', 'status')]), by = c('label', 'status'))
    dValues <- computeDensity(dValues, dHyperValues, dPrior)

    plotObject <- createDensityPlot(dValues, dHyperValues, dPrior)
    plotObject <- adjustxAxis(plotObject, dValues, dHyperValues)

    plotListLabel[['density']] <- plotObject

    # Calculate sum(log(density)) for each status
    logLikelihoods <- dValues[, .(logLikelihood = sum(log(density[density > 0]), na.rm = TRUE)), by = status]

    plotListLabel[['table_logLikelihoods']] <- logLikelihoods

    plotObject <- createCumulativeProbabilityPlot(dValues, dHyperValues, dPrior)
    plotObject <- adjustxAxis(plotObject, dValues, dHyperValues)

    plotListLabel[['cumulatedProbability']] <- plotObject

    dHyperValuesTable <- createHyperValuesTable(dHyperValues)
    plotListLabel[['table_hyperparameter']] <- dHyperValuesTable

    plotListLabel[['priorPlots']] <- createPriorPlots(dPrior)

    plotListAll[[dHyperValues$label[1]]] <- plotListLabel
  }

  return(plotListAll)
}

prepareDtValues <- function(plotData) {
  dtValues <- plotData[valueMode == PARAMETERTYPE$individual] %>%
    setorderv(c('label', 'status', 'statusValue'))

  dtValues[, ecdf := seq_len(.N) / .N, by = c('status', 'label')]
  return(dtValues)
}

prepareDtHyperValues <- function(plotData) {
  dtHyperValues <- plotData[valueMode == PARAMETERTYPE$hyperParameter] %>%
    setorderv(c('label', 'status', 'statusValue'))
  return(dtHyperValues)
}

computeHyperValues <- function(dHyperValues, dPrior, dValues) {
  dHyperValues[, pLB := computeStatFunction(values = statusValue,
                                            parameters = hyperParameter,
                                            distribution = dPrior$hyperDistribution[1],
                                            v = dValues$minValue[1],
                                            type = 'P'), by = 'status']
  dHyperValues[, pUB := computeStatFunction(values = statusValue,
                                            parameters = hyperParameter,
                                            distribution = dPrior$hyperDistribution[1],
                                            v = dValues$maxValue[1],
                                            type = 'P'), by = 'status']
  dHyperValues[, densityNorm := pUB - pLB]
  return(dHyperValues)
}

computeDensity <- function(dValues, dHyperValues, dPrior) {
  dValues[, density := NA_real_]
  for (st in dHyperValues$status) {
    dValues[status == st, density := computeStatFunction(values = dHyperValues[status == st]$statusValue,
                                                         parameters = dHyperValues[status == st]$hyperParameter,
                                                         distribution = dPrior$hyperDistribution[1],
                                                         v = statusValue,
                                                         type = 'D')]
  }
  return(dValues)
}

createDensityPlot <- function(dValues, dHyperValues, dPrior) {
  plotObject <- ggplot() + layerWatermark() +
    geom_point(data = dValues, aes(x = statusValue, y = density / densityNorm, fill = status, shape = status)) +
    labs(y = 'density')

  for (st in dHyperValues$status) {
    plotObject <- plotObject +
      geom_function(
        fun = computeStatFunction,
        args = list(values = dHyperValues[status == st]$statusValue,
                    parameters = dHyperValues[status == st]$hyperParameter,
                    distribution = dPrior$hyperDistribution[1],
                    type = 'D',
                    normalisationFactor = dHyperValues[status == st]$densityNorm[1]),
        aes(color = status),
        data = data.table(status = st))
  }

  plotObject <- customizeLegend(plotObject, colorScalingVector)
  return(plotObject)
}

createCumulativeProbabilityPlot <- function(dValues, dHyperValues, dPrior) {
  ribbondData <- rbind(
    cbind(unique(dHyperValues[, c("status", "pUB", "pLB")]), data.table(x = xlimits[1])),
    cbind(unique(dHyperValues[, c("status", "pUB", "pLB")]), data.table(x = xlimits[2]))
  )

  plotObject <- ggplot() + layerWatermark() +
    geom_point(data = dValues, aes(x = statusValue, y = ecdf * densityNorm + pLB, fill = status, shape = status)) +
    labs(y = 'cumulated probability/proportion') +
    geom_ribbon(aes(x = x, ymin = pUB, fill = status, ymax = 0.99), data = ribbondData, alpha = 0.5) +
    geom_ribbon(aes(x = x, ymin = pLB, fill = status, ymax = 0.01), data = ribbondData, alpha = 0.5) +
    geom_hline(aes(yintercept = pLB, color = status), data = unique(dHyperValues[, c('status', 'pLB')])) +
    geom_hline(aes(yintercept = pUB, color = status), data = unique(dHyperValues[, c('status', 'pUB')])) +
    scale_y_continuous(limits = c(0, 1))

  for (st in dHyperValues$status) {
    plotObject <- plotObject +
      geom_function(
        fun = computeStatFunction,
        args = list(values = dHyperValues[status == st]$statusValue,
                    parameters = dHyperValues[status == st]$hyperParameter,
                    distribution = dPrior$hyperDistribution[1],
                    type = 'P'),
        aes(color = status),
        data = data.table(status = st))
  }

  plotObject <- customizeLegend(plotObject, colorScalingVector)
  return(plotObject)
}

adjustxAxis <- function(plotObject, dValues, dHyperValues) {
  xlimits <- c(max(min(dValues$statusValue) * 0.8, dValues$minValue[1]),
               min(max(dValues$statusValue) * 1.2, dValues$maxValue[1]))
  xscale <- tolower(dValues$scaling[1])
  label = dHyperValues$label[1]

  if (xscale == 'log') {
    plotObject <- plotObject +
      scale_x_log10(limits = xlimits,
                    name = paste(label, '\n (log scale)'),
                    expand = expansion(mult = 0))
  } else {
    plotObject <- plotObject +
      scale_x_continuous(limits = xlimits,
                         name = paste(label, '\n (linear scale)'),
                         expand = expansion(mult = 0))
  }

  if (dValues$minValue[1] == xlimits[1])
    plotObject <- plotObject + geom_vline(xintercept = xlimits[1], linewidth = 2)
  if (dValues$maxValue[1] == xlimits[2])
    plotObject <- plotObject + geom_vline(xintercept = xlimits[2], linewidth = 2)

  return(plotObject)
}

createHyperValuesTable <- function(dHyperValues) {
  dHyperValuesTable <- dHyperValues[, c('hyperParameter', 'status', 'statusValue', 'minValue', 'maxValue')] %>%
    .[, statusValue := signif(statusValue, 4)] %>%
    dcast(... ~ status, value.var = 'statusValue')

  return(dHyperValuesTable)
}

createPriorPlots <- function(dPrior){
  plotListPriors <- list()
  priorTable <- data.table()

  for (pr in split(dPrior, by = 'hyperParameter')){
    if (pr$distribution[1] != 'flat'){

      pNames <-  unlist(pr[1,] %>% dplyr::select(grep('_type$',names(pr),value = TRUE)))
      pValues <-  unlist(pr[1,] %>% dplyr::select(grep('_value$',names(pr),value = TRUE)))

      pr[, pLB := computeStatFunction(values = pValues,
                                      parameters = pNames,
                                      distribution = pr$distribution[1],
                                      v = pr$minValue[1],
                                      type = 'P'), by = 'status']
      pr[, pUB := computeStatFunction(values = pValues,
                                      parameters = pNames,
                                      distribution = pr$distribution[1],
                                      v = pr$maxValue[1],
                                      type = 'P'), by = 'status']
      pr[, densityNorm := pUB - pLB]
      pr[, d := computeStatFunction(values = pValues,
                                    parameters = pNames,
                                    distribution = pr$distribution[1],
                                    v = statusValue,
                                    type = 'D',
                                    normalisationFactor = densityNorm),
         by = 'status']

      priorTable <- rbind(priorTable,
                          pr[,.(logLikelihood = sum(log(d))),
                             by = c('hyperParameter','status')])

      plotObject <- ggplot(data = pr) +
        geom_function(
          fun = computeStatFunction,
          args = list(values = pValues,
                      parameters = pNames,
                      distribution = pr$distribution[1],
                      type = 'D',
                      normalisationFactor = pr$densityNorm[1])) +
        geom_linerange( mapping = aes(x = statusValue,ymin = 0, ymax = d, color = status)) +
        labs(x = pr$hyperParameter[1],
             y = 'density')
      if (tolower(pr$scaling[1]) == 'log'){
        plotObject <- plotObject +
          scale_x_log10(limits = c(pr$minValue[1],pr$maxValue[1]))
      } else {
        plotObject <- plotObject +
          scale_x_continuous(limits = c(pr$minValue[1],pr$maxValue[1]))
      }

      plotObject <- customizeLegend(plotObject, colorScalingVector,showLegends = FALSE,aesthetics = c('color','linetype'))

      plotListPriors[[pr$hyperParameter[1]]] <- plotObject
    }
  }
  plotListPriors[['table_prior']] <- priorTable
  return(plotListPriors)

}

# Function to create combined plots
plotcombinedDistributionPlots <- function(plotListEntry) {
  # Extract plots and tables from the entry
  densityPlot <- plotListEntry[['density']]
  cumulativePlot <- plotListEntry[['cumulatedProbability']]
  logLikelihoodTable <- plotListEntry[['table_logLikelihoods']]
  hyperparameterTable <- plotListEntry[['table_hyperparameter']]
  priorPlots <- plotListEntry[['priorPlots']]

  # Combine the density and cumulative plots
  leftSide <- cowplot::plot_grid(densityPlot + theme(axis.title.x = element_blank()),
                                 cumulativePlot + theme(legend.position = 'none'),
                                 ncol =  1,align = 'v')

  # loglikelihhod table
  logLikelihoodTablePlot <- ggpubr::ggtexttable(rbind(logLikelihoodTable %>%
                           dplyr::mutate(name = 'distribution'),
                           priorPlots[['table_prior']] %>%
                           setnames('hyperParameter','name',skip_absent = TRUE)) %>%
                             dplyr::mutate(logLikelihood = signif(logLikelihood,4)) %>%
    dcast(... ~ status, value.var = 'logLikelihood'), rows = NULL) %>%
      ggpubr::ggpar(title = "Loglikelihoods")

  # Convert tables to ggpubr tables
  plotListRightSide <- list(
      logLikelihoodTablePlot = logLikelihoodTablePlot,
      hyperparameterTablePlot = ggpubr::ggtexttable(hyperparameterTable, rows = NULL))

  # Handle prior plots
  if (length(priorPlots) > 1) {
    # Combine prior plots and extract the prior table
    plotListRightSide[['priorPlots']] <-
      cowplot::plot_grid(plotlist = priorPlots[names(priorPlots) != 'table_prior'])  # Exclude the table from prior plots
  }

  return(cowplot::plot_grid(leftSide,cowplot::plot_grid(plotlist = plotListRightSide,ncol = 1)))
}

plotCombinedCumulativePlots <- function(plotListAll, runName,ncol = 2) {
  # Extract cumulative probability plots from plotListAll
  cumulativePlots <- lapply(plotListAll, function(entry) entry[['cumulatedProbability']] +
                              theme(legend.location = 'none'))

  # Combine the cumulative plots into one plot
  combinedCumulativePlot <- cowplot::plot_grid(plotlist = cumulativePlots, ncol = ncol)

  # Add a shared legend
  legend <- cowplot::get_plot_component(cumulativePlots[[1]] ,"guide-box",return_all = TRUE) + theme(legend.position = 'bottom')

  # Combine the cumulative plot with the legend
  finalPlot <- cowplot::plot_grid(combinedCumulativePlot, legend, ncol = 1, rel_heights = c(1, 0.1))

  # Add title to the final plot
  finalPlot <- finalPlot + ggtitle(runName)

  return(finalPlot)
}


# myRun1----------------

colorScalingVector =  c(
  start = 'lightblue',
  current = 'darkgreen',
  best = 'orange'
)

outputDir <- myRun1$outputDir
runName <- myRun1$runName

dtList <- loadListsForRun(outputDir,runName)

bestStatus <- readRDS(file.path(outputDir, 'bestOptimStatus.RDS'))
currentStatus <- readRDS(file.path(outputDir, 'optimStatus.RDS'))

plotList1 <- checkDistributionsForParameter(dtList, bestStatus, currentStatus)

# myRun2 ----------------

outputDir <- myRun2$outputDir
runName <- myRun2$runName

dtList <- loadListsForRun(outputDir,runName)

bestStatus <- readRDS(file.path(outputDir, 'bestOptimStatus.RDS'))
currentStatus <- readRDS(file.path(outputDir, 'optimStatus.RDS'))

plotList2 <- checkDistributionsForParameter(dtList, bestStatus, currentStatus)

# myRun3 ----------------

outputDir <- myRun3$outputDir
runName <- myRun3$runName

dtList <- loadListsForRun(outputDir,runName)

bestStatus <- readRDS(file.path(outputDir, 'bestOptimStatus.RDS'))
currentStatus <- readRDS(file.path(outputDir, 'optimStatus.RDS'))

plotList3 <- checkDistributionsForParameter(dtList, bestStatus, currentStatus)


plotcombinedDistributionPlots(plotList1$`a2AP_halflife [day(s)]`)

plotcombinedDistributionPlots(plotList2$`a2AP_halflife [day(s)]`)

plotcombinedDistributionPlots(plotList2$`Ab_a2AP_x_2_Kd_FcRn [µmol/l]`)
plotcombinedDistributionPlots(plotList2$free_a2AP_scaling_factor)


plotcombinedDistributionPlots(plotList3$`Ab_a2AP_x_2_Kd_FcRn [µmol/l]`)
plotcombinedDistributionPlots(plotList3$`a2AP_halflife [day(s)]`)

