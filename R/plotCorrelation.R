#' Plot Correlations
#'
#' This function generates a correlation matrix plot for the specified status and checks
#' for relevant columns in the population data.
#'
#' @param dtList A list containing prior and start values data tables.
#' @param statusList A list containing the current and best parameter values.
#' @param titeltxt A string to include in the plot title.
#' @param method A character string specifying the correlation method to use (default is 'spearman').
#'    Input of function `stats::cor()`
#' @param statusToShow A character string indicating which status to show. Options are 'best', 'current', and 'start'.
#' @param scenarioList A list of scenarios to analyze.
#' @param corCut A numeric value for the correlation cutoff threshold. Default is 0.5.
#' @param pValueCut A numeric value for the Chi-squared cutoff threshold. Default is 0.1.
#'
#' @return NULL This function does not return a value; it prints the correlation plot directly.
#' @export
#' @family plotting
plotCorrelations <- function(dtList,
                             statusList,
                             titeltxt = NULL,
                             method = "spearman",
                             statusToShow = c("best", "current", "start"),
                             scenarioList,
                             corCut = 0.5,
                             pValueCut = 0.1) {
  # Validate inputs
  checkmate::assertList(dtList, types = "data.table")
  checkmate::assertList(statusList,types = 'list',min.len = 1,names = 'named')
  checkmate::assertNames(names(statusList),subset.of = c('best','current'))
  checkmate::assertCharacter(titeltxt, null.ok = TRUE)
  statusToShow <- match.arg(statusToShow)
  checkmate::assertCharacter(method)
  checkmate::assertList(scenarioList,types = 'Scenario')
  checkmate::assertDouble(corCut,lower = 0,upper = 1,any.missing = FALSE,len = 1)
  checkmate::assertDouble(pValueCut,lower = 0,upper = 1,any.missing = FALSE,len = 1)

  if (nrow(dtList$startValues) == 0) {
    stop("No distributed parameters available")
  }

  plotData <- preparePlotDataParameterValues(
    dtList = dtList,
    statusList = statusList
  )
  plotData <- reshapePlotDataParameterValues(plotData[valueMode == PARAMETERTYPE$individual])
  plotData <- plotData[status == statusToShow]
  plotData <- addLabel(plotData = plotData, dtPrior = dtList$prior, unitSep = "\n", identifier = "name")

  labels <- unique(plotData$label)

  plotData <- dcast(plotData[, c("statusValue", "label", "individualId")], ... ~ label, value.var = "statusValue")
  pm <- GGally::ggpairs(plotData,
                        columns = labels,
                        switch = "y",
                        diag = list(continuous = GGally::wrap("barDiag", bins = 20, na.rm = TRUE)),
                        upper = list(continuous = GGally::wrap("cor", method = method, use = "complete.obs")),
                        lower = list(continuous = GGally::wrap("points", na.rm = TRUE)),
                        title = titeltxt,
                        mapping = aes(shape = "circle")
  ) +
    theme(strip.placement = "outside")

  # Check for relevant columns in population
  plotList <- checkForRelevantColumnsOfPopulation(
    plotData = plotData,
    labels = labels,
    scenarioList = scenarioList,
    corCut = corCut,
    pValueCut = pValueCut,
    method = method,
    dtMappedPaths = dtList$mappedPaths
  )

  plotList <- c(
    list("correlation_fitparameter" = pm),
    plotList
  )

  return(invisible(plotList))
}
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
#' @param dtMappedPaths A data.table containing mapped paths, which includes linked
#'                      parameters to be excluded from the final dataset.
#'
#' @return A list of plots for significant correlations and Kruskal-Wallis test results.
#' @keywords internal
#' @noRd
checkForRelevantColumnsOfPopulation <- function(plotData,
                                                labels,
                                                scenarioList,
                                                corCut = 0.5,
                                                pValueCut = 0.1,
                                                method = "spearman",
                                                dtMappedPaths) {
  popList <- preparePopulationForCorrelationCheck(scenarioList, dtMappedPaths)
  mergedData = list()
  for (columnType in c('numerics','factors')){
    mergedData[[columnType]] <- merge(plotData, popList[[columnType]],
                                      by.x = "individualId", by.y = "ObservedIndividualId")
  }
  plotList <- list()
  maxCorrelation <- 0
  minPvalue <- 1

  for (label in labels) {
    kryskalResults <-
      analyzeLabelKruskal(mergedData$factors, label, pValueCut,
                          columnVector = setdiff(names(popList$factors), c("ObservedIndividualId")))
    plotList <- c(plotList, kryskalResults$plotList)
    minPvalue <- min(minPvalue,kryskalResults$minPvalue)
    correlationResults <-
      analyzeLabelCorrelations(mergedData$numerics, label, corCut, method,
                               columnVector = setdiff(names(popList$numerics), c("ObservedIndividualId")))
    plotList <- c(plotList, correlationResults$plotList)
    maxCorrelation <- max(correlationResults$maxCorrelation,maxCorrelation)
  }

  if (length(plotList) == 0) {
    message(paste0(
      "No correlated columns found in population.\n",
      "maximal Correlation: ", round(maxCorrelation, 2), "(cut: ", corCut, ")\n",
      "minimal pValue of Chisquare test: ", signif(minPvalue, 2), "(cut: ", pValueCut, ")"
    ))
  }

  return(invisible(plotList))
}

#' Analyze Label Correlations
#'
#' This helper function analyzes correlations for a given label against all population columns.
#'
#' @param mergedData A data.table containing merged plot and population data.
#' @param label A character string representing the label to analyze.
#' @param corCut A numeric value for the correlation cutoff threshold.
#' @param method A character string specifying the correlation method to use.
#' @param columnVector A caharacter vector with column Names to analyse.
#'
#' @return A list of correlation plots for the specified label.
#' @keywords internal
#' @noRd
analyzeLabelCorrelations <- function(mergedData, label, corCut, method,columnVector) {
  plotList <- list()
  maxCorrelation = 0
  for (popCol in columnVector) {
    iNonNans <- which(!is.na(mergedData[[popCol]]) & !is.na(mergedData[[label]]))
    if (length(iNonNans) > 3 ) {
      correlationValue <- cor(mergedData[[label]], mergedData[[popCol]], use = "complete.obs", method = method)
      maxCorrelation <- max(maxCorrelation, abs(correlationValue))
      if (abs(correlationValue) > corCut) {
        plotObject <- createCorrelationPlot(mergedData, label, popCol, correlationValue)
        plotObject$correlationValue <- correlationValue
        plotList[[paste("cor", label, popCol, sep = "_")]] <- plotObject
      }
    }
  }
  return(list(plotList = plotList,maxCorrelation = maxCorrelation ))
}

#' Analyze Label Kruskal-Wallis Tests
#'
#' This helper function performs Kruskal-Wallis tests for a given label against all population columns.
#'
#' @param mergedData A data.table containing merged plot and population data.
#' @param label A character string representing the label to analyze.
#' @param pValueCut A numeric value for the Chi-squared cutoff threshold.
#' @param columnVector A caharacter vector with column Names to analyse.
#'
#' @return A list of Kruskal-Wallis test plots for the specified label.
#' @keywords internal
#' @noRd
analyzeLabelKruskal <- function(mergedData, label, pValueCut,columnVector) {
  plotList <- list()
  minPvalue <- 1
  for (popCol in columnVector) {
    tmpData <- mergedData[,c(label, popCol),with = FALSE] %>%
      setnames(
        old = c(label, popCol),
        new = c("label", "popCol")
      )

    kruskalTestResult <- kruskal.test(label ~ popCol, data = tmpData)
    if (!is.na(kruskalTestResult$p.value)) {
      minPvalue <- min(minPvalue, kruskalTestResult$p.value)
      if (kruskalTestResult$p.value < pValueCut) {
        plotObject <- createKruskalPlot(mergedData, label, popCol, kruskalTestResult$p.value)
        plotList[[paste("kruskal", label, popCol, sep = "_")]] <- plotObject
      }
    }
  }
  return(list(plotList = plotList,minPvalue = minPvalue))
}

#' Create Correlation Plot
#'
#' This helper function generates a scatter plot with a linear regression line for the correlation between two variables.
#'
#' @param data A data.table containing the data for plotting.
#' @param label A character string representing the label on the y-axis.
#' @param popCol A character string representing the population column on the x-axis.
#' @param correlationValue A numeric value representing the correlation coefficient.
#'
#' @return A ggplot object representing the correlation plot.
#' @keywords internal
#' @noRd
createCorrelationPlot <- function(data, label, popCol, correlationValue) {
  ggplotWithWatermark(data = data, mapping = aes(y = data[[label]], x = data[[popCol]])) +
    geom_point(fill = "black", na.rm = TRUE) +
    geom_smooth(method = "lm", formula = y ~ x, na.rm = TRUE) +
    labs(y = label, x = popCol, title = paste("Cor:", round(correlationValue, 2)))
}

#' Create Kruskal-Wallis Plot
#'
#' This helper function generates a box-and-whisker plot for the Kruskal-Wallis test results.
#'
#' @param data A data.table containing the data for plotting.
#' @param label A character string representing the label on the y-axis.
#' @param popCol A character string representing the population column on the x-axis.
#' @param pValue A numeric value representing the p-value from the Kruskal-Wallis test.
#'
#' @return A ggplot object representing the Kruskal-Wallis plot.
#' @keywords internal
#' @noRd
createKruskalPlot <- function(data, label, popCol, pValue) {
  ospsuite.plots::plotBoxWhisker(data = data, mapping = aes(y = data[[label]], x = data[[popCol]])) +
    geom_jitter(fill = "black", na.rm = TRUE) +
    labs(y = label, x = popCol, title = paste("pValue:", signif(pValue, 2))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
#' @family data-preparation
preparePopulationForCorrelationCheck <- function(scenarioList, dtMappedPaths) {
  dtPop <- rbindlist(
    lapply(scenarioList, function(scenario) {
      ospsuite::populationToDataFrame(scenario$population) %>%
        setDT()
    }),
    fill = TRUE
  ) %>%
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
  for (col in setdiff(names(dtPop), c("ObservedIndividualId", numericColumns))) {
    dtPop[[col]] <- factor(dtPop[[col]])
  }

  # Check for identical factors
  dtPop <- excludeIdenticalFactors(dtPop)
  return(list(numerics = dtPop %>% dplyr::select(c("ObservedIndividualId",numericColumns)),
              factors = dtPop %>% dplyr::select(c("ObservedIndividualId",!numericColumns))))
}


#' Exclude Identical Factor Columns
#'
#' This helper function checks for and excludes identical factor columns in the data.table.
#'
#' @param dtPop A data.table containing population data.
#'
#' @return A data.table with identical factor columns excluded.
excludeIdenticalFactors <- function(dtPop) {
  excludedFactors <- c()
  factorColumns <- names(dtPop)[sapply(dtPop, is.factor)]
  for (i in seq_len(length(factorColumns) - 1)) {
    if (!(factorColumns[i] %in% excludedFactors)) {
      nGroupedData <- nrow(dtPop[, .N, by = c(factorColumns[i])])
      for (j in seq(i + 1, length(factorColumns))) {
        nGroupedData2 <- nrow(dtPop[, .N, by = c(factorColumns[i], factorColumns[j])])
        if (nGroupedData == nGroupedData2) {
          message(paste("Factors", factorColumns[i], "and", factorColumns[j], "leads to the same patient groups. Ignore", factorColumns[j], "for analysis."))
          excludedFactors <- factorColumns[j]
        }
      }
    }
  }

  if (length(excludedFactors) > 1) {
    dtPop <- dplyr::select(dtPop, !any_of(excludedFactors))
  }

  return(dtPop)
}
