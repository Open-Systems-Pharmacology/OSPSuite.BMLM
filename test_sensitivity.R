scanLogLikeLihoodForParameter <- function(dtList,
                                          scenarioList,
                                          parameter){

  outputDir <- myRun4$outputDir
  runName <- myRun4$runName
  parameterName <- "a2AP_halflife"

  dtList <- loadListsForRun(outputDir,runName)

  statusBest <- readRDS(file.path(outputDir, 'bestOptimStatus.RDS'))
  dtList <- setParameterToTables(dtList = dtList,
                                 params = statusBest$params)


  dtLog <- evaluateIndividualLoglikelihoods(scenarioList,dtList,outputDir,parameterName)
  dtLog$status = 'best'

  params <- statusBest$params
  ids <- dtList$startValues[parameter == parameterName]$id

  for (param in setdiff(seq(-5,5),unique(dtLog$status))){
    params[ids] <- param

    dtList <- setParameterToTables(dtList = dtList,
                                   params = params)

    dtLogp <- evaluateIndividualLoglikelihoods(scenarioList,dtList,outputDir,parameterName)
    dtLogp$status = param

    dtLog <- rbind(dtLog,dtLogp)

    save(dtLog,file = 'tmp.Rdata')
  }

  ggplot(dtLog,aes(x = value, y = logTimeProfile)) +
    geom_line() +
    geom_point(aes(fill = as.factor(status=='best'))) +
    facet_wrap(vars(individualId), scales = 'free') +
    theme(legend.position = 'none')


}

evaluateIndividualLoglikelihoods <- function(scenarioList,dtList,outputDir,parameterName){

  optimEnv <- initializeOptimEnv(dtList)

  evaluateTimeprofiles(optimEnv = optimEnv,
                       scenarioList = scenarioList,
                       dtList = dtList,
                       simulationRunOptions = SimulationRunOptions$new(showProgress = TRUE),
                       withProtocol = TRUE,
                       outputDir = outputDir)

  loglikelihoods = list()

  for (ind in unique(dtList$startValues$individualId)){
    loglikelihoods[[ind]] <- getLogLikelihood(
      dtPrior = dtList$prior,
      dtStartValues = dtList$startValues[individualId == ind],
      dtRes = rbindlist(optimEnv$dtResList)[individualId == ind]
    ) %>%
      as.list()
  }

  dtLog <- rbindlist(loglikelihoods,idcol = 'individualId') %>%
    merge(dtList$startValues[parameterName == parameter,c('value','individualId')],
          by = 'individualId')

  return(dtLog)
}
