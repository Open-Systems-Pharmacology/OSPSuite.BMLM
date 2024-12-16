message('Start background job')

library(ospsuite.bmlm)

message('reload scenarios')
scenarioList <-
  createScenarios.wrapped(projectConfiguration = projectConfiguration,
                          scenarioNames = names(argList[['scenarioList']]))

do.call(what = optimizeParameters,args = utils::modifyList(x = argList,val = list(scenarioList = scenarioList)))
