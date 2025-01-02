message('Start background job')

library(ospsuite.bmlm)

message('reload scenarios')
scenarioList <-
  createScenarios.wrapped(projectConfiguration = projectConfiguration,
                          scenarioNames = names(argListForJob[['scenarioList']]))

message('start optimization')
do.call(what = optimizeParameters,args = utils::modifyList(x = argListForJob,val = list(scenarioList = scenarioList)))
