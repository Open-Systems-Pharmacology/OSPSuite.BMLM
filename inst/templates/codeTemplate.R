# start BMLM ----------------
# this code snippet has to be inserted in a reportingframework worklow below the definition of a scenarioList
# it assumes, projectConfiguration is initialized, dataObserved exists and scenarios are well defined,
# make sure the configuration tables 'DataGroups' and 'Outputs' in Plots.xslx are updated,
# for each dataGroup a defaultScenario is needed and the outputPaths have to be inserted for each OutputPathId

# add BMLM configuration file and set up parameter identification from snapshot
projectConfiguration <-
  addBMLMPConfiguration(projectConfiguration,
                        bMLMConfigurationFile = "BMLMConfiguration.xlsx",
                        snapshotFile = file.path(projectConfiguration$modelFolder, 'mySnaphot.json'),
                        nameOfParameterIdentfication = 'myParameterIdentification',
                        overwrite = FALSE)

# Manual adjust sheets ParameterDefinition, ParameterMappedPaths, ModelError
configurePriors(projectConfiguration = projectConfiguration,
                dataObserved = dataObserved)

# Initialize an optimization object
myRun <- BMLMOptimization$new(projectConfiguration = projectConfiguration,
                                runName = 'myRun',
                                scenarioList =  scenarioList,
                                dataObserved = dataObserved)

# start the Optimization ( as default this is done as background job)
myRun$startOptimization(projectConfiguration,method = 'SANN')


# check Progress of optimization
myRun$checkParameterLimits()
myRun$checkDistributions()
myRun$checkConvergence()


