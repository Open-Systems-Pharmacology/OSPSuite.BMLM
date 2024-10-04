# start BMLM ----------------
# this code snippet has to be inserted in a reportingframework worklow
# it assumes, projectConfiguration is initialized, dataObserved exists and scenarios are well defined

# add BMLM configuration file and set up parameter identification from snapshot
projectConfiguration <-
  addBMLMPConfiguration(projectConfiguration,
                        bMLMConfigurationFile = "BMLMConfiguration.xlsx",
                        snapshotFile = file.path(projectConfiguration$modelFolder,'mySnaphot.json'),
                        nameOfParameterIdentfication = 'myParameterIdentification',
                        overwrite = FALSE)


# Manual adjust sheets ParameterDefinition,ParameterMappedPaths,OutputDefinitions

configurePriors(projectConfiguration = projectConfiguration,
                dataObserved = dataObserved)

# Manual adjust priors

# initialize list of scenarios
scenarioList <-
  createScenarios.wrapped(projectConfiguration = projectConfiguration,
                          scenarioNames = NULL,
                          doCheckScenarioNameValidity = TRUE)

dtParameter <- initBMLMParameter(projectConfiguration,
                                 scenarioList,
                                 seed = 1234)
