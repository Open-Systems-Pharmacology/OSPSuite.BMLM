# start BMLM ----------------
# this code snippet has to be inserted in a reportingframework worklow below the definition of a scenarioList
# it assumes, projectConfiguration is initialized, dataObserved exists and scenarios are well defined,
# make sure the configuration tables 'DataGroups' and 'Outputs' in Plots.xslx are updated,
# for each dataGroup a defaultScenario is needed and the outputPaths have to be inserted for each OutputPathId

# Configuration step ----------------
# add BMLM configuration file and set up parameter identification from snapshot
projectConfiguration <-
  addBMLMPConfiguration(projectConfiguration,
                        snapshotFile = file.path(projectConfiguration$modelFolder, 'mySnaphot.json'),
                        nameOfParameterIdentfication = 'myParameterIdentification',
                        overwrite = FALSE)

# Manual adjust sheets ParameterDefinition, ParameterMappedPaths, ModelError

# Configure Priors based on BMLM configuration file
configurePriors(projectConfiguration = projectConfiguration,
                dataObserved = dataObserved)

# Set up and start optimization run ----------------

# Initialize an optimization object
myRun <- BMLMOptimization$new(projectConfiguration = projectConfiguration,
                                runName = 'myRun',
                                scenarioList =  scenarioList,
                                dataObserved = dataObserved)

# runs the model at the iitial Values and prints information on simulation time
# and initial loglikelihood at the console and to the logfile
myRun$evaluateInitialValues()

# start the Optimization ( as default this is done as background job)
myRun$startOptimization(
  projectConfiguration = projectConfiguration,
  method = "BFGS",  # Specify the optimization method
  control = list(maxit = 1000)  # Control parameters for the optimization
)

# check Progress of optimization ----------------
myRun$checkParameterLimits()
myRun$checkDistributions()
myRun$checkConvergence()
myRun$checkPredictedVsTime()
myRun$checkPredictedVsObserved()
myRun$checkResidualsAsHistogram()
myRun$checkResidualsVsTime()
myRun$checkResidualsAsQQ()
dtPrior <- myRun$getCurrentConfigTable(projectConfiguration)
dtStartValues <- myRun$getCurrentConfigTable(projectConfiguration,sheetName = 'IndividualStartValues')

# export results ----------------
myRun$exportFinalValuesToBMLConfigTable(projectConfiguration)
myRun$exportResultAsPopulation()
myRun$exportIndividualValuesToConfigTable()
myRun$exportGlobalsParametersToConfigTables()
myRun$exportIndividualResultsToPkml(projectConfiguration,individualId = dataObserved$individualId[1])
