# Load necessary packages
library(data.table)

# Get all distribution names
distributionFunctionsD <- ls("package:stats", pattern = "^d", all.names = TRUE)
distributionFunctionsP <- ls("package:stats", pattern = "^p", all.names = TRUE)
distributionFunctionsR <- ls("package:stats", pattern = "^r", all.names = TRUE)

availableDistributions <- intersect(
  intersect(
    gsub("^d", "", distributionFunctionsD),
    gsub("^p", "", distributionFunctionsP)
  ),
  gsub("^r", "", distributionFunctionsR)
)

# Function to get distribution parameters
# Create list of hyperparameters
hyperParameters <-
  stats::setNames(
    lapply(availableDistributions, function(distributionName) {
      checkmate::assertChoice(x = distributionName, getAllDistributions())

      return(setdiff(
        names(formals(eval(parse(text = paste0("stats::d", distributionName))))),
        c("x", "log")
      ))
    }),
    unlist(availableDistributions)
  )

# Initialize an empty list to store the data
distributionList <- list()

# Loop through each distribution and its parameters
for (distribution in names(hyperParameters)) {
  parameters <- hyperParameters[[distribution]]

  # Get the formal arguments of the density function
  densityFunc <- eval(parse(text = paste0("stats::d", distribution)))
  args <- formals(densityFunc)

  # Loop through each parameter
  for (parameter in parameters) {
    # Get the default value if it exists
    startValue <- NA
    if (parameter %in% names(args) &&
      !is.symbol(args[[parameter]]) && # nolint identation
      !is.language(args[[parameter]])) {
      startValue <- args[[parameter]]
    }

    # Set minValue and maxValue based on common knowledge of the distributions
    minValue <- NA
    maxValue <- NA
    startValue <- NA
    scaling <- 'Linear'

    switch(paste(distribution, parameter, sep = "_"),
      norm_mean = {
        minValue <- "minValue"
        maxValue <- "maxValue"
        startValue <- "startValue"
      },
      norm_sd = {
        minValue <- 0
      },
      lnorm_sdlog = {
        minValue <- 0
      },
      unif_min = {
        minValue <- "minValue"
        maxValue <- "maxValue"
      },
      unif_max = {
        minValue <- "minValue"
        maxValue <- "maxValue"
      }
    )

    # Create a new row for the data.table
    distributionList <- rbind(
      distributionList,
      data.table(
        distribution = distribution,
        parameter = parameter,
        minValue = minValue,
        maxValue = maxValue,
        scaling = scaling,
        startValue = startValue
      )
    )
  }
}

# Convert the list to a data.table
distributionTable <- rbind(distributionList)

# add lnorm_geomean
newLines <- distributionTable[distribution == 'lnorm']
newLines[,distribution := 'lnorm_geomean']
newLines[parameter == 'meanlog', `:=`(parameter = 'geomean',
                                      minValue = 'minValue',
                                      maxValue = 'maxValue',
                                      startValue = 'startValue',
                                      scaling ='Log')]
newLines[parameter == 'sdlog', `:=`(parameter = 'geosd',
                                      minValue = 1,
                                      scaling ='Linear')]

distributionTable <- rbind(distributionTable,
                           newLines)


setDF(distributionTable)

usethis::use_data(distributionTable, overwrite = TRUE, internal = TRUE)
