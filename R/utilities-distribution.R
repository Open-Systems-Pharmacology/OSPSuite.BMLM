# auxiliaries  -----
#' Get All Distributions
#'
#' This function retrieves all distributions available for prior definitions and hyperprameter.
#'
#' @return A vector of unique distribution names.
#' @export
getAllDistributions <- function() {
  unique(distributionTable$distribution)
}


#' Get Distribution Parameters
#'
#' This function retrieves the parameters for a specified distribution.
#'
#' @param distributionName A string representing the name of the distribution.
#'
#' @return A vector of parameters for the specified distribution.
#' @export
getDistributionParameters <- function(distributionName) {
  distributionTable[distributionTable$distribution == distributionName, ]$parameter
}


#' Get Distribution Row
#'
#' This function retrieves a row from the distribution table based on the distribution name and parameter.
#'
#' @param distributionName A string representing the name of the distribution.
#' @param distributionParameter A string representing the parameter of the distribution.
#'
#' @return A data.table containing the distribution row.
#' @keywords internal
getDistributionRow <-
  function(distributionName, distributionParameter) {
    distribution <- parameter <- NULL

    tempDistributionTable <- as.data.table(distributionTable)
    return(tempDistributionTable[distribution == distributionName &
                                   parameter == distributionParameter])
  }

#' Calculate Probability Based on Distribution
#'
#' This function calculates the probability for a given distribution using specified parameters.
#' It extracts the distribution type and its parameters from a data frame row, constructs the
#' appropriate function call, and returns the calculated probability. If an error occurs during
#' calculation, it returns NA and prints an error message.
#'
#' @param row A named vector representing a row from a data frame. It must contain:
#'   - `Distribution`: A string that specifies the distribution type (e.g., "norm" for normal distribution).
#'   - `StartValue`: A numeric value representing the starting point for the probability calculation.
#'   - Additional parameters with names ending in `_type` and `_value` that specify the distribution parameters.
#'
#' @return A numeric value representing the calculated probability. Returns NA in case of an error.
calculateProbability <- function(row, log = FALSE) {
  dist <- paste0("d", row["distribution"])

  if (dist == 'dflat'){
    return(ifelse(log,0,1))
  }

  value <- as.numeric(row["value"])

  # Extract parameters
  paramTypes <-
    row[grepl("_type$", names(row))] # Get all type columns
  paramValues <-
    row[grepl("_value$", names(row))] # Get all value columns

  paramList <-
    stats::setNames(as.numeric(paramValues), as.character(paramTypes))
  # Remove NA values
  paramList <- paramList[!is.na(paramList)]
  paramList[["log"]] <- log

  # Calculate probability based on the distribution
  args <- c(list(value), paramList)
  prob <- tryCatch(
    {
      do.call(dist, args)
    },
    error = function(e) {
      return(NA) # Return NA in case of error
    }
  )

  return(prob)
}

#' Log-Normal Functions with Geometric Mean
#'
#' These functions provide an interface to the log-normal distribution using
#' the geometric mean and geometric standard deviation instead of the log mean and log sd. They internally call the
#' corresponding base R functions: `dlnorm`, `plnorm`, `qlnorm`, and `rlnorm`.
#' e.g. dlnorm_geomean(geomean, geosd, x = x) calls dlnorm(logmean = log(geomean), logsd = log(geosd), x = x)
#'
#' @name lognormal_geomean
#' @aliases dlnorm_geomean plnorm_geomean qlnorm_geomean rlnorm_geomean
#' @section Functions:
#' \describe{
#'   \item{\code{dlnorm_geomean(geomean, geosd, ...)}}{Density of the log-normal distribution.}
#'   \item{\code{plnorm_geomean(q, geomean, geosd, ...)}}{Cumulative distribution function of the log-normal distribution.}
#'   \item{\code{qlnorm_geomean(p, geomean, geosd, ...)}}{Quantile function of the log-normal distribution.}
#'   \item{\code{rlnorm_geomean(n, geomean, geosd, ...)}}{Random generation from the log-normal distribution.}
#' }
#' @export
#' @export dlnorm_geomean
#' @export plnorm_geomean
#' @export qlnorm_geomean
#' @export rlnorm_geomean
#'
#' @param x A numeric vector of quantiles (for `dlnorm_geomean`).
#' @param q A numeric vector of quantiles (for `plnorm_geomean`).
#' @param p A numeric vector of probabilities (for `qlnorm_geomean`).
#' @param n A numeric value indicating the number of random values to generate (for `rlnorm_geomean`).
#' @param geomean A numeric value representing the geometric mean. Default is 1.
#' @param geosd A numeric value representing the standard deviation of the logarithm. Default is 1.
#' @param lower.tail A logical value indicating whether to return the lower tail probability. Default is TRUE.
#' @param log A logical value indicating whether to return the logarithm of the density. Default is FALSE.
#' @param log.p A logical value indicating whether to return the logarithm of the probability. Default is FALSE.
#' @return A numeric value or vector depending on the function:
#' \describe{
#'   \item{dlnorm_geomean}{Density at the specified point.}
#'   \item{plnorm_geomean}{Cumulative probability at the specified quantiles.}
#'   \item{qlnorm_geomean}{Quantiles corresponding to the specified probabilities.}
#'   \item{rlnorm_geomean}{Random values generated from the log-normal distribution.}
#' }
dlnorm_geomean <- function(x, geomean = 1, geosd = 1, log = FALSE) {
  dlnorm(x = x, meanlog = log(geomean), sdlog = log(geosd), log = log)
}

plnorm_geomean <- function(q, geomean = 1, lower.tail = TRUE, geosd = 1, log.p = FALSE) {
  plnorm(q = q, meanlog = log(geomean), sdlog = log(geosd), lower.tail = lower.tail, log.p = log.p)
}

qlnorm_geomean <- function(p, geomean = 1, lower.tail = TRUE, geosd = 1, log.p = FALSE) {
  qlnorm(p = p, meanlog = log(geomean), sdlog = log(geosd), lower.tail = lower.tail, log.p = log.p)
}

rlnorm_geomean <- function(n, geomean = 1, geosd = 1) {
  rlnorm(n = n, meanlog = log(geomean), sdlog = log(geosd))
}
