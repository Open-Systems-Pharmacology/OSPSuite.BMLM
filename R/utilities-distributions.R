#' Title
#'
#' @return
getAllDistributions <- function() {
  distributionFunctionsD <- ls("package:stats", pattern = "^d", all.names = TRUE)
  distributionFunctionsP <- ls("package:stats", pattern = "^p", all.names = TRUE)

  availableDistributions <- c(
    intersect(
      gsub("^d", "", distributionFunctionsD),
      gsub("^p", "", distributionFunctionsP)
    ),
    "unif_log"
  )
  return(availableDistributions)
}


#' Title
#'
#' @param distributionName
#'
#' @return
#' @export
#'
#' @examples
getDistributionParameters <- function(distributionName) {
  checkmate::assertChoice(x = distributionName, getAllDistributions())

  if (distributionName == "unif_log") {
    return(c("min_log", "max_log"))
  }

  return(setdiff(
    names(formals(eval(parse(text = paste0("stats::d", distributionName))))),
    c("x", "log")
  ))
}



#' Title
#'
#' @param distribution
#'
#' @return
translateDistribution <- function(distribution) {
  if (is.na(distribution)) {
    return("")
  }

  distribution <- switch(tolower(distribution),
    normal = "norm",
    lognormal = "lnorm",
    uniform = "unif",
    uniform_log = "unif_log",
    distribution
  )

  return(distribution)
}

#' Title
#'
#' @param distribution
#'
#' @return
reTranslateDistribution <- function(distribution) {
  distribution <- switch(tolower(distribution),
    norm = "normal",
    lnorm = "lognormal",
    unif = "uniform",
    unif_log = "uniform_log",
    distribution
  )

  return(distribution)
}
