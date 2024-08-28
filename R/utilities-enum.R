#' @title Sheet names of BMLM COnfiguration xlsx
#' @export
#' @family enum helpers
BMLMSHEET <-
  ospsuite::enum(
    c(
      "ParameterDefinition",
      "ParameterMappedPaths",
      "OutputDefinitions",
      "DataGroupToModelFile",
      "Prior",
      "IndividualStartValues",
      "Dictionary"
    )
  )


#' @title Parameter types
#' @export
#' @family enum helpers
PARAMETERTYPE <- ospsuite.utils::enum(c(
  "global",
  "hyperParameter",
  "outputError",
  "individual"
))


#' @title Error Model
#' @export
#' @family enum helpers
ERRORMODEL <- ospsuite.utils::enum(c(
  "relative",
  "absolute",
  "log_absolute"
))


