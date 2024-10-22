#' @title Parameter types
#' @export
#' @family enum helpers
PARAMETERTYPE <- ospsuite.utils::enum(c(  #nolint camelCase
  "global",
  "hyperParameter",
  "outputError",
  "individual"
))


#' @title Error Model
#' @export
#' @family enum helpers
ERRORMODEL <- ospsuite.utils::enum(c( #nolint camelCase
  "relative",
  "absolute",
  "log_absolute"
))

#' @title Scalings
#' @export
#' @family enum helpers
SCALING <- ospsuite.utils::enum(c( #nolint camelCase
  log = "Log",
  linear = "Linear"
))
