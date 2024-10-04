
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


