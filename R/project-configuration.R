#' @title ProjectConfiguration
#' @docType class
#' @description An object storing configuration used project-wide
#' @format NULL
#' @export
ProjectConfigurationBMLM <- R6::R6Class(   # nolint object_name_linter
  "ProjectConfiguration",
  inherit = esqlabsR::ProjectConfiguration,
  cloneable = TRUE,
  active = list(
    #' @field BMLMConfigurationFile Path to the file containing BMLM Identification
    BMLMConfigurationFile = function(value) {
      if (missing(value)) {
        value <- private$.BMLMConfigurationFile
      }
      private$.BMLMConfigurationFile <- value
      private$.clean_path(value, self$paramsFolder)
    }
  ),
  private = list(
    .BMLMConfigurationFile = NULL
  ),
  public = list(
    #' Initialize
    #'
    #' @param projectConfigurationFilePath A string representing the path to the
    #' project configuration file.
    initialize = function(projectConfigurationFilePath = character()) {
      super$initialize(
        projectConfigurationFilePath = projectConfigurationFilePath
      )
    }
  )
)
