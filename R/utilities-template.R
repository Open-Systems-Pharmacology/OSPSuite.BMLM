#' Opens the workflow template as new document
#'
#' Per default the workflow template of the package is loaded.
#' To customize it save the file with name "template_workflow.R" in your template directory and
#' set the path to the template via option `options(OSPSuite.RF.PathForWorkflowTemplate = 'myTemplateDirectory'`)
#'
#' @export
openBMLMWorkflowTemplate <- function() {
  rstudioapi::callFun("sendToConsole", "createBMLMWorkflowTemplate()")
}

#' Opens the workflow template as new document
#'
#' @export
createBMLMWorkflowTemplate <- function() {
  ospsuite.reportingframework::createDocumentFromTemplate(
    template = "template_workflow",
    templatePath = system.file("templates", package = "ospsuite.bmlm")
  )
}
