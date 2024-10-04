#' Opens the workflow template as new document
#'
#' Per default the workflow template of the package is loaded.
#' To customize it save the file with name "template_workflow.R" in your template directory and
#' set the path to the template via option `options(OSPSuite.RF.PathForWorkflowTemplate = 'myTemplateDirectory'`)
#'
#' @export
addBMLMWorkflowTemplate <- function() {
  rstudioapi::callFun("sendToConsole", "insertTemplateText()")
}


# addin.R
insertTemplateText <- function() {
  # Specify the text you want to insert
  templatePath <- system.file("templates/codeTemplate.R", package = "ospsuite.bmlm")
  textToInsert <- readLines(templatePath)

  # Get the current document's context
  rstudioapi::insertText(text = textToInsert)
}
