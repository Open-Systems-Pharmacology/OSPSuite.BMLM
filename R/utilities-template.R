#' Opens the workflow template as new document
#'
#' @export
#' @family configuration
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
