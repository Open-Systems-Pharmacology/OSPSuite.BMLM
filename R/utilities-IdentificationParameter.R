readIdentificationParameterFromSnapshot <- function(snaphsotFile, PIName) {
  snp <- jsonlite::fromJSON(snaphsotFile)
  checkmate::assertChoice(PIName, snp$ParameterIdentifications$Name)

  ix <- which(snp$ParameterIdentifications$Name == PIName)

  linkedParameter <- snp$ParameterIdentifications$IdentificationParameters[[ix]]

  definitionDT <- cbind(
    linkedParameter %>%
      dplyr::select(!c("Parameters", "LinkedParameters")) %>%
      setDT(),
    rbindlist(
      lapply(
        seq_len(
          nrow(linkedParameter)
        ),
        function(iRow) {
          linkedParameter$Parameters[[iRow]] %>%
            tidyr::pivot_wider(names_from = Name, values_from = Value) %>%
            setDT()
        }
      ),
      fill = TRUE
    )
  )


  if (!("UseAsFactor" %in% names(definitionDT))) {
    definitionDT$UseAsFactor <- FALSE
  } else {
    definitionDT[, UseAsFactor := ifelse(is.na(UseAsFactor), FALSE, TRUE)]
  }


  dtMapping_Template <- unnest(linkedParameter %>%
    select(c("Name", "LinkedParameters")), "LinkedParameters")

  return(list(
    "Parameter_Definition" = definitionDT,
    "Mapping_template"
  ))
}
