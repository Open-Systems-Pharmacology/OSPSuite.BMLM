#' Get Scaling Function
#'
#' This function returns the appropriate scaling function based on the specified scaling method.
#'
#' @param scalingMethod A character string specifying the scaling method.
#'                      Must be one of "unbounded" or "bounded".
#' @return A function that performs the specified scaling operation.
#' @keywords internal
getScaleFunction <- function(scalingMethod) {
  if (scalingMethod == SCALINGMETHOD$logsig) {
    scaleToLogSig
  } else if (scalingMethod == SCALINGMETHOD$hardBounds) {
    scaleWithinBounds
  } else {
    stop("unknown scaling method")
  }
}
" Get Inverse Scaling Function
#"
#' This function returns the appropriate inverse scaling function based on the specified scaling method.
#'
#' @param scalingMethod A character string specifying the scaling method.
#'                      Must be one of "unbounded" or "bounded".
#' @return A function that performs the specified inverse scaling operation.
#' @keywords internal
getUnscaleFunction <- function(scalingMethod) {
  if (scalingMethod == SCALINGMETHOD$logsig) {
    unscaleFromLogSig
  } else if (scalingMethod == SCALINGMETHOD$hardBounds) {
    unscaleFromBounds
  } else {
    stop("unknown scaling method")
  }
}
#' Prepare Input Data
#'
#' This function prepares the input data for the L-BFGS-B algorithm by merging and transforming
#' the provided data tables. It handles log transformations based on specified conditions.
#'
#' @param dtPrior A data.table containing prior values
#' @param dtStartValues A data.table containing start values
#' @param scalingMethod A character string specifying the scaling method.
#' @param valueColumn Name of column with value of interest either 'value' or 'startValue'.
#' @param optimizationGroup A character string indicating the optimization group;
#'                          must be one of 'both', 'external', or 'internal'.
#'
#' @return A data.table with combined and transformed input data.
#' @keywords internal
getParams <-
  function(dtPrior,
           dtStartValues,
           scalingMethod,
           valueColumn = c("value", "startValue"),
           optimizationGroup = c("both", "external", "internal")) {
    # initialize variables to avoid linter messages
    value <- minValue <- maxValue <- scaling <- NULL

    scaleValueToParam <- getScaleFunction(scalingMethod)

    optimizationGroup <- match.arg(optimizationGroup)
    valueColumn <- match.arg(valueColumn)

    # Select relevant columns from dtPrior and dtStartValues
    dtInput <-
      dtPrior[, c("id", ..valueColumn, "minValue", "maxValue", "scaling", "valueMode")]
    if (nrow(dtStartValues) > 0) {
      dtInput <- rbind(
        dtInput,
        dtStartValues[, c("id", ..valueColumn, "minValue", "maxValue", "scaling")],
        fill = TRUE
      )
    }
    setnames(dtInput, old = valueColumn, new = "value")

    # split parameters for optimizations
    dtInput <- switch(optimizationGroup,
      "external" = dtInput[is.na(valueMode) | valueMode == PARAMETERTYPE$global],
      "internal" = dtInput[valueMode %in% c(PARAMETERTYPE$hyperParameter, PARAMETERTYPE$outputError)],
      dtInput
    )

    checkmate::assertNumeric(dtInput$value, any.missing = FALSE)
    checkmate::assertNumeric(dtInput$minValue, any.missing = FALSE)
    checkmate::assertNumeric(dtInput$maxValue, any.missing = FALSE)
    checkmate::assertNames(tolower(dtInput$scaling), subset.of = unlist(SCALING))

    # Transform params to unbounded values
    dtInput[, param := scaleValueToParam(
      value = value,
      minValue = minValue,
      maxValue = maxValue,
      scaling = tolower(scaling)
    ),
    by = .I
    ]

    initialValues <- stats::setNames(
      dtInput$param,
      dtInput$id
    )

    return(initialValues)
  }

#' Set Parameter to Tables
#'
#' This function updates the parameter values in the provided data.tables based on the optimization results.
#'
#' @param dtList A list containing various data.tables used in the optimization process.
#' @param params A numeric vector of parameters for the likelihood calculation.
#' @param scalingMethod A character string specifying the scaling method.
#'                      Must be one of "logsig" or "hardBounds".
#'
#' @return A list containing the updated data.tables.
#' @keywords internal
setParameterToTables <- function(dtList, params, scalingMethod) {
  unscaleParamToValue <- getUnscaleFunction(scalingMethod)

  for (table in c("prior", "startValues")) {
    if (nrow(dtList[[table]]) > 0) {
      dtList[[table]][id %in% names(params), param := params[id]]
      dtList[[table]][id %in% names(params), value :=
        unscaleParamToValue(
          param = param,
          minValue = minValue,
          maxValue = maxValue,
          scaling = tolower(scaling)
        ),
      by = .I
      ]
    }
  }

  return(dtList)
}

#' Scale to Unbounded Values
#'
#' This function scales input values to an unbounded range using the specified scaling method.
#'
#' @param value A numeric vector of values to be scaled.
#' @param minValue A numeric value representing the minimum bound.
#' @param maxValue A numeric value representing the maximum bound.
#' @param scaling A character string indicating the scaling method; currently supports "log".
#' @return A numeric vector of scaled values.
#' @keywords internal
scaleToLogSig <- function(value, minValue, maxValue, scaling) {
  param <-
    qlogis(scaleWithinBounds(value = value,
                                  minValue = minValue,
                                  maxValue = maxValue,
                                  scaling = scaling))

  param <- pmax(-20, pmin(20, param))
  return(param)
}

#' Inverse Scale from Unbounded Values
#'
#' This function inversely scales parameters from an unbounded range back to their original values.
#'
#' @param param A numeric vector of parameters to be unscaled.
#' @param minValue A numeric value representing the minimum bound.
#' @param maxValue A numeric value representing the maximum bound.
#' @param scaling A character string indicating the scaling method; currently supports "log".
#' @return A numeric vector of unscaled values.
#' @keywords internal
unscaleFromLogSig <- function(param, minValue, maxValue, scaling) {
  unscaleFromBounds(param = plogis(param),
                    minValue = minValue,
                    maxValue = maxValue,
                    scaling = scaling)
}

#' Scale Within Specified Limits
#'
#' This function scales input values to a range between 0 and 1, ensuring they are within specified limits.
#'
#' @param value A numeric vector of values to be scaled.
#' @param minValue A numeric value representing the minimum bound.
#' @param maxValue A numeric value representing the maximum bound.
#' @param scaling A character string indicating the scaling method; currently supports "log".
#' @return A numeric vector of scaled values, or NA if the input is out of bounds.
#' @keywords internal
scaleWithinBounds <- function(value, minValue, maxValue, scaling) {
  if (value <= minValue || value >= maxValue) {
    return(NA_real_)
  }

  if (tolower(scaling) == SCALING$log) {
    (log(value) - log(minValue)) / (log(maxValue) - log(minValue))
  } else {
    (value - minValue) / (maxValue - minValue)
  }
}

#' Inverse Scale from Specified Limits
#'
#' This function inversely scales parameters from a range between 0 and 1 back to their original values.
#'
#' @param param A numeric vector of parameters to be unscaled.
#' @param minValue A numeric value representing the minimum bound.
#' @param maxValue A numeric value representing the maximum bound.
#' @param scaling A character string indicating the scaling method; currently supports "log".
#' @return A numeric vector of unscaled values, or NA if the input is out of bounds.
#' @keywords internal
unscaleFromBounds <- function(param, minValue, maxValue, scaling) {
  if (param <= 0 || param >= 1) {
    return(NA_real_)
  }

  if (tolower(scaling) == SCALING$log) {
    exp(param * (log(maxValue) - log(minValue)) + log(minValue))
  } else {
    param * (maxValue - minValue) + minValue
  }
}
