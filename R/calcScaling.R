#' calcScaling
#'
#' This function creates a GAMS file with scaling of variables. The scaling is
#' calculated based on a gdx file containing all variables of a run.
#'
#' @param gdx path to a gdx file
#' @param file A file name the scaling GAMS code should be written to. If NULL
#' the code is returned by the function
#' @param magnitude The order of magnitude for which variables should be
#' scaled. All variables with average absolute values which are either below
#' 10^(-magnitude) or above 10^(magnitude) will be scaled.
#' @return A vector with the scaling GAMS code if file=NULL, otherwise nothing
#' is returned.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readGDX}}
#' @examples
#' \dontrun{
#' calcScaling("fulldata.gdx")
#' }
#' @export
calcScaling <- function(gdx, file = NULL, magnitude = 2) {
  v <- readGDX(gdx, type = "Variable", select = list("_field" = "level"))
  out <- NULL
  for (x in names(v)) {
    # calculate order of magnitude (oof)
    oof <- round(log10(mean(abs(v[[x]]))))
    if (is.nan(oof)) oof <- 0
    cat("\n oof =", oof, "  ", x)
    if (length(attr(v[[x]], "gdxMetadata")$domain) == 0) {
      sets <- ""
    } else {
      sets <- paste("(", paste(attr(v[[x]], "gdxMetadata")$domain, collapse = ","), ")", sep = "")
    }
    if (oof != -Inf && (oof < -1 * magnitude || oof > 1 * magnitude)) {
      out <- c(out, paste(x, ".scale", sets, " = 1e", oof, ";", sep = ""))
    }
  }
  cat("\n\n")

  if (!is.null(file)) {
    writeLines(out, file)
  } else {
    return(out)
  }
}
