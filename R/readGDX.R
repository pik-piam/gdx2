#' readGDX
#'
#' Function to read gdx files in R. It is a stripped-down reimplementation of
#' readGDX which is now based on magclass structures and uses
#' \code{gamstransfer} as basis.
#'
#' @param gdx file name of a gdx file
#' @param ... search strings defining the objects that should be read from gdx
#' file, with *-autocompletion. Can also be vectors containing more than one
#' search strings
#' @param format Output format. Five choices are currently available
#' \code{simple}, \code{simplest}, \code{first_found}, and \code{name}.
#' Instead of writing the full format name each format has
#' its own abbreviation as shown below.  \describe{
#' \item{simple (s)}{This returns a list of outputs.}
#' \item{simplest (st)}{Behaves like "simple" if
#' more than one object is returned. However, if only one object is read from
#' gdx file the magpie object itself is returned getting rid of the surrounding
#' list structure. This is the recommended format for interactive use.}
#' \item{first_found (f)}{This is a special format for the case that you
#' would like to read in exactly one object but you do not know exactly what
#' the name of the object is. Here, you can list all possible names of the
#' object and the function will return the first object of the list which is
#' found. This is especially useful writing read functions for gdx outputs of
#' models in which the names of a data object might change over time but the
#' function itself should work for all model versions. Having this format helps
#' to make your gdx-based functions backwards compatible to older versions of a
#' gdx file with different naming.}
#' \item{raw (r)}{This returns the data as it comes from
#' \code{gamstransfer::readGDX} without data class conversion.}
#' \item{name (n)}{In this case the function returns the name of all objects
#' found in the gdx which fit to the given search pattern and the given type
#' as vector.}}
#' @param react determines the reaction, when the object you would like to read
#' in does not exist. Available options are "warning" (NULL is returned and a
#' warning is send that the object is missing), "silent" (NULL is returned, but
#' no warning is given) and "error" (The function throws out an error)
#' @param spatial argument to determine the spatial columns in the dataframe to
#' be converted to a magclass object. Defaults to NULL.
#' See \code{\link[magclass]{as.magpie}} for more information.
#' @param temporal argument to determine the temporal columns in the dataframe to
#' be converted to a magclass object. Defaults to NULL.
#' See \code{\link[magclass]{as.magpie}} for more information.
#' @param magpieCells (boolean) determines whether a set "j" gets special treatment
#' by replacing underscores in the set elements with dots. Active by default for
#' historical reasons. Can be ignored in most cases. Makes only a difference, if
#' 1) GDX element depends on set "j", 2) set "j" contains underscores.
#' @return The gdx objects read in the format set with the argument
#' \code{format}.
#' @author Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' readGDX("bla.gdx", "blub*")
#' }
#' @export

readGDX <- function(gdx, ..., format = "simplest", react = "warning",
                    spatial = NULL, temporal = NULL, magpieCells = TRUE) {

  formats <- c(f = "first_found", first_found = "first_found",
               s = "simple", simple = "simple",
               st = "simplest", simplest = "simplest",
               r = "raw", raw = "raw",
               n = "name", name = "name")
  if(is.na(formats[format])) stop("unknown format \"", format, "\"")
  format <- formats[format]

  allPatterns <- c(...)
  if (length(allPatterns) == 0) {
    if (format == "first_found") stop("For format \"first_found\" you have to explicitly give all possible names of the object you would like to read in!")
    allPatterns <- "*"
  }
  # translate name patterns in standard regular expression syntax
  allPatterns <- paste("^", gsub("*", ".*", allPatterns, fixed = TRUE), "$", sep = "")

  if(format != "first_found") {
    # collapse pattern to single search pattern
    allPatterns <- paste(allPatterns, collapse="|")
  }

  items <- names(gamstransfer::readGDX(gdx, records=FALSE))

  for(p in allPatterns) {
    selectedItems <- grep(p, items, value = TRUE)
    if(length(selectedItems) > 0) break
  }

  if (length(selectedItems) == 0) {
    if (react == "warning") warning("No element of ", paste(c(...), collapse = ", "), " found in GDX! NULL returned")
    if (react == "error") stop("No element of ", paste(c(...), collapse = ", "), " found in GDX!")
    return(NULL)
  }

  if (format == "name") return(selectedItems)

  x <- gamstransfer::readGDX(gdx, selectedItems)
  for(i in seq_along(x)) {
    d <- x[[i]]$description
    m <- x[[i]][!(names(x[[i]]) %in% c("records", "description"))]
    if(format != "raw") {
      if(m$class == "Set") {
        x[[i]] <- x[[i]]$records
        if(dim(x[[i]])[2] == 2) x[[i]] <- as.vector(x[[i]][[1]])
      } else if(m$class != "Alias") {
        x[[i]] <- magclass::as.magpie(x[[i]]$records, spatial = spatial,
                                      temporal = temporal)
        # special treatment of set "j" -> replace underscores with dots!
        if (magpieCells && ("j" %in% magclass::getSets(x[[i]]))) {
          magclass::getItems(x[[i]], 1, raw = TRUE) <- sub("_",".", magclass::getItems(x[[i]],1))
        }
      }
      attr(x[[i]], "description") <- d
      attr(x[[i]], "gdxMetadata") <- m
    }
  }

  if(length(x) == 1 && format %in% c("simplest", "first_found")) {
    x <- x[[1]]
  }

  return(x)
}
