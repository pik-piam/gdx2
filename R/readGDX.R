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
#' @param type Type of objects that should be extracted. Available options
#' are "Parameter", "Set", "Alias", "Variable" and "Equation". If NULL all
#' types will be considered.
#' @param react determines the reaction, when the object you would like to read
#' in does not exist. Available options are "warning" (NULL is returned and a
#' warning is send that the object is missing), "silent" (NULL is returned, but
#' no warning is given) and "error" (The function throws out an error)
#' @param followAlias bolean deciding whether the alias or its linked set should be
#' returned.
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
#' @param select preselection of subsets in the data coming from the gdx using
#' the function \code{\link[magclass]{mselect}}. Information has to be provided
#' as a list of selections (e.g. \code{select=list(type="level")}). See
#' \code{\link[magclass]{mselect}} for more information.
#' @param restoreZeros Defines whether 0s, which are typically not stored in a
#' gdx file, should be restored or ignored in the output. By default they will
#' be restored. If possible, it is recommended to use restore_zeros=TRUE. It is
#' faster but more memory consuming. If you get memory errors you should use
#' restore_zeros=FALSE
#' @param addAttributes Boolean which controls whether the description and
#' gdxMetadata should be added as attributes or not
#' @return The gdx objects read in the format set with the argument
#' \code{format}.
#' @author Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' readGDX("bla.gdx", "blub*")
#' }
#' @export

readGDX <- function(gdx, ..., format = "simplest", type = NULL, react = "warning", # nolint: cyclocomp_linter
                    followAlias = FALSE, spatial = NULL, temporal = NULL, magpieCells = TRUE,
                    select = NULL, restoreZeros = TRUE, addAttributes = TRUE) {

  format <- .formatFormat(format)

  selectedItems <- .expandPattern(c(...), gdx, format, type, react)

  if (!is.null(selectedItems) && selectedItems == "###NOMATCH###") return(NULL)

  if (anyDuplicated(selectedItems)) {
    warning("Item(s) selected more than once, but will only be read once!")
    selectedItems <- unique(selectedItems)
  }

  if (format == "name") return(selectedItems)

  if (is.null(selectedItems)) {
    x <- gamstransfer::readGDX(gdx)
  } else {
    x <- NULL
    for (s in selectedItems) {
      tmp <- try(gamstransfer::readGDX(gdx, s), silent = TRUE)
      if (inherits(tmp, "try-error")) {
        if (react == "warning" && format != "first_found") warning(tmp)
        if (react == "error" && format != "first_found") stop(tmp)
      } else if (format == "first_found") {
        x <- tmp
        break
      } else {
        x <- c(x, tmp)
      }
    }
  }

  for (i in seq_along(x)) {
    d <- x[[i]]$description
    m <- x[[i]][!(names(x[[i]]) %in% c("records", "description"))]
    if (format != "raw") {
      if (m$class == "Set") {
        if (is.null(x[[i]]$records)) {
          x[[i]] <- character(0)
        } else {
          x[[i]] <- x[[i]]$records
          if (dim(x[[i]])[2] == 2) x[[i]] <- as.vector(x[[i]][[1]])
        }
      } else if (m$class == "Alias") {
        if (followAlias) x[[i]] <- readGDX(gdx, x[[i]]$aliasWith, followAlias = TRUE)
      } else {
        if (m$class == "Variable") {
          # convert data.table into long format
          .long <- function(x) {
            n <- c("level", "marginal", "lower", "upper", "scale")
            cn <- colnames(x)[!(colnames(x) %in% n)]
            if (length(cn) == 0) {
              out <- data.frame("_field" = n, check.names = FALSE)
            } else {
              out <- rbind(x[cn], x[cn], x[cn], x[cn], x[cn])
              out$"_field" <- rep(n, each = nrow(x))
            }
            out$value <- unlist(x[n])
            return(out)
          }
          x[[i]]$records <- .long(x[[i]]$records)
        }
        if (restoreZeros && length(x[[i]]$domain) > 0) {
          if ("*" %in% x[[i]]$domain) {
            warning("Cannot restore zeros for ", names(x)[i], " as set dependency is not defined!")
          } else {
            dimnames <- readGDX(gdx, x[[i]]$domain, format = "simple", addAttributes = FALSE, followAlias = TRUE)
            if ("_field" %in% colnames(x[[i]]$records)) {
              dimnames$"_field" <- c("level", "marginal", "lower", "upper", "scale")
            }
            out <- array(0, vapply(dimnames, length, 1), dimnames)
            if (!is.null(x[[i]]$records)) {
              out[as.matrix(x[[i]]$records[names(dimnames(out))])] <- x[[i]]$records[, ncol(x[[i]]$records)]
            }
            x[[i]]$records <- out
          }
        }
        x[[i]] <- magclass::as.magpie(x[[i]]$records, spatial = spatial,
                                      temporal = temporal, tidy = TRUE)
        # special treatment of set "j" -> replace underscores with dots!
        if (magpieCells && ("j" %in% magclass::getSets(x[[i]]))) {
          magclass::getItems(x[[i]], 1, raw = TRUE) <- sub("_", ".", magclass::getItems(x[[i]], 1))
        }
        if (!is.null(select)) {
          x[[i]] <- magclass::mselect(x[[i]], select, collapseNames = TRUE)
        }
      }
      if (addAttributes) {
        attr(x[[i]], "description") <- d
        attr(x[[i]], "gdxMetadata") <- m
      }
    }
  }

  if (length(x) == 1 && format %in% c("simplest", "first_found")) {
    x <- x[[1]]
  }

  return(x)
}

.formatFormat <- function(format) {
  formats <- c(f = "first_found", first_found = "first_found",
               s = "simple", simple = "simple",
               st = "simplest", simplest = "simplest",
               r = "raw", raw = "raw",
               n = "name", name = "name")
  if (!is.character(format)) stop("format setting is not a character!")
  if (is.na(formats[format])) stop("unknown format \"", format, "\"")
  return(formats[format])
}

.expandPattern <- function(allPatterns, gdx, format, type, react) {
  if (!is.null(format) && length(allPatterns) == 0) allPatterns <- "*"
  if (length(allPatterns) == 0) {
    if (format == "first_found") {
      stop("For format \"first_found\" you have to explicitly give all possible ",
           "names of the object you would like to read in!")
    }
    return(NULL)
  }
  if (!all(grepl("*", allPatterns, fixed = TRUE)) && is.null(type)) return(allPatterns)

  # translate name patterns in standard regular expression syntax
  patterns <- paste("^", gsub("*", ".*", allPatterns, fixed = TRUE), "$", sep = "")

  elems <- gamstransfer::readGDX(gdx, records = FALSE)
  items <- names(elems)

  if (!is.null(type)) {
    t <- vapply(elems, function(x) return(x$class), "")
    items <- items[t %in% type]
  }

  selectedItems <- NULL
  for (p in patterns) {
    selectedItems <- c(selectedItems, grep(p, items, value = TRUE))
    if (format == "first_found" && length(selectedItems) > 0) break
  }

  if (length(selectedItems) == 0) {
    if (react == "warning") warning("No element of ", paste(allPatterns, collapse = ", "),
                                    " found in GDX! NULL returned")
    if (react == "error") stop("No element of ", paste(allPatterns, collapse = ", "),
                               " found in GDX!")
    return("###NOMATCH###")
  }
  return(selectedItems)
}
