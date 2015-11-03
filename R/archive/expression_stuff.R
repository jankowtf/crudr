
# composeForIndex ---------------------------------------------------------

#' @title
#' Compose for index
#'
#' @description
#' Composes objects to form something that is relevant for indexing.
#'
#' @details
#' TODO
#'
#' @param inst Main input.
#' @template threedots
#' @return See respective methods.
#' @example inst/examples/example-composeForIndex.R
composeForIndex <- function(inst, ...) {
  UseMethod("composeForIndex", inst)
}

#' @title
#' Compose for index
#'
#' @description
#' Generic: \code{\link[crudr]{composeForIndex}}.
#' Method for: \code{\link[base]{character}}
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{character}}.
#' @template threedots
#' @return \code{\link[base]{character}}
#' @example inst/examples/example-composeForIndex.R
composeForIndex.character <- function(
  inst,
  sep = c("[['", "']][['", "']]")
) {
  paste0(sep[1], paste(inst, collapse = sep[2]), sep[3])
}

#' @title
#' Create an object for indexing information
#'
#' @description
#' Creates objects that facilitate indexing of information from various
#' object types.
#'
#' @details
#' TODO
#'
#' @param inst Main information based on which the index object is created.
#' @template threedots
#' @return See respective methods.
#' @example inst/examples/example-createIndex.R
createIndex <- function(inst, ...) {
  UseMethod("createIndex", inst)
}

#' @title
#' Create an object for indexing information
#'
#' @description
#' Generic: \code{\link[crudr]{createIndex}}.
#' Method for: \code{\link[base]{R6}}
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{character}}.
#' @template threedots
#' @return \code{\link[base]{character}}
#' @example inst/examples/example-createIndex.R
createIndex <- function(
  inst,

) {
  expr <- substitute(FUN(X, INDEX),
    list(FUN = as.name('[['), X = as.name("x"), INDEX = c("a", "b")))
  eval(expr)
}

mapTreeFormat <- function(
  values = list(),
  include_root = TRUE,
  overwrite = FALSE
) {
  branches <- names(values)
  branches <- gsub("\\$", "/", branches)
  names(values) <- branches
  leafs <- values

  branches <- branches[branches != ""]
  leafs <- leafs[leafs != ""]

  splitted <- strsplit(branches, "/")
  #   level_1=splitted[[1]]
  #   level_2=2
  out <- list()
  for(level_1 in splitted) {
    for(level_2 in 1:length(level_1)) {
      idx <- createListIndex(level_1[1:level_2])
      path <- createListIndex(level_1[1:level_2], sep = c("", "/", ""))
      expr_get <- sprintf("%s%s", "out", idx)
      expr_set <- if (path %in% names(leafs)) {
        if (is.null(leafs[[path]])) {
          leafs[[path]] <- list()
        }
        sprintf("%s%s <- leafs[[path]]", "out", idx)
      } else {
        # sprintf("%s%s <- list()", "out", idx)
        sprintf("%s%s <- new.env(parent = emptyenv())", "out", idx)
      }
      tmp <- eval(parse(text = expr_get))
      if (is.null(tmp)) {
        eval(parse(text = expr_set))
      } else {
        if (overwrite) {
          eval(parse(text = expr_set))
        } else {
          message("Exists:")
          print(path)
          print(expr_get)
          print(expr_set)
        }
      }
    }
  }
  if (!include_root) {
    out <- out[[1]]
  }
  out
}
