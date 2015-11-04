#' @title
#' Handle threedots input
#'
#' @description
#' Handles threedots input for downstream functions whose main input is
#' provided via \code{...}.
#'
#' @details
#' TODO
#'
#' @param ... Flexible input that should be passed along to downstream
#'  functions.
#' @return \code{\link[base]{list}}
#' @example inst/examples/example-handleThreedots.R
#' @seealso \code{\link[base]{Reserved}, \link[methods]{dotsMethods}}
#' @template authors
#' @template references
#' @export
handleThreedots <- function(...) {
  values <- list(...)
  nms <- names(values)
  if (is.null(nms)) {
    ## --> wrapped into list for batch setting
    unlist(values, recursive = FALSE)
  } else if (any(idx <- nms == "")) {
    ## --> mixed
    c(values[!idx], unlist(values[idx], recursive = FALSE))
    ## --> note that I did not introduce any sorting as before
    ##     in order to not slow things down additionally
  } else {
    ## --> regular
    values
  }
}

#' @title
#' Call function with handle threedots input
#'
#' @description
#' Calls a function whose main input is provided via \code{...} and passes
#' a handled version of the original \code{...} as returend by
#' \code{\link[crudr]{handleThreedots}}.
#'
#' @details
#' TODO
#'
#' @param ... Flexible input that should be passed along to downstream
#'  function \code{fun}.
#' @param fun \code{\link[base]{function}}.
#'  Function to be called with handled threedots input.
#' @return Whatever the function \code{fun} returns.
#' @example inst/examples/example-withHandledThreedots.R
#' @seealso \code{\link[crudr]{handleThreedots}}
#' @template authors
#' @template references
#' @export
withHandledThreedots <- function(..., fun) {
  do.call(fun, handleThreedots(...))
}
