## ---- include = FALSE----------------------------------------------------
library(crudr)
run <- FALSE
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## ---- eval=run-----------------------------------------------------------
#  library(settings)
#  x <- options_manager(a = 1, b = 2)
#  x()

## ---- eval=run-----------------------------------------------------------
#  values <- list(a = 1, b = 2)
#  x <- options_manager(values)
#  x()

## ---- eval=run-----------------------------------------------------------
#  foo <- function(...) {
#    values <- list(...)
#    nms <- names(values)
#    if (is.null(nms)) {
#      ## --> wrapped into list for batch setting
#      do.call(options_manager, unlist(values, recursive = FALSE))
#    } else if (any(idx <- nms == "")) {
#      ## --> mixed
#      values <- c(values[!idx], unlist(values[idx], recursive = FALSE))
#      do.call(options_manager, values[sort(names(values))])
#    } else {
#      ## --> regular
#      options_manager(...)
#    }
#  }

## ---- eval=run-----------------------------------------------------------
#  opts <- foo(a = 1, b = 2)
#  opts()
#  opts <- foo(list(a = 1, b = 2))
#  opts()
#  opts <- foo(list(a = 1), b = 2)
#  opts()

## ---- eval=run-----------------------------------------------------------
#  handleThreedots <- function(...) {
#    values <- list(...)
#    nms <- names(values)
#    if (is.null(nms)) {
#      ## --> wrapped into list for batch setting
#      unlist(values, recursive = FALSE)
#    } else if (any(idx <- nms == "")) {
#      ## --> mixed
#      c(values[!idx], unlist(values[idx], recursive = FALSE))
#      ## --> note that I did not introduce any sorting as before
#      ##     in order to not slow things down additionally
#    } else {
#      ## --> regular
#      values
#    }
#  }
#  withHandledThreedots <- function(..., fun) {
#    do.call(fun, handleThreedots(...))
#  }

## ---- eval=run-----------------------------------------------------------
#  handleThreedots(a = 1, b = 2, c = 3, d = 4)
#  handleThreedots(list(a = 1, b = 2), list(c = 3, d = 4))
#  handleThreedots(list(a = 1), b = 2, list(c = 3), d = 4)

## ---- eval=run-----------------------------------------------------------
#  res <- withHandledThreedots(a = 1, b = 2, c = 3, d = 4,
#    fun = options_manager)
#  res()
#  res <- withHandledThreedots(list(a = 1, b = 2), list(c = 3, d = 4),
#    fun = options_manager)
#  res()
#  res <- withHandledThreedots(list(a = 1), b = 2, list(c = 3), d = 4,
#    fun = options_manager)
#  res()

## ---- eval=run-----------------------------------------------------------
#  res <- withHandledThreedots(a = 1, b = 2, c = 3, d = 4,
#    fun = options_manager, .allowed = list(a = inrange(1, 2)))
#  res()
#  try(res(a = 3))
#  res <- withHandledThreedots(list(a = 1, b = 2), list(c = 3, d = 4),
#    fun = options_manager, .allowed = list(a = inrange(1, 2)))
#  res()
#  try(res(a = 3))
#  res <- withHandledThreedots(list(a = 1), b = 2, list(c = 3), d = 4,
#    fun = options_manager, .allowed = list(a = inrange(1, 2)))
#  res()
#  try(res(a = 3))

## ---- eval=run-----------------------------------------------------------
#  foo <- function(..., special = FALSE) {
#    if (special) {
#      message("hello world!")
#    }
#    list(...)
#  }
#  withHandledThreedots(a = 1, b = 2, c = 3, d = 4, fun = foo)
#  withHandledThreedots(a = 1, b = 2, c = 3, d = 4, fun = foo, special = TRUE)

