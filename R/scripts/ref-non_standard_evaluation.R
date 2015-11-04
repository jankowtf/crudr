
# Non-standard evaluation ------------------------------------------------------

# http://adv-r.had.co.nz/Computing-on-the-language.html

sample_df <- data.frame(a = 1:5, b = 5:1, c = c(5, 3, 1, 4, 1))
a <- 10
eval(quote(a), sample_df)
#> [1] 1 2 3 4 5
eval(a, sample_df)
#> [1] 10

eval(quote(b), sample_df)
#> [1] 5 4 3 2 1
eval(b, sample_df)
#> Error in eval(b, sample_df): obj

subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  print(condition_call)
  print(quote(condition))
  r <- eval(condition_call, x)
  x[r, ]
}
subset2(sample_df, a >= 4)

inst <- Crud$new()
id <- paste(letters, collapse = "/")
arglist <- list("hello world!")
names(arglist) <- id
fun <- inst$create
do.call(fun, arglist)
inst$read()

foo <- function(...) {
  cache <- substitute(...)
  #   print(cache)
  #   print(class(cache))
  if (is.call(cache)) {

  }

  list(
    cache = eval(cache),
    threedots = list(...)
  )
}
foo(a = 1)

id <- paste(letters, collapse = "/")
foo(id)

# foo(paste(letters, collapse = "/"), a = 1)
# foo(substitute(paste(letters, collapse = "/") <- 5))
# as.name(paste(letters, collapse = "/")) <- 5

library(settings)
bar <- function(..., do_someting = FALSE) {
  #   cache <- substitute(...)
  #   list(
  #     cache = cache,
  #     cache_eval = eval(cache),
  #     threedots = list(...),
  #     opts = options_manager(...)
  #   )
  values <- list(...)
  nms <- names(values)
  if (is.null(nms)) {
    ## --> wrapped into list for batch setting
    do.call(options_manager, unlist(values, recursive = FALSE))
  } else if (any(idx <- nms == "")) {
    ## --> mixed
    values <- c(values[!idx], unlist(values[idx], recursive = FALSE))
    do.call(options_manager, values[sort(names(values))])
  } else {
    ## --> regular
    options_manager(...)
  }
}
opts <- bar(a = 1, b = 2)
opts <- bar(list(a = 1, b = 2))
opts <- bar(list(a = 1), b = 2)
opts()

handleThreedots <- function(...) {
  values <- list(...)
  nms <- names(values)
  if (is.null(nms)) {
    ## --> wrapped into list for batch setting
    unlist(values, recursive = FALSE)
  } else if (any(idx <- nms == "")) {
    ## --> mixed
    c(values[!idx], unlist(values[idx], recursive = FALSE))
  } else {
    ## --> regular
    values
  }
}
handleThreedots(a = 1, b = 2, c = 3, d = 4)
handleThreedots(list(a = 1, b = 2), list(c = 3, d = 4))
handleThreedots(list(a = 1), b = 2, list(c = 3), d = 4)

withHandledThreedots <- function(..., fun) {
  do.call(fun, handleThreedots(...))
}
res <- withHandledThreedots(a = 1, b = 2, c = 3, d = 4, fun = options_manager)
res()
res <- withHandledThreedots(list(a = 1, b = 2), list(c = 3, d = 4),
  fun = options_manager)
res()
res <- withHandledThreedots(list(a = 1), b = 2, list(c = 3), d = 4,
  fun = options_manager)
res()

res <- withHandledThreedots(a = 1, b = 2, c = 3, d = 4,
  fun = options_manager, .allowed = list(a = inrange(1, 2)))
res()
res(a = 3)
res <- withHandledThreedots(list(a = 1, b = 2), list(c = 3, d = 4),
  fun = options_manager, .allowed = list(a = inrange(1, 2)))
res()
res(a = 3)
res <- withHandledThreedots(list(a = 1), b = 2, list(c = 3), d = 4,
  fun = options_manager, .allowed = list(a = inrange(1, 2)))
res()
res(a = 3)

foo <- function(..., special = FALSE) {
  if (special) {
    message("hello world!")
  }
  list(...)
}
res <- withHandledThreedots(a = 1, b = 2, c = 3, d = 4,
  fun = foo)
res <- withHandledThreedots(a = 1, b = 2, c = 3, d = 4,
  fun = foo, special = TRUE)
