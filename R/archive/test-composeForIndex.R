context("composeForIndex")

test_that("composeForIndex", {
  inst <- letters[1:5]

  expect_identical(composeForIndex(inst),
    "[['a']][['b']][['c']][['d']][['e']]")

  expect_identical(composeForIndex(inst, sep = c("", "/", "")),
    paste(letters[1:5], collapse = "/"))
})

test_that("stackoverflow", {
  skip("Stackoverflow")
  x <- list(a = list(b = 1))
  x

  x[["a"]][["b"]]

  foo <- function(idx, obj = character(), sep = c("[['", "']][['", "']]")) {
    out <- paste0(sep[1], paste(idx, collapse = sep[2]), sep[3])
    if (length(obj)) {
      out <- paste0(obj, out)
    }
    out
  }
  foo(c("a", "b"))
  foo(c("a", "b"), "x")

  expr <- parse(text = foo(c("a", "b"), "x"))
  eval(expr)

  bar <- function(x, sep = c("/")) {
    unlist(strsplit(x, sep))
  }
  bar("a/b")

  foo(bar("a/b"))
  foo(bar("a/b"), "x")

  expr <- parse(text = foo(bar("a/b"), "x"))
  eval(expr)

  expr <- substitute(assign(X, VALUE),
    list(X = "x_2", VALUE = as.name("letters")))
  eval(expr)
  x_2

  expr <- substitute(FUN(X, INDEX),
    list(FUN = as.name('[['), X = as.name("x"), INDEX = "a"))
  eval(expr)
  # lapply(expr, function(ii) ii)

  expr_2 <- expression(x[["a"]][["b"]])[[1]]
  lapply(expr_2, function(ii) ii)

  idx <- c("a", "b")
  ii=2
  obj = x
  bar <- function(idx, obj) {
    for (ii in 1:length(idx)) {
      if (ii == 1) {
        X <- as.name("obj")
      } else {
        X <- expr
      }
      INDEX <- idx[ii]
      expr <- substitute(FUN(X, INDEX),
        list(FUN = as.name('[['), X = X, INDEX = INDEX))
    }
    expr
  }
  expr <- bar(c("a", "b"), x)
  eval(expr)

  expr <- substitute(FUN(X, INDEX),
    list(FUN = as.name('[['), X = as.name("x"), INDEX = c("a", "b")))
  eval(expr)

  # install.packages("microbenchmark")
  require(microbenchmark)
  res <- microbenchmark(
    "1" = {
      expr <- substitute(assign(X, VALUE),
        list(X = "x_2", VALUE = as.name("letters")))
      eval(expr)
    },
    "2" = {
      expr <- substitute(FUN(X, INDEX),
        list(FUN = as.name('[['), X = as.name("x"), INDEX = "a"))
      eval(expr)
    },
    "3" = {
      expr <- parse(text = foo(c("a", "b"), "x"))
      eval(expr)
    },
    "4" = {
      expr <- bar(c("a", "b"), x)
      eval(expr)
    },
    "5" = {
      expr <- substitute(FUN(X, INDEX),
        list(FUN = as.name('[['), X = as.name("x"), INDEX = c("a", "b")))
      eval(expr)
    }
  )
  res
})
