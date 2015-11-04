Sys.setenv(LANGUAGE = "en")

# createCrudExpressionTree: no assignment -------------------------------

context("createCrudExpressionTree: no assignment")

test_that("createCrudExpressionTree: no assignment", {
  inst <- list(a = list(b = list(c = 10)))
  expect_is(res <- createCrudExpressionTree(inst, c("a", "b", "c")), "list")

  ## Exists //
  expect_true(eval(res[[1]]$has))
  expect_true(eval(res[[2]]$has))
  expect_true(eval(res[[3]]$has))

  ## Get //
  expect_identical(eval(res[[1]]$read), inst$a)
  expect_identical(eval(res[[2]]$read), inst$a$b)
  expect_identical(eval(res[[3]]$read), inst$a$b$c)

  ## Set //
  expect_identical(eval(res[[1]]$create), list())
  expect_identical(inst, list(a = list()))
  expect_identical(eval(res[[2]]$create), list())
  expect_identical(inst, list(a = list(b = list())))
  expect_identical(eval(res[[3]]$create), NULL)
  expect_identical(inst, list(a = list(b = list())))
})

# createCrudExpressionTree: assignment ----------------------------------

context("createCrudExpressionTree: assignment")

test_that("createCrudExpressionTree: assignment", {
  inst <- list(a = 1)
  expect_is(res <- createCrudExpressionTree(inst, c("a", "b", "c"), 1), "list")

  ## Exists //
  expect_true(eval(res[[1]]$has))
  expect_false(eval(res[[2]]$has))
  expect_error(eval(res[[3]]$has), "subscript out of bounds")

  ## Get //
  expect_identical(eval(res[[1]]$read), inst$a)
  expect_error(eval(res[[2]]$read), "subscript out of bounds")
  expect_error(eval(res[[3]]$read), "recursive indexing failed")

  ## Set //
  expect_identical(eval(res[[1]]$create), list())
  expect_identical(inst, list(a = list()))
  expect_identical(eval(res[[2]]$create), list())
  expect_identical(inst, list(a = list(b = list())))
  expect_identical(eval(res[[3]]$create), 1)
  expect_identical(inst, list(a = list(b = list(c = 1))))
})
