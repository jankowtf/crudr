
# applyCrudExpressionTree -------------------------------

context("applyCrudExpressionTree")

test_that("applyCrudExpressionTree", {
  inst <- list(a = list(b = 1))
  tree <- createCrudExpressionTree(inst, c("a", "b", "c"), fail_safe = TRUE)

  ## Exists //
  expect_identical(
    applyCrudExpressionTree(tree, where = inst, type = "exists"),
    list("a" = TRUE, "a/b" = TRUE, "a/b/c" = FALSE)
  )

  ## Get //
  expect_is(
    res <- applyCrudExpressionTree(tree, where = inst, type = "get"),
    "list"
  )
  expect_identical(res[[1]], list(b = 1))
  expect_identical(res[[2]], 1)
  expect_is(res[[3]], "try-error")

  ## Set //
  expect_identical(
    res <- applyCrudExpressionTree(tree, where = inst, type = "set"),
    list("a" = list(), "a/b" = list(), "a/b/c" = NULL)
  )
  expect_identical(inst, list(a = list(b = list())))
})

context("applyCrudExpressionTree: assign")

test_that("applyCrudExpressionTree: assign", {
  inst <- list(a = list(b = 1))
  tree <- createCrudExpressionTree(inst, c("a", "b", "c"),
    value = 1, fail_safe = TRUE)

  ## Exists //
  expect_identical(
    applyCrudExpressionTree(tree, where = inst, type = "exists"),
    list("a" = TRUE, "a/b" = TRUE, "a/b/c" = FALSE)
  )

  ## Get //
  expect_is(
    res <- applyCrudExpressionTree(tree, where = inst, type = "get"),
    "list"
  )
  expect_identical(res[[1]], list(b = 1))
  expect_identical(res[[2]], 1)
  expect_is(res[[3]], "try-error")

  ## Set //
  expect_identical(
    res <- applyCrudExpressionTree(tree, where = inst, type = "set"),
    list("a" = list(), "a/b" = list(), "a/b/c" = 1)
  )
  expect_identical(inst, list(a = list(b = list(c = 1))))
})
