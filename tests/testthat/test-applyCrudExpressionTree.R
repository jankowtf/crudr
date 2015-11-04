
# applyCrudExpressionTree -------------------------------

context("applyCrudExpressionTree")

test_that("applyCrudExpressionTree", {
  inst <- list(a = list(b = 1))
  tree <- createCrudExpressionTree(inst, c("a", "b", "c"), fail_safe = TRUE)

  expect_true(all(sapply(tree, function(ii) {
    all(names(ii) %in% c("has", "create", "read", "update", "delete"))
  })))

  ## Has //
  expect_identical(
    applyCrudExpressionTree(tree, type = "has"),
    list("a" = TRUE, "a/b" = TRUE, "a/b/c" = FALSE)
  )

  ## Read //
  expect_is(
    res <- applyCrudExpressionTree(tree, type = "read"),
    "list"
  )
  expect_identical(res[[1]], list(b = 1))
  expect_identical(res[[2]], 1)
  expect_is(res[[3]], "try-error")

  ## Create //
  expect_identical(
    res <- applyCrudExpressionTree(tree, type = "create"),
    list("a" = list(), "a/b" = list(), "a/b/c" = NULL)
  )
  expect_identical(inst, list(a = list(b = list())))

  ## Update //
  expect_identical(
    res <- applyCrudExpressionTree(tree, type = "update"),
    list("a" = list(), "a/b" = list(), "a/b/c" = NULL)
  )
  expect_identical(inst, list(a = list(b = list())))

  ## Delete //
  expect_identical(
    res <- applyCrudExpressionTree(tree, type = "delete", fail_safe = TRUE),
    list("a/b/c" = NULL, "a/b" = NULL, "a" = NULL)
  )
  expect_identical(inst, structure(list(), names = character()))
})

context("applyCrudExpressionTree: assign")

test_that("applyCrudExpressionTree: assign", {
  inst <- list(a = list(b = 1))
  tree <- createCrudExpressionTree(inst, c("a", "b", "c"),
    value = 1, fail_safe = TRUE)

  ## Has //
  expect_identical(
    applyCrudExpressionTree(tree, type = "has"),
    list("a" = TRUE, "a/b" = TRUE, "a/b/c" = FALSE)
  )

  ## Read //
  expect_is(
    res <- applyCrudExpressionTree(tree, type = "read"),
    "list"
  )
  expect_identical(res[[1]], list(b = 1))
  expect_identical(res[[2]], 1)
  expect_is(res[[3]], "try-error")

  ## Create //
  expect_identical(
    res <- applyCrudExpressionTree(tree, type = "create"),
    list("a" = list(), "a/b" = list(), "a/b/c" = 1)
  )
  expect_identical(inst, list(a = list(b = list(c = 1))))

  ## Update //
  expect_identical(
    res <- applyCrudExpressionTree(tree, type = "update"),
    list("a" = list(), "a/b" = list(), "a/b/c" = 1)
  )
  expect_identical(inst, list(a = list(b = list(c = 1))))

  ## Delete //
  expect_identical(
    res <- applyCrudExpressionTree(tree, type = "delete", fail_safe = TRUE),
    list("a/b/c" = NULL, "a/b" = NULL, "a" = NULL)
  )
  expect_identical(inst, structure(list(), names = character()))
})
