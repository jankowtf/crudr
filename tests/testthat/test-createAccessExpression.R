
# createAccessExpression: sugar = [[ --------------------------------------

context("createAccessExpression: sugar = [[")

test_that("createAccessExpression: sugar = [[", {
  inst <- list(a = list(b = list(c = 10)))
  expect_equivalent(createAccessExpression(inst, c("a", "b")),
    expression(inst[[c("a", "b")]])[[1]])
  expect_equivalent(createAccessExpression(inst, c("a", "b"), name_obj = "x"),
    expression(x[[c("a", "b")]])[[1]])
  expect_equivalent(createAccessExpression(inst, c("a", "b"), value = 1),
    expression(inst[[c("a", "b")]] <- 1)[[1]])
  expect_identical(createAccessExpression(inst, c("a", "b"), evaluate = TRUE),
    inst$a$b)
})

# createAccessExpression: sugar = [[: as name -----------------------------

context("createAccessExpression: sugar = [[: as name")

test_that("createAccessExpression: sugar = [[: as name", {
  inst <- list(a = list(b = list(c = 10)))
  expect_equivalent(
    as.character(res <- createAccessExpression(inst, c("a", "b"),
      as_name_obj = FALSE)),
    as.character(expression(list(a = list(
      b = list(c = 10)))[[c("a", "b")]])[[1]])
  )
#   deparse(res)
#   deparse(expression(list(a = list(b = list(c = 10)))[[c("a", "b")]])[[1]])
  expect_equivalent(
    as.character(res <- createAccessExpression(inst, c("a", "b"), value = 1,
      as_name_value = TRUE)),
    as.character(expression(inst[[c("a", "b")]] <- value)[[1]])
  )
  expect_equivalent(
    as.character(res <- createAccessExpression(inst, c("a", "b"), value = 1,
      as_name_obj = FALSE, as_name_value = TRUE)),
    as.character(expression(list(a = list(
      b = list(c = 10)))[[c("a", "b")]] <- value)[[1]])
  )

  expect_equivalent(res <- createAccessExpression(inst, c("a", "b"), value = 1),
    expression(inst[[c("a", "b")]] <- 1)[[1]])

  if (FALSE) {
    library(microbenchmark)
    res <- microbenchmark(
      "1" = createAccessExpression(inst, c("a", "b"), sugar = "[["),
      "2" = createAccessExpression(inst, c("a", "b"), sugar = "$")
    )
    res
  }
})

# createAccessExpression: sugar = $ ---------------------------------------

context("createAccessExpression: sugar = $")

test_that("createAccessExpression: sugar = $", {
  inst <- list(a = list(b = list(c = 10)))
  expect_equivalent(res <- createAccessExpression(inst, c("a", "b"), sugar = "$"),
    expression(inst$a$b)[[1]])
  expect_equivalent(res <- createAccessExpression(inst, c("a", "b"), sugar = "$",
    value = 1),
    expression(inst$a$b <- 1)[[1]]
  )
})

# createAccessExpression: return values -----------------------------------

context("createAccessExpression: return values")

test_that("createAccessExpression: return values", {
  inst <- list(a = list(b = list(c = 10)))

  ## Conventional //
  expect_identical(
    createAccessExpression(inst, c("a", "b"), value = 1, evaluate = TRUE),
    1
  )
  expect_identical(
    createAccessExpression(inst, c("a", "b"), value = 1, evaluate = TRUE,
      eval_in_parent = TRUE),
    1
  )
  expect_identical(inst, list(a = list(b = 1)))

  ## Non-conventional //
  expect_identical(
    createAccessExpression(inst, c("a", "b"), value = 100, evaluate = TRUE,
      return_conventional = FALSE),
    list(a = list(b = 100))
  )
  expect_null(
    createAccessExpression(inst, c("a", "b"), value = 1, evaluate = TRUE,
      eval_in_parent = TRUE, return_conventional = FALSE)
  )
  expect_identical(inst, list(a = list(b = 1)))
})
