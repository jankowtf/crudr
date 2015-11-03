
# createCrudExpression: sugar = [[ --------------------------------------

context("createCrudExpression: sugar = [[")

test_that("createCrudExpression: sugar = [[", {
  inst <- list(a = list(b = list(c = 10)))
  expect_equivalent(createCrudExpression(inst, c("a", "b")),
    expression(inst[[c("a", "b")]])[[1]])
  expect_equivalent(createCrudExpression(inst, c("a", "b"), name_obj = "x"),
    expression(x[[c("a", "b")]])[[1]])
  expect_equivalent(createCrudExpression(inst, c("a", "b"), value = 1),
    expression(inst[[c("a", "b")]] <- 1)[[1]])
  expect_identical(createCrudExpression(inst, c("a", "b"), evaluate = TRUE),
    inst$a$b)
})

# createCrudExpression: sugar = [[: as name -----------------------------

context("createCrudExpression: sugar = [[: as name")

test_that("createCrudExpression: sugar = [[: as name", {
  inst <- list(a = list(b = list(c = 10)))
  expect_equivalent(
    as.character(res <- createCrudExpression(inst, c("a", "b"),
      as_name_obj = FALSE)),
    as.character(expression(list(a = list(
      b = list(c = 10)))[[c("a", "b")]])[[1]])
  )
  #   deparse(res)
  #   deparse(expression(list(a = list(b = list(c = 10)))[[c("a", "b")]])[[1]])
  expect_equivalent(
    as.character(res <- createCrudExpression(inst, c("a", "b"), value = 1,
      as_name_value = TRUE)),
    as.character(expression(inst[[c("a", "b")]] <- value)[[1]])
  )
  expect_equivalent(
    as.character(res <- createCrudExpression(inst, c("a", "b"), value = 1,
      as_name_obj = FALSE, as_name_value = TRUE)),
    as.character(expression(list(a = list(
      b = list(c = 10)))[[c("a", "b")]] <- value)[[1]])
  )

  expect_equivalent(res <- createCrudExpression(inst, c("a", "b"), value = 1),
    expression(inst[[c("a", "b")]] <- 1)[[1]])

  if (FALSE) {
    library(microbenchmark)
    res <- microbenchmark(
      "1" = createCrudExpression(inst, c("a", "b"), sugar = "[["),
      "2" = createCrudExpression(inst, c("a", "b"), sugar = "$")
    )
    res
  }
})

# createCrudExpression: sugar = $ ---------------------------------------

context("createCrudExpression: sugar = $")

test_that("createCrudExpression: sugar = $", {
  inst <- list(a = list(b = list(c = 10)))
  expect_equivalent(res <- createCrudExpression(inst, c("a", "b"), sugar = "$"),
    expression(inst$a$b)[[1]])
  expect_equivalent(res <- createCrudExpression(inst, c("a", "b"), sugar = "$",
    value = 1),
    expression(inst$a$b <- 1)[[1]]
  )
})

# createCrudExpression: return values -----------------------------------

context("createCrudExpression: return values")

test_that("createCrudExpression: return values", {
  inst <- list(a = list(b = list(c = 10)))

  ## Conventional //
  expect_identical(
    createCrudExpression(inst, c("a", "b"), value = 1, evaluate = TRUE),
    1
  )
  expect_identical(
    createCrudExpression(inst, c("a", "b"), value = 1, evaluate = TRUE,
      in_parent = TRUE),
    1
  )
  expect_identical(inst, list(a = list(b = 1)))

  ## Non-conventional //
  expect_identical(
    createCrudExpression(inst, c("a", "b"), value = 100, evaluate = TRUE,
      return_conventional = FALSE),
    list(a = list(b = 100))
  )
  expect_null(
    createCrudExpression(inst, c("a", "b"), value = 1, evaluate = TRUE,
      in_parent = TRUE, return_conventional = FALSE)
  )
  expect_identical(inst, list(a = list(b = 1)))
})

# createCrudExpression: bridge ------------------------------------------

context("createCrudExpression: bridge: in parent")

test_that("createCrudExpression: bridge: in parent", {
  inst <- list()
  value <- 1
  expect_equivalent(createCrudExpression(inst, c("a", "b", "c"), 1,
    evaluate = TRUE, in_parent = TRUE),
    value
  )
  expect_identical(inst, list(a = list(b = list(c = 1))))

  inst <- list()
  value <- 1
  expect_equivalent(createCrudExpression(inst, c("a", "b", "c"), 1,
    evaluate = TRUE, in_parent = TRUE, return_conventional = FALSE),
    NULL
  )
  expect_identical(inst, list(a = list(b = list(c = 1))))
})

context("createCrudExpression: bridge: in current")

test_that("createCrudExpression: bridge: in current", {
  inst <- list()
  value <- 1
  expect_equivalent(createCrudExpression(inst, c("a", "b", "c"), 1,
    evaluate = TRUE),
    value
  )
  expect_identical(inst, list())

  inst <- list()
  value <- 1
  expect_equivalent(createCrudExpression(inst, c("a", "b", "c"), 1,
    evaluate = TRUE, return_conventional = FALSE),
    list(a = list(b = list(c = 1)))
  )
  expect_identical(inst, list())
})

# createCrudExpression: bridge: preserve existing -----------------------

context("createCrudExpression: bridge: preserve existing")

test_that("createCrudExpression: bridge: preserve existing", {
  inst <- list(a = list(b = 1))
  value <- 1
  expect_equivalent(createCrudExpression(inst, c("a", "b", "c"), value,
    evaluate = TRUE, in_parent = TRUE),
    value
  )
  expect_identical(inst, list(a = list(b = c(1, c = 1))))

  value <- list(x = 99)
  expect_equivalent(createCrudExpression(inst, c("a", "b", "c"),
    value, evaluate = TRUE, in_parent = TRUE),
    value
  )
  expect_identical(inst, list(a = list(b = list(1, c = list(x = 99)))))

  value <- list(y = TRUE)
  expect_equivalent(createCrudExpression(inst, c("a", "b", "c", "d", "e"),
    value, evaluate = TRUE, in_parent = TRUE),
    value
  )
#   expect_identical(inst, list(a = list(b = list(1, c = list(x = 99,
#     d = list(e = list(y = TRUE)))))))
  expect_identical(inst, list(a = list(b = list(1, c = list(
    d = list(e = list(y = TRUE)))))))
  ## TODO 2015-11-04: fix inconsistency
  ## --> now overwritten due to list-wrapping. Desirable for this other occasions,
  ##    but here it's problematic
})

context("createCrudExpression: bridge: strictness")

test_that("createCrudExpression: bridge: strictness", {
  inst <- list(a = list(b = 1))
  value <- 1
  expect_message(
    expect_equivalent(res <- createCrudExpression(inst, c("a", "b", "c", "d"), value,
      evaluate = TRUE, in_parent = TRUE, strict = 1),
      value
    ),
    "createCrudExpression.default: bridging required"
  )
  expect_identical(inst, list(a = list(b = list(1, c = list(d = 1)))))

  inst <- list(a = list(b = 1))
  value <- 1
  expect_warning(
    expect_equivalent(res <- createCrudExpression(inst, c("a", "b", "c", "d"), value,
      evaluate = TRUE, in_parent = TRUE, strict = 2),
      value
    ),
    "createCrudExpression.default: bridging required"
  )
  expect_identical(inst, list(a = list(b = list(1, c = list(d = 1)))))

  inst <- list(a = list(b = 1))
  value <- 1
  expect_error(
    expect_equivalent(res <- createCrudExpression(inst, c("a", "b", "c", "d"), value,
      evaluate = TRUE, in_parent = TRUE, strict = 3),
      value
    ),
    "createCrudExpression.default: bridging required"
  )
  expect_identical(inst, list(a = list(b = 1)))
})
