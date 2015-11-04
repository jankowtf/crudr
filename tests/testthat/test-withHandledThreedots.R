context("withHandledThreedots")

test_that("withHandledThreedots", {
  library(settings)
  expect_is(
    res <- withHandledThreedots(a = 1, b = 2, c = 3, d = 4,
      fun = options_manager),
    "function"
  )
  expect_identical(res(),
    list(a = 1, b = 2, c = 3, d = 4)
  )

  res <- withHandledThreedots(list(a = 1, b = 2), list(c = 3, d = 4),
    fun = options_manager)
  expect_identical(res(),
    list(a = 1, b = 2, c = 3, d = 4)
  )
  res <- withHandledThreedots(list(a = 1), b = 2, list(c = 3), d = 4,
    fun = options_manager)
  expect_identical(res(),
    list(b = 2, d = 4, a = 1, c = 3)
  )

  ## Additional arguments //
  res <- withHandledThreedots(a = 1, b = 2, c = 3, d = 4,
    fun = options_manager, .allowed = list(a = inrange(1, 2)))
  expect_identical(res(),
    list(a = 1, b = 2, c = 3, d = 4)
  )
  expect_error(res(a = 3),
    "Option value out of range\\. Allowed values are in")
  res <- withHandledThreedots(list(a = 1, b = 2), list(c = 3, d = 4),
    fun = options_manager, .allowed = list(a = inrange(1, 2)))
  expect_error(res(a = 3),
    "Option value out of range\\. Allowed values are in")
  res <- withHandledThreedots(list(a = 1), b = 2, list(c = 3), d = 4,
    fun = options_manager, .allowed = list(a = inrange(1, 2)))
  expect_error(res(a = 3),
    "Option value out of range\\. Allowed values are in")
})
