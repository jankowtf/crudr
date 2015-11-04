context("handleThreedots")

## TODO: Rename context
## TODO: Add more tests

test_that("handleThreedots", {
  expect_identical(
    handleThreedots(a = 1, b = 2, c = 3, d = 4),
    list(a = 1, b = 2, c = 3, d = 4)
  )
  expect_identical(
    handleThreedots(list(a = 1, b = 2), list(c = 3, d = 4)),
    list(a = 1, b = 2, c = 3, d = 4)
  )
  expect_identical(
    handleThreedots(list(a = 1), b = 2, list(c = 3), d = 4),
    list(b = 2, d = 4, a = 1, c = 3)
  )
})
