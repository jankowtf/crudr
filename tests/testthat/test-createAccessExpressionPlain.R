
# createAccessExpressionPlain -------------------------------------

context("createAccessExpressionPlain")

test_that("createAccessExpressionPlain", {
  inst <- list(a = list(b = list(c = 10)))
  expect_equivalent(createAccessExpressionPlain(inst, c("a", "b")),
    expression(inst[[c("a", "b")]])[[1]])
  expect_equivalent(createAccessExpressionPlain(inst, c("a", "b"), value = 1),
    expression(inst[[c("a", "b")]] <- 1)[[1]])
})
