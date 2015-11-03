
# createCrudExpressionPlain -------------------------------------

context("createCrudExpressionPlain")

test_that("createCrudExpressionPlain", {
  inst <- list(a = list(b = list(c = 10)))
  expect_equivalent(createCrudExpressionPlain(inst, c("a", "b")),
    expression(inst[[c("a", "b")]])[[1]])
  expect_equivalent(createCrudExpressionPlain(inst, c("a", "b"), value = 1),
    expression(inst[[c("a", "b")]] <- 1)[[1]])
})
