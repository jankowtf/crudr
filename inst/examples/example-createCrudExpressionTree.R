
# Example object ----------------------------------------------------------

inst = list()

# No assignment value -----------------------------------------------------

createCrudExpressionTree(inst, c("a", "b", "c"))
createCrudExpressionTree(inst, c("a", "b", "c"), name_obj = "x")
createCrudExpressionTree(inst, c("a", "b", "c"), as_name_obj = FALSE)

createCrudExpressionTree(inst, c("a", "b", "c"), fail_safe = TRUE)

# Assignment value --------------------------------------------------------

createCrudExpressionTree(inst, c("a", "b", "c"), value = 1)
createCrudExpressionTree(inst, c("a", "b", "c"), value = 1,
  name_value = "y", as_name_value = TRUE)

createCrudExpressionTree(inst, c("a", "b", "c"), value = 1, fail_safe = TRUE)

