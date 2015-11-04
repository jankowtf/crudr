inst = list(a = list(b = 1))

tree <- createCrudExpressionTree(inst, c("a", "b", "c"),
  value = 1, fail_safe = TRUE)

applyCrudExpressionTree(tree, "has")
applyCrudExpressionTree(tree, "create")
inst
applyCrudExpressionTree(tree, "read")
applyCrudExpressionTree(tree, "update")
inst
## --> currently exactly identical to the `create` expression
applyCrudExpressionTree(tree, "delete")
inst
