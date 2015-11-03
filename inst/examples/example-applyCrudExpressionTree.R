inst = list(a = list(b = 1))

tree <- createCrudExpressionTree(inst, c("a", "b", "c"),
  value = 1, fail_safe = TRUE)

applyCrudExpressionTree(tree, "exists")
applyCrudExpressionTree(tree, "get")
applyCrudExpressionTree(tree, "set")

inst
