inst <- list(a = list(b = list(c = 10)))

(expr <- createCrudExpressionPlain(inst, c("a", "b")))
eval(expr)

(expr <- createCrudExpressionPlain(inst, c("a", "b"), 1))
eval(expr)
inst
