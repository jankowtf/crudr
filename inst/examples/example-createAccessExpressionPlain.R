inst <- list(a = list(b = list(c = 10)))

(expr <- createAccessExpressionPlain(inst, c("a", "b")))
eval(expr)

(expr <- createAccessExpressionPlain(inst, c("a", "b"), 1))
eval(expr)
inst
