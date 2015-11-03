
# Example object ----------------------------------------------------------

inst <- list(a = list(b = list(c = 10)))

# Extract -----------------------------------------------------------------

createAccessExpression(inst, c("a", "b"))

createAccessExpression(inst, c("a", "b"), evaluate = TRUE)
## --> evaluated inside the function

createAccessExpression(inst, c("a", "b"), evaluate = TRUE, eval_in_parent = TRUE)
## --> evaluated in the frame of `inst`

# Assign ------------------------------------------------------------------

createAccessExpression(inst, c("a", "b", "c"), 100)
createAccessExpression(inst, c("a", "b", "c"), 100, evaluate = TRUE)
## --> evaluated inside the function
inst
## --> `inst$a$b$c` still is `10`

createAccessExpression(inst, c("a", "b", "c"), 100, evaluate = TRUE,
  eval_in_parent = TRUE)
## --> evaluated in the frame of `inst`
inst
## --> `inst$a$b$c` is now `100`

# Return values -----------------------------------------------------------

inst <- list(a = list(b = list(c = 10)))

createAccessExpression(inst, c("a", "b", "c"), 100, evaluate = TRUE,
  return_conventional = FALSE)
## --> evaluated inside the function and entire "updated" object `inst`returned
inst
## --> `inst$a$b$c` still is `10`

createAccessExpression(inst, c("a", "b", "c"), 1, evaluate = TRUE,
  eval_in_parent = TRUE, return_conventional = FALSE)
## --> evaluated in the frame of `inst`. In this case, `NULL` is returned
##     in order to stress the point that the actual object being altered
##     has nothing to do with the return value
inst
## --> `inst$a$b$c` is now `1`

# Sugar = $ ---------------------------------------------------------------

## For the sake of completeness the function also allows you to use the
## syntactic sugar `$` instead of `[[`. While this might be usefull in
## certain usage scenarios, it needs to be noted that comes with additional
## computiational costs (see benchmark)

inst <- list(a = list(b = list(c = 10)))
createAccessExpression(inst, c("a", "b"), sugar = "$")
createAccessExpression(inst, c("a", "b"), value = 1, sugar = "$")

library(microbenchmark)
res <- microbenchmark(
  "extract_1" = createAccessExpression(inst, c("a", "b"), sugar = "[["),
  "extract_2" = createAccessExpression(inst, c("a", "b"), sugar = "$"),
  "assign_1" = createAccessExpression(inst, c("a", "b"), 1, sugar = "[["),
  "assign_2" = createAccessExpression(inst, c("a", "b"), 1, sugar = "$")
)
res

# Comparison to plain method ----------------------------------------------

## While `createAccessExpression` offers more features,
## `createAccessExpressionPlain` is substantially faster.

library(microbenchmark)
res <- microbenchmark(
  "extract_1" = createAccessExpression(inst, c("a", "b")),
  "extract_2" = createAccessExpressionPlain(inst, c("a", "b")),
  "assign_1" = createAccessExpression(inst, c("a", "b"), 1),
  "assign_2" = createAccessExpressionPlain(inst, c("a", "b"), 1)
)
res
