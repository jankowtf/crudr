
# Example object ----------------------------------------------------------

inst <- list(a = list(b = list(c = 10)))

# Extract -----------------------------------------------------------------

createCrudExpression(inst, c("a", "b"))

createCrudExpression(inst, c("a", "b"), evaluate = TRUE)
## --> evaluated inside the function

createCrudExpression(inst, c("a", "b"), evaluate = TRUE, in_parent = TRUE)
## --> evaluated in the frame of `inst`

# Assign ------------------------------------------------------------------

createCrudExpression(inst, c("a", "b", "c"), 100)
createCrudExpression(inst, c("a", "b", "c"), 100, evaluate = TRUE)
## --> evaluated inside the function
inst
## --> `inst$a$b$c` still is `10`

createCrudExpression(inst, c("a", "b", "c"), 100, evaluate = TRUE,
  in_parent = TRUE)
## --> evaluated in the frame of `inst`
inst
## --> `inst$a$b$c` is now `100`

# Return values -----------------------------------------------------------

inst <- list(a = list(b = list(c = 10)))

createCrudExpression(inst, c("a", "b", "c"), 100, evaluate = TRUE,
  return_conventional = FALSE)
## --> evaluated inside the function and entire "updated" object `inst`returned
inst
## --> `inst$a$b$c` still is `10`

createCrudExpression(inst, c("a", "b", "c"), 1, evaluate = TRUE,
  in_parent = TRUE, return_conventional = FALSE)
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
createCrudExpression(inst, c("a", "b"), sugar = "$")
createCrudExpression(inst, c("a", "b"), value = 1, sugar = "$")

library(microbenchmark)
res <- microbenchmark(
  "extract_1" = createCrudExpression(inst, c("a", "b"), sugar = "[["),
  "extract_2" = createCrudExpression(inst, c("a", "b"), sugar = "$"),
  "assign_1" = createCrudExpression(inst, c("a", "b"), 1, sugar = "[["),
  "assign_2" = createCrudExpression(inst, c("a", "b"), 1, sugar = "$")
)
res

# Comparison to plain method ----------------------------------------------

## While `createCrudExpression` offers more features,
## `createCrudExpressionPlain` is substantially faster.

library(microbenchmark)
res <- microbenchmark(
  "extract_1" = createCrudExpression(inst, c("a", "b")),
  "extract_2" = createCrudExpressionPlain(inst, c("a", "b")),
  "assign_1" = createCrudExpression(inst, c("a", "b"), 1),
  "assign_2" = createCrudExpressionPlain(inst, c("a", "b"), 1)
)
res


# Bridging ----------------------------------------------------------------

inst <- list(a = list(b = 1))
createCrudExpression(inst, c("a", "b", "c", "d"), value = 1, evaluate = TRUE,
  return_conventional = FALSE)
createCrudExpression(inst, c("a", "b", "c", "d"), value = 1, evaluate = TRUE,
  return_conventional = FALSE, strict = 1)
try(
  createCrudExpression(inst, c("a", "b", "c", "d"), value = 1, evaluate = TRUE,
  return_conventional = FALSE, strict = 2)
)
try(
  createCrudExpression(inst, c("a", "b", "c", "d"), value = 1, evaluate = TRUE,
  return_conventional = FALSE, strict = 3)
)
