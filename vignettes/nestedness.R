## ---- include = FALSE----------------------------------------------------
library(crudr)
stand_alone <- TRUE
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## ---- eval = stand_alone-------------------------------------------------
(inst <- list(github = list(users = list(user_1 = list(name = "none")))))
createCrudExpression(inst, id = c("github", "users", "user_1", "name"), 
  value = "rappster", in_parent = TRUE)
inst

## ---- eval = stand_alone-------------------------------------------------
(inst <- list(github = list(users = list(user_1 = list(name = "none")))))
createCrudExpression(inst, id = "github/users/user_1/name", 
  value = "rappster", in_parent = TRUE)
inst

## ---- eval = stand_alone-------------------------------------------------
(inst <- list(github = "rappster"))
createCrudExpression(inst, id = "github/user", value = "rappster",
  in_parent = TRUE)
inst

## ---- eval = stand_alone-------------------------------------------------
try(inst$github$user)
inst[["github"]][["user"]]

## ---- eval = stand_alone-------------------------------------------------
inst <- list(github = "rappster")
createCrudExpression(inst, id = "github/user", value = "rappster",
  in_parent = TRUE, use_tree = TRUE)
inst

## ---- eval = stand_alone-------------------------------------------------
try(inst$github$user)
inst <- list(github = "rappster")

## ---- eval = stand_alone-------------------------------------------------
inst <- list()
createCrudExpression(inst, id = letters, value = "hello world!", 
  in_parent = TRUE)
inst

## ---- eval = stand_alone-------------------------------------------------
inst <- list(github = list(users = list(user_1 = list(name = "none"))))

library(microbenchmark)
microbenchmark(
  "1" = createCrudExpression(inst, id = "github/users/user_1/name", 
    value = "rappster", in_parent = TRUE),
  "2" = createCrudExpression(inst, id = "github/users/user_1/name/a/b/c", 
    value = "rappster", in_parent = TRUE, use_tree = TRUE)
)

