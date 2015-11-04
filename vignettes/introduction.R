## ---- include = FALSE----------------------------------------------------
library(crudr)
run <- FALSE
root_dir <- paste(rep("../", 1), collapse = "")
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)
knitr::opts_knit$set(root.dir = root_dir)

## ----echo=FALSE----------------------------------------------------------
crudr_uml <- '
  @startuml
  ICrud *-- Crud: implements <
  Crud <|-- Crud.Settings: extends <

  class ICrud {
    +init()
    +has()
    +create()
    +read()
    +update()
    +delete()
    +reset()
    -stopIfInterface()
  }
  class Crud {
    +main: environment
    -initial_state: list
    -stopIfEmpty()
    -createMessage(),
    -cacheInitialState()
  }
  class Crud.Settings {
    +main: function
    -cacheInitialState()
  }
  @enduml
'
# knitr::opts_knit$set(root.dir = root_dir)
uml_file <- crudr:::plantuml3(
  crudr_uml, 'Architecture', 
  filename = "vignettes/uml",
  jarfile = "lib/plantuml.jar",
  normalize = TRUE
)
# print(uml_file)
# uml_file <- normalizePath(uml_file)
# print(uml_file)
# print(getwd())
# knitr::opts_knit$set(root.dir = getwd())

## ---- eval = run---------------------------------------------------------
#  inst <- ICrud$new()
#  try(inst$init())
#  try(inst$has())
#  try(inst$create())
#  try(inst$read())
#  try(inst$update())
#  try(inst$delete())
#  try(inst$reset())

## ---- eval = run---------------------------------------------------------
#  inst <- Crud$new()
#  class(inst)
#  inherits(inst, "ICrud")

## ---- eval = run---------------------------------------------------------
#  inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))
#  
#  value <- inst$getMain()
#  as.list(value, sorted = TRUE)
#  
#  inst$setMain(as.environment(list(a = 10, b = 20)))
#  inst$getMain()$a
#  inst$getMain()$b

## ---- eval = run---------------------------------------------------------
#  inst <- Crud$new()

## ---- eval = run---------------------------------------------------------
#  inst$init(a = 1, b = 2)

## ---- eval = run---------------------------------------------------------
#  inst$has("a")
#  inst$has("b")
#  inst$has("a", "b")
#  
#  inst$has("c")
#  inst$has("a", "c")

## ---- eval = run---------------------------------------------------------
#  inst$create(c = 3)
#  inst$create(d = 4, f = 5)
#  inst$read()
#  
#  try(inst$create(c = 30))
#  inst$read()
#  ## --> c already existed with value = 3
#  inst$create(c = 30, strict = 1)
#  try(inst$create(c = 30, strict = 2))
#  try(inst$create(c = 30, strict = 3))
#  inst$create(c = 30, overwrite = TRUE)
#  inst$read()
#  ## --> c overwritten

## ---- eval = run---------------------------------------------------------
#  inst$read()
#  inst$read("a")
#  inst$read("a", "b")
#  
#  inst$read("a", "b", "x")
#  try(inst$read("a", "b", "x", strict = 3))

## ---- eval = run---------------------------------------------------------
#  inst$read("a")
#  inst$update(a = 10)
#  inst$read("a")
#  
#  inst$read("a", "b")
#  inst$update(a = 1000, b = 2000)
#  inst$read("a", "b")
#  
#  inst$update(x = 99)
#  inst$read("x")
#  
#  inst$read("a", "x")
#  inst$update(a = 1, x = 99)
#  try(inst$update(a = 1, x = 99, strict = 3))
#  inst$read("a", "x")

## ---- eval = run---------------------------------------------------------
#  inst$read()
#  inst$delete("f")
#  
#  inst$delete("d", "c")
#  inst$read()
#  
#  inst$delete("x")
#  inst$read()
#  inst$delete("b", "x")
#  try(inst$delete("b", "x", strict = 3))
#  inst$read()

## ---- eval = run---------------------------------------------------------
#  inst$reset()
#  inst$read()
#  
#  inst$reset(type = "hard")
#  inst$read()

