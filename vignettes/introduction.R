## ---- include = FALSE----------------------------------------------------
library(crudr)
stand_alone_mode <- TRUE
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

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
uml_file <- crudr:::plantuml3(crudr_uml, 'Architecture', filename = "uml")

## ---- eval = stand_alone_mode--------------------------------------------
inst <- ICrud$new()
try(inst$init())
try(inst$has())
try(inst$create())
try(inst$read())
try(inst$update())
try(inst$delete())
try(inst$reset())

## ---- eval = stand_alone_mode--------------------------------------------
inst <- Crud$new()
class(inst)
inherits(inst, "ICrud")

## ---- eval = stand_alone_mode--------------------------------------------
inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))

value <- inst$getMain()
as.list(value, sorted = TRUE)

inst$setMain(as.environment(list(a = 10, b = 20)))
inst$getMain()$a
inst$getMain()$b

## ---- eval = stand_alone_mode--------------------------------------------
inst <- Crud$new()

## ---- eval = stand_alone_mode--------------------------------------------
inst$init(a = 1, b = 2)

## ---- eval = stand_alone_mode--------------------------------------------
inst$has("a")
inst$has("b")
inst$has("a", "b")

inst$has("c")
inst$has("a", "c")

## ---- eval = stand_alone_mode--------------------------------------------
inst$create(c = 3)
inst$create(d = 4, f = 5)
inst$read()

try(inst$create(c = 30))
inst$read()
## --> c already existed with value = 3
inst$create(c = 30, strict = 1)
try(inst$create(c = 30, strict = 2))
try(inst$create(c = 30, strict = 3))
inst$create(c = 30, overwrite = TRUE)
inst$read()
## --> c overwritten

## ---- eval = stand_alone_mode--------------------------------------------
inst$read()
inst$read("a")
inst$read("a", "b")

inst$read("a", "b", "x")
try(inst$read("a", "b", "x", strict = 3))

## ---- eval = stand_alone_mode--------------------------------------------
inst$read("a")
inst$update(a = 10)
inst$read("a")

inst$read("a", "b")
inst$update(a = 1000, b = 2000)
inst$read("a", "b")

inst$update(x = 99)
inst$read("x")

inst$read("a", "x")
inst$update(a = 1, x = 99)
try(inst$update(a = 1, x = 99, strict = 3))
inst$read("a", "x")

## ---- eval = stand_alone_mode--------------------------------------------
inst$read()
inst$delete("f")

inst$delete("d", "c")
inst$read()

inst$delete("x")
inst$read()
inst$delete("b", "x")
try(inst$delete("b", "x", strict = 3))
inst$read()

## ---- eval = stand_alone_mode--------------------------------------------
inst$reset()
inst$read()

inst$reset(type = "hard")
inst$read()

