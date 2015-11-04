context("uml")

test_that("UML stuff", {
  skip("R CMD check has problems with jar file")
  library(testhelper)
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
  jarfile <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/plantuml.txt")
  )
  file.exists(jarfile)
  jarfile <- normalizePath(readLines(jarfile))
  # print(getwd())
  # print(list.files(dirname(jarfile)))
  expect_is(res <- plantuml3(crudr_uml, 'Architecture', jarfile = jarfile),
    "character")
  expect_true(file.exists(res))
  expect_true(file.exists(gsub("\\.png$", ".uml", res)))
})
