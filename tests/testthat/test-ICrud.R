context("ICrud: instantiate")

test_that("ICrud", {
  expect_is(inst <- ICrud$new(), "ICrud")
})

context("ICrud: getter/setter")

test_that("ICrud", {
  inst <- ICrud$new()
  expect_error(inst$getMain())
  expect_error(inst$setMain())
})

context("ICrud: CRUD")

test_that("ICrud", {
  inst <- ICrud$new()
  expect_error(inst$init())
  expect_error(inst$has())
  expect_error(inst$create())
  expect_error(inst$read())
  expect_error(inst$update())
  expect_error(inst$delete())
  expect_error(inst$reset())
})
