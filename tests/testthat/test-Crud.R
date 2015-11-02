
# Crud: instantiate ----------------------------------------------

context("Crud: instantiate")

test_that("Crud", {
  expect_is(inst <- Crud$new(), "Crud")
  expect_true(inherits(inst, "ICrud"))
  expect_is(inst$main, "environment")

  target <- list(a = 1, b = 2, c = 3)
  main <- as.environment(target)
  expect_is(inst <- Crud$new(main = main), "Crud")
  expect_true(inherits(inst, "ICrud"))
  expect_identical(as.list(inst$main, sorted = TRUE), target)
})

# Crud: getter/setter --------------------------------------------

context("Crud: getter/setter")

test_that("Crud", {
  main <- as.environment(list(a = 1, b = 2, c = 3))
  inst <- Crud$new(main = main)
  expect_identical(inst$getMain(), main)

  main <- as.environment(list(a = 10, b = 20, c = 30))
  expect_identical(inst$setMain(main), main)
  expect_identical(inst$getMain(), main)
})

# Crud: init -----------------------------------------------------

context("Crud: init")

test_that("Crud: init", {
  inst <- Crud$new()
  expect_identical(inst$init(a = 1, b = 2), TRUE)
  expect_identical(as.list(inst$getMain(), sorted = TRUE), list(a = 1, b = 2))
})

# Crud: has ------------------------------------------------------

context("Crud: has")

test_that("Crud: has", {
  main <- as.environment(list(a = 1, b = 2))

  inst <- Crud$new(main = main)
  ## Atomic + existing:
  expect_identical(inst$has("a"), list(a = TRUE))
  ## Atomic + non-existing:
  expect_identical(inst$has("c"), list(c = FALSE))

  ## Multiple + existing:
  expect_identical(inst$has("a", "b"), list(a = TRUE, b = TRUE))
  ## Multiple + non-existing:
  expect_identical(inst$has("c", "d"), list(c = FALSE, d = FALSE))
  ## Multiple with non-existing:
  expect_identical(inst$has("a", "c"), list(a = TRUE, c = FALSE))

  ## Empty //
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": has: no query information"))
  expect_error(inst$has(), pattern)
})

# Crud: create ---------------------------------------------------

context("Crud: create")

test_that("Crud: create", {
  main <- as.environment(list(a = 1))
  inst <- Crud$new(main = main)

  ## Atomic + new:
  expect_identical(inst$create(b = 2), list(b = TRUE))
  expect_identical(as.list(inst$getMain(), sorted = TRUE), list(a = 1, b = 2))

  ## Atomic + existing:
  inst <- Crud$new(main = main)
  expect_identical(inst$create(a = 10), list(a = FALSE))
  expect_identical(as.list(inst$getMain(), sorted = TRUE), list(a = 1))

  target <- list(a = FALSE)
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1], ": create: invalid: a"))
  expect_message(
    expect_identical(inst$create(a = 10, strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$create(a = 10, strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$create(a = 10, strict = 3), target),
    pattern
  )

  ## Multiple + new:
  inst <- Crud$new(main = main)
  expect_identical(inst$create(b = 2, c = 3), list(b = TRUE, c = TRUE))
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(a = 1, b = 2, c = 3))

  ## Multiple + existing:
  inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))
  target <- list(a = FALSE, b = FALSE)
  expect_identical(inst$create(a = 10, b = 20), target)
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": create: invalid: a, b"))
  expect_message(
    expect_identical(inst$create(a = 10, b = 20, strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$create(a = 10, b = 20, strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$create(a = 10, b = 20, strict = 3), target),
    pattern
  )

  ## Multiple + with existing:
  inst <- Crud$new(main = as.environment(list(a = 1)))
  target <- list(a = FALSE, b = TRUE)
  expect_identical(inst$create(a = 10, b = 20), target)
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(a = 1, b = 20))

  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": create: invalid: a"))
  inst <- Crud$new(main = as.environment(list(a = 1)))
  expect_message(
    expect_identical(inst$create(a = 10, b = 20, strict = 1), target),
    pattern
  )
  inst <- Crud$new(main = as.environment(list(a = 1)))
  expect_warning(
    expect_identical(inst$create(a = 10, b = 20, strict = 2), target),
    pattern
  )
  inst <- Crud$new(main = as.environment(list(a = 1)))
  expect_error(
    expect_identical(inst$create(a = 10, b = 20, strict = 3), target),
    pattern
  )

  ## Multiple + with existing + overwrite:
  inst <- Crud$new(main = as.environment(list(a = 1)))
  target <- list(a = TRUE, b = TRUE)
  expect_identical(inst$create(a = 10, b = 20, overwrite = TRUE), target)
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(a = 10, b = 20))
  expect_identical(inst$create(a = 100, b = 200, overwrite = TRUE), target)
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(a = 100, b = 200))

  ## Empty //
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": create: no query information"))
  expect_error(inst$create(), pattern)
})

# Crud: read -----------------------------------------------------

context("Crud: read")

test_that("Crud: read", {
  main <- as.environment(list(a = 1, b = 2))
  inst <- Crud$new(main = main)

  ## All:
  expect_identical(inst$read(), list(a = 1, b = 2))

  ## Atomic + existing:
  expect_identical(inst$read("a"), list(a = 1))

  ## Atomic and non-existing:
  expect_identical(inst$read("c"), list(c = NULL))

  target <- list(c = NULL)
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1], ": read: invalid: c"))
  expect_message(
    expect_identical(inst$read("c", strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$read("c", strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$read("c", strict = 3), target),
    pattern
  )

  ## Multiple + existing:
  expect_identical(inst$read("a", "b"), list(a = 1, b = 2))

  ## Multiple with non-existing:
  expect_identical(inst$read("a", "c"), list(a = 1, c = NULL))

  target <- list(a = 1, c = NULL)
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1], ": read: invalid: c"))
  expect_message(
    expect_identical(inst$read("a", "c", strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$read("a", "c", strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$read("a", "c", strict = 3), target),
    pattern
  )
})

# Crud: update ---------------------------------------------------

context("Crud: update")

test_that("Crud: update", {
  main <- as.environment(list(a = 1, b = 2))
  inst <- Crud$new(main = main)

  ## Atomic + existing:
  expect_identical(inst$update(a = 10), list(a = TRUE))
  expect_identical(as.list(inst$getMain(), sorted = TRUE), list(a = 10, b = 2))

  ## Atomic + non-existing:
  target <- list(c = FALSE)
  expect_identical(inst$update(c = 3), target)

  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1], ": update: invalid: c"))
  expect_message(
    expect_identical(inst$update(c = 3, strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$update(c = 3, strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$update(c = 3, strict = 3), target),
    pattern
  )

  ## Multipe + existing:
  expect_identical(inst$update(a = 10, b = 20), list(a = TRUE, b = TRUE))
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(a = 10, b = 20))

  ## Multipe + with non-existing:
  target <- list(a = TRUE, c = FALSE)
  inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))
  expect_identical(inst$update(a = 10, c = 20), target)
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(a = 10, b = 2))

  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1], ": update: invalid: c"))

  inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))
  expect_message(
    expect_identical(inst$update(a = 10, c = 20, strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$update(a = 10, c = 20, strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$update(a = 10, c = 20, strict = 3), target),
    pattern
  )

  ## Empty //
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": update: no query information"))
  expect_error(inst$update(), pattern)
})

# Crud: delete ---------------------------------------------------

context("Crud: delete")

test_that("Crud: delete", {
  inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))

  ## Atomic + existing:
  expect_identical(inst$delete("a"), list(a = TRUE))
  expect_identical(inst$read(), list(b = 2))

  ## Atomic + non-existing:
  target <- list(c = FALSE)
  expect_identical(inst$delete("c"), target)

  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": delete: invalid: c"))
  expect_message(
    expect_identical(inst$delete("c", strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$delete("c", strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$delete("c", strict = 3), target),
    pattern
  )

  ## Multipe + existing:
  inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))
  expect_identical(inst$delete("a", "b"), list(a = TRUE, b = TRUE))
  expect_identical(inst$read(), list())

  ## Multipe + with non-existing:
  target <- list(a = TRUE, c = FALSE)
  inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))
  expect_identical(inst$delete("a", "c"), target)
  expect_identical(inst$read(), list(b = 2))

  inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": delete: invalid: c"))
  expect_message(
    expect_identical(inst$delete("a", "c", strict = 1), target),
    pattern
  )
  inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))
  expect_warning(
    expect_identical(inst$delete("a", "c", strict = 2), target),
    pattern
  )
  inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))
  expect_error(
    expect_identical(inst$delete("a", "c", strict = 3), target),
    pattern
  )

  ## Empty //
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": delete: no query information"))
  expect_error(inst$delete(), pattern)
})

# Crud: reset ---------------------------------------------------

context("Crud: reset")

test_that("Crud: delete", {
  inst <- Crud$new(main = as.environment(list(a = 1, b = 2)))
  inst$main$a <- 10

  ## Soft //
  expect_identical(inst$reset(), TRUE)
  expect_identical(as.list(inst$getMain(), sorted = TRUE), list(a = 1, b = 2))

  inst$main$a <- 10
  expect_identical(inst$reset(type = "soft"), TRUE)
  expect_identical(as.list(inst$getMain(), sorted = TRUE), list(a = 1, b = 2))

  ## Hard //
  inst$main$a <- 10
  expect_identical(inst$reset(type = "hard"), TRUE)
  expect_identical(as.list(inst$getMain(), sorted = TRUE), list())
})
