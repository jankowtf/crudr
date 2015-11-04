
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

context("Crud: init: nested")

test_that("Crud: init: nested", {
  # Crud$debug("init")
  inst <- Crud$new()
  expect_identical(inst$init("a/b/c" = 1, "x/y/z" = 2), TRUE)
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(
      a = list(b = list(c = 1)),
      x = list(y = list(z = 2))
    )
  )
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

test_that("Crud: has: nested", {
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)

  ## Atomic + existing:
  expect_identical(inst$has("a/b/c"), list("a/b/c" = TRUE))
  ## Atomic + non-existing:
  expect_identical(inst$has("A/B/C"), list("A/B/C" = FALSE))

  ## Multiple + existing:
  expect_identical(inst$has("a/b/c", "x/y/z"),
    list("a/b/c" = TRUE, "x/y/z" = TRUE))
  ## Multiple + non-existing:
  expect_identical(inst$has("A/B/C", "X/Y/Z"),
    list("A/B/C" = FALSE, "X/Y/Z" = FALSE))
  ## Multiple with non-existing:
  expect_identical(inst$has("a/b/c", "A/B/C"),
    list("a/b/c" = TRUE, "A/B/C" = FALSE))
})

# Crud: create ---------------------------------------------------

context("Crud: create")

test_that("Crud: create", {
  # Crud$debug("create")
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

context("Crud: create: nested")

test_that("Crud: create: nested", {
  # Crud$debug("create")
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)

  ## Atomic + new:
  expect_identical(inst$create("c/b/a" = 2), list("c/b/a" = TRUE))
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(
      a = list(b = list(c = 1)),
      c = list(b = list(a = 2)),
      x = list(y = list(z = 2))
    )
  )

  ## Atomic + existing:
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  expect_identical(inst$create("a/b/c" = 10), list("a/b/c" = FALSE))
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(
      a = list(b = list(c = 1)),
      x = list(y = list(z = 2))
    )
  )

  target <- list("a/b/c" = FALSE)
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1], ": create: invalid: a/b/c"))
  expect_message(
    expect_identical(inst$create("a/b/c" = 10, strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$create("a/b/c" = 10, strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$create("a/b/c" = 10, strict = 3), target),
    pattern
  )

  ## Multiple + new:
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  expect_identical(inst$create("c/b/a" = 2, "c/b/x" = 3),
    list("c/b/a" = TRUE, "c/b/x" = TRUE))
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(
      a = list(b = list(c = 1)),
      c = list(b = list(a = 2, x = 3)),
      x = list(y = list(z = 2))
    )
  )

  ## Multiple + existing:
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  target <- list("a/b/c" = FALSE, "x/y/z" = FALSE)
  expect_identical(inst$create("a/b/c" = 10, "x/y/z" = 20), target)
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": create: invalid: a/b/c, x/y/z"))
  expect_message(
    expect_identical(inst$create("a/b/c" = 10, "x/y/z" = 20,
      strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$create("a/b/c" = 10, "x/y/z" = 20,
      strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$create("a/b/c" = 10, "x/y/z" = 20,
      strict = 3), target),
    pattern
  )

  ## Multiple + with existing:
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  target <- list("a/b/c" = FALSE, "c/b/a" = TRUE)
  expect_identical(inst$create("a/b/c" = 10, "c/b/a" = 20), target)
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(
      a = list(b = list(c = 1)),
      c = list(b = list(a = 20)),
      x = list(y = list(z = 2))
    )
  )

  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": create: invalid: a/b/c"))
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  expect_message(
    expect_identical(inst$create("a/b/c" = 10, "c/b/a" = 20,
      strict = 1), target),
    pattern
  )
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  expect_warning(
    expect_identical(inst$create("a/b/c" = 10, "c/b/a" = 20,
      strict = 2), target),
    pattern
  )
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  expect_error(
    expect_identical(inst$create("a/b/c" = 10, "c/b/a" = 20,
      strict = 3), target),
    pattern
  )

  ## Multiple + with existing + overwrite:
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  target <- list("a/b/c" = TRUE, "x/y/z" = TRUE)
  expect_identical(inst$create("a/b/c" = 10, "x/y/z" = 20,
    overwrite = TRUE), target)
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(
      a = list(b = list(c = 10)),
      x = list(y = list(z = 20))
    )
  )
  expect_identical(inst$create("a/b/c" = 100, "x/y/z" = 200,
    overwrite = TRUE), target)
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(
      a = list(b = list(c = 100)),
      x = list(y = list(z = 200))
    )
  )
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

context("Crud: read: nested")

test_that("Crud: read: nested", {
  # Crud$debug("read")
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)

  ## All:
  expect_identical(inst$read(),
    list(
      a = list(b = list(c = 1)),
      x = list(y = list(z = 2))
    )
  )

  ## Atomic + existing:
  expect_identical(inst$read("a/b/c"), list("a/b/c" = 1))

  ## Atomic and non-existing:
  expect_identical(inst$read("A/B/C"), list("A/B/C" = NULL))

  target <- list("A/B/C" = NULL)
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1], ": read: invalid: A/B/C"))
  expect_message(
    expect_identical(inst$read("A/B/C", strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$read("A/B/C", strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$read("A/B/C", strict = 3), target),
    pattern
  )

  ## Multiple + existing:
  expect_identical(inst$read("a/b/c", "x/y/z"),
    list("a/b/c" = 1, "x/y/z" = 2))

  ## Multiple with non-existing:
  expect_identical(inst$read("a/b/c", "A/B/C"),
    list("a/b/c" = 1, "A/B/C" = NULL))

  target <- list("a/b/c" = 1, "A/B/C" = NULL)
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1], ": read: invalid: A/B/C"))
  expect_message(
    expect_identical(inst$read("a/b/c", "A/B/C", strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$read("a/b/c", "A/B/C", strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$read("a/b/c", "A/B/C", strict = 3), target),
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

context("Crud: update: nested")

test_that("Crud: update: nested", {
  # Crud$debug("update")
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)

  ## Atomic + existing:
  expect_identical(inst$update("a/b/c" = 10), list("a/b/c" = TRUE))
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(
      a = list(b = list(c = 10)),
      x = list(y = list(z = 2))
    )
  )

  ## Atomic + non-existing:
  target <- list("A/B/C" = FALSE)
  expect_identical(inst$update("A/B/C" = 3), target)

  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1], ": update: invalid: A/B/C"))
  expect_message(
    expect_identical(inst$update("A/B/C" = 3, strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$update("A/B/C" = 3, strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$update("A/B/C" = 3, strict = 3), target),
    pattern
  )

  ## Multipe + existing:
  expect_identical(inst$update("a/b/c" = 10, "x/y/z" = 20),
    list("a/b/c" = TRUE, "x/y/z" = TRUE))
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(
      a = list(b = list(c = 10)),
      x = list(y = list(z = 20))
    )
  )

  ## Multipe + with non-existing:
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  target <- list("a/b/c" = TRUE, "A/B/C" = FALSE)

  expect_identical(inst$update("a/b/c" = 10, "A/B/C" = 20), target)
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(
      a = list(b = list(c = 10)),
      x = list(y = list(z = 2))
    )
  )

  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": update: invalid: A/B/C"))

  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  expect_message(
    expect_identical(inst$update("a/b/c" = 10, "A/B/C" = 20, strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$update("a/b/c" = 10, "A/B/C" = 20, strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$update("a/b/c" = 10, "A/B/C" = 20, strict = 3), target),
    pattern
  )
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

context("Crud: delete: nested")

test_that("Crud: delete: nested", {
  # Crud$debug("delete")
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)

  ## Atomic + existing:
  expect_identical(inst$delete("a/b/c"), list("a/b/c" = TRUE))
  expect_identical(inst$read(),
    list(a = list(b = structure(list(), names = character())),
      x = list(y = list(z = 2))
    )
  )

  ## Atomic + non-existing:
  target <- list("A/B/C" = FALSE)
  id <- "A/B/C"
  expect_identical(inst$delete(id), target)

  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": delete: invalid: A/B/C"))
  expect_message(
    expect_identical(inst$delete(id, strict = 1), target),
    pattern
  )
  expect_warning(
    expect_identical(inst$delete(id, strict = 2), target),
    pattern
  )
  expect_error(
    expect_identical(inst$delete(id, strict = 3), target),
    pattern
  )

  ## Multipe + existing:
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  expect_identical(inst$delete("a/b/c", "x/y/z"),
    list("a/b/c" = TRUE, "x/y/z" = TRUE))
  expect_identical(inst$read(),
    list(
      a = list(b = structure(list(), names = character())),
      x = list(y = structure(list(), names = character()))
    )
  )

  ## Multipe + with non-existing:
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)

  target <- list("a/b/c" = TRUE, "A/B/C" = FALSE)
  expect_identical(inst$delete("a/b/c", "A/B/C"), target)
  expect_identical(inst$read(),
    list(
      a = list(b = structure(list(), names = character())),
      x = list(y = list(z = 2))
    )
  )

  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  pattern <- gsub("\\.", "\\\\.", paste0(class(inst)[1],
    ": delete: invalid: A/B/C"))
  expect_message(
    expect_identical(inst$delete("a/b/c", "A/B/C", strict = 1), target),
    pattern
  )
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  expect_warning(
    expect_identical(inst$delete("a/b/c", "A/B/C", strict = 2), target),
    pattern
  )
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  expect_error(
    expect_identical(inst$delete("a/b/c", "A/B/C", strict = 3), target),
    pattern
  )
})

# Crud: reset ---------------------------------------------------

context("Crud: reset")

test_that("Crud: reset", {
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

context("Crud: reset: nested")

test_that("Crud: reset: nested", {
  inst <- Crud$new()
  inst$init("a/b/c" = 1, "x/y/z" = 2)
  inst$main$a$b$c <- 10

  ## Soft //
  expect_identical(inst$reset(), TRUE)
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(
      a = list(b = list(c = 1)),
      x = list(y = list(z = 2))
    )
  )

  inst$main$a$b$c <- 10
  expect_identical(inst$reset(type = "soft"), TRUE)
  expect_identical(as.list(inst$getMain(), sorted = TRUE),
    list(
      a = list(b = list(c = 1)),
      x = list(y = list(z = 2))
    )
  )

  ## Hard //
  inst$main$a$b$c <- 10
  expect_identical(inst$reset(type = "hard"), TRUE)
  expect_identical(as.list(inst$getMain(), sorted = TRUE), list())
})
