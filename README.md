crudr
======

[![Build Status](https://travis-ci.org/rappster/crudr.png)](https://travis-ci.org/rappster/crudr)
[![Coverage Status](https://img.shields.io/codecov/c/github/rappster/crudr/master.svg)](https://codecov.io/github/rappster/crudr?branch=${github_branch})
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%203%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/crudr)](http://cran.r-project.org/package=crudr)
[![Downloads from the RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/crudr)](http://cran.rstudio.com/package=crudr)

## Description 

Provides generic interfaces that can be systematically
    implemented for all contexts that match a CRUD paradigm (create, read,
    update, delete).

## Purpose

Encourages code reusage without compromising robustness. 

The key idea is to seperate **interfaces** (definition of available operations/actions) from explicit **implementations**.

## Installation 

```
require("devtools")
devtools::install_github("rappster/crudr")
require("crudr")
```

## Examples

```
## Instantiate + init //
inst <- Crud$new()
inst$init(a = 1, b = 2)

## Has //
inst$has("a")
inst$has("a", "b")
inst$has("a", "b", "c")

## Read //
inst$read("a")
inst$read("a", "b")
inst$read("a", "b", "c")

## Create //
inst$create(x = 10)
inst$read("x")
inst$create(x = 100, y = 200)
## --> `x` already exists, can't be created unless overwrite = TRUE
inst$read("x", "y")
inst$create(x = 100, y = 200, overwrite = TRUE)
inst$read("x", "y")

## Update //
inst$update(a = 10)
inst$read("a")
inst$update(a = 100, b = 200)
inst$read("a", "b")
inst$update(a = 1, abc = TRUE)
## --> `abc` had not been created and can thus not be updated

## Delete //
inst$delete("a")
inst$has("a")
inst$delete("b", "x")

## Reset //
inst$read()
inst$reset()
inst$read()
inst$reset("hard")
inst$read()
```

## Nested values

```
## Instantiate + init //
inst <- Crud$new()
inst$init("a/b/c" = 1, "x/y/z" = 2)

## Has //
inst$has("a")
inst$has("a", "b")
inst$has("a", "b", "x")
inst$has("a/b")
inst$has("a/b/c", "x/y")

## Read //
inst$read("a")
inst$read("a", "b")
inst$read("a", "b", "x")
inst$read("a/b")
inst$read("a/b/c")
inst$read("a/b/c", "x")

## Create //
inst$create("a/b/new_1" = 10)
inst$read("a/b/new_1")
inst$create("a/b/new_1" = 100, "a/b/new_2" = 200)
## --> `a/b/new_1` already exists, can't be created unless overwrite = TRUE
inst$read("a/b/new_1", "a/b/new_2")
inst$create("a/b/new_1" = 100, "a/b/new_2" = 200, overwrite = TRUE)
inst$read("a/b/new_1", "a/b/new_2")

## Update //
inst$update("a/b/c" = 10)
inst$read("a/b/c")
inst$update("a/b/c" = 100, "x/y/z" = 200)
inst$read("a/b/c", "x/y/z")
inst$update("a/b/c" = 1, "a/b/notthere" = TRUE)
## --> `a/b/notthere` had not been created and can thus not be updated

inst$read("a")
inst$update("a/b" = "I'm not a list anymore")
inst$read("a")

## Delete //
inst$delete("a/b")
inst$has("a/b")
inst$delete("a", "x/y/z")

## Reset //
inst$read()
inst$reset()
inst$read()
inst$reset("hard")
inst$read()
```

## Flexible input format

Also see vignette [Handling threedots input in batch scenarios](https://github.com/rappster/crudr/tree/master/vignettes/gist_threedots.Rmd) (rendered versions available in R via `vignette("gist_threedots", package = "crudr")`)

```
## Instantiate + init //
inst <- Crud$new()
values <- list(e = 1, "e/foo/bar" = 1)
inst$init(a = 1, "b/foo/bar" = 1, list(c = 1, "d/foo/bar" = 1), values)

## Has //
inst$has("a", "b/foo", "c", "d/foo/bar", "e", "e/foo", "x", "b/foobar", "d/foo/bar/something")
values <- list("e", "e/foo")
inst$has("a", "b/foo", list("c", "d/foo/bar"), values, "x", "b/foobar", "d/foo/bar/something")

## Read //
inst$read("a", "b/foo", "c", "d/foo/bar", "e", "e/foo", "x", "b/foobar", "d/foo/bar/something")
values <- list("e", "e/foo")
inst$read("a", "b/foo", list("c", "d/foo/bar"), values, "x", "b/foobar", "d/foo/bar/something")

## Create //
values <- list("z" = 1, "zz/foo/bar" = 1)
inst$create("x" = 1, "xx/foo/bar" = 1, list("y" = 1, "yy/foo/bar" = 1), values)
inst$read()

## Update //
values <- list("z" = 10, "zz/foo/bar" = 10)
inst$update("x" = 10, "xx/foo/bar" = 10, list("y" = 10, "yy/foo/bar" = 10), values)
inst$read()

## Delete //
values <- list("z", "zz/foo/bar")
inst$delete("x", "xx/foo/bar", list("y", "yy/foo/bar"), values)
inst$read()

## Reset //
inst$read()
inst$reset()
inst$read()
inst$reset("hard")
inst$read()
```

## Further examples

See vignettes: 

- [Introduction](https://github.com/rappster/crudr/tree/master/vignettes/introduction.Rmd) (rendered versions available in R via `vignette("introduction", package = "crudr")`)
  
- [Implementation for package `settings`](https://github.com/rappster/crudr/tree/master/vignettes/impl_settings_package.Rmd) (rendered versions available in R via `vignette("impl_settings_package", package = "crudr")`)
    
- [Nestedness](https://github.com/rappster/crudr/tree/master/vignettes/nestedness.Rmd) (rendered versions available in R via `vignette("nestedness", package = "crudr")`)    

- [Handling threedots input in batch scenarios](https://github.com/rappster/crudr/tree/master/vignettes/gist_threedots.Rmd) (rendered versions available in R via `vignette("gist_threedots", package = "crudr")`)
