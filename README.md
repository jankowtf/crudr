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
inst <- Crud$new()
inst$init(a = 1, b = 2)

inst$has("a")
inst$has("a", "b")
inst$has("a", "b", "c")

inst$read("a")
inst$read("a", "b")
inst$read("a", "b", "c")

inst$create(x = 10)
inst$read("x")
inst$create(x = 100, y = 200)
inst$read("x", "y")
inst$create(x = 100, y = 200, overwrite = TRUE)
inst$read("x", "y")

inst$update(a = 10)
inst$read("a")
inst$update(a = 100, b = 200)
inst$read("a", "b")
inst$update(a = 1, abc = TRUE)
## --> `abc` had not been created and can thus not be updated

inst$delete("a")
inst$has("a")
inst$delete("b", "x")

inst$read()
inst$reset()
inst$read()
inst$reset("hard")
inst$read()
```

## Further examples

See vignettes: 

- [Introduction](https://github.com/rappster/crudr/tree/master/vignettes/introduction.Rmd):
    Run `vignette("introduction", package = "crudr")`
  
- [Implementation for package `settings`](https://github.com/rappster/crudr/tree/master/vignettes/impl_settings_package.Rmd):
    Run `vignette("impl_settings_package", package = "crudr")`
    
- [Nestedness](https://github.com/rappster/crudr/tree/master/vignettes/nestedness.Rmd):
    Run `vignette("nestedness", package = "crudr")`    
