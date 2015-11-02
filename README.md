crudr
======

## crudr [![Build Status](https://travis-ci.org/rappster/crudr.png)](https://travis-ci.org/rappster/crudr)  
[![Coverage Status](https://img.shields.io/codecov/c/github/rappster/crudr/master.svg)](https://codecov.io/github/rappster/crudr?branch=${github_branch}) 
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/crudr)](http://cran.r-project.org/package=crudr)   
[![Downloads from the RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/crudr)](http://cran.rstudio.com/package=crudr)

## Description 

Provides generic interfaces that can be systematically
    implemented for all contexts that match a CRUD paradigm (create, read,
    update, delete).

## Purpose

Encourages reusage of code without compromising robustness. The key idea is to seperate the interface (definition of available operations/actions) from explicit implementation.

## Installation 

```
require("devtools")
devtools::install_github("rappster/crudr")
require("crudr")
```

## Examples

See vignettes: 

- [Introduction](https://github.com/rappster/crudr/tree/master/vignettes/introduction.html)
- [Implementation for `settings` package](https://github.com/rappster/crudr/tree/master/vignettes/impl_settings_package.html)
