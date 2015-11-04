#' @title
#' Base class for CRUD Operations
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#'
#' @field main \code{\link{list}}
#'  Main information
#'
#' @section Getters/setters:
#'
#' \itemize{
#'  \item{See interface} {
#'    \code{\link[crudr]{ICrud}}
#'  }
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{See interface} {
#'    \code{\link[crudr]{ICrud}}
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/example-Crud.R
#' @include interface_classes.R
#' @import R6
#' @export
Crud <- R6Class(
  classname = "Crud",
  inherit = ICrud,
  portable = TRUE,
  public = list(
    ## Fields //
    main = "environment",

    ## Methods //
    initialize = function(
      main = new.env(parent = emptyenv())
    ) {
      self$main <- main
      private$cacheInitialState()
    },
    getMain = function() {
      self$main
    },
    setMain = function(value) {
      self$main <- value
    },
    init = function(..., cache_state = TRUE) {
      values <- list(...)
      if (any(grepl("/", names(values)))) {
        inst <- as.list(self$getMain())
        ## Slower but a bit more convenient //
        inst <- as.list(self$getMain())
        # inst <- self$getMain()
        for (ii in 1:length(values)) {
          createCrudExpression(inst, id = names(values)[ii], values[[ii]],
            in_parent = TRUE)
        }

        ## Faster but can't bridge gaps yet //
#         envir <- environment()
#         for (ii in 1:length(values)) {
#           try(
#             eval(createCrudExpressionPlain(inst, names(values)[ii],
#               values[[ii]]), envir = envir),
#             silent = TRUE
#           )
#         }
        self$setMain(as.environment(inst))
      } else {
        self$setMain(as.environment(list(...)))
      }
      if (cache_state) {
        private$cacheInitialState()
      }
      TRUE
    },
    has = function(...) {
      values <- as.character(list(...))
      if (!length(values)) {
        private$stopIfEmpty()
      }
      if (any(grepl("/", values))) {
        inst <- as.list(self$getMain())
        out <- lapply(values, function(ii) {
          res <- try(eval(createCrudExpressionPlain(inst, ii)), silent = TRUE)
          !is.null(res) && !inherits(res, "try-error")
        })
      } else {
        nms <- ls(self$getMain(), all.names = TRUE)
        out <- as.list(values %in% nms)
      }
      names(out) <- values
      out
    },
    create = function(..., strict = 0:3, overwrite = FALSE) {
      strict <- as.numeric(match.arg(as.character(strict), as.character(0:3)))
      values <- list(...)
      if (!length(values)) {
        private$stopIfEmpty()
      }

      out <- as.list(rep(FALSE, length(values)))
      names(out) <- names(values)

      fun <- self$has
      idx_has <- unlist(do.call(fun, as.list(names(values))))
      # idx_has <- unlist(self$has())
      values_in <- values[idx_has]
      values_out <- values[!idx_has]

      if (length(values_in)) {
        msg <- private$createMessage(
          sprintf("invalid: %s", paste(names(values_in), collapse = ", "))
        )
        if (strict <= 2) {
          if (strict == 1) {
            message(msg)
          } else if (strict == 2) {
            warning(msg)
          }
        } else if (strict == 3) {
          stop(msg)
        }
        if (overwrite) {
#           if (any(grepl("/", names(values)))) {
#
#           } else {
            # self$update(values_in, strict = strict)
            fun <- self$update
            do.call(fun, c(values_in, list(strict = strict)))
            out[names(values_in)] <- TRUE
          # }
        }
      }

      if (length(values_out)) {
        fun <- self$init
        do.call("fun", c(self$read(), values_out, list(cache_state = FALSE)))
        ## TODO 2015-11-02: possibly inefficent, find better implementation
        out[names(values_out)]  <- TRUE
      }
      out
    },
    read = function(..., strict = 0:3) {
      strict <- as.numeric(match.arg(as.character(strict), as.character(0:3)))
      values <- as.character(list(...))
      if (!length(values)) {
        out <- as.list(self$getMain(), sorted = TRUE)
      } else {
        if (any(grepl("/", values))) {
          inst <- as.list(self$getMain())
          out <- lapply(values, function(ii) {
            res <- try(eval(createCrudExpressionPlain(inst, ii)), silent = TRUE)
            if (inherits(res, "try-error")) {
              NULL
            } else {
              res
            }
          })
          values_out <- values[sapply(out, is.null)]
          if (length(values_out)) {
            msg <- private$createMessage(
              sprintf("invalid: %s", paste(values_out, collapse = ", "))
            )
            if (strict <= 2) {
              if (strict == 1) {
                message(msg)
              } else if (strict == 2) {
                warning(msg)
              }
            } else if (strict == 3) {
              stop(msg)
            }
          }
          names(out) <- values
        } else {
          out <- vector("list", length(values))
          names(out) <- values

          # idx_has <- self$has(values)
          fun <- self$has
          idx_has <- unlist(do.call(fun, as.list(values)))
          values_in <- values[idx_has]
          values_out <- values[!idx_has]

          if (length(values_out)) {
            msg <- private$createMessage(
              sprintf("invalid: %s", paste(values_out, collapse = ", "))
            )
            if (strict <= 2) {
              if (strict == 1) {
                message(msg)
              } else if (strict == 2) {
                warning(msg)
              }
            } else if (strict == 3) {
              stop(msg)
            }
          }

          if (length(values_in)) {
            out[values_in] <- as.list(self$getMain(), sorted = TRUE)[values_in]
          }
        }
      }
      out
    },
    update = function(..., strict = 0:3) {
      strict <- as.numeric(match.arg(as.character(strict), as.character(0:3)))
      values <- list(...)
      if (!length(values)) {
        private$stopIfEmpty()
      }
      out <- as.list(rep(FALSE, length(values)))
      names(out) <- names(values)

      # idx_has <- self$has(names(values))
      fun <- self$has
      idx_has <- unlist(do.call(fun, as.list(names(values))))
      values_in <- values[idx_has]
      values_out <- values[!idx_has]

      if (length(values_out)) {
        msg <- private$createMessage(
          sprintf("invalid: %s", paste(names(values_out), collapse = ", "))
        )
        if (strict <= 2) {
          if (strict == 1) {
            message(msg)
          } else if (strict == 2) {
            warning(msg)
          }
        } else if (strict == 3) {
          stop(msg)
        }
      }

      if (length(values_in)) {
        if (any(grepl("/", names(values_in)))) {
          inst <- as.list(self$getMain())
          envir <- environment()
          lapply(1:length(values_in), function(ii) {
            res <- try(
              eval(createCrudExpressionPlain(inst, names(values_in)[ii],
                values_in[[ii]]), envir = envir),
              silent = TRUE
            )
            if (inherits(res, "try-error")) {
              # print(res)
              FALSE
            } else {
              TRUE
            }
          })
          self$setMain(as.environment(inst))
          out[names(values_in)] <- TRUE
        } else {
          envir <- self$getMain()
          sapply(1:length(values_in), function(ii) {
            assign(names(values_in)[ii], values_in[[ii]], envir = envir)
          })
          out[names(values_in)] <- TRUE
        }
      }
      out
    },
    delete = function(..., strict = 0:3) {
      strict <- as.numeric(match.arg(as.character(strict), as.character(0:3)))
      values <- as.character(list(...))
      if (!length(values)) {
        private$stopIfEmpty()
      }
      out <- as.list(rep(FALSE, length(values)))
      names(out) <- values

      # idx_has <- self$has(values)
      fun <- self$has
      idx_has <- unlist(do.call(fun, as.list(values)))
      values_in <- values[idx_has]
      values_out <- values[!idx_has]

      if (length(values_out)) {
        msg <- private$createMessage(
          sprintf("invalid: %s", paste(values_out, collapse = ", "))
        )
        if (strict <= 2) {
          if (strict == 1) {
            message(msg)
          } else if (strict == 2) {
            warning(msg)
          }
        } else if (strict == 3) {
          stop(msg)
        }
      }

      if (length(values_in)) {
        if (any(grepl("/", values))) {
          inst <- as.list(self$getMain())
          envir <- environment()
          out <- lapply(values, function(ii) {
            expr <- createCrudExpressionPlain(inst, ii, NULL, allow_null = TRUE)
            # print(expr)
            res <- try(eval(expr, envir = envir), silent = TRUE)
            if (inherits(res, "try-error")) {
              # print(res)
              FALSE
            } else {
              TRUE
            }
          })
          self$setMain(as.environment(inst))
          names(out) <- values
        } else {
          envir <- self$getMain()
          sapply(values_in, function(ii) {
            rm(list = ii, envir = envir)
          })
          out[values_in] <- TRUE
        }
      }
      out
    },
    reset = function(type = c("soft", "hard")) {
      type <- match.arg(type, c("soft", "hard"))
      if (type == "soft") {
        self$setMain(as.environment(private$initial_state))
      } else if (type == "hard") {
        envir <- self$getMain()
        sapply(ls(envir, all.names = TRUE), function(ii) {
          rm(list = ii, envir = envir)
        })
      }
      TRUE
    }
  ),
  private = list(
    initial_state = list(),
    cacheInitialState = function() {
      private$initial_state <- as.list(self$getMain(), sorted = TRUE)
    },
    stopIfEmpty = function(cl = sys.call(-1)) {
      stop(paste0(class(self)[1], ": ",
        gsub(".*\\$", "", as.character(cl)[1]),
        ": no query information"))
    },
    createMessage = function(msg = "something happened", cl = sys.call(-1)) {
      paste0(class(self)[1], ": ",
        gsub(".*\\$", "", as.character(cl)[1]), ": ", msg)
    }
  )
)
