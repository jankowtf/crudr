
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
#' @include base_classes.R
#' @import R6
#' @importFrom settings options_manager
#' @export
Crud.Settings <- R6Class(
  classname = "Crud.Settings",
  inherit = Crud,
  portable = TRUE,
  public = list(
    initialize = function(
      ...,
      main = settings::options_manager()
    ) {
      super$initialize(...)
      self$main <- main
      private$cacheInitialState()
    },
    has = function(...) {
      values <- as.character(list(...))
      if (!length(values)) {
        private$stopIfEmpty()
      }
      nms <- names(self$getMain()())
      out <- as.list(values %in% nms)
      names(out) <- values
      out
    },
    init = function(..., cache_state = TRUE) {
      self$setMain(settings::options_manager(...))
      if (cache_state) {
        private$cacheInitialState()
      }
      TRUE
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
          # self$update(values_in, strict = strict)
          fun <- self$update
          do.call(fun, c(values_in, list(strict = strict)))
          out[names(values_in)] <- TRUE
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
        out <- self$getMain()()
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
          out[values_in] <- self$getMain()(values_in)
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
        fun <- self$getMain()
        do.call("fun", values_in)
        out[names(values_in)] <- TRUE
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
        current <- self$read()
        new <- setdiff(names(current), values_in)
        if (length(new)) {
          self$reset(type = "hard")
          fun <- self$create
          do.call("fun", c(current[new], list(overwrite = TRUE)))
        } else {
          self$reset(type = "hard")
        }
        out[values_in] <- TRUE
      }
      out
    },
    reset = function(type = c("soft", "hard")) {
      type <- match.arg(type, c("soft", "hard"))
      if (type == "soft") {
        # settings::reset(self$getMain())
        fun <- self$init
        do.call(fun, c(private$initial_state, list(cache_state = FALSE)))
      } else if (type == "hard") {
        self$setMain(settings::options_manager())
      }
      TRUE
    }
  ),
  private = list(
    cacheInitialState = function() {
      private$initial_state <- try(self$getMain()(), silent = TRUE)
      ## TODO 2015-11-02: figure out the implications of inheritance and
      ## auto-execution of methods when instances of superclasses are created.
      ## On calling `$initialize()`, instance of superclass seems to be created
      ## which results in problems with `self$getMain()()` as in the superclass
      ## `main` is an environment and not the return value of
      ## `settings::options_manager()`
    }
  )
)
